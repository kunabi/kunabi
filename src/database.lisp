(in-package :kunabi)

;;(defparameter *q* (make-instance 'queue))
(defvar *h* (thread-safe-hash-table))
(defvar *db* nil)
(defvar *pcallers* 5)
(defvar *files* nil)
(defvar *conn* nil)
(defvar syncing nil)

(defparameter to-db (pcall-queue:make-queue))

(defvar *fields* '(
		   :additionalEventData
		   :awsRegion
		   :errorCode
		   :errorMessage
		   :eventID
		   :eventName
		   :eventSource
		   :eventTime
		   :eventType
		   :eventVersion
		   :recipientAccountId
		   :requestID
		   :requestParameters
		   :resources
		   :responseElements
		   :sourceIPAddress
		   :userAgent
		   :userIdentity
		   :userName
		   ))

(defun db-have-we-seen-this-file (file)
  (cond
    ((equal :sqlite *db-backend*) (sqlite-have-we-seen-this-file file))
    ((equal :postgres *db-backend*)(sqlite-have-we-seen-this-file file))
    ((equal :manardb *db-backend*)(manardb-have-we-seen-this-file file))
    (t (format t "unknown *db-backend*:~A~%" *db-backend*))))

(defun db-mark-file-processed (file)
  (cond
    ((equal :sqlite *db-backend*) (sqlite-mark-file-processed file))
    ((equal :postgres *db-backend*)(psql-mark-file-processed file))
    ((equal :manardb *db-backend*)(manardb-mark-file-processed file))
    (t (format t "unknown *db-backend*:~A~%" *db-backend*))))

(defun db-recreate-tables (db)
  (cond
    ((equal :sqlite *db-backend*) (sqlite-recreate-tables))
    ((equal :postgres *db-backend*)(psql-recreate-tables))
    ((equal :manardb *db-backend*)(manardb-recreate-tables))
    (t (format t "unknown *db-backend*:~A~%" *db-backend*))))

(defun normalize-insert (record)
  (cond
    ((equal :sqlite *db-backend*) (sqlite-normalize-insert record))
    ((equal :postgres *db-backend*)(psql-normalize-insert record))
    ((equal :manardb *db-backend*)(manardb-normalize-insert record))
    (t (format t "unknown *db-backend*:~A~%" *db-backend*))))

(defun db-get-or-insert-id (table value)
  (cond
    ((equal :sqlite *db-backend*) (sqlite-get-or-insert-id table value))
    ((equal :postgres *db-backend*)(psql-get-or-insert-id table value))
    ((equal :manardb *db-backend*)(manardb-get-or-insert-id table value))
    (t (format t "unknown *db-backend*:~A~%" *db-backend*))))

(defun db-do-query (query)
  (cond
    ((equal :sqlite *db-backend*) (sqlite-do-query query))
    ((equal :postgres *db-backend*)(psql-do-query query))
    ((equal :manardb *db-backend*)(manardb-do-query query))
    (t (format t "unknown *db-backend*:~A~%" *db-backend*))))

(defun db-drop-table (query)
  (cond
    ((equal :sqlite *db-backend*) (sqlite-drop-table query))
    ((equal :postgres *db-backend*)(psql-drop-table query))
    ((equal :manardb *db-backend*)(manardb-drop-table query))
    (t (format t "unknown *db-backend*:~A~%" *db-backend*))))

(defun make-safe-string (str)
  (if (stringp str)
      (replace-all str "'" "")
      str))

(defun process-record (record fields)
  (loop for i in fields
     collect (make-safe-string (get-value i record))))

(defun get-value (field record)
  (cond
    ((equal :accessKeyId field)(fetch-value '(:|userIdentity| :|accessKeyId|) record))
    ((equal :additionalEventData field)(getf record :|additionalEventData|))
    ((equal :awsRegion field)(getf record :|awsRegion|))
    ((equal :errorCode field)(getf record :|errorCode|))
    ((equal :errorMessage field)(getf record :|errorMessage|))
    ((equal :eventID field)(getf record :|eventID|))
    ((equal :eventName field)(getf record :|eventName|))
    ((equal :eventSource field)(getf record :|eventSource|))
    ((equal :eventTime field)(getf record :|eventTime|))
    ((equal :eventType field)(getf record :|eventType|))
    ((equal :eventVersion field)(getf record :|eventVersion|))
    ((equal :recipientAccountId field)(getf record :|recipientAccountId|))
    ((equal :requestID field)(getf record :|requestID|))
    ((equal :requestParameters field)(getf record :|requestParameters|))
    ((equal :resources field)(getf record :|resources|))
    ((equal :responseElements field)(getf record :|responseElements|))
    ((equal :sourceIPAddress field)(get-hostname-by-ip (getf record :|sourceIPAddress|)))
    ((equal :userAgent field)(getf record :|userAgent|))
    ((equal :userIdentity field)(getf record :|userIdentity|))
    ((equal :userName field)(fetch-value '(:|userIdentity| :|sessionContext| :|sessionIssuer| :|userName|) record))
    ;;((equal :userName field)(fetch-value '(:|userIdentity| :|userName|) record))
    (t (format nil "Unknown arg:~A~%" field))))


(defun try-twice (table query)
  (let ((val (or
	      (ignore-errors (db-get-or-insert-id table query))
	      (db-get-or-insert-id table query))))
    val))

;;create unique index concurrently if not exists event_names_idx1 on event_names(id)

(defun get-index-value (table value)
  (let ((one (ignore-errors (db-get-or-insert-id table value))))
    (unless (typepc one 'integer)
      (setf one (db-get-or-insert-id table value)))
    one))

(defun get-tables()
  (format nil "~{~A~^, ~}" *fields*))

(defun load-file-values ()
  (unless *files*
    (setf *files*
	  (db-do-query "select value from files"))
    (mapcar #'(lambda (x)
		(setf (gethash (car x) *h*) t))
	    *files*))
  *h*)

(defun emit-drain-file (queue)
  "Dump the queue to a csv file for import to postgres"
  (format t "Draining ~A log entries into postgres...~%" (pcall-queue:queue-length queue))
  (with-open-file (drain "/tmp/loadme.txt" :direction :output :if-exists :supersede)
    (format drain "\COPY log(~{~A~^, ~}) FROM STDIN;~%" *fields*)
    (loop while (not (pcall-queue:queue-empty-p queue))
       do (progn
	    (format drain "~A~%" (pcall-queue:queue-pop queue))))
    (format drain "\\.~%"))
  (uiop:run-program (format nil "cat /tmp/loadme.txt|psql -U kunabi -d kunabi"))
  (format t "Draining complete.~%"))

(defun periodic-sync ()
  (if (null syncing)
      (progn
	(setf syncing t)
	;;(psql-commit)
	(let ((q-len (pcall-queue:queue-length to-db)))
	  (format t "Sync limit of ~A hit." q-len)
	  (emit-drain-file to-db)
	  ;;(psql-begin)
	  (setf syncing nil))
	(format t "sync already running...~%"))))

(defun block-if-syncing ()
  (loop while syncing
     do (progn
	  (format t "s")
	  (sleep 1))))
