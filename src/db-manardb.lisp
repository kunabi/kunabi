(in-package :kunabi)
(defvar *manard-files* (thread-safe-hash-table))
(defvar *kunabi-fields* (thread-safe-hash-table))
(defvar *kunabi-counters* (thread-safe-hash-table))
(defvar *kunabi-need-files* nil)


(defvar ct-fields '(
		    kunabi::additionalEventData
		    kunabi::awsRegion
		    kunabi::errorCode
		    kunabi::errorMessage
		    ;;kunabi::eventID
		    kunabi::eventName
		    kunabi::eventSource
		    ;;kunabi::eventTime
		    kunabi::eventType
		    kunabi::eventVersion
		    kunabi::recipientAccountId
		    ;;kunabi::requestID
		    ;;kunabi::requestParameters
		    kunabi::resources
		    ;;kunabi::responseElements
		    kunabi::sourceIPAddress
		    kunabi::userAgent
		    ;;kunabi::userIdentity
		    kunabi::userName
		    ))

(defun init-manardb()
  (unless (boundp 'manardb:use-mmap-dir)
    (manardb:use-mmap-dir (or (uiop:getenv "KUNABI") "~/ct-manardb/")))
  (if (and (eql (hash-table-count *manard-files*) 0) *kunabi-need-files*)
      (allocate-file-hash)))

(manardb:defmmclass files ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass additionalEventData ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass awsRegion ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass errorCode ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass errorMessage ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass eventID ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass eventName ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass eventSource ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass eventTime ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass eventType ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass eventVersion ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass recipientAccountId ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass requestID ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass requestParameters ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass resources ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass responseElements ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass sourceIPAddress ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass userAgent ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass userIdentity ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass userName ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass ct ()
  ((addionalEventData :initarg :additionalEventData :accessor additionalEventData)
   (awsRegion :initarg :awsRegion :accessor awsRegion)
   (errorCode :initarg :errorCode :accessor errorCode)
   (errorMessage :initarg :errorMessage :accessor errorMessage)
   (eventID :initarg :eventID :accessor eventID)
   (eventName :initarg :eventName :accessor eventName)
   (eventSource :initarg :eventSource :accessor eventSource)
   (eventTime :initarg :eventTime :accessor eventTime)
   (eventType :initarg :eventType :accessor eventType)
   (eventVersion :initarg :eventVersion :accessor eventVersion)
   (recipientAccountId :initarg :recipientAccountId :accessor recipientAccountId)
   (requestID :initarg :requestID :accessor requestID)
   (requestParameters :initarg :requestParameters :accessor requestParameters)
   (resources :initarg :resources :accessor resources)
   (responseElements :initarg :responseElements :accessor responseElements)
   (sourceIPAddress :initarg :sourceIPAddress :accessor sourceIPAddress)
   (userAgent :initarg :userAgent :accessor userAgent)
   (userIdentity :initarg :userIdentity :accessor userIdentity)
A   (userName :initarg :userName :accessor username)
   ))

(defun create-klass-hash (klass)
  (multiple-value-bind (id seen)
      (gethash klass *kunabi-fields*)
    (unless seen
      (setf (gethash klass *kunabi-fields*)
	    (thread-safe-hash-table)))))

;;(fare-memoization:define-memo-function get-obj (klass new-value)

(defun get-obj (klass new-value)
  "Return the object for a given value of klass"
  (let ((obj nil))
    (unless (or (null klass) (null new-value))
      (progn
	(create-klass-hash klass)
	(multiple-value-bind (id seen)
	    (gethash new-value (gethash klass *kunabi-fields*))
	  (if seen
	      (setf obj id)
	      (progn
		(setf obj (make-instance klass :value new-value))
		(setf (gethash new-value (gethash klass *kunabi-fields*)) obj))))))
    ;;(format t "get-obj: klass:~A new-value:~A obj:~A seen:~A id:~A~%" klass new-value obj seen id))))
    obj))

(defun manardb-have-we-seen-this-file (file)
  (let ((name (get-filename-hash file)))
    (multiple-value-bind (id seen)
	(gethash name *manard-files*)
      seen)))

(defun manardb-get-files (file)
  (format t "manardb-get-files:~A~%" file)
  (remove-if-not
   (lambda (x) (string-equal
		(get-filename-hash file)
		(slot-value x :value2)))
   (manardb:retrieve-all-instances 'kunabi::files)))

(defun manardb-mark-file-processed (file)
  (let ((name (get-filename-hash file)))
    (setf (gethash name *manard-files*) t)
    (make-instance 'files :value name :idx 1)))

(defun allocate-file-hash ()
  (manardb:doclass (x 'kunabi::files :fresh-instances nil)
    (setf (gethash (slot-value x 'value) *manard-files*) t)))

(defun allocate-klass-hash (klass)
  (or (hash-table-p (gethash klass *kunabi-fields*))
      (progn
	(format t "allocating class:~A~%" klass)
	(create-klass-hash klass)
	(manardb:doclass (x klass :fresh-instances nil)
	  (with-slots (value idx) x
	    (setf (gethash value
			   (gethash klass *kunabi-fields*)) idx)))
	(setf (gethash klass *kunabi-counters*)
	      (get-max-id-from-hash
	       (gethash klass *kunabi-fields*))))))

(defun get-max-id-from-hash (hash)
  ;;(format t "hash: ~A~%" hash)
  (let* ((idxs (alexandria:hash-table-values hash))
	 (max-id 0))
    ;;(format t "idxs: ~A~%" idxs)
    (and idxs
	 (setf max-id
	       (apply #'max
		      (mapcar #'(lambda (x)
				  (if (stringp x) (parse-integer x) x)) idxs))))
    max-id))

(defun init-ct-hashes ()
  (mapc
   #'(lambda (x)
       (allocate-klass-hash x))
   ct-fields))

(defun get-stats ()
  (format t "Totals ct:~A files:~A flows:~A vpc-files:~A ec:~A srcaddr:~A dstaddr:~A srcport:~A dstport:~A protocol:~A~%"
	  (manardb:count-all-instances 'kunabi::ct)
	  (manardb:count-all-instances 'kunabi::files)
	  (manardb:count-all-instances 'kunabi::flow)
	  (manardb:count-all-instances 'kunabi::flow-files)
	  (manardb:count-all-instances 'kunabi::errorCode)
	  (manardb:count-all-instances 'kunabi::srcaddr)
	  (manardb:count-all-instances 'kunabi::dstaddr)
	  (manardb:count-all-instances 'kunabi::srcport)
	  (manardb:count-all-instances 'kunabi::dstport)
	  (manardb:count-all-instances 'kunabi::protocol)
	  ))

(defun find-username (userIdentity)
  (let ((a (fetch-value '(:|sessionContext| :|sessionIssuer| :|userName|) userIdentity))
	(b (fetch-value '(:|sessionContext| :|userName|) userIdentity))
	(c (fetch-value '(:|userName|) userIdentity))
	(d (car (last (cl-ppcre:split ":" (fetch-value '(:|arn|) userIdentity)))))
	(e (fetch-value '(:|type|) userIdentity))
	(len (length userIdentity)))
    ;;(format t "a: ~A b:~A c:~A d:~A len:~A username:~A" a b c d len username)
    (or a b c d e)))

;;(cl-ppcre:regex-replace #\newline 'userIdentity " "))))))

(defun get-unique-values (klass)
  "Return unique list of klass objects"
  (manardb:doclass (x klass :fresh-instances nil)
    (with-slots (value idx) x
      (format t "~%~A: ~A" idx value))))

(defun get-unique-values-list (klass)
  "Return unique list of klass objects"
  (let ((results '()))
    (manardb:doclass (x klass :fresh-instances nil)
      (with-slots (value idx) x
	(push value results)))
    results))





;; lists
(defun get-ct-files ()
  "Return unique list of ct files"
  (get-unique-values 'kunabi::files))

(defun get-event-list ()
  "Return unique list of events"
  (get-unique-values 'kunabi::eventname))

(defun get-errorcode-list ()
  "Return unique list of events"
  (get-unique-values 'kunabi::errorcode))

(defun get-name-list ()
  "Return unique list of events"
  (get-unique-values 'kunabi::username))

(defun get-sourceips-list ()
  "Return unique list of events"
  (get-unique-values 'kunabi::sourceIPAddress))

(defun get-val (obj)
  (if (null obj)
      obj
      (slot-value obj 'value)))

(defun get-obj-by-val (klass val)
  (let ((obj-list nil))
    (manardb:doclass (x klass :fresh-instances nil)
      (with-slots (value) x
	(if (string-equal val value)
	    (push x obj-list))))
    obj-list))

(defun ct-get-by-klass-value  (klass value &optional inverse)
  (format t "~{~A~}" (ct-get-by-klass-value-real (klass value inverse))))

(defun ct-get-by-klass-value-real (klass value &optional inverse)
  (allocate-klass-hash klass)
  (let* ((results '())
	 (klass-hash (gethash klass *kunabi-fields*))
	 (slotv nil))
    (multiple-value-bind (id seen)
	(gethash value klass-hash)
      (when (or seen (and inverse (null value)))
	(manardb:doclass (x 'kunabi::ct :fresh-instances nil)
	  (with-slots (userName eventTime eventName eventSource sourceIPAddress userAgent errorMessage errorCode userIdentity) x
	    (cond
	      ((equal (find-class klass) (find-class 'kunabi::userName)) (setf slotv userName))
	      ((equal (find-class klass) (find-class 'kunabi::eventName)) (setf slotv eventName))
	      ((equal (find-class klass) (find-class 'kunabi::eventSource)) (setf slotv eventSource))
	      ((equal (find-class klass) (find-class 'kunabi::sourceIPAddress)) (setf slotv sourceIPAddress))
	      ((equal (find-class klass) (find-class 'kunabi::errorMessage)) (setf slotv errorMessage))
	      ((equal (find-class klass) (find-class 'kunabi::errorCode)) (setf slotv errorCode)))
	    (when slotv
	      (push
	       (format nil "|~A|~A|~A|~A|~A|~A|~A|~A|~A|~%"
		       (get-val-by-idx 'kunabi::eventTime eventTime)
		       (get-val-by-idx 'kunabi::eventName eventName)
		       (get-val-by-idx 'kunabi::userName userName)
		       (get-val-by-idx 'kunabi::eventSource eventSource)
		       (get-val-by-idx 'kunabi::sourceIPAddress sourceIPAddress)
		       (get-val-by-idx 'kunabi::userAgent userAgent)
		       (get-val-by-idx 'kunabi::errorMessage errorMessage)
		       (get-val-by-idx 'kunabi::errorCode errorCode)
		       (find-username (get-val-by-idx 'kunabi::userIdentity userIdentity)))
	       results))))))
    results))



(defun ct-get-all-errors ()
  (ct-get-by-klass-value 'kunabi::errorCode nil t))

(defun ct-get-by-name (name)
  (ct-get-by-klass-value 'kunabi::userName name))

(defun ct-get-by-errorcode (name)
  (ct-get-by-klass-value 'kunabi::errorCode name))

(defun ct-get-by-errorMessage (name)
  (ct-get-by-klass-value 'kunabi::errorMessage name))

(defun ct-get-by-eventName (name)
  (ct-get-by-klass-value 'kunabi::eventName name))

(defun ct-get-by-eventSource (name)
  (ct-get-by-klass-value 'kunabi::eventSource name))

(defun ct-get-by-sourceIPAddress (name)
  (ct-get-by-klass-value 'kunabi::sourceIPAddress name))

(defun manardb-recreate-tables ()
  (format t "manardb-recreate-tables~%"))

(defun manardb-normalize-insert-2 (record)
  ;;manardb-nomalize-insert (NIL us-west-1 NIL NIL 216e957f-230e-42ea-bfc7-e0d07d321a8b DescribeDBInstances rds.amazonaws.com 2015-08-07T19:04:52Z AwsApiCall 1.03 224108527019 2f1d4165-3d37-11e5-aae4-c1965b0823e9 NIL NIL NIL bogus.example.com signin.amazonaws.com (invokedBy signin.amazonaws.com sessionContext (attributes (creationDate 2015-08-07T11:17:07Z mfaAuthenticated true)) userName meylor accessKeyId ASIAIOHLZS2V2QON52LA accountId 224108527019 arn arn:aws:iam::224108527019:user/meylor principalId AIDAJVKKNU5BSTZIOF3EU type IAMUser) meylor)
  (destructuring-bind (
		       additionalEventData
		       awsRegion
		       errorCode
		       errorMessage
		       eventID
		       eventName
		       eventSource
		       eventTime
		       eventType
		       eventVersion
		       recipientAccountId
		       requestID
		       requestParameters
		       resources
		       responseElements
		       sourceIPAddress
		       userAgent
		       userIdentity
		       userName
		       )
      record
    (make-instance 'ct
		   :additionalEventData additionalEventData
		   :awsRegion awsRegion
		   :errorCode errorCode
		   :errorMessage errorMessage
		   :eventID eventID
		   :eventName eventName
		   :eventSource eventSource
		   :eventTime eventTime
		   :eventType eventType
		   :eventVersion eventVersion
		   :recipientAccountId recipientAccountId
		   :requestID requestID
		   :requestParameters requestParameters
		   :resources resources
		   :responseElements responseElements
		   :sourceIPAddress sourceIPAddress
		   :userAgent userAgent
		   :userIdentity userIdentity
		   :userName userName
		   )
    ))

(defun compress-str (str)
  (when str
    (let ((store-me nil))
      (cond
	((consp str) (setf store-me (format nil "~{~A ~}" str)))
	((stringp str) (setf store-me str))
	)
      (if (< (length store-me) 10)
	  (flexi-streams:octets-to-string (thnappy:compress-string store-me))
	  store-me))))

(defun manardb-normalize-insert (record)
  (destructuring-bind (
		       additionalEventData
		       awsRegion
		       errorCode
		       errorMessage
		       eventID
		       eventName
		       eventSource
		       eventTime
		       eventType
		       eventVersion
		       recipientAccountId
		       requestID
		       requestParameters
		       resources
		       responseElements
		       sourceIPAddress
		       userAgent
		       userIdentity
		       userName
		       )
      record
    (let ((additionalEventData-i (get-idx 'kunabi::additionalEventData additionalEventData))
	  (awsRegion-i (get-idx 'kunabi::awsRegion awsRegion))
	  (errorCode-i (get-idx 'kunabi::errorCode errorCode))
	  (errorMessage-i (get-idx 'kunabi::errorMessage errorMessage))
	  (eventID-i (get-idx 'kunabi::eventID eventID))
	  (eventName-i (get-idx 'kunabi::eventName eventName))
	  (eventSource-i (get-idx 'kunabi::eventSource eventSource))
	  (eventTime-i (get-idx 'kunabi::eventTime eventTime))
	  (eventType-i (get-idx 'kunabi::eventType eventType))
	  (eventVersion-i (get-idx 'kunabi::eventVersion eventVersion))
	  (recipientAccountId-i (get-idx 'kunabi::recipientAccountId recipientAccountId))
	  (requestID-i (get-idx 'kunabi::requestID requestID))
	  (requestParameters-i (get-idx 'kunabi::requestParameters requestParameters))
	  (resources-i (get-idx 'kunabi::resources resources))
	  (responseElements-i (get-idx 'kunabi::responseElements (compress-str responseElements)))
	  (sourceIPAddress-i (get-idx 'kunabi::sourceIPAddress sourceIPAddress))
	  (userAgent-i (get-idx 'kunabi::userAgent userAgent))
	  (userIdentity-i (get-idx 'kunabi::userIdentity userIdentity))
	  (userName-i (get-idx 'kunabi::userName (or userName (find-username userIdentity)))))
      (make-instance 'ct
		     :additionalEventData additionalEventData-i
		     :awsRegion awsRegion-i
		     :errorCode errorCode-i
		     :errorMessage errorMessage-i
		     :eventID eventID-i
		     :eventName eventName-i
		     :eventSource eventSource-i
		     :eventTime eventTime-i
		     :eventType eventType-i
		     :eventVersion eventVersion-i
		     :recipientAccountId recipientAccountId-i
		     :requestID requestID-i
		     :requestParameters requestParameters-i
		     :resources resources-i
		     :responseElements responseElements-i
		     :sourceIPAddress sourceIPAddress-i
		     :userAgent userAgent-i
		     :userIdentity userIdentity-i
		     :userName userName-i
		     ))))

(defun cleanse (var)
  (typecase var
    (null (string var))
    (string var)
    (list (format nil "~{~s = ~s~%~}" var))))

(defun manardb-get-or-insert-id (table value)
  (format t "manard-get-or-insert-id table:~A value:~A~%" table value)
  )

(defun manardb-drop-table (query)
  (format t "manardb-drop-table query:~A~%" query)
  )

(defun manardb-do-query (query)
  (format nil "manardb-do-query query:~A~%" query)
  )
