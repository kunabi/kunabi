(in-package :kunabi)
(defvar *sqlite-db* ":memory:")
;;(defvar *sqlite-db* "/tmp/kunabi.db")
(defvar *sqlite-conn* nil)

(defun sqlite-disconnect (conn)
  (if (equal *db-backend* :sqlite)
      (sqlite:disconnect conn)))

(defun sqlite-drop-table (table &optional (conn *sqlite-conn*))
  (sqlite:execute-non-query conn (format nil "drop table if exists ~A" table)))

(defun sqlite-do-query (query &optional (db *sqlite-conn*))
  "do query"
  (declare (special *conn*))
  ;;(format t "~%Q: ~A ~%" query)
  (sqlite:execute-to-list db query))

(defun sqlite-have-we-seen-this-file (file)
  t)

(defun sqlite-establish-connection ()
  (setf *print-circle* nil)
  (format t "~% db-backend:~A~%" *db-backend*)
  (if (equal *db-backend* :sqlite)
      (progn
	(format t "~%sqlite-db:~A in sqlite-establish-connection!!!~%" *sqlite-db*)
	(if (null *sqlite-conn*)
	    (progn
	      (setf *sqlite-conn* (sqlite:connect *sqlite-db*))
	      (sqlite:execute-non-query *sqlite-conn* "pragma journal_mode = wal"))))))

(defun sqlite-emit-conn ()
  (if (equal *db-backend* :sqlite)
      (progn
	(setf *print-circle* nil)
	;;(format t "~% in sqlite-emit-conn!!!~%")
	(let ((conn (sqlite:connect *sqlite-db*)))
	  (sqlite:execute-non-query conn "pragma journal_mode = wal")
	  conn))))

(defun sqlite-recreate-tables (&optional (db *sqlite-conn*))
  (setf *print-circle* nil)
  (force-output)
  (ignore-errors
    (sqlite-drop-table "files" db)
    (sqlite-drop-table "log" db)
    (sqlite:execute-non-query db "drop view ct"))
  (mapcar
   #'(lambda (x)
       (sqlite-drop-table x db)) *fields*)
  (sqlite-create-tables))

(defun sqlite-create-tables (&optional (db *sqlite-db*))
  (sqlite-create-table "files" db)
  (mapcar
   #'(lambda (x)
       (sqlite-create-table x db)) *fields*)
  (format t "~%create table log(id integer primary key autoincrement, ~{~A ~^ integer, ~} integer)" *fields*)
  (sqlite:execute-non-query *sqlite-conn* (format nil "create table log(id integer primary key autoincrement, ~{~A ~^ integer, ~} integer)" *fields*))
  (sqlite:execute-non-query *sqlite-conn*
			    (format nil "create view ct as select ~{~A.value as~:* ~A ~^,  ~} from log, ~{~A ~^, ~} where ~{~A.id = ~:*log.~A ~^and ~};" *fields* *fields* *fields*)))

(defun sqlite-create-table (table &optional (db *sqlite-db*))
  (force-output)
  (format t "ct:~A db:~A~%" table db)
  (sqlite:execute-non-query *sqlite-conn* (format nil "create table ~A(id integer primary key autoincrement, value text)" table))
  (sqlite:execute-non-query *sqlite-conn* (format nil "create unique index ~A_idx1 on ~A(id)" table table))
  (sqlite:execute-non-query *sqlite-conn* (format nil "create unique index ~A_idx2 on ~A(value)" table table)))

(fare-memoization:define-memo-function sqlite-get-or-insert-id (table value &optional (db *sqlite-conn*))
  ;;(defun sqlite-get-or-insert-id
  "get or set id"
  ;;(format t "~%sgoii: table:~A value:~A" table value)
  (setf *print-circle* nil)
  ;;(declare (special *conn*))
  (let ((insert (format nil "insert or ignore into ~A(value) values('~A')" table value))
	(query (format nil "select id from ~A where value = '~A'" table value))
        (id nil))
    ;;(sqlite:with-transaction *sqlite-conn*
      (sqlite:execute-non-query *sqlite-conn* insert)
      (setf id (sqlite:execute-single *sqlite-conn* query))
    id))

(defun sqlite-get-ids (record)
  (let ((n 0))
;;    (format t "~%record:~A~%"  record)
    (loop for i in *fields*
       collect (let ((value (sqlite-get-or-insert-id i (format nil "~A" (nth n record)))))

		 (incf n)
		 (if (null value)
		     (format t "i:~A val:~A try:~A~%"  i (nth n record) value))
		 value))))


(defun sqlite-normalize-insert (record)
  (let ((values (sqlite-get-ids record))
	(tables (get-tables)))
    ;;(format t "values:~{~A~^,~} tables:~A~%" record values tables)
    (db-do-query (format nil "insert into log(~{~A~^, ~}) values(~{~A~^, ~})" *fields* values))))
