(in-package :kunabi)
;;(declaim (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))

(defvar *manard-flow-files* (thread-safe-hash-table))
(ql:quickload :split-sequence :cl-date-time-parser :local-time)
(defvar *mytasks* (list))

(defvar vpc-fields '(
		     kunabi::bytez
		     kunabi::date
		     kunabi::dstaddr
		     kunabi::dstport
		     kunabi::endf
		     kunabi::interface-id
		     kunabi::packets
		     kunabi::protocol
		     kunabi::srcaddr
		     kunabi::srcport
		     kunabi::start
		     kunabi::status
		     kunabi::action
		     ))

(manardb:defmmclass date ()
  ((file :initarg :name :reader file)))

(manardb:defmmclass interface-id ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass srcaddr ()
  ((value :initarg :value :accessor srcaddr-value)
   (idx :initarg :idx :accessor srcaddr-idx)))

(manardb:defmmclass dstaddr ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass srcport ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass dstport ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass protocol ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass packets ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass bytez ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass start ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass endf ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass action ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass status ()
  ((value :initarg :value :accessor value)
   (idx :initarg :idx :accessor idx)))

(manardb:defmmclass flow ()
  ((date :initarg :date :accessor date)
   (interface-id :initarg :interface-id :reader interface-id)
   (srcaddr :initarg :srcaddr :reader srcaddr)
   (srcport :initarg :srcport :reader srcport)
   (dstaddr :initarg :dstaddr :reader dstaddr)
   (dstport :initarg :dstport :reader dstport)
   (protocol :initarg :protocol :reader protocol)
   (packets :initarg :packets :accessor packets)
   (bytez :initarg :bytez :accessor bytez)
   (start :initarg :start :accessor start)
   (endf :initarg :endf :accessor endf)
   (action :initarg :action :accessor action)
   (status :initarg :status :accessor status)))

(manardb:defmmclass flow-files ()
  ((file :initarg :file :accessor file)))

(defun init-vpc-hashes ()
  (mapc
   #'(lambda (x)
       (allocate-klass-hash x))
   vpc-fields))

(defun vpc-flows-report-async (workers path)
  (allocate-vpc-file-hash)
  (init-vpc-hashes)
  (let ((workers (parse-integer workers)))
    (setf (pcall:thread-pool-size) workers)
    (walk-ct path #'async-vf-file)
    (mapc
     #'(lambda (x)
	 (handler-case
	     (pcall:join x)
	   (t (e) (format t "~%~%Error:~A on join of ~A" e x))))
     *mytasks*)))

(defun get-by-ip (val)
  (manardb:doclass (x 'kunabi::flow :fresh-instances nil)
    (with-slots (interface-id srcaddr dstaddr srcport dstport protocol) x
      (let* ((srcaddr-i (get-val srcaddr))
	     (dstaddr-i (get-val dstaddr))
	     (smatch (usocket:ip= val srcaddr-i))
	     (dmatch (usocket:ip= val dstaddr-i)))
	(if (or smatch dmatch)
	    (format t "|~A|~A|~A|~A|~A|~A|~%"
		    (get-val interface-id)
		    srcaddr-i
		    dstaddr-i
		    (get-val srcport)
		    (get-val dstport)
		    (get-val protocol)
		    ))))))

(defun vpc-flows-report-sync (path)
  (init-vpc-hashes)
  (allocate-vpc-file-hash)
  (force-output)
  (unless (null path)
    (walk-ct path #'sync-vf-file)))

(defun async-vf-file (x)
  (push (pcall:pexec
	  (funcall #'process-vf-file x)) *mytasks*))

(defun sync-vf-file (x)
  (process-vf-file x))

(defun find-by-srcaddr (value)
  (manardb:doclass (x 'kunabi::flow :fresh-instances nil)
    (with-slots (
		 interface-id
		 srcaddr
		 dstaddr
		 srcport
		 dstport
		 protocol
		 packets
		 bytez
		 start
		 endf
		 action
		 status
		 ) x
      (if (cl-ppcre:all-matches value srcaddr)
	  (format t "|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A~%"
		  interface-id
		  srcaddr
		  dstaddr
		  srcport
		  dstport
		  protocol
		  packets
		  bytez
		  start
		  endf
		  action
		  status)))))

;;(fare-memoization:define-memo-function get-val-by-idx (klass idx)
(defun get-val-by-idx (klass idx)
  (allocate-klass-hash klass)
  (let*
      ((klass-hash (gethash klass *kunabi-fields*))
       (rev-hash (reverse-hash-kv klass-hash))
       (val nil))
    (if (hash-table-p klass-hash)
	(progn
	  (setf val (gethash idx rev-hash))
	  (unless val
	    (setf val idx)))
	(format t "get-val-by-idx: klass:~A has no hash:~A....~%" klass (type-of rev-hash)))
    val))

(defun list-all-vpc ()
  (init-vpc-hashes)
  (manardb:doclass (x 'kunabi::flow :fresh-instances nil)
    (with-slots (interface-id
		 srcaddr
		 dstaddr
		 srcport
		 dstport
		 protocol
		 packets
		 bytez
		 start
		 endf
		 action
		 status) x
      (format t "|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A~%"
	      (get-val-by-idx 'kunabi::interface-id interface-id)
	      (get-val-by-idx 'kunabi::srcaddr srcaddr)
	      (get-val-by-idx 'kunabi::dstaddr dstaddr)
	      (get-val-by-idx 'kunabi::srcport srcport)
	      (get-val-by-idx 'kunabi::dstport dstport)
	      (get-val-by-idx 'kunabi::protocol protocol)
	      (get-val-by-idx 'kunabi::packets packets)
	      (get-val-by-idx 'kunabi::bytez bytez)
	      (get-val-by-idx 'kunabi::start start)
	      (get-val-by-idx 'kunabi::endf endf)
	      (get-val-by-idx 'kunabi::action action)
	      (get-val-by-idx 'kunabi::status status)))))

(defun allocate-vpc-file-hash ()
  (if (eql (hash-table-count *manard-flow-files*) 0)
      (init-manardb)
      (mapc
       #'(lambda (x)
	   (setf (gethash (slot-value x 'file) *manard-flow-files*) t))
       (manardb:retrieve-all-instances 'kunabi::flow-files))))

(defun flows-have-we-seen-this-file (file)
  (let ((name (get-full-filename file)))
    (multiple-value-bind (id seen)
	(gethash name *manard-flow-files*)
      seen)))

(defun flow-mark-file-processed (file)
  (let ((name (get-full-filename file)))
    (setf (gethash name *manard-flow-files*) t)
    (make-instance 'flow-files :file name)))

(defun get-vpc-date-list ()
  "Return uniqure list of events"
  (get-unique-values 'kunabi::date))

(defun get-vpc-interface-id-list ()
  "Return uniqure list of events"
  (get-unique-values 'kunabi::interface-id))

(defun get-vpc-srcaddr-list ()
  "Return uniqure list of events"
  (get-unique-values 'kunabi::srcaddr))

(defun get-vpc-dstaddr-list ()
  "Return uniqure list of events"
  (get-unique-values 'kunabi::dstaddr))

(defun get-vpc-srcport-list ()
  "Return uniqure list of events"
  (get-unique-values 'kunabi::srcport))

(defun get-vpc-dstport-list ()
  "Return uniqure list of events"
  (get-unique-values 'kunabi::dstport))

(defun get-vpc-protocols-list ()
  "Return uniqure list of events"
  (get-unique-values 'kunabi::protocol))

(defun get-vpc-action-list ()
  "Return uniqure list of events"
  (get-unique-values 'kunabi::action))

(defun geft-vpc-status-list ()
  "Return uniqure list of events"
  (get-unique-values 'kunabi::status))

(defun process-vf-file (file)
  (when (equal (pathname-type file) "gz")
    ;;(room t)
    (unless (flows-have-we-seen-this-file file)
      (handler-case
	  (progn
	    (format t "+")
	    (flow-mark-file-processed file)
	    (gzip-stream:with-open-gzip-file (in file)
	      (let ((i 0)
		    (btime (get-internal-real-time)))
		(loop
		   for line = (read-line in nil nil)
		   while line
		   collect (progn
			     (incf i)
			     (process-vf-line line)))
		(let* ((etime (get-internal-real-time))
		       (delta (/ (float (- etime btime)) (float internal-time-units-per-second)))
		       (rps (/ (float i) (float delta))))
		  (format t "~%rps:~A rows:~A delta:~A" rps i delta))
		#+sbcl (sb-ext:gc :full t) ;;ard
		)))
	(t (e) (format t "~%SHIT~%SHIT~%Error:~A~%SHIT~%SHIT~%" e)))
      (format t "-"))))

(declaim (inline process-vf-line))
(defun process-vf-line (line)
  (let* ((tokens (split-sequence:split-sequence #\Space line))
	 (length (list-length tokens)))
    (if (= 15 length)
	(destructuring-bind (date version account_id interface-id srcaddr dstaddr srcport dstport protocol packets bytez start end action status)
	    tokens
	  (insert-flows date interface-id srcaddr dstaddr srcport dstport protocol packets bytez start end action status)))))

(defun to-epoch (date)
  (local-time:timestamp-to-unix (local-time:universal-to-timestamp (cl-date-time-parser:parse-date-time date))))

(defun vpc-search-by-srcport (port)
  (init-vpc-hashes)
  (let* ((klass 'kunabi::srcport)
	 (klass-hash (gethash klass *kunabi-fields*)))
    (multiple-value-bind (id seen)
	(gethash port klass-hash)
      (if seen
	  (progn
	    (manardb:doclass (x 'kunabi::flow :fresh-instances nil)
	      (with-slots (interface-id srcaddr dstaddr srcport dstport protocol packets bytez start endf action status) x
		(and (= srcport id)
		     (format t "|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A~%"
			     (get-val-by-idx 'kunabi::interface-id interface-id)
			     (get-val-by-idx 'kunabi::srcaddr srcaddr)
			     (get-val-by-idx 'kunabi::dstaddr dstaddr)
			     (get-val-by-idx 'kunabi::srcport srcport)
			     (get-val-by-idx 'kunabi::dstport dstport)
			     (get-val-by-idx 'kunabi::protocol protocol)
			     (get-val-by-idx 'kunabi::packets packets)
			     (get-val-by-idx 'kunabi::bytez bytez)
			     (get-val-by-idx 'kunabi::start start)
			     (get-val-by-idx 'kunabi::endf endf)
			     (get-val-by-idx 'kunabi::action action)
			     (get-val-by-idx 'kunabi::status status))))))
	  (format t "Error: have not seen source port ~A~%" port)))))

(defun vpc-search-by-dstport (value)
  (init-vpc-hashes)
  (let* ((klass 'kunabi::dstport)
	 (klass-hash (gethash klass *kunabi-fields*)))
    (multiple-value-bind (id seen)
	(gethash value klass-hash)
      (if seen
	  (progn
	    (manardb:doclass (x 'kunabi::flow :fresh-instances nil)
	      (with-slots (interface-id srcaddr dstaddr srcport dstport protocol packets bytez start endf action status) x
		(and (= dstport id)
		     (format t "|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A~%"
			     (get-val-by-idx 'kunabi::interface-id interface-id)
			     (get-val-by-idx 'kunabi::srcaddr srcaddr)
			     (get-val-by-idx 'kunabi::dstaddr dstaddr)
			     (get-val-by-idx 'kunabi::srcport srcport)
			     (get-val-by-idx 'kunabi::dstport dstport)
			     (get-val-by-idx 'kunabi::protocol protocol)
			     (get-val-by-idx 'kunabi::packets packets)
			     (get-val-by-idx 'kunabi::bytez bytez)
			     (get-val-by-idx 'kunabi::start start)
			     (get-val-by-idx 'kunabi::endf endf)
			     (get-val-by-idx 'kunabi::action action)
			     (get-val-by-idx 'kunabi::status status))))))
	  (format t "Error: have not seen dest port ~A~%" value)))))

(defun vpc-search-by-srcaddr (value)
  (init-vpc-hashes)
  (let* ((klass 'kunabi::srcaddr)
	 (klass-hash (gethash klass *kunabi-fields*)))
    (multiple-value-bind (id seen)
	(gethash value klass-hash)
      (if seen
	  (progn
	    (manardb:doclass (x 'kunabi::flow :fresh-instances nil)
	      (with-slots (interface-id srcaddr dstaddr srcport dstport protocol packets bytez start endf action status) x
		(and (= srcaddr id)
		     (format t "|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A~%"
			     (get-val-by-idx 'kunabi::interface-id interface-id)
			     (get-val-by-idx 'kunabi::srcaddr srcaddr)
			     (get-val-by-idx 'kunabi::dstaddr dstaddr)
			     (get-val-by-idx 'kunabi::srcport srcport)
			     (get-val-by-idx 'kunabi::dstport dstport)
			     (get-val-by-idx 'kunabi::protocol protocol)
			     (get-val-by-idx 'kunabi::packets packets)
			     (get-val-by-idx 'kunabi::bytez bytez)
			     (get-val-by-idx 'kunabi::start start)
			     (get-val-by-idx 'kunabi::endf endf)
			     (get-val-by-idx 'kunabi::action action)
			     (get-val-by-idx 'kunabi::status status))))))
	  (format t "Error: have not seen srcaddr ~A~%" value)))))

(defun vpc-search-by-dstaddr (value)
  (init-vpc-hashes)
  (let* ((klass 'kunabi::dstaddr)
	 (klass-hash (gethash klass *kunabi-fields*)))
    (multiple-value-bind (id seen)
	(gethash value klass-hash)
      (if seen
	  (progn
	    (manardb:doclass (x 'kunabi::flow :fresh-instances nil)
	      (with-slots (interface-id srcaddr dstaddr srcport dstport protocol packets bytez start endf action status) x
		(and (= dstaddr id)
		     (format t "|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A~%"
			     (get-val-by-idx 'kunabi::interface-id interface-id)
			     (get-val-by-idx 'kunabi::srcaddr srcaddr)
			     (get-val-by-idx 'kunabi::dstaddr dstaddr)
			     (get-val-by-idx 'kunabi::srcport srcport)
			     (get-val-by-idx 'kunabi::dstport dstport)
			     (get-val-by-idx 'kunabi::protocol protocol)
			     (get-val-by-idx 'kunabi::packets packets)
			     (get-val-by-idx 'kunabi::bytez bytez)
			     (get-val-by-idx 'kunabi::start start)
			     (get-val-by-idx 'kunabi::endf endf)
			     (get-val-by-idx 'kunabi::action action)
			     (get-val-by-idx 'kunabi::status status))))))
	  (format t "Error: have not seen dstaddr ~A~%" value)))))

;; (defun vpc-search-by (klass value field)
;;   (init-vpc-hashes)
;;   (let* ((klass-hash (gethash klass *kunabi-fields*)))
;;     (multiple-value-bind (id seen)
;; 	(gethash value klass-hash)
;;       (if seen
;; 	  (progn
;; 	    (manardb:doclass (x 'kunabi::flow :fresh-instances nil)XS
;; 	      (with-slots (interface-id srcaddr dstaddr srcport dstport protocol packets bytez start endf action status) x
;; 		(and (= field id)
;; 		     (format t "|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A|~A~%"
;; 			     (get-val-by-idx 'kunabi::interface-id interface-id)
;; 			     (get-val-by-idx 'kunabi::srcaddr srcaddr)
;; 			     (get-val-by-idx 'kunabi::dstaddr dstaddr)
;; 			     (get-val-by-idx 'kunabi::srcport srcport)
;; 			     (get-val-by-idx 'kunabi::dstport dstport)
;; 			     (get-val-by-idx 'kunabi::protocol protocol)
;; 			     (get-val-by-idx 'kunabi::packets packets)
;; 			     (get-val-by-idx 'kunabi::bytez bytez)
;; 			     (get-val-by-idx 'kunabi::start start)
;; 			     (get-val-by-idx 'kunabi::endf endf)
;; 			     (get-val-by-idx 'kunabi::action action)
;; 			     (get-val-by-idx 'kunabi::status status))))))
;; 	  (format t "Error: have not seen value:~A for class:~A~%" value klass)))))

;; (defun vpc-search-by-srcport (value)
;;   (vpc-search-by 'kunabi::srcport value 'srcport))

;; (defun vpc-search-by-srcip (value)
;;   (vpc-search-by 'kunabi::srcip value 'srcip))

(defun get-next-idx (klass-hash)
  (and (hash-table-p klass-hash)
       (progn
	 (let* ((idxs (alexandria:hash-table-values klass-hash))
		(max-id 0))
	   (and idxs
		(progn
		  ;;(format t "gnix: klass:~A length:~A~%" klass-hash (hash-table-size klass-hash))
		  ;;(setf max-id (+ 1 (apply #'max (mapcar #'(lambda (x) (if (stringp x) (parse-integer x) x)) idxs))))))
		  (setf max-id (+ 1 (hash-table-count klass-hash)))))
	   max-id))))

(defun get-next-id (klass)
  (let ((counter (gethash klass *kunabi-counters*)))
    (if counter
	(setf (gethash klass *kunabi-counters*) (+ counter 1)))))


;;(fare-memoization:define-memo-function get-idx (klass new-value)
(defun get-idx (klass new-value)
  "Return the object for a given value of klass"
  (if (and klass new-value)
      (let ((klass-hash (gethash klass *kunabi-fields*))
	    (nid nil)
	    (obj nil))
	(if (hash-table-p klass-hash)
	    ;;(format t "XXX: no valid hash table for klass:~A in get-idx~%" klass))
	    (multiple-value-bind (id seen)
		(gethash new-value klass-hash)
	      (if seen
		  (setf nid id)
		  (progn ;; new item
		    ;;(setf nid (get-next-idx klass-hash))
		    (setf nid (incf (gethash klass *kunabi-counters*)))
		    (setf obj (make-instance klass :value new-value :idx nid))
		    (setf (gethash new-value klass-hash) nid)
		    )))
	    (setf nid new-value))
	nid)))

(defun insert-flows (date interface-id srcaddr dstaddr srcport dstport protocol packets bytez start endf action status)
  (let ((date2 (to-epoch date))
	(interface-id-i (get-idx 'kunabi::interface-id interface-id))
	(srcaddr-i (get-idx 'kunabi::srcaddr srcaddr))
	(srcport-i (get-idx 'kunabi::srcport srcport))
	(dstaddr-i (get-idx 'kunabi::dstaddr dstaddr))
	(dstport-i (get-idx 'kunabi::dstport dstport))
	(protocol-i (get-idx 'kunabi::protocol protocol))
	(packets-i (get-idx 'kunabi::packets packets))
	(bytez-i (get-idx 'kunabi::bytez bytez))
	(start-i (get-idx 'kunabi::start start))
	(endf-i (get-idx 'kunabi::endf endf))
	(action-i (get-idx 'kunabi::action action))
	(status-i (get-idx 'kunabi::status status)))

    ;;(format t "~{~A, ~}~%" (list date2 interface-id-i srcaddr-i dstaddr-i srcport-i dstport-i protocol-i packets-i bytez-i start-i endf-i action-i status-i))

    (make-instance 'flow
		   :date date2
		   :interface-id interface-id-i
		   :srcaddr srcaddr-i
		   :srcport srcport-i
		   :dstaddr dstaddr-i
		   :dstport dstport-i
		   :protocol protocol-i
		   :packets packets-i
		   :bytez bytez-i
		   :start start-i
		   :endf endf-i
		   :action action-i
		   :status status-i)))
