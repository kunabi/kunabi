(in-package :kunabi)

(fare-memoization:define-memo-function get-hostname-by-ip (ip)
  (if (boundp '*benching*)
      "bogus.example.com"
      (progn
	(if (cl-ppcre:all-matches "^(?:[0-9]{1,3}\.){3}[0-9]{1,3}$" ip)
	    (let ((name
		   #+allegro
		    (ignore-errors (socket:ipaddr-to-hostname ip))
		    #+sbcl
		    (ignore-errors (sb-bsd-sockets:host-ent-name
				    (sb-bsd-sockets:get-host-by-address
				     (sb-bsd-sockets:make-inet-address ip))))
		    #+lispworks
		    (ignore-errors (comm:get-host-entry ip :fields '(:name)))
		    #+clozure
		    (ignore-errors (ccl:ipaddr-to-hostname (ccl:dotted-to-ipaddr ip)))))
	      (if (null name)
		  ip
		  name))
	    ip))))

(defun read-json-gzip-file (file)
  (handler-case
      (progn
	(let ((json (get-json-gzip-contents file)))
	  (jonathan:parse json)))
    (t (e) (error-print "read-json-gzip-file" e))))

#+ecl
(defun get-json-gzip-contents (file)
  (uiop:run-program
   (format nil "zcat ~A" file)
   :output :string))

(defun error-print (fn error)
  (format t "~%~A~%~A~%~A~%~A~%~A~%" fn fn error fn fn))

;; #-(or clozure sbcl allegro)
;; (defun get-json-gzip-contents (file)
;;   (uiop:run-program
;;    (format nil "zcat ~A" file)
;;    :output :string))

;; #+(or clozure sbcl allegro)

#-ecl
(defun get-json-gzip-contents (file)
  (handler-case
      (progn (first (gzip-stream:with-open-gzip-file (in file)
		      (loop for l = (read-line in nil nil)
			 while l collect l))))
    (t (e) (error-print "get-json-gzip-contents" e))))


(defun cdr-assoc (item a-list &rest keys)
  (cdr (apply #'assoc item a-list keys)))

(define-setf-expander cdr-assoc (item a-list &rest keys)
  (let ((item-var (gensym))
        (a-list-var (gensym))
        (store-var (gensym)))
    (values
     (list item-var a-list-var)
     (list item a-list)
     (list store-var)
     `(let ((a-cons (assoc ,item-var ,a-list-var ,@ keys)))
        (if a-cons
            (setf (cdr a-cons) ,store-var)
            (setf ,a-list (acons ,item-var ,store-var ,a-list-var)))
        ,store-var)
     `(cdr (assoc ,item-var ,a-list-var ,@ keys)))))

;; (defclass queue ()
;;   ((list :initform nil)
;;    (tail :initform nil)))

;; (defmethod print-object ((queue queue) stream)
;;   (print-unreadable-object (queue stream :type t)
;;     (with-slots (list tail) queue
;;       (cond ((cddddr list)
;; 	     ;; at least five elements, so print ellipsis
;; 	     (format stream "(~{T~S ~}... ~S)"
;; 		     (subseq list 0 3) (first tail)))
;; 	    ;; otherwise print whole list
;; 	    (t (format stream "~:S" list))))))

;; (defmethod dequeue ((queue queue))
;;   (with-slots (list) queue
;;     (pop list)))

;; (defmethod queue-length ((queue queue))
;;   (with-slots (list) queue
;;     (list-length list)))

;; (defmethod enqueue (new-item (queue queue))
;;   (with-slots (list tail) queue
;;     (let ((new-tail (list new-item)))
;;       (cond ((null list) (setf list new-tail))
;; 	    (t (setf (cdr tail) new-tail)))
;;       (setf tail new-tail)))
;;   queue)

(defun flatten (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defun get-full-filename (x)
  (let* ((split (split-sequence:split-sequence #\/ (directory-namestring x)))
	 (length (list-length split))
	 (dir1 (nth (- length 2) split))
	 (dir2 (nth (- length 3) split))
	 (dir3 (nth (- length 4) split)))
    (format nil "~A/~A/~A/~A" dir3 dir2 dir1 (file-namestring x))))

(defun thread-safe-hash-table ()
  "Return A thread safe hash table"
  (let ((size 10000000)
	(rehash-size 2.0)
	(rehash-threshold 0.7))
    #+(or abcl ecl) (make-hash-table :test 'equalp)
    #+sbcl
    (make-hash-table :synchronized t :test 'equalp
		     ;;:size size :rehash-size rehash-size :rehash-threshold rehash-threshold
		     )
    #+ccl
    (make-hash-table :shared :lock-free :test 'equalp
		     ;;:size size :rehash-size rehash-size :rehash-threshold rehash-threshold
		     )
    #+(or allegro lispworks)
    (make-hash-table :test 'equalp
		     ;;:size size :rehash-size rehash-size :rehash-threshold rehash-threshold
		     )
    ))

(fare-memoization:define-memo-function reverse-hash-kv (old)
  (let ((new (make-hash-table)))
    (maphash #'(lambda (k v) (setf (gethash v new) k)) old) new))


;; from David McClain

;; #+lispworks
;; (defun %par (fn &rest fns)
;;   (let* ((nfns (length fns))
;;          (sem  (mp:make-semaphore :count 0)))
;;     (dolist (fn fns)
;;       (mp:funcall-async (lambda ()
;;                           (unwind-protect
;; 			       (funcall fn)
;;                             (mp:semaphore-release sem)))
;;                         ))
;;     (prog1
;;         (funcall fn)
;;       (mp:semaphore-acquire sem :count nfns))))

;; #+lispworks
;; (defmacro par1 (&rest clauses)
;;   ;; like PROG1, but executes all clauses in parallel, synchronizing
;;   ;; at the closing paren, and returning value of first clause
;;   (when clauses
;;     (if (rest clauses)
;;         `(%par ,@(mapcar #`(lambda ()
;;                              ,a1)
;;                          clauses))
;; 	;; else
;; 	(first clauses))))

;; #+lispworks
;; (defmacro par (&rest clauses)
;;   ;; like PROGN, but executes all clauses in parallel, synchronizing
;;   ;; at the closing paren, and returning value of last clause
;;   `(par1 ,@(nreverse clauses)))


(defun get-size (file)
  (let ((stat (get-stat file)))
    #+allegro (progn
		(require 'osi)
		(excl.osi:stat-size stat))
    #+lispworks (progn
		  (sys:file-stat-size stat))
    #+sbcl (sb-posix:stat-size stat)
    #+ccl (elt stat 2)
    ))
