(ql:quickload '(:clack :ningle :kunabi))

(defvar *app* (make-instance 'ningle:<app>))
(kunabi::init-manardb)

(setf (ningle:route *app* "/") "Welcome to kunabi")


(defun web/get-user-list ()
  (let ((users (kunabi::get-unique-values-list 'kunabi::username)))
    (format nil "~{<li><a href=\"http://localhost:5002/user-search?user=~A\"> ~:* ~A </aref></li>~}" users users)))

(defun web/search-user (user)
  (let* ((activity (kunabi::ct-get-by-klass-value-real 'kunabi::userName user)))
    (format nil "~{<li>~A</li>~}" activity)))

(setf (ningle:route *app* "/users") `(200 (:content-type "text/html") (,(web/get-user-list))))

(setf (ningle:route *app* "/user-search")
      #'(lambda (params)
	  (let ((user (cdr (assoc "user" params :test #'string=))))
	    `(200 (:content-type "text/html") (,(web/search-user user))))))

(clack:clackup *app* :port 5002 :server
	       :hunchentoot)
               ;;:toot)
					;:toot)
