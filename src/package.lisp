#+allegro (progn
	    (load (merge-pathnames "~/quicklisp/setup.lisp" *default-pathname-defaults*))
	    (ql:quickload '(
			    :cl-date-time-parser
			    :cl-fad
			    :fare-memoization
			    :gzip-stream
			    :jonathan
			    :pcall
			    ;;:postmodern
			    :manardb
			    ;;:s-sql
			    :zs3
			    ;;:sqlite
			    :uiop
			    :thnappy
			    :trivial-garbage
			    :usocket
			    )))


(defpackage :kunabi
  (:use :cl :zs3
	#+allegro :prof
	)

  (:export
   #:main
   #:run-bench))
