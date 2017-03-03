(require 'asdf)
(require 'cmp)

(setf *load-verbose* nil)
(setf *compile-verbose* nil)
(setf c::*suppress-compiler-warnings* t)
(setf c::*suppress-compiler-notes* t)
(setf c::*compile-in-constants* t)

(push (make-pathname :name nil :type nil :version nil :defaults *load-truename*)
      asdf:*central-registry*)

(setf *compile-print* nil)
(ql:bundle-systems '(
	       :cffi-grovel
	       :cl-date-time-parser
	       :cl-fad
	       :cl-json
	       :closer-mop
	       :fare-memoization
	       :gzip-stream
	       :jonathan
	       :local-time
	       :manardb
	       :pcall
	       :pcall-queue
	       :split-sequence
	       :thnappy
	       :trivial-garbage
	       :uiop
	       :usocket
	       :zs3
		     ) :to "bundle/")

(load "bundle/bundle.lisp")
(asdf:operate 'asdf:load-op 'kunabi)
;;(asdf:operate 'asdf:load-op 'closer-mop)

;;(asdf:operate 'asdf:load-bundle-op 'uiop)
;;(asdf:operate 'asdf:load-bundle-op 'kunabi)
;;(ql:quickload :closer-mop)

(asdf:make-build :kunabi :type :program :prologue-code '(progn (require :asdf)(require :uiop)) :epilogue-code '(kunabi::main) :move-here "./")
;;(asdf:make :kunabi :type :program :epilogue-code 'kunabi:main)
