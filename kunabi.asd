#-asdf3 (error "kunabi requires ASDF 3")

(defsystem #:kunabi
  :name "kunabi"
  :description "A Common Lisp client library for the Kunabi"
  :version "0.0.1"
  :license "MIT"
  :class :package-inferred-system
  :build-operation program-op
  :build-pathname "kunabi"
  :defsystem-depends-on (:asdf-package-system)
  :entry-point "kunabi:main"
  :depends-on (
	       :asdf
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
	       :clack

	       ;;:postmodern
	       ;;:sqlite
	       )
  :components ((:module src :serial t
			:components (
				     (:file "package")
				     (:file "ctcl")
				     (:file "utils")
				     (:file "version" :depends-on ("package"))
				     (:file "bench" :depends-on ("package" "ctcl"))
				     (:file "database" :depends-on ("package"))
				     ;;	       (:file "db-postgres" :depends-on ("package" "database"))
				     ;;	       (:file "db-sqlite" :depends-on ("package" "database"))
				     (:file "db-manardb" :depends-on ("package" "database"))
				     (:file "flows" :depends-on ("package" "ctcl" "utils" "database"))
				     (:file "main" :depends-on ("package" "ctcl" "utils" "database" "flows"))
				     (:file "pkgdcl" :depends-on ("package" "ctcl" "utils" "database"))
				     ))))
