#-cmucl (load "~/quicklisp/setup.lisp")
(ql:quickload '(
		:fare-memoization
		:cl-fad
		:gzip-stream
		:cl-json
		:pcall
		;;:postmodern
		:manardb
		:usocket
		))

(princ "pkgdcl")
(load (compile-file "pkgdcl.lisp"))
(princ "database")
(load (compile-file "database.lisp"))
;;(load (compile-file "db-postgres.lisp"))
;;(load (compile-file "db-sqlite.lisp"))
(load (compile-file "db-manardb.lisp"))
(princ "bench")
(load (compile-file "bench.lisp"))
;; (princ "rucksack")
;; (load (compile-file "collector/rucksack.lisp"))
(princ "utils")
(load (compile-file "utils.lisp"))
(princ "ctcl")
(load (compile-file "ctcl.lisp"))
(princ "main")
(load (compile-file "main.lisp"))
(in-package :ctcl)
