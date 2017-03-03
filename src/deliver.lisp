(load (merge-pathnames "~/quicklisp/setup.lisp" *default-pathname-defaults*))
(ql:quickload '(
		:cl-date-time-parser
		:cl-fad
		:cl-json
		:fare-memoization
		:gzip-stream
		:jonathan
		:manardb
		:pcall
		;;:postmodern
		;;:sqlite
		:thnappy
		:uiop
		:usocket
		))

;;(load "package.lisp")

(require 'asdf)
(asdf:operate 'asdf:load-op 'kunabi)

#+(or ccl clisp ecl)
(ql:quickload "trivial-dump-core")

#+sbcl
(sb-ext:save-lisp-and-die "dist/sbcl/kunabi" :compression 5 :executable t :toplevel 'kunabi:main :save-runtime-options t)

#+(or ccl ccl64 )
(trivial-dump-core:save-executable "../dist/ccl/kunabi" #'kunabi:main)

#+cmucl
;;(save-lisp :executable t
(trivial-dump-core:save-executable "../dist/cmucl/kunabi" #'kunabi:main)

#+clisp
(ext:saveinitmem "dist/clisp/kunabi" :init-function #'kunabi:main :executable t :quiet t)

#+lispworks
(deliver 'kunabi:main "../dist/lispworks/kunabi" 0 :keep-package-manipulation t :multiprocessing t :keep-eval t :keep-fasl-dump t :keep-editor t :keep-foreign-symbols t :keep-function-name t :keep-gc-cursor t :keep-keyword-names t :keep-lisp-reader t :keep-macros t :keep-modules t :keep-top-level t :license-info nil  :keep-walker t :KEEP-PRETTY-PRINTER t)

#+allegro
(let ((lfiles '(
		"package.lisp"
		"utils.lisp"
		"ctcl.lisp"
		"database.lisp"
		;;"db-postgres.lisp"
		;;"db-sqlite.lisp"
		"db-manardb.lisp"
		"main.lisp"
		"bench.lisp"
		"flows.lisp"
		)))
  (mapcar #'compile-file lfiles)
  (cl-user::generate-executable "../dist/allegro/kunabi" '(
							  "package.fasl"
							  "utils.fasl"
							  "database.fasl"
							  ;;"db-postgres.fasl"
							  ;;"db-sqlite.fasl"
							  "db-manardb.fasl"
							  "ctcl.fasl"
							  "main.fasl"
							  "bench.fasl"
							  "flows.fasl"
							  )
				:runtime :partners
				:show-window :shownoactivate
				:system-dlls-path "system-dlls/"
				:temporary-directory #P"/tmp/"
				:verbose nil
				:discard-compiler nil
				:discard-local-name-info t
				:discard-source-file-info t
				:discard-xref-info t
				:ignore-command-line-arguments t
				:include-compiler t
				:include-composer nil
				:include-debugger t
				:include-devel-env nil
				:include-ide nil
				:runtime :partners
				:suppress-allegro-cl-banner t
				:newspace 16777216
				:oldspace 33554432))
