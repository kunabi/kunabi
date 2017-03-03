(load "collector/load.lisp")
(ql:quickload :cl-store)
(defvar *q* (cl-store:restore "~/q.lisp"))
