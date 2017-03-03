;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/android:android-othello-user.lisp,v 1.14.1.1 2014/05/27 20:55:56 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/android/android-othello-user.lisp
;;
;; This file is used by (copies of)  ./deliver-android-othello.lisp or
;; ./deliver-android-othello-with-lisp.lisp
;;
;; Implements the lisp-to-java calls for the Othello demo running
;; on Android.
;;
;; See the top of ../misc/othello.lisp for documentation of the interface
;; that is needed.
;;
;; The java-to-lisp calls do not actually need implementation in Lisp, they are done
;; by direct calls to the interface defined by the lispworks.jar. See
;; calls to
;;        com.lispworks.LispWorksNativeLibrary.callIntV        and
;;        com.lispworks.LispWorksNativeLibrary.callVoidV
;; in ./OthelloDemo/src/com/lispworks/example/othellodemo/Othello.java
;;
;; Nevertheless, this file also contain example of LISP-PROXY that can used
;; to implement the OthelloServer which the Java code uses as the interface
;; for calling out.

;; All Java methods that are called here manipulate the GUI, so
;; we need to call them on the main thread.
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------



(in-package "CL-USER")



(lw-ji:define-java-callers "com.lispworks.example.othellodemo.Othello"
  (android-update-state-string "updateState")
  (android-update-change-a-square "change")
  (android-signal-bad-move "signalBadMove"))

;;; The following four functions define the functions that
;;; the code in misc/othello needs.

(defun othello-user-update-state-string (string)
  (android-funcall-in-main-thread 'android-update-state-string string))

(defun othello-user-change-a-square (index player)
  (android-funcall-in-main-thread 'android-update-change-a-square index player))


(defun othello-user-signal-bad-move ()
  (android-funcall-in-main-thread 'android-signal-bad-move))

;;; This one does not need android-funcall-in-main-thread, because lw-ji:format-to-java-host
;;; does the right thing on Android whatever thread it is called on.

(defun othello-user-print-diagnostics-message (string)
  (lw-ji:format-to-java-host "~a~%" string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVAL-FOR-ANDROID  - called directly from Java ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Call EVAL on the string and report the result.
;;; We want to print what the evaluation outputs, and then print the result,
;;; so we bind *standard-output* and *trace-output* to a string stream, and then
;;; output the content of string stream followed by the result of the evaluation.
;;; Return 0 for any failure, 1 for success. Actual errors are reported by ANDROID-REPORT-ERROR,
;;; which is calls Java to record the error.
;;; This is called from Java by callIntV("EVAL-FOR-ANDROID", input)

;;; *previous-eval-for-android-results*  - this remembers the results of eval-for-android so we can set * and friends.
;;; Currently it global. To make it local, can use (mp:process-private-property 'eval-for-android-results).

;;; The Java code checks if this function is defined and accordingly decides if
;;; it can evaluate lisp forms (Java method com.lispworks.examples.LispPanel.canEvaluate).
;;; It will not be defined when it is shaken out.

(defglobal-variable *previous-eval-for-android-results*  '(nil nil nil))

(defun eval-for-android (string)
  (catch 'abort-call-eval
    (handler-bind ((error #'(lambda (condition)
                              (let* ((error-string (format nil "Error inside EVAL-FOR-ANDROID: ~a" condition))
                                     (log-file (dbg:log-bug-form error-string
                                                                 :condition condition :log-file :temp)))
                                (lw-ji:report-error-to-java-host error-string log-file))
                              (throw 'abort-call-eval 0))))
      (let (res)
        (let ((printed
               (with-output-to-string(*standard-output*)
                 (let ((*trace-output* *standard-output*))
                   (destructuring-bind (/// // /) *previous-eval-for-android-results*
                     (let ((* (car /)) (** (car //)) (*** (car ///)))

                       (setq res  (multiple-value-list (eval (read-from-string string))))
                       (setq *previous-eval-for-android-results* (list // / res))))))))
          (let ((format-string (if (> (length printed) 0)
                                  "~%>>evaluating the form: ~a~% >> printed:~%~a~2%>> Got result(s):~% ~{~s~%~}"
                                "~% >>evaluating the form: ~a~%~* >> Got result(s):~% ~{~s~%~}")))
            (lw-ji:format-to-java-host format-string  string printed res)))
        1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Examples of proxies that implement the KunabiServer that is declared
;;; in ./KunabiDemo/src/com/lispworks/example/kunabidemo/Kunabi.java, and is
;;; used to do the calls from Java to Lisp. The Java code has its own implementation
;;; which is used by default.

;;; This function is just to tell us when we use the LISP-PROXY.

(defun call-kunabi-clicked-full(where)
  (lw-ji:format-to-java-host "The full proxy plays square number ~d " where)
  (kunabi-clicked where))

;;; "Full" implementation, define all the methods that the interface declares
(lw-ji:define-lisp-proxy lisp-kunabi-server-full
  ("com.lispworks.example.kunabidemo.Kunabi.KunabiServer"
   ("init" init-kunabi)
   ("playSquare" call-kunabi-clicked-full)
   ("undoMove" undo-move)
   ("setComputerPlays" set-computer-plays)))



;;; "Lazy" implementation. Define a default function that handles
;;; everything. Here it does not save much, but in some cases it may be
;;; more convenient.

(lw-ji:define-lisp-proxy lisp-kunabi-server-lazy
  ("com.lispworks.example.kunabidemo.Kunabi.KunabiServer")
  (:options :default-function lisp-kunabi-server-lazy-default-function))

;;; The default function for lisp-kunabi-server-lazy. We can get away
;;; with just checking the first character of the method name
(defun lisp-kunabi-server-lazy-default-function (method-name &optional arg)
  (case (char method-name 0)
    (#\i  (init-kunabi arg))
    (#\p   (lw-ji:format-to-java-host "The lazy proxy plays square number ~d " arg) ; This just to show that it is the lazy proxy that runs
           (kunabi-clicked arg))
    (#\u (undo-move))
    (#\s (set-computer-plays arg))))

;;; Match Java definition in ./KunabiDemo/src/com/lispworks/example/kunabidemo/Kunabi.java
(defconstant KUNABI_SERVER_TYPE_JAVA 0)        ; We do not expect to see this one
(defconstant KUNABI_SERVER_TYPE_FULL_PROXY 1)
(defconstant KUNABI_SERVER_TYPE_LAZY_PROXY 2)

;;; This one so Java can call it directly.
(defun create-lisp-kunabi-server (which)
  (let ((name (if (eq which  KUNABI_SERVER_TYPE_FULL_PROXY)
                  'lisp-kunabi-server-full
                'lisp-kunabi-server-lazy)))
    (lw-ji:make-lisp-proxy name)))


;;; Ensure the symbols that are going to be called directly from Java
;;; are kept after delivering.
;;; Strictly speaking does not need to keep the Kunabi symbols, because they are
;;; referenced by the proxy.  We keep them here as a matter of principle.

(deliver-keep-symbols
 'init-kunabi 'kunabi-clicked 'undo-move 'set-computer-plays  ; Kunabi symbols
 'create-lisp-kunabi-server  ; proxy creation
 )
