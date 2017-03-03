;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/android:toast.lisp,v 1.2.1.1 2014/05/27 20:55:54 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

;;----------------------------------------------------------------------------
;;
;; examples/android/toast.lisp
;;;
;;; This code demonstrates raising a "Toast" (a little bit of text that floats
;;; for a while above the current application without interfering with it).
;;;
;;; It defines the function:
;;;
;;; RAISE-A-TOAST string &key duration gravity x-offset y-offset h-margin v-margin )
;;; 
;;; DURATION - :short or :long
;;; GRAVITY - an integer, a string designator, or a list. 
;;; X-OFFSET, Y-OFFSET  - integers.
;;; H-MARGIN, V-MARGIN  - floats. 
;;;
;;; for example:
;;;
;;; (raise-a-toast "Bla Bla Bla" :x-offset 0 :y-offset 0)   ; display at top left corner 
;;; (raise-a-toast "Bla Bla Bla" )                          ; display at the center 

;;; The Othello demo (see ./README) loads this file when delivered "with Lisp", and
;;; the prepared forms contain a form to call it.

;;; Java interface used:
;;;    LW-JI:CHECK-READ-JAVA-FIELD
;;;    LW-JI:DEFINE-JAVA-CALLERS
;;;
;;; Android specific interface:
;;;
;;;   HCL:ANDROID-FUNCALL-IN-MAIN-THREAD
;;;   HCL::ANDROID-GET-APPLICATION-CONTEXT

(in-package "CL-USER")

;;; Allow the caller to pass a designator for a string, look up the
;;; string as a field in android.view.Gravity, and if fail return 0.
;;; See the documentation for android.view.Gravity what names are
;;; known. Most useful are :CENTER, :LEFT, RIGHT, :TOP, :BOTTOM.
;;; :START and :END

(defun get-gravity-value (gravity-name)
  (let ((full-name (string-append "android.view.Gravity." (string-upcase (string gravity-name)))))
    (or (lw-ji:check-read-java-field full-name) 0)))

;;; Allow gravity to be an integer, or a string designator, or a list
;;; of gravity values.

(defun interpret-gravity (what)
  (if (listp what)
      (let ((g 0))  
        (dolist (sub what)
          (setq g (logior (interpret-gravity sub) g)))
        g)
    (if (integerp what)
        what
      (get-gravity-value what))))

;;; For duration we use the documented values directly. 
(defun interpret-duration (duration)
  (case duration
    (:short 0)
    (:long 1)
    (t "Unknown DURATION (not :SHORT or :LONG): ~s" duration)))

;;; The Java callers we use. 
(lw-ji:define-java-callers "android.widget.Toast"
  (make-toast "makeText") ; Context, string or resid, duration (0 - short, 1 - long ) 
  (toast-set-margin "setMargin")  ; float (horizontal), float (vertical)
  (toast-set-gravity  "setGravity") ; int (gravity), int (x), int (y)
  (toast-show "show")
 )


;;; This is called on the main thread to actually do the work. 
;;; First get the application context, and then use the Java callers defined
;;; above to make the toast and show it. 
(defun internal-raise-a-toast (string duration-int gravity x-offset y-offset h-margin v-margin)
  (let ((context (hcl::android-get-application-context)))
    (let ((toast (make-toast context
                             string
                             duration-int)))
      (toast-set-gravity toast gravity  x-offset y-offset)
      (toast-set-margin toast h-margin v-margin)
      (toast-show toast))))

;;; Check the argument, and call internal-raise-a-toast above to actually do the work. 

(defun raise-a-toast (string &key (duration :short) gravity (x-offset 0) (y-offset 0) (h-margin 0f0) (v-margin 0f0) )
  (assert (integerp x-offset) nil "RAISE-A-TOAST: X-OFFSET is not an integer : ~s" x-offset)
  (assert (integerp y-offset) nil "RAISE-A-TOAST: Y-OFFSET is not an integer : ~s" y-offset)
  (assert (floatp h-margin)  nil  "RAISE-A-TOAST: H-MARGIN is not a float : ~s" h-margin)
  (assert  (floatp v-margin) nil  "RAISE-A-TOAST: v-MARGIN is not a float : ~s" v-margin)
  (let ((duration-int (interpret-duration duration))
        (gravity (interpret-gravity gravity)))
    (hcl:android-funcall-in-main-thread 'internal-raise-a-toast 
                                        string
                                        duration-int
                                        gravity
                                        x-offset
                                        y-offset
                                        h-margin
                                        v-margin)))
