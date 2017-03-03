;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/android:deliver-android-kunabi.lisp,v 1.3.1.1 2014/05/27 20:55:54 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/android/deliver-android-kunabi.lisp
;;
;; A delivery file for the Android Kunabi example.
;;
;; Copy this file and edit the value of *PROJECT-PATH*
;; to point to where you have your copy of the KunabiDemo
;; project. See README for details.
;;
;; Then use this file on an ARM machine as the build script for the
;; Android delivery image:
;;
;; <android-delivery-image> -init <copy-of-this-file>
;;
;; The android-delivery-image in LispWorks 7.0 is called
;; lispworks-7-0-0-arm-linux-android.
;;
;; This file delivers "without Lisp", so you cannot evaluate forms.
;; Use ./deliver-android-kunabi-with-lisp.lisp to deliver "with Lisp",
;; which allows evaluating forms.
;;

;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(defvar *project-path*  nil)


;;; The next two lines "load the application".
;;; We use :load :delete :output-file :temp to avoid compiling in the examples directory,
;;; and not leaving the compiled file in the directory.
(compile-file (example-file "misc/kunabi") :load :delete :output-file :temp)
(compile-file (example-file "android/android-kunabi-user.lisp") :load :delete :output-file :temp)



(deliver-to-android-project nil *project-path* 5)

(quit)
