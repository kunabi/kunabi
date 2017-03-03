;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/android:dialog.lisp,v 1.4.1.2 2014/11/19 16:03:21 martin Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

;;----------------------------------------------------------------------------
;;
;; examples/android/dialog.lisp
;;
;;; This code demonstrates using the Java interface to raise
;;; an Android alert dialog.
;;; To actually work, the Java side must have called setCurrentActivity.

;;; The code defines the function:

#|
 raise-alert-dialog title &key (theme :default-light)
                                (ok-title "Ok") ok-callback ok-args
                                (Cancel-title "Cancel") cancel-callback  cancel-args
                                (apply-title "Apply") apply-callback apply-args)

For example:

 (raise-alert-dialog "What do you want to eat?"
   :ok-title "Chicken" :ok-callback   '(raise-alert-dialog "Here is some chicken")
   :cancel-title "Salad" :cancel-callback '(raise-alert-dialog "We don't have salad"))
|#

;;; The Kunabi demo (see ./README) loaded this file when delivered "with Lisp", and
;;; the prepared forms has the form above to try it.

;;; The Java interface it uses:
;;;   LW-JI:DEFINE-JAVA-CALLERS
;;;   LW-JI:DEFINE-JAVA-CONSTRUCTOR
;;;   LW-JI:CHECK-READ-JAVA-FIELD
;;;   LW-JI:DEFINE-LISP-PROXY
;;;   LW-JI:MAKE-LISP-PROXY
;;;   LW-JI:FORMAT-TO-JAVA-HOST
;;;   LW-JI:REPORT-ERROR-TO-JAVA-HOST
;;;
;;; Android specific interface:
;;;
;;;    HCL:ANDROID-GET-CURRENT-ACTIVITY

;;; To raise the dialog you need to have the current activity and execute
;;; in the main (GUI) thread. HCL:ANDROID-GET-CURRENT-ACTIVITY checks
;;; that it executes on the main thread, and the activity that it returns
;;; should be the current activity, as long as the Java method that sets it,
;;; com.lispworks.Manager.setCurrentActivity, is used correctly.


(in-package "CL-USER")

;;; The methods and constructor we use to build the dialog.
(lw-ji:define-java-callers "android.app.AlertDialog.Builder"
  (alert-dialog-builder-create "create") ; returns the AlertDialog
  (alert-dialog-builder-set-negative "setNegativeButton") ; string listener
  (alert-dialog-builder-set-positive "setPositiveButton") ; string listener
  (alert-dialog-builder-set-neutral  "setNeutralButton") ; string listener
  (alert-dialog-builder-set-title "setTitle")
  (alert-dialog-builder-show "show")   ; create the dialog and show it.
 )

(lw-ji:define-java-constructor make-alert-dialog-builder "android.app.AlertDialog.Builder")  ; signaure: context &optional theme (int)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The callback in the dialog-interface-on-click-listener proxy and the proxy itself, which
;;; we use for the listeners of the buttons in the dialog.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DIALOG-INTERFACE-ON-CLICK-EXECUTE - the callback in the proxy
;;; USER-DATA  is one of the callbacks that RAISE-ALERT-DIALOG got, potentially
;;; consed with the arguments. It is passed to this function  because
;;; attach-callback-to-alert-dialog-button  make the proxy with this user-data
;;; user-data, and the proxy is defined with :with-user-data T.
;;; DIALOG and WHICH are the arguments that were from Java.


(defun dialog-interface-on-click-execute (user-data dialog which)
  (declare (ignore dialog))
  (lw-ji:format-to-java-host "DIALOG-INTERFACE-ON-CLICK-EXECUTE with WHICH ~d user-data ~s~%"
                             which user-data)
  (if (consp user-data)
      (apply (car user-data) (cdr user-data))
    (funcall user-data)))


(lw-ji:define-lisp-proxy dialog-interface-on-click-listener
  ("android.content.DialogInterface.OnClickListener" ("onClick" dialog-interface-on-click-execute :with-user-data t)))

;;; ATTACH-CALLBACK-TO-ALERT-DIALOG-BUTTON
;;; Attach the Lisp callback to the button (also set the title of it). This
;;; is done by making a proxy of dialog-interface-on-click-listener
;;; interface (defined above) which implements the interface "android.content.DialogInterface.OnClickListener"
;;; with the user given callback as the user data (which is used by
;;; dialog-interface-on-click-execute above), and then using the Java method to set
;;; it in the dialog. .

(defun attach-callback-to-alert-dialog-button (builder which-button title callback args)
  (let* ((user-data (if args (cons callback args) callback))
         (setter (case which-button
                   (:ok 'alert-dialog-builder-set-positive)
                   (:cancel 'alert-dialog-builder-set-negative)
                   (:apply 'alert-dialog-builder-set-neutral)
                   (t (error "ATTACH-CALLBACK-TO-ALERT-DIALOG-BUTTON: which button not one of :ok :cancel :apply ~s" which-button))))
         (proxy (lw-ji:make-lisp-proxy 'dialog-interface-on-click-listener :user-data user-data)))
    (funcall setter builder title proxy)))

;;; INTERPRET-DIALOG-THEME
;;; Interpret a keyword for the theme. Using lw-ji:check-read-java-field here
;;; is a little overkill, we could use the documented constants.
(defun interpret-dialog-theme (theme)
  (if (integerp theme)
      theme  ; hopefully the caller knows what they do
   (let ((field-suffix  (case theme
                        (:default-light "DEVICE_DEFAULT_LIGHT")
                        (:default-dark "DEVICE_DEFAULT_DARK")
                        (:holo-dark "HOLO_DARK")
                        (:holo-light "HOLO_LIGHT")
                        (:traditional "TRADITIONAL"))))
     (let ((full-name (string-append "android.app.AlertDialog.THEME_" field-suffix)))
       (lw-ji:check-read-java-field full-name)))))



;;; RAISE-ALERT-DIALOG
;;; (1) Check that we can do it using ANDROID-GET-CURRENT-ACTIVITY, which also returns
;;;     the activity to use as the context argument when making the builder.
;;; (2) Make the builder with the appropriate theme if supplied.
;;; (3) Set the title.
;;; (4) Set buttons titles and callbacks.
;;; (5) Create and show the dialog (the builder "show" method creates the dialog first).

(defun raise-alert-dialog (title &key (theme :default-light) (ok-title "Ok") ok-callback ok-args
                                 (Cancel-title "Cancel") cancel-callback  cancel-args
                                 (apply-title "Apply") apply-callback apply-args)
  (if-let (activity (hcl:android-get-current-activity))   ; (1)
      (let* ((builder (if theme                           ; (2)
                          (make-alert-dialog-builder activity (interpret-dialog-theme theme))
                        (make-alert-dialog-builder activity ))))
        (alert-dialog-builder-set-title builder title)    ; (3)
        (when ok-callback                                 ;(4)
          (attach-callback-to-alert-dialog-button builder :ok ok-title ok-callback ok-args))
        (when cancel-callback
          (attach-callback-to-alert-dialog-button builder :cancel cancel-title cancel-callback cancel-args))
        (when apply-callback
          (attach-callback-to-alert-dialog-button builder :apply apply-title apply-callback apply-args))
        (alert-dialog-builder-show builder))             ; (5)
    (lw-ji:report-error-to-java-host (format nil "Tries to raise a dialig when no current activity with title ~s" title)
                                     nil)))
