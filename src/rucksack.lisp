(in-package :ctcl)

(defvar *rs-directory* #p"~/rs-ct/")

(with-rucksack (rs *rs-directory* :if-exists :supersede)
  (with-transaction ()
    (defclass ct-details ()
      ((unique-id    :initarg :unique-id :accessor unique-id-of 
		     :index :number-index
                     :unique t
		     :documentation "A unique number for each event in our DB")
      (event-version :initarg  :event-version :accessor event-version-of)
      (user-identity :initarg :user-identity :accessor user-identity-of)
      (event-time :initarg :event-time :accessor event-time-of)
      (event-source :initarg :event-source :accessor event-source-of)
      (event-name :initarg :event-name :accessor event-name-of)
      (aws-region :initarg :aws-region :accessor aws-region-of)
      (source-ip-address :initarg :source-ip-address :accessor source-ip-address-of)
      (user-agent :initarg :user-agent :accessor user-agent-of)
      (request-parameters :initarg :request-parameters :accessor request-parameters-of)
      (response-elements :initarg :response-elements :accessor response-elements-of)
      (request-id :initarg :request-id :accessor request-id-of)
      (event-id :initarg :event-id :accessor event-id-of)
      (event-type :initarg :event-type :accessor event-type-of))
      (:documentation
       "The cloutrail-details class")
      (:index t)
      (:metaclass persistent-class)) 
    ))

(defvar *unique-id* 0)

(defmethod initialize-instance :after ((obj ct-details) &key)
  (setf (unique-id-of obj) (incf *unique-id*)))

;; (defmethod print-object ((obj ct-details) stream)
;;   (print-unreadable-object (obj stream :type t)
;;     (with-slots (unique-id event-name event-source user-agent event-type) obj
;;       (format stream "~A: '~A' ~A '~A' '~A"
;; 	      unique-id event-name event-source user-agent event-type))))

(defmethod print-object ((obj ct-details) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (unique-id event-version user-identity event-time event-source event-name aws-region source-ip-address user-agent request-parameters response-elements request-id event-id event-type) obj
      (format stream "~A: '~A' ~A '~A' '~A' '~A' '~A' '~A' '~A' '~A' '~A' '~A' '~A' '~A' " unique-id event-version user-identity event-time event-source event-name aws-region source-ip-address user-agent request-parameters response-elements request-id event-id event-type))))


(defun make-event (event-version user-identity event-time event-source event-name aws-region source-ip-address user-agent request-parameters response-elements request-id event-id event-type)
  (format t ".")
  (with-rucksack (rs *rs-directory*)
    (with-transaction ()
	(make-instance 'ct-details 
		       :event-version event-version
		       :user-identity user-identity
		       :event-time event-time
		       :event-source event-source
		       :event-name event-name
		       :aws-region aws-region
		       :source-ip-address source-ip-address
		       :user-agent user-agent
		       :request-parameters request-parameters
		       :response-elements response-elements
		       :request-id request-id
		       :event-id event-id
		       :event-type event-type))))

;;(make-event event-version user-identity event-time event-source aws-region source-ip-address user-agent request-parameters response-elements request-id event-id event-type)

(defun print-all-events ()
  (with-rucksack (rs *rs-directory*)
    (with-transaction ()
      (rucksack-map-class rs 'ct-details 
			  (lambda (object)
			    (format t "~A~%" object))))))

(defun find-ct-by-event-name (event-name)
  (with-rucksack (rs *rs-directory*)
    (with-transaction ()
      (rucksack-map-slot rs 'ct-details 'event-name
			 (lambda (event)
			   (return-from find-ct-by-event-name event))
			 :equal event-name)))
  nil)

;;(find-ct-by-event-name "CreatePlatformEndpoint")



;; (defun find-ct-by-name (name)
;;   (with-rucksack (rs *rs-directory*)
;;     (with-transaction ()
;;       (rucksack-map-slot rs 'ct-details 'name 
;; 			 (lambda (contact)
;; 			   (return-from find-contact-by-name contact))
;; 			 :equal name)))
;;   nil)

;; (defun find-contacts-by-name-range (&optional start end)
;;   (let (ret)
;;     (with-rucksack (rs *rs-directory*)
;;       (with-transaction ()
;; 	(rucksack-map-slot rs 'ct-details 'name 
;; 			   (lambda (contact)
;; 			     (push contact ret))
;; 			   :min start :max end :include-min t :include-max t)))
;;     ; reverse the list so it's in the expected order
;;     (nreverse ret)))

;; (defun delete-object-by-name (name)
;;   (with-rucksack (rs *rs-directory*)
;;     (with-transaction ()
;;       (let ((contact (find-contact-by-name name)))
;; 	(when contact
;; 	  (rucksack::rucksack-delete-object rs contact))))))


;; (dolist (contact (find-contacts-by-name-range "a" "c"))
;; 	 (format t "~A~%" contact)) 
;; (dolist (contact (find-contacts-by-name-range "c"))
;; 	 (format t "~A~%" contact)) 
