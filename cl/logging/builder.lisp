(defpackage xyz.revecloud.re.logging.builder
  (:use :cl)
  (:export #:init-logging #:set-operation #:make-log-record)
  (:import-from :xyz.revecloud.re.tools.tools "escape-double-quote"))

(in-package :xyz.revecloud.re.logging.builder)

(defparameter *application* nil
  "The application calling the logger.")

(defparameter *operation* nil
  "The operation, function, macro, calling the logger.")

(defun init-logging (application)
  "Init the logging system and set the application context."
  (setf *application* application))

(defun set-operation (operation)
  "Set the operation context."
  (setf *operation* operation))

(defun make-compta-error-message (subject what? context)
  "Return a string formatted as an error message for the compta package."
  (escape-double-quote (format nil "~a: ~a. ~a." subject what? context)))

(defun make-log-record (log-level error-type message &rest args)
  "Renvoie un list des information concernant en enregistrement dans le journal."
  `(log
    (context
     (application ,*application*)
     (operation ,*operation*))
    (log-level ,log-level)
    (type ,error-type)
    (record-date ,(get-universal-time))
    (message ,message)
    ,@args))

(defun make-log-record-unknown-account-type (compte faux-type)
  "Retourne une structure décrivant le type d'erreur type de compte inconnu."
  (let ((message-to-join (make-compta-error-message faux-type
                                                    "type de compte inconnu"
                                                    (format nil "Erreur lors de l'ajout du compte ~a" compte))))
    (make-log-record 'error
                     'unknown-account-type-error
                     message-to-join
                     (list 'account-type-received faux-type)
                     (list 'account (escape-double-quote compte)))))

(defun make-log-record-unknown-transaction-type (compte-destination faux-type transaction-date)
  "Retourne une structure décrivant le type d'erreur type de compte inconnu."
  (let ((message-to-join (make-compta-error-message faux-type
                                                    "type de transaction inconnu"
                                                    (format nil "Erreur lors de l'ajout d'une transaction. Destination: ~a - Date: ~a" compte-destination transaction-date))))
    (make-log-record 'error
                     'unknown-transaction-type-error
                     message-to-join
                     (list 'transaction-type-received faux-type)
                     (list 'destination-account (escape-double-quote compte-destination))
                     (list 'transaction-date (escape-double-quote transaction-date)))))

(defun make-log-record-database-not-set (operation)
  "Retourne une structure décrivant le type d'erreur type de compte inconnu."
  (make-log-record 'error
                   'database-not-set-error
                   (escape-double-quote "Aucune base de donnée ouverte.")
                   (list 'requested-operation operation)))
