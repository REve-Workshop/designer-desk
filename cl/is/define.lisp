(defpackage pim.re.error
  (:use #:cl #:xyz.revecloud.re.logging.builder)
  (:export #:init
           #:make-log-record-unknown-account-type
           #:make-log-record-unknown-transaction-type
           #:make-log-record-database-not-set)
  (:import-from #:xyz.revecloud.re.logging.builder "make-log-record"))

(in-package pim.re.error)

(defun make-compta-error-message (subject what? context)
  "Return a string formatted as an error message for the compta package."
  (escape-double-quote (format nil "~a: ~a. ~a." subject what? context)))

(defun make-log-record-unknown-account-type (compte faux-type)
  "Retourne une structure décrivant le type d'erreur, type de compte inconnu."
  (let ((message-to-join (make-compta-error-message faux-type
                                                    "type de compte inconnu"
                                                    (format nil "Erreur lors de l'ajout du compte ~a" compte))))
    (make-log-record 'error
                     'unknown-account-type-error
                     message-to-join
                     (list 'account-type-received faux-type)
                     (list 'account (escape-double-quote compte)))))

(defun make-log-record-unknown-transaction-type (compte-destination faux-type transaction-date)
  "Retourne une structure décrivant le type d'erreur, type de transaction inconnue."
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
  "Retourne une structure décrivant le type d'erreur, base de donnée non définie."
  (make-log-record 'error
                   'database-not-set-error
                   (escape-double-quote "Aucune base de donnée ouverte.")
                   (list 'requested-operation operation)))
