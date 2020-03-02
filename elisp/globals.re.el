;;; globals.re.el --- Global variable definitions for skeletons.

;; Add License

;; Author: Roland Everaert <computing.re@revecloud.xyz>
;; Version: 0.1
;; Keywords: tool, programming, project

;;; Commentary:
;;
;; This file defines variables to be available globally. They are
;; currently used to help fill the skeletons defined in
;; \"cl-skeletons.re\".

;;; Installation:
;;
;; Add this file to your load-path.
;; Add the following line in your Emacs configuration file:
;;
;; (require 'globals.re)

;;; Configuration:
;;
;;; Code:

(defvar reve:project-version nil
  "Version of the project.")

(defvar reve:license-abbreviation nil
  "License name abbreviation for the project.")

(defvar reve:short-license nil
  "The short text of the license specified by REVE:PROJECT-VERSION.")

(defvar reve:author-initial nil
  "The author initials.

This variable should be set according to the variable
USER-FULL-NAME.")

(provide 'globals.re)
;;; globals.re ends here
