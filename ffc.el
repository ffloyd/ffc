;;; ffc.el --- Ffloyd's Furious Config: Emacs configuration microframework

;; Copyright (C) 2018-2018 Roman Kolesnev

;; Author: Roman Kolesnev <rvkolesnev@gmail.com>
;; Maintainer: Roman Kolesnev <rvkolesnev@gmail.com>
;; URL: https://github.com/ffloyd/ffc
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (pkg-info "0.4"))
;; Created: 8 Jan 2018

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides internally simple and convinient
;; tool for taking control over your init.el.
;;

;;; Code:

;;
;; 0xFFC dependencies
;;

(require 'cl-lib)
(require 'subr-x)

;;
;; 0xFFC global variables
;;

(defvar ffc-features-alist
  nil
  "Associative list of ffc macro features definitions and its metadata.")

(defvar ffc-pipeline-list
  nil
  "Ordered list of active features names.")

(defvar ffc-alist
  nil
  "Associative list of ffc config definitions and its metadata.")

(defvar ffc-loaded-list
  nil
  "List of loaded ffe-config definitions.")

(defvar ffc-failed-alist
  nil
  "Associative list of failed to load ffe-config definitions connected with happened errors.")

;;
;; 0xFFC errors' definitions
;;

(define-error 'ffc-error "0xFFC error")

;; Feature specific
(define-error 'ffc-feature-invalid-name-error
  "Feature NAME should be a keyword" 'ffc-error)
(define-error 'ffc-feature-already-defined-error
  "Feature with such NAME already defined" 'ffc-error)
(define-error 'ffc-feature-invalid-on-define-error
  "Feature ON-DEFINE-LAMBDA should be a function" 'ffc-error)
(define-error 'ffc-feature-invalid-on-load-error
  "Feature ON-LOAD-LAMBDA should be a function" 'ffc-error)
(define-error 'ffc-feature-not-found-error
  "Feature not found" 'ffc-error)

;; Pipeline specific
(define-error 'ffc-pipeline-redefinition-error
  "Pipeline may be chosen only once. Redefinition is forbidden." 'ffc-error)

;; Config defining specific
(define-error 'ffc-invalid-name-error "Config NAME should be a symbol" 'ffc-error)
(define-error 'ffc-invalid-docstring-error "Config DOCSTRING should be a string" 'ffc-error)
(define-error 'ffc-already-defined-error "Config with such name already defined" 'ffc-error)
(define-error 'ffc-feature-not-in-pipeline-error "Feature not present in current pipeline" 'ffc-error) 

;; Config loading specific
(define-error 'ffc-undefined-error "Udefined config" 'ffc-error)
(define-error 'ffc-double-loading-error "Config with such name already loaded" 'ffc-error)

;;
;; 0xFFC private layer
;;

(defun ffc--define-feature (name on-define on-load)
  "Define new ffc macro feature."

  (unless (keywordp name)
    (signal 'ffc-feature-invalid-name-error `(,name)))

  (if (alist-get name ffc-features-alist)
      (signal 'ffc-feature-already-defined-error `(,name)))
  
  (unless (functionp on-define)
    (signal 'ffc-feature-invalid-on-define-error `(,on-define)))

  (unless (functionp on-load)
    (signal 'ffc-feature-invalid-on-load-error `(,on-load)))

  (push `(,name ,on-define ,on-load) ffc-features-alist))

(defun ffc--setup-pipeline (pipeline-list)
  "Setups pipeline."

  (if ffc-pipeline-list
      (signal 'ffc-pipeline-redefinition-error nil))

  (mapc (lambda (feature-name)
          (unless (alist-get feature-name ffc-features-alist)
            (signal 'ffc-feature-not-found-error `(,feature-name))))
        pipeline-list)

  (setq ffc-pipeline-list pipeline-list))

(defun ffc--apply-feature (feature-name feature-data execute-definer execute-loader)
  "Apply feature."

  (if-let ((feature-callbacks (alist-get feature-name ffc-features-alist)))
      (let ((on-define-callback (car feature-callbacks))
            (on-load-callback (cadr feature-callbacks)))
        (if execute-definer
            (funcall on-define-callback feature-data))
        (if execute-loader
            (funcall on-load-callback feature-data)))))

(defun ffc--apply-pipeline (features-data-alist execute-definers execute-loaders)
  "Applies pipeline to features' data alist."

  (mapc (lambda (feature-name)
          (if-let ((feature-data (alist-get feature-name features-data-alist)))
            (ffc--apply-feature feature-name feature-data execute-definers execute-loaders)))
        ffc-pipeline-list))

(defun ffc--define-config (name docstring &optional features-data-alist)
  "Define new configuration."

  (unless (symbolp name)
    (signal 'ffc-invalid-name-error `(,name)))

  (unless (stringp docstring)
    (signal 'ffc-invalid-docstring-error `(,docstring)))

  (if (alist-get name ffc-alist)
      (signal 'ffc-already-defined-error `(,name)))

  (mapc (lambda (feature-cons)
          (let ((feature-name (car feature-cons)))
            (unless (member feature-name ffc-pipeline-list)
              (signal 'ffc-feature-not-in-pipeline-error `(,feature-name)))))
        features-data-alist)

  (ffc--apply-pipeline features-data-alist t nil)

  (push `(,name . (,docstring . ,features-data-alist)) ffc-alist))

(defun ffc--load-config (name)
  "Load defined configuration."

  (if (member name ffc-loaded-list)
      (signal 'ffc-double-loading-error `(,name))) 
  
  (if-let ((config (alist-get name ffc-alist))
           (features-data-alist (cdr config)))
      (progn
        (ffc--apply-pipeline features-data-alist nil t)
        (push name ffc-loaded-list))
    (signal 'ffc-undefined-error `(,name))))

;;
;; 0xFFC public layer
;;

(cl-defmacro ffc-feature (keyword-name &rest args &key definer loader)
  "Define new ffc macro feature."

  `(ffc--define-feature ,keyword-name ,definer ,loader))

(defmacro ffc-pipeline (&rest features)
  "Setups pipleline."

  `(ffc--setup-pipeline ',features))

(defun ffc-apply ()
  "Load all unloaded configurations in definition order."

  (let* ((reversed-config-names (mapcar #'car ffc-alist))
         (config-names (reverse reversed-config-names)))
    (mapc #'ffc--load-config config-names)))

(provide 'ffc)
;;; ffc.el ends here
