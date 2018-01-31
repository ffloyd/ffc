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

(require 'cl-lib)
(require 'subr-x)

(defvar ffc-alist
  nil
  "Associative list of ffc config definitions and its metadata.")

(defvar ffc-fetures-alist
  nil
  "Associative list of ffc macro features definitions and its metadata.")

(defvar ffc-loaded-list
  nil
  "List of loaded ffe-config definitions.")

(defvar ffc-failed-list
  nil
  "List of failed to load ffe-config definitions.")

(define-error 'ffc-error "ffe-config error")

(define-error 'ffc-invalid-name-error "Config NAME should be a symbol" 'ffc-error)
(define-error 'ffc-invalid-docstring-error "Config DOCSTRING should be a string" 'ffc-error)
(define-error 'ffc-invalid-on-define-error "Config ON-DEFINE should be a function" 'ffc-error)
(define-error 'ffc-invalid-on-load-error "Config ON-LOAD should be a function" 'ffc-error)

(define-error 'ffc-already-defined-error "Config with such name already defined" 'ffc-error)
(define-error 'ffc-undefined-error "Udefined config" 'ffc-error)

(define-error 'ffc-invalid-feature-name-error
  "Feature NAME should be a keyword" 'ffc-error)
(define-error 'ffc-invalid-feature-on-define-error
  "Feature ON-DEFINE-LAMBDA should be a function" 'ffc-error)
(define-error 'ffc-invalid-feature-on-load-error
  "Feature ON-LOAD-LAMBDA should be a function" 'ffc-error)

(defun ffc-define (name docstring on-define on-load)
  "Define new configuration."

  (unless (symbolp name)
    (signal 'ffc-invalid-name-error `(,name)))

  (unless (stringp docstring)
    (signal 'ffc-invalid-docstring-error `(,docstring)))

  (unless (functionp on-define)
    (signal 'ffc-invalid-on-define-error `(,on-define)))

  (unless (functionp on-load)
    (signal 'ffc-invalid-on-load-error `(,on-load)))

  (if (alist-get name ffc-alist)
      (signal 'ffc-already-defined-error `(,name)))

  (let* ((config-alist `((name . ,name)
                         (docstring . ,docstring)
                         (on-define . ,on-define)
                         (on-load . ,on-load)))
         (config-cons (cons name config-alist)))
    (push config-cons ffc-alist))

  (funcall on-define))

(defun ffc-load (name)
  "Load defined configuration."

  (if-let ((config (alist-get name ffc-alist)))
      (funcall (alist-get 'on-load config))
    (signal 'ffc-undefined-error `(,name))))

(defun ffc-apply ()
  "Load all unloaded configurations in definition order."

  (let* ((reversed-config-names (mapcar #'car ffc-alist))
         (config-names (reverse reversed-config-names)))
    (mapc #'ffc-load config-names)))

(defun ffc-define-feature (key on-define-lambda on-load-lambda)
  "Define new ffc macro feature."

  (unless (keywordp key)
    (signal 'ffc-invalid-feature-name-error `(,key)))
  
  (unless (functionp on-define-lambda)
    (signal 'ffc-invalid-feature-on-define-error `(,on-define-lambda)))

  (unless (functionp on-load-lambda)
    (signal 'ffc-invalid-feature-on-load-error `(,on-load-lambda)))

  (push `(,key ,on-define-lambda ,on-load-lambda) ffc-features-alist))

(provide 'ffc)
;;; ffc.el ends here
