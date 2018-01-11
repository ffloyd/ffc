;;; ffe-config.el --- Emacs configuration microframework

;; Copyright (C) 2018-2018 Roman Kolesnev

;; Author: Roman Kolesnev <rvkolesnev@gmail.com>
;; Maintainer: Roman Kolesnev <rvkolesnev@gmail.com>
;; URL: https://github.com/ffloyd/ffe-config
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

(defvar ffe-config-alist
  nil
  "Associative list of ffe-config definitions and its metadata.")

(defvar ffe-config-loaded-list
  nil
  "List of loaded ffe-config definitions.")

(defvar ffe-config-failed-list
  nil
  "List of failed to load ffe-config definitions.")

(define-error 'ffe-config-error "ffe-config error")
(define-error 'ffe-config-undefined-error "Undefined config" 'ffe-config-error)
(define-error 'ffe-config-no-straight-error "Straight.el not found or initialized" 'ffe-config-error)
(define-error 'ffe-config-invalid-name-error "Invalid name for configuration (should be symbol)" 'ffe-config-error)
(define-error 'ffe-config-double-load-error "Config already loaded" 'ffe-config-error)
(define-error 'ffe-config-already-defined-error "Config already defined" 'ffe-config-error)

(cl-defun ffe-config (name docstring &rest args &key deps init packs conf)
  "Define configuration."

  ;; NAME must be symbol
  (unless (symbolp name)
    (signal 'ffe-config-invalid-name-error name))

  ;; Name should be free
  (if (ffe-config-p name)
      (signal 'ffe-config-already-defined-error `(,name)))
  
  ;; Check dependencies existance
  (cl-mapc (lambda (dep)
             (unless (ffe-config-p dep)
               (signal 'ffe-config-undefined-error `(,dep))))
           deps)

  ;; Checks straight.el if key :packs uses
  (if packs
      (unless (featurep 'straight)
        (signal 'ffe-config-no-straight-error nil)))
  
  (let* ((config-alist `((name . ,name)
                         (docstring . ,docstring)
                         (deps . ,deps)
                         (init . ,init)
                         (packs . ,packs)
                         (conf . ,conf)))
         (config-cons (cons name config-alist)))
    (push config-cons ffe-config-alist)))

(defun ffe-config-p (name)
  "Checks if config with given identifier present"

  (if (alist-get name ffe-config-alist) t))

(defun ffe-config-load (name)
  "Loads defined configuration"

  ;; Check for defined config
  (unless (ffe-config-p name)
    (signal 'ffe-config-undefined-error `(,name)))

  ;; Prevent double loading
  (if (ffe-config-loaded-p name)
      (signal 'ffe-config-double-load-error `(,name)))
  
  ;; Unsafe loading of feature
  (let-alist (alist-get name ffe-config-alist)
    (if .deps
        (cl-mapc (lambda (dep)
                   (unless (ffe-config-loaded-p dep)
                     (ffe-config-load dep)))
                 .deps))
    (if .init
        (funcall .init))
    (if .packs
        (progn
          (cl-mapc #'straight-use-package .packs)
          (cl-mapc #'require .packs)))
    (if .conf
        (funcall .conf)))

  ;; Register config as loaded
  (push name ffe-config-loaded-list))

(defun ffe-config-safe-load (name)
  "Safe load defined configuration"

  (let ((result t))
    (condition-case-unless-debug err
        (ffe-config-load name)
      (error (message
              "Error hapened while loading '%S' and its dependencies: %S"
              name (error-message-string err))
             (setq result nil)))
  
  result))

(defun ffe-config-loaded-p (name)
  "Checks if config with given identifier present"

  (if (member name ffe-config-loaded-list) t))

(defun ffe-config-safe-load-all ()
  "Safe loads all defined configurations"

  (let ((config-names (cl-mapcar #'car ffe-config-alist)))
    (cl-mapc #'ffe-config-safe-load config-names)))

(cl-defmacro ffc (name docstring &rest args &key deps init packs conf)
  "Syntax sugar macros around ffe-config function"

  `(ffe-config ',name ,docstring
               :deps ',deps
               :init (lambda () ,init)
               :packs ',packs
               :conf (lambda () ,conf)))

(provide 'ffe-config)

;;; ffe-config.el ends here
