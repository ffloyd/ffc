(require 'f)

(defvar ffe-config-support-path
  (f-dirname load-file-name))

(defvar ffe-config-features-path
  (f-parent ffe-config-support-path))

(defvar ffe-config-root-path
  (f-parent ffe-config-features-path))

(add-to-list 'load-path ffe-config-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'ffe-config)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run

 ;; define helper variables
 (defvar ffe-config-test/straight-installed nil nil)
 (defvar ffe-config-test/test-dep-fetched nil nil)
 (defvar ffe-config-test/test-dep-required nil nil)
 (defvar ffe-config-test/init-callback-executed nil nil)
 (defvar ffe-config-test/conf-callback-executed nil nil)
 (defvar ffe-config-test/messages nil nil)

 ;; mock featurep function
 (defun ffe-config-test/featurep-patch (real-impl feature)
   "Straight.el installation simulation patch"

   (if (equal 'straight feature)
       ffe-config-test/straight-installed
     (real feature)))

 (advice-add 'featurep :around 'ffe-config-test/featurep-patch)
   
 ;; mock require function
 (defun ffe-config-test/require-patch (real-impl feature)
   "(require 'test-dep) simulation patch"

   (if (equal 'test-dep feature)
       (progn
         (setq ffe-config-test/test-dep-required t)
         t)
     (real feature)))
   
 (advice-add 'require :around 'ffe-config-test/require-patch)

 ;; mock straight-use-package function
 (defun straight-use-package (package)
   "(straight-use-package 'test-dep) simulation patch"

   (if (or
        (equal 'test-dep package)
        (equal 'test-dep (car package)))
       (setq ffe-config-test/test-dep-fetched t)
     (error "Unexpected straight-use-package call: %S" package)))

 ;; supress messages during testing
 (defun ffe-config-test/message-patch (real-imlp &rest args)
   "Redirect messages to ffe-config-test/messages"

   (push (apply 'format-message args) ffe-config-test/messages))
 
 (advice-add 'message :around 'ffe-config-test/message-patch)
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run reset all realted variables
 (setq ffe-config-alist nil)
 (setq ffe-config-loaded-list nil)
 (setq ffe-config-test/straight-installed nil)
 (setq ffe-config-test/init-callback-executed nil)
 (setq ffe-config-test/config-callback-executed nil)
 (setq ffe-config-test/test-dep-fetched nil)
 (setq ffe-config-test/test-dep-required nil)
 (setq ffe-config-test/messages nil)
 )

(Teardown
 ;; After when everything has been run
 (advice-remove 'featurep 'ffe-config-test/featurep-patch)
 (advice-remove 'require 'ffe-config-test/require-patch)
 (advice-remove 'message 'ignore)
 )
