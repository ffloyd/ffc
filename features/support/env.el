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
 (defvar ffe-config-test/straight-installed)

 (defun ffe-config-test/featurep-patch (real-impl feature)
   "Straight.el installation simulation patch"

   (if (equal 'straight feature)
       ffe-config-test/straight-installed
     (real feature)))
   
 (advice-add 'featurep :around 'ffe-config-test/featurep-patch)
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 (setq ffe-config-alist nil)
 (setq ffe-config-loaded-alist nil)
 (setq ffe-config-test/straight-installed nil)
 )

(Teardown
 ;; After when everything has been run
 (advice-remove 'featurep 'ffe-config-test/featurep-patch)
 )
