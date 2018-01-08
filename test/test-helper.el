;;; test-helper.el --- Helpers for ffe-config-test.el

;;
;; Initialization
;;

(require 'el-mock)

(require 'undercover)
(undercover "ffe-config.el")

(defvar ffe-config/test-root
  (directory-file-name (file-name-directory load-file-name))
  "Directory with tests.")

(defvar ffe-config/lib-root
  (directory-file-name (file-name-directory ffe-config/test-root))
  "Library root.")

(load-file (expand-file-name "ffe-config.el" ffe-config/lib-root))

;;
;; Helpers
;;

(defmacro with-sandbox (&rest body)
  "Evaluate BODY with sandboxed ffe-config vars."
  
  `(let (ffe-config-alist
         ffe-config-loaded-list
         ffe-config-failed-list)
     ,@body))

;;; test-helper.el ends here
