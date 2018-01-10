;;
;; Step definitions
;;

(Given "^I have no configurations$"
       (lambda ()
         (ignore)))

(When "^I define empty configuration \"\\([^\"]+\\)\"$"
      (lambda (name-str)
        (let ((name (intern name-str)))
          (ffe-config name "Some config"))))

(Then "^I have defined configurations:$"
      (lambda (name-strings)
        (let ((names (hf/strll-to-syml name-strings)))
          (cl-mapc (lambda (name)
                     (should (ffe-config-p name)))
                   names))))

(When "^I define configuration \"\\([^\"]+\\)\" with params:$"
      (lambda (name-str kwargs-table)
        (hf/define-config name-str kwargs-table)))

(Then "^I get error when defining configuration \"\\([^\"]+\\)\" with params:$"
      (lambda (name-str kwargs-table)
        (should-error (hf/define-config name-str kwargs-table))))

(Given "^I have no straight.el initialized$"
       (lambda ()
         (ignore)))

(Given "^I have straight.el initialized$"
       (lambda ()
         (setq ffe-config-test/straight-installed t)))

(When "^I load configuration \"\\([^\"]+\\)\"$"
      (lambda (name-str)
        (let ((name (intern name-str)))
          (ffe-config-load name))))

(Then "^I have loaded configurations:$"
      (lambda (name-strings)
        (let ((names (hf/strll-to-syml name-strings)))
          (cl-mapc (lambda (name)
                     (should (ffe-config-loaded-p name)))
                   names))))

(Then "^I get error when loading configuration \"\\([^\"]+\\)\"$"
      (lambda (name-str)
        (let ((name (intern name-str)))
          (should-error (ffe-config-load name)))))

(Then "^I have package \"test-dep\" fetched$"
      (lambda ()
        (should ffe-config-test/test-dep-fetched)))

(And "^I have package \"test-dep\" required$"
     (lambda ()
       (should ffe-config-test/test-dep-required)))

(Then "^I have :init callback executed$"
      (lambda ()
        (should ffe-config-test/init-callback-executed)))

(And "^I have :conf callback executed$"
     (lambda ()
       (should ffe-config-test/conf-callback-executed)))

(When "^I safe load configuration \"\\([^\"]+\\)\"$"
      (lambda (name-str)
        (let ((name (intern name-str)))
          (ffe-config-safe-load name))))

(Then "^I have no loaded configurations$"
      (lambda ()
        (should-not ffe-config-loaded-list)))

;;
;; Helper functions
;;

(defun hf/table-to-plist (table)
  "Extracts parsed elisp value from Gherkin table

For example this expression:

(let ((table '((\"header1\" \"header2\") . ((\"one\" \"symbol\")
                                        (\"two\" \"(list)\")
                                        (\"three\" \"\\\"string\\\"\")))))
  (ffe-config/table-to-kwargs table))

equals following form: (:one symbol :two (list) :three \"string\") 
"

  (let ((rows (cdr table))
        (reducer (lambda (acc el)
                   (let* ((raw-key (car el))
                          (key (car (read-from-string (concat ":" raw-key))))
                          (raw-value (eval (cadr el)))
                          (value (car (read-from-string raw-value))))
                     (plist-put acc key value)
                   ))))
    (cl-reduce reducer rows :initial-value '())))


(defun hf/define-config (name-str kwars-table)

  (let* ((name (intern name-str))
         (kwargs (hf/table-to-plist kwars-table))
         (sexp `(ffe-config ',name "Some Doc" ,@kwargs)))
    (eval sexp)))

(defun hf/strll-to-syml (strings)
  "Converts list like ((\"one\") (\"two\")) to (one two)" 

  (cl-mapcar (lambda (str-in-list)
               (intern (car str-in-list)))
             strings))
