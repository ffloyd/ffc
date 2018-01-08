;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I define empty configuration \"\\([^\"]+\\)\"$"
  (lambda (name)
    (ffe-config (intern name) "Some configuration description")
    ))

(Then "^I should have \"\\([^\"]+\\)\" defined$"
  (lambda (config)
    (should (ffe-config-p (intern config)))
    ))

(But "^I should not have \"\\([^\"]+\\)\" loaded$"
  (lambda (config)
    (should-not (ffe-config-loaded-p (intern config)))
    ))

(And "^I define empty configuration \"\\([^\"]+\\)\" with dependency \"\\([^\"]+\\)\"$"
  (lambda (name dep)
    (let ((name-sym (intern name))
          (dep-sym (intern dep)))
      (ffe-config name-sym "Some configuration description"
                  :deps `(,dep-sym)))
    ))

(Given "^I have no defined configurations$"
  (lambda ()
    (ignore)
    ))

(Then "^I cannot define configuration \"\\([^\"]+\\)\" with dependency \"\\([^\"]+\\)\"$"
  (lambda (name dep)
    (let ((name-sym (intern name))
          (dep-sym (intern dep)))
      (should-error (ffe-config name-sym "Some configuration description"
                                :deps `(,dep-sym))))
    ))

(Given "^I have straight.el loaded$"
  (lambda ()
    (setq ffe-config-test/straight-installed t)
    ))

(When "^I define configuration \"\\([^\"]+\\)\" with package \"\\([^\"]+\\)\"$"
  (lambda (name pack)
    (let ((name-sym (intern name))
          (pack-sym (intern pack)))
      (ffe-config name-sym "Some configuration description"
                  :packs `(,pack-sym)))
    ))

(Given "^I haven't straight.el$"
  (lambda ()
    (setq ffe-config-test/straight-installed nil)
    ))

(Then "^I cannot define configuration \"\\([^\"]+\\)\" with package \"\\([^\"]+\\)\"$"
  (lambda (name pack)
    (let ((name-sym (intern name))
          (pack-sym (intern pack)))
      (should-error (ffe-config name-sym "Some configuration description"
                                :packs `(,pack-sym))))
    ))

(When "^I define configuration \"\\([^\"]+\\)\" with all keyword arguments$"
  (lambda (name)
    (ffe-config (intern name) "Some configuration description"
                :deps '()
                :init (lambda () (ignore))
                :packs '(cl-lib)
                :config (lambda () (ignore)))
    ))
