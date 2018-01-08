(Given "^no defined configurations$"
  (lambda ()
    (ignore)
    ))

(Then "^I should get error when loading \"\\([^\"]+\\)\" configuration$"
  (lambda (name)
    (let ((name-sym (intern name)))
      (should-error
       (ffe-config-load name-sym)
       :type 'ffe-config-error))
    ))

(Given "^defined \"\\([^\"]+\\)\" configuration and its dependecy \"\\([^\"]+\\)\"$"
  (lambda (name depname)
    (let ((name-sym (intern name))
          (depname-sym (intern depname)))
      (ffe-config depname-sym "Dependency description")
      (ffe-config name-sym "Config description"
                  :deps `(,depname-sym)
                  :init (lambda () (setq ffe-config-test/init-callback-executed t))
                  :packs '(test-dep)
                  :config (lambda ()
                            (if ffe-config-test/init-callback-executed
                                (setq ffe-config-test/config-callback-executed t)
                              (error "Config callback executed before init")))))
    ))

(When "^I load \"\\([^\"]+\\)\" configuration$"
  (lambda (name)
    (ffe-config-load (intern name))
    ))

(Then "^I should have \"\\([^\"]+\\)\" in loaded configurations list$"
  (lambda (name)
    (should (ffe-config-loaded-p (intern name)))
    ))

(And "^I should have packages installed and required$"
  (lambda ()
    (should ffe-config-test/test-dep-fetched)
    (should ffe-config-test/test-dep-required)
    ))

(And "^I should have :init and :config callbacks executed$"
  (lambda ()
    (should ffe-config-test/init-callback-executed)
    (should ffe-config-test/config-callback-executed)
    ))

(Given "^loaded configuration \"\\([^\"]+\\)\"$"
  (lambda (name)
    (let ((name-sym (intern name)))
      (ffe-config name-sym "Some config")
      (ffe-config-load name-sym))
    ))
