;;; ffe-config-test.el --- Tests for ffe-config

(ert-deftest ffe-config/define-empty ()
  "Defines empty config"

  (with-sandbox
   (ffe-config 'example "Some doc")
   (should (alist-get 'example ffe-config-alist))))

(ert-deftest ffe-config/undefined-dependency ()
  "Raises error when configuration dependency is undefined"

  (with-sandbox
   (should-error
    (ffe-config 'example "Some doc" :deps '(some-dep))
    :type 'ffe-config-undefined-error)))

(ert-deftest ffe-config/missing-straight ()
  "Raises error when :packs key used but straight.el undefined"

  (with-sandbox
   (should-error
    (ffe-config 'example "Some doc" :packs '(cl-lib))
    :type 'ffe-config-no-straight-error)))

(ert-deftest ffe-config/straight-check ()
  "Defines config with :packs keyword if straight installed"

  (with-sandbox
   (with-mock
     (mock (featurep 'straight) => t)
     (ffe-config 'example "Some doc" :packs '(cl-lib))
     (should (alist-get 'example ffe-config-alist)))))

(ert-deftest ffe-config/string-as-name ()
  "Should raise error if name isn't symbol"

  (with-sandbox
   (should-error
    (ffe-config "example" "Some doc")
    :type 'ffe-config-invalid-name-error)))

(ert-deftest ffe-config-p/test ()
  "Return t only for defined config"

  (with-sandbox
   (push (cons 'example "Some config") ffe-config-alist)
   (should (ffe-config-p 'example))
   (should-not (ffe-config-p 'not-example))))

(ert-deftest ffe-config-load/undefined-configuration ()
  "Raises error if configuration undefined"

  (with-sandbox
   (should-error
    (ffe-config-load 'example)
    :type 'ffe-config-undefined-error)))

(ert-deftest ffe-config-load/load-dependecies ()
  "Sucessfully loads dependencies"

  (with-sandbox
   (ffe-config 'dependency "Dependency description")
   (ffe-config 'example "Config description" :deps '(dependency))
   (ffe-config-load 'example)
   (should (ffe-config-loaded-p 'example))
   (should (ffe-config-loaded-p 'dependency))))
  
 
(ert-deftest ffe-config-loaded-p/test ()
  "Return t only for defined config"

  (with-sandbox
   (push 'example ffe-config-loaded-list)
   (should (ffe-config-loaded-p 'example))
   (should-not (ffe-config-loaded-p 'not-example))))
 
;;; ffe-config-test.el ends here
