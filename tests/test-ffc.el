(when (require 'undercover nil t)
  (undercover "ffc.el"))

(require 'ffc)

;;
;; Helpers
;;

(defun helper/reset-state ()
  "Reset all FFC variables to default values"

  (setq ffc-alist nil)
  (setq ffc-features-alist nil)
  (setq ffc-loaded-list nil)
  (setq ffc-failed-list nil))

;;
;; Tests
;;

(describe "ffc-define function"
          :var (name docstring on-define on-load) ;; helper variables

          (before-each
           (helper/reset-state))
          
          (before-all ;; build correct arguments
           (setq name 'name-symbol)
           (setq docstring "Documentation string.")
           (setq on-define #'ignore)
           (setq on-load (lambda () (ignore))))
           
          (it "defines new configuration when args is correct"
              (ffc-define name docstring on-define on-load)
              (expect (alist-get name ffc-alist) :to-be-truthy))

          (it "raises error when NAME isn't a symbol"
              (expect
               (ffc-define "not a symbol" docstring on-define on-load)
               :to-throw 'ffc-invalid-name-error))

          (it "raises error when DOCSTRING isn't a string"
              (expect
               (ffc-define name 'not-a-string on-define on-load)
               :to-throw 'ffc-invalid-docstring-error))

          (it "raises error when ON-DEFINE isn't a function"
              (expect
               (ffc-define name docstring "not a function" on-load)
               :to-throw 'ffc-invalid-on-define-error))

          (it "raises error when ON-LOAD isn't a function"
              (expect
               (ffc-define name docstring on-define "not a function")
               :to-throw 'ffc-invalid-on-load-error))

          (it "raises error when configuration with same NAME already loaded"
              (ffc-define name docstring on-define on-load)
              (expect
               (ffc-define name docstring on-define on-load)
               :to-throw 'ffc-already-defined-error))

          (it "executes ON-DEFINE function"
              (setq on-define (lambda ()
                                (setq executed t)))
              (let ((executed nil))
                (ffc-define name docstring on-define on-load)
                (expect executed :to-be t))))

(describe "ffc-load function"
          :var (on-load-executed)

          (before-each
           (helper/reset-state)
           (setq on-load-executed nil)

           (ffc-define 'my-conf
                       "Example configuration"
                       #'ignore
                       (lambda () (setq on-load-executed t)))

           (ffc-define 'broken-conf
                       "Example broken configuration"
                       #'ignore
                       (lambda () (signal 'error "I'm fucked up"))))

          (it "adds correct executed config to ffc-loaded-list"
              (ffc-load 'my-conf)
              (expect ffc-loaded-list
                      :to-equal '(my-conf)))
          
          (it "adds failed config and happened error to ffc-failed-alist"
              (ffc-load 'broken-conf)
              (expect ffc-failed-alist
                      :to-equal '((broken-conf error . "I'm fucked up"))))
          
          (it "calls ON-LOAD when configuration NAME defined"
              (ffc-load 'my-conf)
              (expect on-load-executed :to-be t))

          (it "raises error when configuration NAME undefined"
              (expect
               (ffc-load 'i-am-not-defined)
               :to-throw 'ffc-undefined-error))

          (it "raises error when configuration NAME already loaded"
              (ffc-load 'my-conf)
              (expect
               (ffc-load 'my-conf)
               :to-throw 'ffc-double-loading-error)))
          
(describe "ffc-apply function"
          :var (execution-order)
          
          (before-each ;; nilify affected library variables
           (helper/reset-state))

          (it "loads configs in definition order"
              (setq execution-order nil)
              (ffc-define 'my-conf-1
                          "Example configuration 1"
                          #'ignore
                          (lambda () (push 1 execution-order)))

              (ffc-define 'my-conf-2
                          "Example configuration 2"
                          #'ignore
                          (lambda () (push 2 execution-order)))
              
              (ffc-apply)

              ;; it's not (1 2) bcs `push` adds element to head of a list
              (expect execution-order :to-equal '(2 1))))

(describe "ffc-define-feature function"
          :var (on-def-lambda on-load-lambda)

          (before-all
           (setq on-def-lambda (lambda (data)
                                 (lambda () "for on-define")))
           (setq on-load-lambda (lambda (data)
                                  (lambda () "for on-load"))))
          
          (before-each
           (helper/reset-state))

          (it "creates a new feature in ffc-features-alist"
              (ffc-define-feature :feature-1
                                  on-def-lambda
                                  on-load-lambda)
              (expect ffc-features-alist
                      :to-equal `((:feature-1 ,on-def-lambda ,on-load-lambda))))

          (it "raises error if on-define-lambda is not a function"
              (expect
               (ffc-define-feature :feature-1 'not-a-function on-load-lambda)
               :to-throw 'ffc-invalid-feature-on-define-error))

          (it "raises error if on-load-lambda is not a function"
              (expect
               (ffc-define-feature :feature-1 on-def-lambda 'not-a-function)
               :to-throw 'ffc-invalid-feature-on-load-error))

          (it "raises error if key is not a keyword"
              (expect
               (ffc-define-feature 'not-a-keyword on-def-lambda on-load-lambda)
               :to-throw 'ffc-invalid-feature-name-error)))

(describe "ffc-feature macro"
          (before-each
           (helper/reset-state))

          (it "defines feature"
              (ffc-feature my-feature
                           :definer (progn
                                      "I'm definer block")
                           :loader (progn
                                     "I'm loader block"))
              (expect ffc-features-alist
                      :to-equal
                      '((:my-feature
                         (lambda (data)
                           (progn "I'm definer block"))
                         (lambda (data)
                           (progn "I'm loader block"))))))

          (it "can create empty feature"
              (ffc-feature empty-feature)
              (expect ffc-features-alist
                      :to-equal
                      '((:empty-feature
                         (lambda (data) nil)
                         (lambda (data) nil))))))
                           
