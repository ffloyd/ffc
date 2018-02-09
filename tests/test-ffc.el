(when (require 'undercover nil t)
  (undercover "ffc.el"))

(require 'ffc)

;;
;; Helpers
;;

(defun helper/reset-state ()
  "Reset all FFC variables to default values"

  (setq ffc-features-alist nil)
  (setq ffc-pipeline nil)
  (setq ffc-alist nil)
  (setq ffc-loaded-list nil)
  (setq ffc-failed-list nil))

;;
;; Tests
;;

(describe "ffc--define-feature"
          :var (keyword-key on-def-lambda on-load-lambda)
          
          (before-all
           (setq keyword-name :some-keyword)
           (setq on-def-lambda (lambda (data) "Some definer" (ignore)))
           (setq on-load-lambda (lambda (data) "Some loader" (ignore))))

          (before-each
           (helper/reset-state))

          (it "defines feature when args are correct"
              (ffc--define-feature keyword-name
                                   on-def-lambda
                                   on-load-lambda)

              (expect (alist-get keyword-name ffc-features-alist)
                      :to-equal
                      `(,on-def-lambda ,on-load-lambda)))

          (it "requires NAME to be a keyword"
              (expect
               (ffc--define-feature 'not-a-keyword on-def-lambda on-load-lambda)
               :to-throw 'ffc-feature-invalid-name-error))

          (it "prevents recreating features"
              (ffc--define-feature keyword-name
                                   on-def-lambda
                                   on-load-lambda)
              (expect
               (ffc--define-feature keyword-name
                                    on-def-lambda
                                    on-load-lambda)
               :to-throw 'ffc-feature-already-defined-error))

          (it "requires ON-DEFINE to be a function"
              (expect
               (ffc--define-feature keyword-name "not-a-function" on-load-lambda)
               :to-throw 'ffc-feature-invalid-on-define-error))

          (it "requires ON-LOAD to be a function"
              (expect
               (ffc--define-feature keyword-name on-def-lambda "not-a-function")
               :to-throw 'ffc-feature-invalid-on-load-error)))
                                 
(describe "ffc--setup-pipeline"
          (before-each
           (helper/reset-state)
           
           (ffc--define-feature :feature_a 'ignore 'ignore)
           (ffc--define-feature :feature_b 'ignore 'ignore))

          (it "setups pipeline from existing features"
              (ffc--setup-pipeline '(:feature_a :feature_b))
              (expect ffc-pipeline
                      :to-equal
                      '(:feature_a :feature_b)))

          (it "forbids pipeline redefinition"
              (ffc--setup-pipeline '(:feature_a))

              (expect (ffc--setup-pipeline '(:feature_b))
               :to-throw 'ffc-pipeline-redefinition-error)

              (expect ffc-pipeline
                      :to-equal
                      '(:feature_a)))

          (it "forbids usage of undefined features"
              (expect (ffc--setup-pipeline '(:feature_a :undefined))
                      :to-throw 'ffc-feature-not-found-error)))
              
