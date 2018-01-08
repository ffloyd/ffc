(Then "^safe load of \"\\([^\"]+\\)\" returns nil$"
  (lambda (name)
    (should-not (ffe-config-safe-load (intern name)))
    ))

(Then "^I sucessfully safe load \"\\([^\"]+\\)\"$"
  (lambda (name)
    (should (ffe-config-safe-load (intern name)))
    ))
