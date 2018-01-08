Feature: Safe loading of config
  In order of safe (error proof) loading
  User invokes ffe-config-safe-load

  Rules:
  - returns t if loading successfull
  - returns nil if loading unsucessfull

  Scenario: Safe loading unexisted feature
    Given no defined configurations
    Then safe load of "example" returns nil
    
  Scenario: Safe loading defined feature and dependency
    Given I have straight.el loaded
    Given defined "example" configuration and its dependecy "dependency"
    Then I sucessfully safe load "example"

  Scenario: Safe loading already loaded feature
    Given I have straight.el loaded
    Given defined "example" configuration and its dependecy "dependency"
    When I load "example" configuration
    Then safe load of "example" returns nil
