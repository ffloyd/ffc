Feature: Loading defined configuration
  After defining config
  User loads it by invoking ffe-config-load
  
  Rules
  - Config may be loaded only once
  - Loaded configs appears in loaded configs list
  
  Scenario: Loading undefined configuration
    Given no defined configurations
    Then I should get error when loading "example" configuration
 
  Scenario: Loading defined configuration
    Given I have straight.el loaded
    Given defined "example" configuration and its dependecy "dependency"
    When I load "example" configuration
    Then I should have "example" in loaded configurations list
    And I should have "dependency" in loaded configurations list
    And I should have packages installed and required
    And I should have :init and :config callbacks executed

  Scenario: Loaded already loaded configuration
    Given loaded configuration "example"
    Then I should get error when loading "example" configuration
    
