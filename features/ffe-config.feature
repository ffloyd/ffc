Feature: Defining confuguration via ffe-config function
  In order to define new configuration
  User invoke ffe-config function

  Rules:
  - Configuration only defined, not loaded
  - Configuration name should be a symbol
  - Config dependencies should be already defined
  - Config updating forbidden
  - Keyword :packages availble only if striaght.el installed

  Scenario: Define empty configuration
    When I define empty configuration "example"
    Then I should have "example" defined
    But I should not have "example" loaded

  Scenario: Define configuration with string as name
    Then I cannot define configuration with string name
    
  Scenario: Define two dependent cofigurations
    When I define empty configuration "dependency"
    And I define empty configuration "example" with dependency "dependency"
    Then I should have "example" defined
    And I should have "dependency" defined
    
  Scenario: Define configuration with undefined dependency
    Given I have no defined configurations
    Then I cannot define configuration "example" with dependency "dependency"

  Scenario: Define configuration with package to load
    Given I have straight.el loaded
    When I define configuration "example" with package "cl-lib" 
    Then I should have "example" defined

  Scenario: Define configuration with package to load when straight.el unavailable
    Given I haven't straight.el
    Then I cannot define configuration "example" with package "cl-lib"
    
  Scenario: Define full featured configuration
    Given I have straight.el loaded
    When I define configuration "example" with all keyword arguments
    Then I should have "example" defined
