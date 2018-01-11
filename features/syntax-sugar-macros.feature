Feature: Syntax sugar macros
  In order to simplify defining configuration
  User applies ffc macro

  Scenario: Define empty config
    Given I have no configurations
    When I define empty configuration "example" via ffc macros
    Then I have defined configurations:
      | example |

  Scenario: Define full-featured config
    Given I have straight.el initialized
    And I define empty configuration "dependency"
    When I define configuration "example" via ffc macros with params:
      | Param | Value        |
      | deps  | (dependency) |
      | init  | (ignore)     |
      | packs | (test-dep)   |
      | conf  | (ignore)     |
    Then I have defined configurations:
      | dependency |
      | example    |
