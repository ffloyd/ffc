Feature: Safe loading
  In order to supress errors while loading configuration
  User applies ffe-config-safe-load function

  Scenario: Load defined config
    Given I define empty configuration "example"
    When I safe load configuration "example"
    Then I have loaded configurations:
      | example |

  Scenario: Load undefined config
    Given I have no configurations
    When I safe load configuration "example"
    Then I have no loaded configurations

  Scenario: Load incorrect config
    Given I define configuration "example" with params:
      | Param | Value                                          |
      | init  | (lambda () (error "Unfortunately, fuckoff!" )) |
    When I safe load configuration "example"
    Then I have no loaded configurations
