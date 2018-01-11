Feature: Safe load all
  In order to safe load all loadable configurations
  User applies ffe-config-safe-load-all function

  Scenario: Load all when no configs defined
    Given I have no configurations
    When I safe load all configurations
    Then I have no loaded configurations
    
  Scenario: Load several empty configurations
    Given I define empty configuration "example1"
    And I define empty configuration "example2"
    When I safe load all configurations
    Then I have loaded configurations:
      | example1 |
      | example2 |

  Scenario: Load all when incorrect config present
    Given I define configuration "incorrect" with params:
      | Param | Value                                          |
      | init  | (lambda () (error "Unfortunately, fuckoff!" )) |
    And I define empty configuration "correct1"
    And I define empty configuration "correct2"
    When I safe load all configurations
    Then I have loaded exact following configurations:
      | correct1 |
      | correct2 |
 
