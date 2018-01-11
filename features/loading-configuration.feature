Feature: Loading configuration
  In order to load defined configuration
  User applies `ffe-config-load` function

  Scenario: Loading empty configuration
    Given I define empty configuration "example"
    When I load configuration "example"
    Then I have loaded configurations:
      | example |

  Scenario: Loading undefined configuration fails
    Given I have no configurations
    Then I get error when loading configuration "example"

  Scenario: Loading configuration with dependency
    Given I define empty configuration "dependency"
    And I define configuration "example" with params:
      | Param | Value         |
      | deps  | '(dependency) |
    When I load configuration "example"
    Then I have loaded configurations:
      | dependency |
      | example    |
      
  Scenario: Shared dependency
    Given I define empty configuration "dependency"
    And I define configuration "example1" with params:
      | Param | Value         |
      | deps  | '(dependency) |
    And I define configuration "example2" with params:
      | Param | Value         |
      | deps  | '(dependency) |
    When I load configuration "example1"
    And I load configuration "example2"
    Then I have loaded configurations:
      | dependency |
      | example1   |
      | example2   |

  Scenario: Loading configuration with package
    Given I have straight.el initialized
    And I define configuration "example" with params:
      | Param | Value       |
      | packs | '(test-dep) |
    When I load configuration "example"
    Then I have package "test-dep" fetched
    And I have package "test-dep" required
    
  Scenario: Loading configuration with package recepie
    Given I have straight.el initialized
    And I define configuration "example" with params:
      | Param | Value                     |
      | packs | '((test-dep :no-build t)) |
    When I load configuration "example"
    Then I have package "test-dep" fetched
    And I have package "test-dep" required

  Scenario: Loading configuration with callbacks
    Given I define configuration "example" with params:
      | Param | Value                                                       |
      | init  | (lambda () (setq ffe-config-test/init-callback-executed t)) |
      | conf  | (lambda () (setq ffe-config-test/conf-callback-executed t)) |
    When I load configuration "example"
    Then I have :init callback executed  
    And I have :conf callback executed
