Feature: Defining configuration
  In order to define new configuration
  User applies `ffe-config` function

  Scenario: Empty configuration
    Given I have no configurations
    When I define empty configuration "example"
    Then I have defined configurations:
      | example |

  Scenario: With dependencies
    Given I define empty configuration "dependency"
    When I define configuration "example" with params:
      | Param | Value         |
      | deps  | '(dependency) |
    Then I have defined configurations:
      | dependency |
      | example    |

  Scenario: Missing dependency
    Given I have no configurations
    Then I get error when defining configuration "example" with params:
      | Param | Value         |
      | deps  | '(dependency) |

  Scenario: Key :packs forbidden when straight.el uninitialized
    Given I have no straight.el initialized
    Then I get error when defining configuration "example" with params:
      | Param | Value     |
      | packs | '(cl-lib) |

  Scenario: Key :packs allowed when straight.el initialized
    Given I have straight.el initialized
    When I define configuration "example" with params:
      | Param | Value     |
      | packs | '(cl-lib) |
    Then I have defined configurations:
      | example |

  Scenario: Configuration with all keys provided
    Given I have straight.el initialized
    And I define empty configuration "dependency"
    When I define configuration "example" with params:
      | Param | Value         |
      | deps  | '(dependency) |
      | init  | 'ignore       |
      | packs | '(cl-lib)     |
      | conf  | 'ignore       |
    Then I have defined configurations:
      | dependency |
      | example    |

  Scenario: Configuration with unexpected keys provided
    Given I have no configurations
    Then I get error when defining configuration "example" with params:
      | Param    | Value          |
      | bullshit | bullshit-value |
