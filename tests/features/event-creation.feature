Feature: Event Creation
  As an authenticated member
  I want to create events
  So that I can add them to the calendar

  Scenario: Redirect non-authenticated users
    Given I am not authenticated
    When I navigate to the event creation page
    Then I should be redirected to the home page
    And I should see membership instructions

  Scenario: Create event as authenticated user
    Given I am authenticated
    When I navigate to the event creation page
    And I fill in the event form with valid data
    And I submit the form
    Then I should see a success message
    And the event should be created

  Scenario: Validate required fields
    Given I am authenticated
    When I navigate to the event creation page
    And I submit the form without filling required fields
    Then I should see validation errors