# Reference: agents/stories/view-event-list.md, agents/stories/import-events.md
# ADRs: ADR-0012, ADR-0004

Feature: Event List Management
  As an authenticated member
  I want to view and manage events in a list
  So that I can organize and import events

  Scenario: View event list as authenticated user
    Given I am authenticated
    When I navigate to the events page
    Then I should see a list of events
    And I should see edit and delete buttons for each event

  Scenario: Import events from KML
    Given I am authenticated
    And I am on the events page
    When I upload a valid KML file
    Then events should be imported and appear in the list

  Scenario: Filter events by title
    Given I am authenticated
    And I am on the events page
    When I enter "test" in the title filter
    Then only events with "test" in the title should be displayed

  Scenario: Filter events by date
    Given I am authenticated
    And I am on the events page
    When I select a date in the date filter
    Then only events on that date should be displayed

  Scenario: Filter events by status
    Given I am authenticated
    And I am on the events page
    When I select "Draft" in the status filter
    Then only draft events should be displayed

  Scenario: Sort events by title
    Given I am authenticated
    And I am on the events page
    When I click the title sort button
    Then events should be sorted alphabetically by title

  Scenario: Sort events by date
    Given I am authenticated
    And I am on the events page
    When I click the date sort button
    Then events should be sorted by date