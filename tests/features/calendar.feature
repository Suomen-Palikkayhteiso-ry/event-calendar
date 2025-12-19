# Reference: agents/stories/view-calendar.md
# ADRs: ADR-0012, ADR-0007

Feature: Calendar Navigation and Event Viewing
  As a user
  I want to view and navigate the calendar
  So that I can see upcoming events

  Scenario: Display the calendar page
    Given I am on the home page
    Then I should see the page title "Palikkakalenteri"
    And I should see the calendar header
    And I should see instructions for non-members

  Scenario: Navigate calendar dates
    Given I am on the home page
    Then I should see the calendar component
    And I should see previous and next navigation buttons

  Scenario: Display events on calendar
    Given I am on the home page
    Then the calendar should load without errors
    And events should be displayed if available

  Scenario: Handle date selection
    Given I am on the home page
    Then I should be able to select dates on the calendar

  Scenario: View event details
    Given I am on the home page
    And there is an event displayed on the calendar
    When I click on the event
    Then I should be navigated to the event detail page
    And I should see the event title, description, and dates