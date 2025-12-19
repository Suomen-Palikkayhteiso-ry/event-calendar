# Reference: agents/stories/view-events-on-map.md
# ADRs: ADR-0012, ADR-0006

Feature: Map View
  As a user
  I want to view events on a map
  So that I can see their geographical locations

  Scenario: Display map with events
    Given I am on the map page
    Then I should see a map centered on Helsinki
    And event markers should be displayed on the map

  Scenario: Interact with map markers
    Given I am on the map page
    And there are event markers on the map
    When I click on a marker
    Then I should see event information