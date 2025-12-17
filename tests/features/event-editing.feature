# Reference: agents/stories/edit-event.md, agents/stories/delete-event.md

Feature: Event Editing and Deletion
  As an authenticated member
  I want to edit and delete events
  So that I can manage event information

  Scenario: Edit existing event
    Given I am authenticated
    And there is an existing event
    When I click the edit button for the event
    And I modify the event details
    And I submit the changes
    Then I should see a success message
    And the event should be updated

  Scenario: Delete event
    Given I am authenticated
    And there is an existing event
    When I change the event status to deleted
    Then I should see a success message
    And the event should no longer be visible