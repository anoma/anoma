# Prototypes

A prototype should start with a description of its goals. These can include, but are not limited to a proof of concept of novel ideas or alternative approaches, comparing different libraries and gathering feedback.

To get started on a prototype, please:
- open an issue on this repository
- add a sub-page to this section with a link to the issue

The page outlines the goals and possibly contains any notes that are not suitable to be added to the prototype source itself, while the issue should track the sub-task, their progress, and assignees.

The code quality is of lesser importance in prototypes. To put the main focus on the prototype's goals, we don't need to worry much about testing, linting and doc strings.

## Advancing a successful prototype

Once the goals of the prototype have been completed, we can assess if we'd like to advance the prototype to a development version. 

In order to advance a prototype, in general we'll want to:
- review & clean-up the code for lint, format and best practices
- enable common Rust lints
- review any new dependencies
- add docs for any public interface (internally public too)
- add automated tests
- if the prototype has diverged from the original design, update these pages 
