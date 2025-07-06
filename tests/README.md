# Interpreter Challenge Tester

This is a program that validates your progress on the "Build your own interpreter" challenge.

## Requirements for binary

- Following environment variables:
  - `CODECRAFTERS_SUBMISSION_DIR` - root of the user's code submission
  - `CODECRAFTERS_TEST_CASES_JSON` - test cases in JSON format

HDM notes:
- need to export these if you want them to be available in makefile:
```export CODECRAFTERS_SUBMISSION_DIR=abc```

## User code requirements

- A binary named `your_program.sh` that executes the program.
- A file named `codecrafters.yml`, with the following values:
  - `debug`
