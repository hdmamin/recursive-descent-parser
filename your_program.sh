#!/bin/sh

#
# Use this script to run your program LOCALLY.
#
# Note: Changing this script WILL NOT affect how CodeCrafters runs your program.
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit early if any commands fail

# HDM modifications to get offline testing working
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export PYTHONPATH="$SCRIPT_DIR/src:$PYTHONPATH"

# Copied from .codecrafters/run.sh
# - Edit this to change how your program runs locally
# - Edit .codecrafters/run.sh to change how your program runs remotely
exec pipenv run python3 -m lox.main "$@"
# Or use this to run in interactive mode.
# exec pipenv run ipython3 -i -m lox.main "$@"
