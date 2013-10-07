#!/bin/bash
###############################################################################
#
#                 THE BASIL MESSAGE HANDLING LIBRARY
#
###############################################################################
#
# PROJECT CONTEXT:
#
#  This script builds check_character_sets, runs the python script
#  check_character_sets.py, then cleans up the build.
#
###############################################################################

echo "** BUILDING CHECK_CHARACTER_SETS **"
echo

gnatmake -I../src check_character_sets

echo "** RUNNING CHECK_CHARACTER_SETS.PY **"
echo

./check_character_sets.py

echo "** CLEANING UP BUILD FILES **"
echo

gnatclean check_character_sets
