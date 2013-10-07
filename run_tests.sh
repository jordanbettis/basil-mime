#!/bin/bash
###############################################################################
#
#                  THE BASIL MESSAGE HANDLING LIBRARY
#
###############################################################################
#
# PROJECT CONTEXT:
#  
#  This script builds and runs the test suite. The test suite's .gpr
#  file includes basil's .gpr file, so running this script is
#  sufficient to build the library and then test it.
#
#  Please note that you need to have aunit installed and there needs
#  to be a valid reference to it in test_basil.gpr.
#
###############################################################################

# Basil is built to be linked dynamically, and we don't override it
# from the test suite, but if you're running the test suite before
# installing basil in your system library path, you need to add the
# ./library directory to the path before running it.
LINK_COMMAND="env LD_LIBRARY_PATH=LD_LIBRARY_PATH:./library"

gnatmake -Ptest_basil && $LINK_COMMAND ./functional_tests
