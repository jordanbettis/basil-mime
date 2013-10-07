#!/bin/bash
###############################################################################
#
#                 THE BASIL MESSAGE HANDLING LIBRARY
#
###############################################################################
#
# PROJECT CONTEXT:
#
#  This file removes any auto-generated .ads files from the GENERATED
#  SOURCE (../generated) directory.
#
###############################################################################

SUBTYPES="application audio image message model multipart text video"
SOURCE_DIR="../generated"

for SUBTYPE in $SUBTYPES
do
    if [[ -a $SOURCE_DIR/basil\_generated-$SUBTYPE\_subtypes.ads ]]
    then
        rm $SOURCE_DIR/basil\_generated-$SUBTYPE\_subtypes.ads
    fi
done
