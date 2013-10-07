#!/bin/bash
###############################################################################
#
#                 THE BASIL MESSAGE HANDLING LIBRARY
#
###############################################################################
#
# PROJECT CONTEXT:
#
#  This file fetches each of the subtype files from
#  <http://www.iana.org/assignments/>, runs them through
#  generate_subtype.py to produce the auto-generated ads files for the
#  subtype definitions.
#
#  To run this script you need:
#     python
#     wget
#     html2text
#
###############################################################################

SUBTYPES="application audio image message model multipart text video"
IANA_URL="http://www.iana.org/assignments/media-types"
SOURCE_DIR="../generated"

for SUBTYPE in $SUBTYPES
do
    if [[ -a basil-$SUBTYPE\_subtypes.ads ]]
    then
        rm basil\_generated-$SUBTYPE\_subtypes.ads
    fi
done

for SUBTYPE in $SUBTYPES
do
    wget -O - $IANA_URL/$SUBTYPE/ | html2text -ascii -nobs -width 800 | ./generate_subtype.py $SUBTYPE
done

for SUBTYPE in $SUBTYPES
do
    mv basil\_generated-$SUBTYPE\_subtypes.ads $SOURCE_DIR/basil\_generated-$SUBTYPE\_subtypes.ads
done

if [[ -a generate_subtype.pyc ]]
then
    rm generate_subtype.pyc
fi
