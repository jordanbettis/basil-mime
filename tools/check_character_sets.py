#!/usr/bin/python
###############################################################################
#
#                 THE BASIL MESSAGE HANDLING LIBRARY
#
###############################################################################
#
# PROJECT CONTEXT:
#
#  generate_subtype.py generates an ads file to represent each subtype
#  registered at IANA. However, handling character sets is more
#  complex and I expect the character set registry to be updated more
#  slowly than that of the MIME subtypes, so auto-generating the list
#  of character sets does not make as much sense.
#
#  Instead, I provide this script to download the file at
#  <http://www.iana.org/assignments/character-sets> and check that
#  basil knows about each.
#
###############################################################################

import urllib
import os

from sets import Set

class CharacterSets:
    """This defines a character set object. Its constructor takes the
    IANA registry file as input to produce the 'items' list of
    character set names and aliases"""
    def __init__(self, iana_file):

        self.items = []

        for line in iana_file:

            if line.startswith("Name: "):
                c_line = line[len("Name: "):]
                c_line = c_line.strip(" \r\n")
                index = c_line.find(" ")
                if index != -1:
                    self.items.append(c_line[:index])
                else:
                    if not c_line.startswith("None"):
                        self.items.append(c_line)

            elif line.startswith("Alias: "):
                c_line = line[len("Alias: "):]
                c_line = c_line.strip(" \r\n")
                index = c_line.find(" ")
                if index != -1:
                    self.items.append(c_line[:index])
                else:
                    if not c_line.startswith("None"):
                        self.items.append(c_line)

if __name__ == "__main__":
    iana_document_file = urllib.urlopen\
                         ("http://www.iana.org/assignments/character-sets")
    iana_document = iana_document_file.readlines()
    charsets = CharacterSets(iana_document)
    print
    print "-=-=-=-=-=- CHECK CHARACTER SETS START OUTPUT -=-=-=-=-=-"
    print
    for charset in charsets.items:
        ada_access = os.popen ("./check_character_sets", "w")
        ada_access.write (charset + "\n")
        ada_access.close()
    print
    print "-=-=-=-=-=- CHECK CHARACTER SETS STOP OUTPUT  -=-=-=-=-=-"
    print
