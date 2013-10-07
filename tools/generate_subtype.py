#!/usr/bin/python
###############################################################################
#
#                 THE BASIL MESSAGE HANDLING LIBRARY
#
###############################################################################
#
# PROJECT CONTEXT:
#
#  This python script is used to generate the Content_Subtype files
#  which define the enumerations used by the Basil library, based on
#  data from IANA at <http://www.iana.org/assignments/media-types/>
#  and cleansed by the html2text program.
#
#  It takes as input the output of text2html, and the first argument
#  must be the name of the content type.
#
###############################################################################

import os
import sys
import time
import socket

from sets import Set

class TranslationError(Exception):
    """This script is rather brittle and subject to failure when the
    format of the IANA document changes. We try to at least detect
    when things are going wrong, and raise this exception so the
    script can be modified."""

    pass

class EnumerationLabel:
    """Enumeration labels are formed by translating a MIME subtype
    label passed when the object is created. Printing the object
    prints the label"""

    def __init__(self, word):
        self.word = word
        self._enumerationify()
        self._capitalize_canonically()
        self._validate()

    def __str__(self):
        return self.word

    def _enumerationify(self):

        if self.word.upper().endswith("-P") or \
           self.word.upper().endswith("-D") :

            raise TranslationError("Can not translate this label: " \
                                   + self.word)

        self.word = self.word.replace(".", "_D_")
        self.word = self.word.replace("+", "_P_")
        self.word = self.word.replace("-", "_")
        self.word = self.word.rstrip("_")
        self.word = "ST_" + self.word

    def _capitalize_canonically(self):

        state = "caps"
        canonical_word = ""

        for char in self.word:
            if char == "_":
                state = "caps"
            elif state == "caps":
                char = char.upper()
                state = "normal"
            canonical_word += char

        self.word = canonical_word

    def _validate(self):

        if self.word.find('__') != -1:
            raise TranslationError("Label validation failure: " + self.word)

        if self.word.replace("_", "").isalnum() != True:
            raise TranslationError("Label validation failure: " + self.word)

class EnumerationArray:
    """This class defines an object containing a set of
    EnumerationLabel class objects. Its constructor takes an array of
    lines consisting of an IANA type description document and the
    expected MIME type, to generate the EnumerationLabels list"""

    def __init__(self, iana_document, subtype):

        iana_document = self._strip_matter(iana_document, "after")
        iana_document = self._strip_matter(iana_document, "before")
        iana_document = self._cleanse(iana_document)

        if iana_document[0] != subtype:
            raise TranslationError("Wrong Subtype: " + self.subtype)

        iana_document.pop(0)

        self.items = []

        for label in iana_document:
            self.items.append(EnumerationLabel(label))

    def _strip_matter(self, iana_document, direction):

        delimiter = self._get_delimiter(iana_document)

        if direction == "after":
            return iana_document[delimiter + 1:]

        elif direction == "before":
            return iana_document[:delimiter]

    def _get_delimiter(self, iana_document):
        for i, line in enumerate(iana_document):
            if Set(line) == Set(['\n','=']):
                return i

    def _cleanse(self, iana_document):
        new_doc = []

        for line in iana_document:

            line = line.lstrip(" ")
            line = line.replace("_-_", " ")
            line = line[:line.find(" ")]

            new_doc.append(line)

        return new_doc

class AdaSourceSkeleton:
    """This class is initialized using the name of a file which
    contains the skeleton that contains the source file."""

    def __init__(self, filename):
        skeleton_file = file(filename)
        self.skeleton = skeleton_file.read()
        skeleton_file.close()

    def generate_file(self, enumerations, subtype):
        date = self._get_date()
        cap_subtype = subtype.capitalize()
        enums = self._get_enumerations(enumerations)

        username = os.getenv("LOGNAME")
        if username == "":
            username = "unknown"

        hostname = socket.gethostname()
        if hostname == "":
            hostname = "unknown"

        output = self.skeleton.replace("LCASE_SUBTYPE_NAME", subtype)
        output = output.replace("UCASE_SUBTYPE_NAME", cap_subtype)
        output = output.replace("GENERATED_DATE", date)
        output = output.replace("SUBTYPE_ENUMERATION_LIST", enums)
        output = output.replace("USER_NAME", username)
        output = output.replace("HOST_NAME", hostname)

        return output

    def _get_date(self):
        return time.strftime("%a, %d %b %Y %H:%M %Z", time.localtime())

    def _get_enumerations(self, enumerations):
        spacing = "      "
        enum_list = "ST_Unknown,\n      ST_Experimental,\n"

        for enum in enumerations.items:
            enum_list += spacing + enum.__str__() + ",\n"

        return enum_list[:len(enum_list)-2]

if __name__ == "__main__":
    iana_document = sys.stdin.readlines()
    enums = EnumerationArray(iana_document, sys.argv[1])
    skel = AdaSourceSkeleton("content_subtypes_skeleton.ads")
    output_file = file("basil_generated-" + sys.argv[1] + "_subtypes.ads", "w")
    output_file.write (skel.generate_file(enums, sys.argv[1]))
