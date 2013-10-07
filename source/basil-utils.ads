-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
--                  BASIL MESSAGE HANDLING LIBRARY
--
--                Copyright (C) 2008, Jordan Bettis
--             GNU GPL (General Public License) Notice
--
--  Basil is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option)
--  any later version.  Basil is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details.  You should
--  have received a copy of the GNU General Public License distributed
--  with Basil; see file COPYING.  If not, write to the Free Software
--  Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
--  02110-1301, USA.
--
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- PROJECT CONTEXT:
--
--   This package contains a few useful utilities that we want to be
--   able to use throughout the Basil library.
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Strings.Maps;             use Ada.Strings.Maps;

with Basil.Strings;

private package Basil.Utils is

  -----------------------------------------------------------------------------
  --  These two functions were recommended by Niestu (niestu.com) to
  --  make string handling more manageable.
  function US (Source : in String)
              return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  function S (Source : in Ada.Strings.Unbounded.Unbounded_String)
             return String
    renames Ada.Strings.Unbounded.To_String;

  -----------------------------------------------------------------------------
  --  These are the same as above, but for translating to and from
  --  Basil.Strings.Encoded_String types
  function ES (Source : in String)
              return Basil.Strings.Encoded_String
    renames Basil.Strings.To_Encoded_String;

  function ES (Source : in Unbounded_String)
              return Basil.Strings.Encoded_String
    renames Basil.Strings.To_Encoded_String;

  function S  (Source : in Basil.Strings.Encoded_String)
             return String
    renames Basil.Strings.To_String;

  --  This is named EUS instead of US to deal with the weird way Ada
  --  treats string literals (US ("") would be ambigious if this
  --  function was named US because Ada would see "" as being both a
  --  string and an encoded string.)
  function EUS (Source : in Basil.Strings.Encoded_String)
             return Unbounded_String
    renames Basil.Strings.To_Unbounded_String;

  -----------------------------------------------------------------------------
  --  This function replaces all instances of the Target string in the
  --  Source_String with the Replacement string.
  procedure Replace_All (Source_String : in out Unbounded_String;
                         Target        : in     String;
                         Replacement   : in     String);

  -----------------------------------------------------------------------------
  --  This function takes a string that may or may not contain
  --  characters that are in the set of 'requires quoting.' If it
  --  contains such characters, those characters are escaped by
  --  placing a '\' character in front, and the entire string is
  --  wrappedin quotation marks '"'. Otherwise, the string is passed
  --  back out unmodified.
  --
  --  If you want the string returned without being bounded by '"'
  --  marks even if characters were escaped, set Add_Quotation to
  --  False.
  function Enquote_If_Needed (Source           : in String;
                              Requires_Quoting : in Character_Set;
                              Add_Quotation    : in Boolean := True)
                             return String;

  -----------------------------------------------------------------------------
  --  This function takes a string that may or may not be enquoted
  --  (wrapped in quotation markes and embedded with escape '\'
  --  characters). If the string is enquoted it will return a version
  --  with the quote marks and escape characters removed. Otherwise it
  --  will return the string unmodified.
  --
  --  In some circumstances there might be a string which is enquoted
  --  but not encircled by quote characters ('"'). If that may be the
  --  case, set Has_Quotation to False, and the function will try to
  --  unenquote any escaped characters even if the string is not
  --  bounded by quote characters.
  function Unenquote_If_Needed (Source        : in String;
                                Has_Quotation : in Boolean := True)
                               return String;

  -----------------------------------------------------------------------------
  --  Our rule is that we remove escaping when we store strings as
  --  objects, and we add it when we serialize. This is a problem for
  --  the domain part because RFC 2822 defines two forms of domain. A
  --  normal domain contains dot-atom text, but a literal domain,
  --  which is bracketed, can contain any domain-text, plus escaping.
  --
  --  This function works by checking to see if the domain is
  --  bracketed or not. If it is, then the domain-text is escaped,
  --  otherwise, nothing is done and the caller is free to test that
  --  the domain is a valid dot-atom.
  function Enquote_Domain (Source : in String) return String;

  -----------------------------------------------------------------------------
  --  This function performs the opposite of Enquote_Domain. If the
  --  domain is bracketed it will unescape it. Otherwise, it will do
  --  nothing.
  function Unenquote_Domain (Source : in String) return String;

end Basil.Utils;
