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
-- EXTERNAL DOCUMENTATION:
--
--   MRCY.SPC.002
--
-------------------------------------------------------------------------------
-- PROJECT CONTEXT:
--
--   This package provides the Encoded_String type, associated
--   conversion functions, and functions for creating and decoding
--   Quoted-Printable and Base64 encoded strings in entity bodies and
--   headers.
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

with Basil.Typed_Texts;
with Basil.Multitype_Texts;

package Basil.Strings is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --  ENCODED STRING TYPE & HANDLING

   --  Restricted to the definition of a string from RFC 2045 p. 5
   subtype Encoded_Character is Character range
     Character'Val(1) .. Character'Val(127);
   type Encoded_String is array(Positive range <>) of Encoded_Character;

   ----------------------------------------------------------------------------
   --  These are converstion functions to help handle Encoded Strings
   function To_Encoded_String (Input : String)           return Encoded_String;
   function To_Encoded_String (Input : Unbounded_String) return Encoded_String;
   function To_String         (Input : Encoded_String)   return String;
   function To_Unbounded_String (Input : Encoded_String)
                                return Unbounded_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --  RFC 2047 HEADER STRING HANDLING

   ----------------------------------------------------------------------------
   --  The encoding/decoding functions only support these content
   --  transfer encoding methods.
   subtype Supported_Encoding is Content_Transfer_Encoding_ID range
     CTE_Quoted_Printable .. CTE_Base64;

   --------------------------------------------------------------------------
   --  This function takes a string containing text in a particular
   --  encoding and a character set ID describing the encoding. The
   --  user may also specify that it uses either Quoted Printable or
   --  Base64 for the encoding.
   --
   --  If the Character_Set is compatable with US-ASCII, it will scan
   --  the string looking for illegal characters ( ? : etc) and
   --  encapsulate the words containing them per RFC 2047.
   --
   --  However, if the text is in *any other* character set it will
   --  encapsulate the *entire item* as a single encapsulated word (or
   --  possibly several if it's too long).
   function Quote_Header (Unquoted_Value       : in String;
                          Character_Set        : in Character_Set_ID;
                          Requested_Encoding   : in Supported_Encoding
                          := CTE_Quoted_Printable)
                         return Encoded_String;

   ----------------------------------------------------------------------------
   --  This function works the same as above, except with a Typed_Text
   --  value instead of a seperate Value string and Character_Set
   --  identifier.
   function Quote_Header (Unquoted_Value       : in Typed_Texts.Typed_Text;
                          Requested_Encoding   : in Supported_Encoding
                          := CTE_Quoted_Printable)
                         return Encoded_String;

   ----------------------------------------------------------------------------
   --  This version of Quote_Header takes a Multitype_Text.List.
   --
   --  For each item in the list, if the Character_Set is compatable
   --  with US-ASCII, it will scan the string looking for illegal
   --  characters ( ? : etc) and encapsulate the words containing them
   --  as above.
   --
   --  However, if the item is in *any other* character set it will
   --  encapsulate the *entire item* as a single encapsulated word (or
   --  possibly several if it's too long).
   --
   --  Also note that if you have two contigious items in your list
   --  that have the *same* character set, this function will
   --  concatenate the strings together before encapsulation. If an
   --  encoded string is too long it will break it into multiple
   --  encoded words. This might break some brittle legacy encodings
   --  but solves an entierly different problem with MIME
   --  encapsulation. The workaround is to transcode everything into
   --  UTF-8 and not send stuff out in legacy encodings.
   function Quote_Header (Unquoted_Value       : in Multitype_Texts.List;
                          Requested_Encoding   : in Supported_Encoding
                          := CTE_Quoted_Printable)
                         return Encoded_String;

   ----------------------------------------------------------------------------
   --  This function takes a string containing RFC 2047 encapsulated
   --  words (basically header values) and returns a Multitype_Text.List
   --  with those words unencapsulated.
   --
   --  Reject_Badness will cause the function to raise an exception
   --  rather than process strings that are malformed but not fatally
   --  so.
   function Unquote_Header (Quoted_Value   : in Encoded_String;
                            Reject_Badness : in Boolean := False)
                           return Multitype_Texts.List;

   ----------------------------------------------------------------------------
   --  Because the cases where a header string has multiple character
   --  sets, or text is encapsulated in a character set that is not
   --  compatable with US-ASCII are rare, this 'simplier' version of
   --  Unquote_Header is provided.
   --
   --  If and only if there is a single character set for every
   --  encapsulated word in the string and that character set is
   --  compatable with US-ASCII, then this will return the Typed_Text
   --  object. Otherwise it will raise Invalid_Character_Set.
   function Unquote_Header (Quoted_Value   : in Encoded_String;
                            Reject_Badness : in Boolean := False)
                           return Typed_Texts.Typed_Text;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --  RFC 2045 STRING QUOTING

   ----------------------------------------------------------------------------
   --  This function takes a string containing text or data and quotes
   --  it in either Quoted-Printable or Base64 (it chooses Base64 if
   --  none is specified).
   function Quote_RFC2045 (Unquoted_String : in String;
                           Quoting_Type    : in Supported_Encoding :=
                             CTE_Base64)
                          return Encoded_String;

   ----------------------------------------------------------------------------
   --  This function takes a string containg text or data quoted using
   --  either Quoted-Printable or Base64 and returns an unquoted
   --  representation.
   --
   --  Reject_Badness will cause the function to raise an exception
   --  rather than parse strings that are malformed but not fatally
   --  so.
   function Unquote_RFC2045 (Quoted_String  : in Encoded_String;
                             Quoting_Type   : in Supported_Encoding;
                             Reject_Badness : in Boolean := False)
                            return String;

   ----------------------------------------------------------------------------
   --  This function takes a string that may have a combination of
   --  CR, LF, and CRLF-style newlines and replaces all lone CR and
   --  LFs with CRLF sequences.
   function Ensure_Message_Newlines (Value : in String)
                                    return String;

   ----------------------------------------------------------------------------
   --  This function takes a string that may have a combination of
   --  CR, LF, and CRLF-style newlines and converts the CRLF and
   --  CRs to lone LF characters.
   function Ensure_Unix_Newlines (Value : in String)
                                 return String;

end Basil.Strings;
