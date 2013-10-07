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
--   None
--
-------------------------------------------------------------------------------
-- PROJECT CONTEXT:
--
--   This package provides support for encoding and decoding the
--   Base64 content transfer encoding.
--
-------------------------------------------------------------------------------

with Ada.Strings.Maps;            use Ada.Strings.Maps;
with Ada.Characters.Latin_1;

package Basil.Strings.Quoted_Printable is

   ----------------------------------------------------------------------------
   --  This function takes a string and retruns an encoded
   --  representation using the rules of the quoted printable encoding
   --  type. The Quote_Additional character set should contain any
   --  characters beyond those specified by RFC 2045 as needing
   --  quoting (Basil.Tokens.Requires_Quoting) which the caller wishes
   --  to have quoted.
   --
   --  If Wrap_Long_Lines is True this function will insert soft
   --  newlines to ensure that none of the lines are longer than the
   --  RFC 2045 maximum of 78 characters;
   function Encode (Input_String     : in String;
                    Quote_Additional : in Character_Set := To_Set("");
                    Wrap_Long_Lines  : in Boolean := True)
                   return Encoded_String;

   ----------------------------------------------------------------------------
   --  This function takes a string encoded using the Quoted Printable
   --  scheme and returns a decoded representation of it. RFC 2045
   --  lists a number of things that can cause quoted printable text
   --  to be considered 'malformed' (see RFC 2045 p22). Normally these
   --  conditions are ignored, but if Reject_Badness is set to True
   --  the function will raise an appropriate exception.
   function Decode (Input_String    : in Encoded_String;
                    Reject_Badness  : in Boolean := False)
                   return String;

end Basil.Strings.Quoted_Printable;
