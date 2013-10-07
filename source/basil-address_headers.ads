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
--  MRCY.SPC.002
--
-------------------------------------------------------------------------------
-- PROJECT CONTEXT:
--
--  This file provides functionality to handle individual address
--  header objects. To handle lists of address (in particular, to
--  parse or compile them, see Basil.Address_Headers.Lists.
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

with Basil.Strings;               use Basil.Strings;

package Basil.Address_Headers is

   type Address_Header is private;

   ----------------------------------------------------------------------------
   --  This function creates a address header object based on the
   --  inputs.
   --
   --  The local part may contain any character valid in an
   --  encoded-string, but it SHOULD conform to the specification of
   --  the dot-atom from RFC 2822 p 12. The domain part MUST be a
   --  dot-atom or a domain-literal bracketed by '[]' (RFC 2822 p 17).
   --
   --  The display name can contain any character allowed in an
   --  Encoded String. Do not enclose in double quotes or escape any
   --  characters. If you need to include non-ASCII characters, do so
   --  by encapsulating the string using Enquote_Header from
   --  Basil.Strings. Non-ASCII characters are not permitted by the
   --  standard in the local or domain part.
   --
   --  The Group_Label is a rarely-used feature of RFC 2822 for
   --  address organization. Consecutive addresses with the same group
   --  label will be joined together when the string is serialized
   --  into a single group. The group label may contain any text
   --  permitted in an encoded string. Do not escape any characters.
   function To_Address_Header (Key            : in Encoded_String;
                               Display_Name   : in Encoded_String;
                               Local_Part     : in Encoded_String;
                               Domain_Part    : in Encoded_String;
                               Group_Label    : in Encoded_String := "")
                              return Address_Header;

   --------------------------------------------------------------------------
   --  Each of these accessor functions returns the named part of the
   --  address header object. If the part contained escaped
   --  characters, these will return the string *without* the
   --  escaping. However, if the Display_Name or Group_Label contains
   --  non-ASCII parts that were encapsulated per RFC 2047,
   --  Get_Display_Name and Get_Group_Label will *not* remove the
   --  encapsualzation.
   function Get_Key          (Header : in Address_Header) return String;
   function Get_Display_Name (Header : in Address_Header) return String;
   function Get_Local_Part   (Header : in Address_Header) return String;
   function Get_Domain_Part  (Header : in Address_Header) return String;
   function Get_Group_Label  (Header : in Address_Header) return String;

   ----------------------------------------------------------------------------
   --  This function returns a properly escaped address in the form
   --  local-part@domain-part.
   function Get_Address  (Header : in Address_Header) return String;

   ----------------------------------------------------------------------------
   --  This function returns a full, properly escaped serialized
   --  address in the form "Display Name" <local-part@domain-part>.
   --  (note that Display Name will only be enquoted if it contains
   --  characters that must be escaped. The group, if set, will be
   --  ignored.
   function Get_Full_Address  (Header : in Address_Header) return String;

private

   type Address_Header is
      record
         Key          : Unbounded_String;
         Display_Name : Unbounded_String;
         Local_Part   : Unbounded_String;
         Domain_Part  : Unbounded_String;
         Group_Label  : Unbounded_String;
      end record;

end Basil.Address_Headers;
