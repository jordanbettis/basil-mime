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
--  This package provides the Header object, which stores
--  RFC2822-defined headers for inclusion in lists as components of
--  Entity-class objects.
--
-------------------------------------------------------------------------------


with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Basil.Strings;           use Basil.Strings;

package Basil.Headers is

   type Header is private;

   Empty_Header: constant Header;

   ----------------------------------------------------------------------------
   --  This function creates a header object by unserializing the
   --  serialized header in the input string starting at the index
   --  specified by 'index.' If 'start' is zero then it will begin
   --  parsing at Input'First.
   --
   --  If this function encounters a newline sequence at the start
   --  position it will assume it has encountered the end of a header
   --  list and will signal this by returning an empty header
   --  object. Any other situation in which the function can not find
   --  a valid header at the start of the input will cause it to raise
   --  an exception.
   function From_String (Input : in String;
                         Index : in Positive := 1)
                        return Header;

   ----------------------------------------------------------------------------
   --  This version of From_String provides more information about the
   --  string than the above, for use in unserializing entities. It
   --  populates the New_Header with the serialized header at Index in
   --  the string, and updates Index to refer to the first character
   --  after the trailing newline sequence of the header.
   --
   --  If it encounters a newline at the start position it will place
   --  the Index after the newline and return an empty header.
   procedure From_String (New_Header :    out Header;
                          Input      : in     String;
                          Index      : in out Positive);

   ----------------------------------------------------------------------------
   --  This function serializes a header object. The serilized string
   --  will end with a CRLF sequence.
   function To_String (Input_Header : in Header) return String;

   ----------------------------------------------------------------------------
   --  This function creates a new header object based on the
   --  input. The key and value will be validated to the extent
   --  practicable to ensure that they have no illegal characters per
   --  RFC 2822 (beyond those already excluded by the Encoded_String
   --  type.
   --
   --  This function folds the value. It must not contain newline
   --  sequences. For unstructured headers, passing the function
   --  through Basil.Strings.Encapsulate_Header will format it
   --  properly for input into this function. For structured values,
   --  use their To_Header functions instead of this one.
   --
   --  The key and value, plus the ': ' and any folding that the
   --  function may do shall not exceed 998 characters. A good
   --  conservative limit for the caller to ensure that you won't
   --  trigger an exception by this rule would be to ensure that your
   --  key and are together are no longer than 960 characters.
   function To_Header (Key   : in Encoded_String;
                       Value : in Encoded_String)
                      return Header;

   ----------------------------------------------------------------------------
   --  These functions retrieve the value and key of a given header
   --  object.
   function Get_Value (Input_Header : in Header) return String;
   function Get_Key   (Input_Header : in Header) return String;

   ----------------------------------------------------------------------------
   --  This functions sets the value of existing header objects. The
   --  value will be validated to the extent practable to ensure that
   --  it has no illegal characters per RFC 2822.
   --
   --  This function folds the value. It must not contain newline
   --  sequences. For unstructured headers, passing the function
   --  through Basil.Strings.Encapsulate_Header will format it
   --  properly for input into this function.
   --
   --  The key and value, plus the ': ' and any folding that the
   --  function may do shall not exceed 998 characters. A good rule if
   --  you want to always avoid an exception is to not exceed 960
   --  characters.
   procedure Set_Value (Target_Header : in out Header;
                        Value         : in     Encoded_String);

   ----------------------------------------------------------------------------
   --  This function sets the key of an existing header ojbect to the
   --  supplied string. The key will be validated to the extent
   --  practable to ensure that it has no illegal characters per RFC
   --  2822. The standard does not specify a maximum length for a key,
   --  but since no line may be longer than 998 characters, this
   --  function will reject any key longer.
   procedure Set_Key   (Target_Header : in out Header;
                        Key           : in     Encoded_String);

   ----------------------------------------------------------------------------
   --  This function exists for the DLL Container instantiation in
   --  Basil.Headers.Abstract_Lists.
   function Are_Equal (Left, Right : Header)
                      return Boolean;

private

   type Header is
      record
         Key       : Unbounded_String;
         Value     : Unbounded_String;
      end record;

   Empty_Header    : constant Header :=
     Header'(To_Unbounded_String (String'("")),
             To_Unbounded_String (String'("")));

end Basil.Headers;
