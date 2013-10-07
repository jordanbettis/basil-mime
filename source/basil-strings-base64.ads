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

with Basil.Strings;

package Basil.Strings.Base64 is

   ----------------------------------------------------------------------------
   --  This function takes a string and returns an encoded
   --  representation using the rules of the base64 encoding type.
   function Encode (Input_String    : in String;
                    Wrap_Long_Lines : in Boolean := True)
                   return Encoded_String;

   ----------------------------------------------------------------------------
   --  This function takes a string encoded using the Base64 scheme
   --  and returns a decoded representation of it.
   --
   --  RFC 3548 recommends rejecting characters which RFC 2045 mandates
   --  the encoder shall ignore. If you wish to have the RFC 3548
   --  behavior of rejecting data with extraneous characters, then
   --  set Reject_Badness to True.
   function Decode (Input_String   : in Encoded_String;
                    Reject_Badness : in Boolean := False)
                   return String;

end Basil.Strings.Base64;
