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
--  This package provides the message ID header object and functions
--  to operate on it. Message ID headers are grouped into lists and
--  functionality is provided to parse message headers containing
--  message IDs, and to compile lists of message ID headers into
--  message headers, in the Basil.Message_ID_Headers.Lists package.
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

with Basil.Strings;               use Basil.Strings;

package Basil.Message_ID_Headers is

   type Message_ID_Header is private;

   ----------------------------------------------------------------------------
   --  This function returns a Message_ID_Header object containing the
   --  provided key and a randomally generated, valid, globally-unique
   --  message ID per the requirements of RFC 2822.
   function Generate_Message_ID (Key         : in Encoded_String)
                                return Message_ID_Header;

   ----------------------------------------------------------------------------
   --  This function creates a message ID header based on the
   --  inputs. Since the most significant requirement of a message ID
   --  is that it contains enough random components to be guarenteed
   --  globally unique, it's best to use the function above to let the
   --  library generate one for you.
   --
   --  The local part may contain any character valid in an
   --  encoded-string, but it SHOULD conform to the specification of
   --  the dot-atom from RFC 2822 p 12. The domain part MUST be a
   --  dot-atom or a domain-literal bracketed by '[]' (RFC 2822 p 17).
   --
   --  Do not escape or encapsulate any input.
   function To_Message_ID_Header (Key            : in Encoded_String;
                                  Local_Part     : in Encoded_String;
                                  Domain_Part    : in Encoded_String)
                                 return Message_ID_Header;

   ----------------------------------------------------------------------------
   --  Each of these accessor functions returns the named part of the
   --  message ID header object.
   function Get_Key (Header : in Message_ID_Header) return String;
   function Get_Local_Part  (Header : in Message_ID_Header) return String;
   function Get_Domain_Part (Header : in Message_ID_Header) return String;

   ----------------------------------------------------------------------------
   --  This function returns the entire message ID in the form
   --  <local-part@domain-part>.
   function Get_Message_ID  (Header : in Message_ID_Header) return String;

private

   type Message_ID_Header is
      record
         Key         : Unbounded_String;
         Local_Part  : Unbounded_String;
         Domain_Part : Unbounded_String;
      end record;

end Basil.Message_ID_Headers;
