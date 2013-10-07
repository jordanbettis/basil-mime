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
--  This package implements a parser for header values containing RFC
--  2822 message IDs. For an BNF of the language matched, please see
--  the body of this package.
--
-------------------------------------------------------------------------------

with Ada.Containers.Doubly_Linked_Lists;

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

private package Basil.Parse_Message_IDs is

   --  A Message_ID consists of local part and a domain part that are
   --  syntactically identicial to the parts of the addr-spec for
   --  addresses.
   type Message_ID_Item is
      record
         Local_Part   : Unbounded_String;
         Domain_Part  : Unbounded_String;
      end record;

   package Semantics_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Message_ID_Item);

   ---------------------------------------------------------------------------
   --  This function takes the value of a header containing an RFC
   --  2822 compliant structured Message ID list and returns an
   --  Semnatics_List object containing a representation of the
   --  list.
   function Parse_Message_IDs_List (Header_Value : in String)
                                   return Semantics_Lists.List;

end Basil.Parse_Message_IDs;
