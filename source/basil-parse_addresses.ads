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
--  This package implements a very flexible and forgiving parser for
--  RFC 2822 address headers. As such it is not particularly useful
--  for validating address headers. For an BNF of the language
--  matched, please see the body of this package.
--
-------------------------------------------------------------------------------

with Ada.Containers.Doubly_Linked_Lists;

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

private package Basil.Parse_Addresses is

   --  Lists of addresses can either consist of an unrelated set of
   --  addresses, or address formed into groups. To designate address
   --  that are part of a group we place Group_Start and Group_End
   --  marker items in the list.
   type List_Item_Type is
     (Address,
      Group_Start,
      Group_End);

   type Address_Item (Item_Type: List_Item_Type := Address) is
      record
         case Item_Type is
            when Address     =>
               Display_Name : Unbounded_String;
               Local_Part   : Unbounded_String;
               Domain_Part  : Unbounded_String;
            when Group_Start =>
               Group_Label  : Unbounded_String;
            when Group_End   =>
               End_Label    : Unbounded_String;
         end case;
      end record;

   package Semantics_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Address_Item);

   ----------------------------------------------------------------------------
   --  This function takes the value of a header containing an RFC
   --  2822 compliant structured address list and returns an
   --  Semantics List object containing a representation of the
   --  list.
   function Parse_Address_List (Header_Value : in String)
                               return Semantics_Lists.List;

end Basil.Parse_Addresses;
