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
--  The MIME system allows the possibility of strings of text existing
--  in any one of may different character sets. It is therefore useful
--  to have a text string type that carries metadata along with it.
--
--  This package defines a Typed_Text private object which stores text
--  with metada, as well as access functions.
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;

package Basil.Typed_Texts is

   type Typed_Text is private;

   ----------------------------------------------------------------------------
   --  These construct and return a Typed_Text object given a value and
   --  a Character_Set_ID describing that value.
   function To_Typed_Text (Value         : in String;
                           Character_Set : in Character_Set_ID)
                          return Typed_Text;

   function To_Typed_Text (Value         : in Unbounded_String;
                           Character_Set : in Character_Set_ID)
                          return Typed_Text;

   ----------------------------------------------------------------------------
   --  These functions retrieve the value of a type text object.
   function Get_Value (Text : in Typed_Text) return String;
   function Get_Value (Text : in Typed_Text) return Unbounded_String;

   ----------------------------------------------------------------------------
   --  This retrives the Character_Set_ID of a Typed_Text object.
   function Get_Character_Set (Text : in Typed_Text) return Character_Set_ID;

   ----------------------------------------------------------------------------
   --  This function is used by Mutitype_Texts for the instantiation
   --  of Ada.Containers.Doubly_Linked_Lists
   function Are_Equal (Left  : in Typed_Text;
                       Right : in Typed_Text)
                      return Boolean;

private

   type Typed_Text is
      record
         Value         : Unbounded_String;
         Character_Set : Character_Set_ID;
      end record;

end Basil.Typed_Texts;
