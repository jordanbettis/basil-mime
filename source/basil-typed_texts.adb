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

with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;

with Basil.Utils;                     use Basil.Utils;

package body Basil.Typed_Texts is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_Typed_Text (Value         : in String;
                           Character_Set : in Character_Set_ID)
                          return Typed_Text is

      Text : Typed_Text;

   begin -- To_Typed_Text

      Text.Value         := US (Value);
      Text.Character_Set := Character_Set;

      return Text;

   end To_Typed_Text;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_Typed_Text (Value         : in Unbounded_String;
                           Character_Set : in Character_Set_ID)
                          return Typed_Text is

      Text : Typed_Text;

   begin -- To_Typed_Text

      Text.Value         := Value;
      Text.Character_Set := Character_Set;

      return Text;

   end To_Typed_Text;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Value (Text : in Typed_Text) return String is

   begin -- Get_Value

      return S (Text.Value);

   end Get_Value;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Value (Text : in Typed_Text) return Unbounded_String is

   begin -- Get_Value

      return Text.Value;

   end Get_Value;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Character_Set (Text : in Typed_Text) return Character_Set_ID is

   begin -- Get_Character_Set

      return Text.Character_Set;

   end Get_Character_Set;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Are_Equal (Left  : in Typed_Text;
                       Right : in Typed_Text)
                      return Boolean is

   begin -- Are_Equal

      if Left.Character_Set /= Right.Character_Set then
         return False;
      elsif Left.Value /= Right.Value then
         return False;
      else
         return True;
      end if;

   end Are_Equal;

end Basil.Typed_Texts;
