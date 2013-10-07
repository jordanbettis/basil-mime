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

with Ada.Strings.Maps;             use Ada.Strings.Maps;

with Basil.Content_Types;          use Basil.Content_Types;
with Basil.Entities.Serialization; use Basil.Entities.Serialization;
with Basil.Tokens;                 use Basil.Tokens;
with Basil.Automata;               use Basil.Automata;
with Basil.Utils;                  use Basil.Utils;

package body Basil.Entities.MIME is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_String (Source : in MIME_Entity)
                      return String is

   begin -- To_String

      return Serialize_Entity (Source);

   end To_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function From_String (Source : in String;
                         Start  : in Positive := 1)
                        return MIME_Entity is

      Index  : Positive := Start;
      Entity : MIME_Entity;

   begin -- From_String

      From_String (New_Entity => Entity,
                   Source     => Source,
                   Index      => Index);

      return Entity;

   end From_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure From_String (New_Entity :    out MIME_Entity;
                          Source     : in     String;
                          Index      : in out Positive) is

   begin -- From_String

      Unserialize_Entity (Source       => Source,
                          Delimiter    => "",
                          Index        => Index,
                          Entity       => New_Entity,
                          Get_Children => Get_Children'Access);

   end From_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_MIME_Entity (Entity_Type : in Content_Type;
                            Body_Part   : in Encoded_String)
                           return MIME_Entity is

      Output : MIME_Entity;

   begin -- To_MIME_Entity

      Set_Content_Type (Output, Entity_Type);

      Set_Body_Part (Output, Body_Part);

      return Output;

   end To_MIME_Entity;

   --------------------------------------------------------------------------
   --------------------------------------------------------------------------
   function To_MIME_Entity (Entity_Type : in Content_Type;
                            Body_Part   : in String)
                           return MIME_Entity is

      Output : MIME_Entity;

   begin -- To_MIME_Entity

      Set_Content_Type (Output, Entity_Type);

      Set_Body_Part (Output, Body_Part);

      return Output;

   end To_MIME_Entity;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Children (Source : in     String;
                           Index  : in out Positive;
                           Entity : in out Abstract_Entity'Class) is

      Index_End    : Natural;
      Child_Entity : MIME_Entity;

   begin -- Get_Children

      while Source'Last > Index
        and then Source (Index .. Index + 1) /= "--"
      loop

         Unserialize_Entity (Source    => Source,
                             Delimiter =>
                               S (Entity.Entity_Type.Multipart_Delimiter),
                             Index     => Index,
                             Entity    => Child_Entity,
                             Get_Children => Get_Children'Access);

         Append (Entity.Children.all, Child_Entity);

         Child_Entity := (Empty_Entity with null record);

      end loop;

      if Source'Last > Index
        and then Source (Index .. Index + 1) = "--"
      then

         Index := Index + 2;

      end if;

      Index_End :=
        Automata.Get_Token_End (Source     => Source,
                                First      => Index,
                                Automation => Newline_Sequence);

      if Index_End /= 0 then
         Index := Index_End;
      end if;

   end Get_Children;

end Basil.Entities.MIME;
