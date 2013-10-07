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

with Basil.Parse_Message_IDs;    use Basil.Parse_Message_IDs;
with Basil.Utils;                use Basil.Utils;

with Ada.Characters.Handling;    use Ada.Characters.Handling;

package body Basil.Message_ID_Headers.Lists is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Compile (Message_IDs : in List) return Headers.Lists.List is

      IDs_Cursor   : Cursor;
      Value        : Unbounded_String;
      Previous_Key : Unbounded_String;

      Headers_List : Headers.Lists.List;

      Sortable_IDs : Basil.Message_ID_Headers.Abstract_Lists.List;
      Sorted_IDs   : List;

      -------------------------------------------------------------------------
      function Less_Than (Left, Right: Message_ID_Header) return Boolean;

      function Less_Than (Left, Right: Message_ID_Header) return Boolean is

      begin -- Less_Than

         return To_Upper (S (Left.Key)) < To_Upper (S (Right.Key));

      end Less_Than;

      package Sorter is
         new Basil.Message_ID_Headers.Abstract_Lists.Generic_Sorting
        ("<" => Less_Than);

   begin -- Compile

      Sortable_IDs := Abstract_Lists.List (Message_IDs);
      Sorter.Sort (Abstract_Lists.List (Sortable_IDs));
      Sorted_IDs := (Sortable_IDs with null record);

      IDs_Cursor := First (Sorted_IDs);

      if Has_Element (IDs_Cursor) then
         Previous_Key := Element (IDs_Cursor).Key;
      end if;


      while Has_Element (IDs_Cursor) loop

         if To_Upper (S (Element (IDs_Cursor).Key))
           /= To_Upper (S (Previous_Key))
         then

            Headers.Lists.Append (Headers_List,
                                  To_Header (ES (Previous_Key), ES (Value)));
            Value := US ("");
            Previous_Key := Element (IDs_Cursor).Key;

         end if;

         Append (Value, Get_Message_ID (Element (IDs_Cursor)) & " ");

         Next (IDs_Cursor);

      end loop;

      if Length (Value) > 0 then
         Headers.Lists.Append (Headers_List,
                               To_Header (ES (Previous_Key), ES (Value)));
      end if;

      return Headers_List;

   end Compile;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Parse (Input : in Headers.Lists.List) return List is

      IDs_List       : List;
      Next_List      : List;
      Headers_Cursor : Headers.Lists.Cursor := Headers.Lists.First (Input);

   begin -- Parse

      while Headers.Lists.Has_Element (Headers_Cursor) loop

         Next_List := Parse (Headers.Lists.Element (Headers_Cursor));

         Splice (Target    => IDs_List,
                 Before    => No_Element,
                 Source    => Next_List);

         Headers.Lists.Next (Headers_Cursor);

      end loop;

      return IDs_List;

   end Parse;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Parse (Input : in Header) return List is

      Semantics_List : Semantics_Lists.List;
      List_Cursor    : Semantics_Lists.Cursor;

      IDs_List       : List;
      Value          : Unbounded_String;

   begin -- Parse

      Semantics_List := Parse_Message_IDs_List (Get_Value (Input));

      List_Cursor := Semantics_Lists.First (Semantics_List);

      while Semantics_Lists.Has_Element (List_Cursor) loop

         Append (Container => IDs_List,
                 New_Item  => Message_ID_Header'
                 (Key         => US (Get_Key (Input)),
                  Local_Part  =>
                    Semantics_Lists.Element (List_Cursor).Local_Part,
                  Domain_Part =>
                    Semantics_Lists.Element (List_Cursor).Domain_Part));

         Semantics_Lists.Next (List_Cursor);

      end loop;

      return IDs_List;

   end Parse;


end Basil.Message_ID_Headers.Lists;
