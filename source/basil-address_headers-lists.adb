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

with Basil.Parse_Addresses;      use Basil.Parse_Addresses;
with Basil.Tokens;               use Basil.Tokens;
with Basil.Utils;                use Basil.Utils;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings.Maps;           use Ada.Strings.Maps;

with Ada.Text_IO;                use Ada.Text_IO;

package body Basil.Address_Headers.Lists is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Compile (Addresses : in List) return Headers.Lists.List is

      -------------------------------------------------------------------------
      procedure Start_Stop_Group (Value       : in out Unbounded_String;
                                  Address     : in     Address_Header;
                                  Print_Comma : in     Boolean;
                                  Previous    : in     Unbounded_String);

      procedure Start_Stop_Group (Value       : in out Unbounded_String;
                                  Address     : in     Address_Header;
                                  Print_Comma : in     Boolean;
                                  Previous    : in     Unbounded_String) is

      begin -- Start_Stop_Group

         if Length (Previous) < 1 and Length (Address.Group_Label) > 1 then

            -- No group to group
            if Print_Comma then
               Append (Value, ", ");
            end if;

            Append (Value, US (Enquote_If_Needed (S (Address.Group_Label),
                                                  not (RFC2822_Atom_Text
                                                       or To_Set(" ")))
                               & ": "));

         elsif Length (Previous) > 1 and Length (Address.Group_Label) < 1 then

            -- Group to no group
            Append (Value, "; ");

         elsif Length (Previous) > 1 and Length (Value) > 1 then

            -- Group to new group
            Append (Value,
                    US ("; "
                        & Enquote_If_Needed (S (Address.Group_Label),
                                             not (RFC2822_Atom_Text
                                                  or To_Set(" ")))
                        & ": "));

         end if;

      end Start_Stop_Group;

      -------------------------------------------------------------------------
      function Less_Than (Left, Right: in Address_Header) return Boolean;

      function Less_Than (Left, Right: in Address_Header) return Boolean is

      begin -- Less_Than

         return To_Upper (S (Left.Key)) < To_Upper (S (Right.Key));

      end Less_Than;

      package Sorter is
         new Basil.Address_Headers.Abstract_Lists.Generic_Sorting
        ("<" => Less_Than);

      -------------------------------------------------------------------------
      Addresses_Cursor   : Cursor;
      Value              : Unbounded_String;
      Previous_Key       : Unbounded_String;
      Previous_Group     : Unbounded_String;

      Headers_List : Headers.Lists.List;

      Sortable_Addresses : Basil.Address_Headers.Abstract_Lists.List;
      Sorted_Addresses   : List;
      Print_Comma        : Boolean := False;

   begin -- Compile

      Sortable_Addresses := Abstract_Lists.List (Addresses);
      Sorter.Sort (Abstract_Lists.List (Sortable_Addresses));
      Sorted_Addresses   := (Sortable_Addresses with null record);

      Addresses_Cursor := First (Sorted_Addresses);

      if Has_Element (Addresses_Cursor) then
         Previous_Key := Element (Addresses_Cursor).Key;
      end if;

      while Has_Element (Addresses_Cursor) loop

         if To_Upper (S (Element (Addresses_Cursor).Key))
           /= To_Upper (S (Previous_Key))
         then

            if Length (Previous_Group) > 1 then
               Previous_Group := US ("");
               Append (Value, ";");
            end if;

            Headers.Lists.Append (Headers_List,
                                  To_Header (ES (Previous_Key), ES (Value)));

            Value := US ("");
            Print_Comma := False;
            Previous_Key := Element (Addresses_Cursor).Key;

         end if;

         if Element (Addresses_Cursor).Group_Label /= Previous_Group then

            Start_Stop_Group (Value       => Value,
                              Address     => Element (Addresses_Cursor),
                              Print_Comma => Print_Comma,
                              Previous    => Previous_Group);

            Previous_Group := Element (Addresses_Cursor).Group_Label;

            Print_Comma := False;

         end if;

         if Print_Comma then
            Append (Value, ", ");
         end if;

         Append (Value, Get_Full_Address
                 (Element (Addresses_Cursor)));

         Print_Comma := True;

         Next (Addresses_Cursor);

      end loop;

      if Length (Previous_Group) > 1 then
         Append (Value, ";");
      end if;

      Headers.Lists.Append (Headers_List,
                            To_Header (ES (Previous_Key), ES (Value)));

      return Headers_List;

   end Compile;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Parse (Input : in Headers.Lists.List) return List is

      Addresses_List : List;
      Next_List      : List;
      Headers_Cursor : Headers.Lists.Cursor := Headers.Lists.First (Input);

   begin -- Parse

      while Headers.Lists.Has_Element (Headers_Cursor) loop

         Next_List := Parse (Headers.Lists.Element (Headers_Cursor));

         Splice (Target    => Addresses_List,
                 Before    => No_Element,
                 Source    => Next_List);

         Headers.Lists.Next (Headers_Cursor);

      end loop;

      return Addresses_List;

   end Parse;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Parse (Input : in Header) return List is

      Semantics_List : Semantics_Lists.List;
      List_Cursor    : Semantics_Lists.Cursor;

      Addresses_List : List;
      Value          : Unbounded_String;
      Current_Group  : Unbounded_String;

   begin -- Parse

      Semantics_List := Parse_Address_List (Get_Value (Input));

      List_Cursor := Semantics_Lists.First (Semantics_List);

      while Semantics_Lists.Has_Element (List_Cursor) loop

         case Semantics_Lists.Element (List_Cursor).Item_Type is

            when Address =>

               Append (Container => Addresses_List,
                       New_Item  => Address_Header'
                         (Key          => US (Get_Key (Input)),
                          Display_Name =>
                            Semantics_Lists.Element (List_Cursor).Display_Name,
                          Local_Part   =>
                            Semantics_Lists.Element (List_Cursor).Local_Part,
                          Domain_Part  =>
                            Semantics_Lists.Element (List_Cursor).Domain_Part,
                          Group_Label  => Current_Group));

            when Group_Start =>

               Current_Group :=
                 Semantics_Lists.Element (List_Cursor).Group_Label;

            when Group_End =>

               Current_Group := US ("");

         end case;

         Semantics_Lists.Next (List_Cursor);

      end loop;

      return Addresses_List;

   end Parse;


end Basil.Address_Headers.Lists;
