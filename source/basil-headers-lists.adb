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

with Ada.Characters.Handling;         use Ada.Characters.Handling;

with Basil.Headers.Abstract_Lists;

with Basil.Strings;                   use Basil.Strings;
with Basil.Utils;                     use Basil.Utils;

package body Basil.Headers.Lists is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Ensure_Header_Exists (Header_List  : in out List;
                                   Key          : in     Encoded_String;
                                   Value        : in     Encoded_String) is

      Header_Object : Header := To_Header (Key => Key,
                                           Value => Value);

   begin -- Ensure_Header_Exists

      Ensure_Header_Exists (Header_List, Header_Object);

   end Ensure_Header_Exists;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Ensure_Header_Exists (Header_List   : in out List;
                                   Header_Object : in     Header) is


      List_Cursor    : Cursor := First (Header_List);

   begin -- Ensure_Header_Exists

      while Has_Element (List_Cursor) loop

         if To_Upper (S (Abstract_Lists.Element (List_Cursor).Key))
           = To_Upper (S (Header_Object.Key))
         then

            Replace_Element (Container => Header_List,
                             Position  => List_Cursor,
                             New_Item  => Header_Object);

            return;

         end if;

         Next (List_Cursor);

      end loop;

      Append (Container => Header_List,
              New_Item  => Header_Object);

   end Ensure_Header_Exists;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Purge_Header (Header_List : in out List;
                           Key         : in     String) is

      List_Cursor : Cursor := First (Header_List);
      Next_Position : Cursor;

   begin -- Purge_Header

      while Has_Element (List_Cursor) loop

         if To_Upper (S (Abstract_Lists.Element (List_Cursor).Key))
           = To_Upper (Key)
         then

            Next_Position := Next (List_Cursor);

            Delete (Container => Header_List,
                    Position  => List_Cursor);

            List_Cursor := Next_Position;

         else

            Next (List_Cursor);

         end if;

      end loop;

   end Purge_Header;

end Basil.Headers.Lists;
