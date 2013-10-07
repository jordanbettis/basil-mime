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
--  MRCY.SPC.002
--
-------------------------------------------------------------------------------
-- PROJECT CONTEXT:
--
--  This package provides a list of header objects, and the operations
--  to manipulate them, principally for construction of the header
--  list component of entity objects.
--
-------------------------------------------------------------------------------

with Ada.Containers;

with Basil.Headers.Abstract_Lists;

with Basil.Strings;                   use Basil.Strings;

package Basil.Headers.Lists is

   --  Derived from the tagged type to bring in the Ada DLL Container
   --  operations.
   type List is new Basil.Headers.Abstract_Lists.List with null record;

   Empty_List : constant List;

   ----------------------------------------------------------------------------
   --  The Cursor subtype and the subsequent function renames here
   --  bring operations into this package from the DLL instantiation
   --  that are not inherited through the tagged type, so that users
   --  of this package won't have to use Basil.Headers.Abstract_Lists.
   subtype Cursor is Basil.Headers.Abstract_Lists.Cursor;

   No_Element : constant Cursor;

   function Element     (Position : in     Cursor) return Header
     renames Basil.Headers.Abstract_Lists.Element;

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : in Header))
     renames Basil.Headers.Abstract_Lists.Query_Element;

   function Next        (Position : in     Cursor) return Cursor
     renames Basil.Headers.Abstract_Lists.Next;

   procedure Next       (Position : in out Cursor)
     renames Basil.Headers.Abstract_Lists.Next;

   function Previous    (Position : in     Cursor) return Cursor
     renames Basil.Headers.Abstract_Lists.Previous;

   procedure Previous   (Position : in out Cursor)
     renames Basil.Headers.Abstract_Lists.Previous;

   function Has_Element (Position : in     Cursor) return Boolean
     renames Basil.Headers.Abstract_Lists.Has_Element;

   ----------------------------------------------------------------------------
   --  This procedure ensures that there is a header in Header_List
   --  with the given key and value. It does this by seaching the list
   --  by key. If a header in the list contains the key, it will
   --  overwrite the value of that header. If there are multiple
   --  headers with the key, it will overwrite the value of the first
   --  matching header it finds. If there is no header in the list
   --  with the key, it will add one ot the end.
   procedure Ensure_Header_Exists (Header_List  : in out List;
                                   Key          : in     Encoded_String;
                                   Value        : in     Encoded_String);

   ----------------------------------------------------------------------------
   --  This version of Ensure_Header_Exists takes a header object
   --  instead of key and value strings.
   --
   --  If a header in the list contains the key of the Header_Object,
   --  it will overwrite the value of that header. If there are
   --  multiple headers with the key, it will overwrite the value of
   --  the first matching header it finds. If there is no header in
   --  the list with the key, it will append Header_Object to the
   --  list.
   procedure Ensure_Header_Exists (Header_List   : in out List;
                                   Header_Object : in     Header);

   ----------------------------------------------------------------------------
   --  This procedure ensures that a header with the given key does
   --  *not* exist in the header list by deleting every header in the
   --  list with the key.
   procedure Purge_Header (Header_List : in out List;
                           Key         : in     String);

private

   Empty_List : constant List   := (Abstract_Lists.List with null record);
   No_Element : constant Cursor := Basil.Headers.Abstract_Lists.No_Element;

end Basil.Headers.Lists;
