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
--  This package provides a list of address header objects, and the
--  operations to parse and compile structured header values that
--  contain one or more addresses.
--
-------------------------------------------------------------------------------


with Ada.Containers;

with Basil.Address_Headers.Abstract_Lists;

with Basil.Strings;                   use Basil.Strings;
with Basil.Headers;                   use Basil.Headers;
with Basil.Headers.Lists;

package Basil.Address_Headers.Lists is

   --  Derived from the tagged type to bring in the Ada DLL Container
   --  operations.
   type List is new Basil.Address_Headers.Abstract_Lists.List with null record;

   Empty_List : constant List;

   ----------------------------------------------------------------------------
   --  The Cursor subtype and the subsequent function renames here
   --  bring operations into this package from the DLL instantiation
   --  that are not inherited through the tagged type, so that users
   --  of this package won't have to use
   --  Basil.Address_Headers.Abstract_Lists.
   subtype Cursor is Basil.Address_Headers.Abstract_Lists.Cursor;

   No_Element : constant Cursor;

   function Element     (Position : in     Cursor) return Address_Header
     renames Basil.Address_Headers.Abstract_Lists.Element;

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : in Address_Header))
     renames Basil.Address_Headers.Abstract_Lists.Query_Element;

   function Next        (Position : in     Cursor) return Cursor
     renames Basil.Address_Headers.Abstract_Lists.Next;

   procedure Next       (Position : in out Cursor)
     renames Basil.Address_Headers.Abstract_Lists.Next;

   function Previous    (Position : in     Cursor) return Cursor
     renames Basil.Address_Headers.Abstract_Lists.Previous;

   procedure Previous   (Position : in out Cursor)
     renames Basil.Address_Headers.Abstract_Lists.Previous;

   function Has_Element (Position : in     Cursor) return Boolean
     renames Basil.Address_Headers.Abstract_Lists.Has_Element;

   ----------------------------------------------------------------------------
   --  This function takes a list of message ID header objects and
   --  returns a message header object with the given key and the
   --  value set to a serialized representation of the message IDs.
   function Compile (Addresses : in List) return Headers.Lists.List;

   ----------------------------------------------------------------------------
   --  This function takes a list of entity headers whose values
   --  contain serialized lists of message IDs and returns a message
   --  ID header object list containing representations of each of the
   --  IDs.
   --
   --  Note that this function assumes that all headers in the list
   --  contain message IDs. Do not pass an message's entire header
   --  list unmodified.
   function Parse (Input : in Headers.Lists.List) return List;

   ----------------------------------------------------------------------------
   --  This is a convenience function that takes a single entity
   --  header object and returns a message IDs headers list object
   --  representation of the serialized message IDs in its value.
   function Parse (Input : in Header) return List;

private

   Empty_List : constant List   := (Abstract_Lists.List with null record);
   No_Element : constant Cursor
     := Basil.Address_Headers.Abstract_Lists.No_Element;

end Basil.Address_Headers.Lists;
