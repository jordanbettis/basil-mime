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
--  This package provides the MIME entity objects, and operations to
--  act on them. Since MIME entities are implementations of the
--  abstract entities, please see basil-entities.ads for many more
--  operations on these objects, including operations to manipulate
--  nested children of multipart entities.
--
-------------------------------------------------------------------------------

package Basil.Entities.MIME is

   --  Tagged to inherit the operations for the entity object. Please
   --  see basil-entities.ads.
   type MIME_Entity is new Basil.Entities.Abstract_Entity with private;

   Empty_MIME_Entity : constant MIME_Entity;

   ----------------------------------------------------------------------------
   --  These renames and subtypes pull the elements of Basil.Entities
   --  in that were not pulled in by the tagged type. This prevents
   --  you having to use Basil.Entities.
   subtype Child_Cursor is Basil.Entities.Child_Cursor;

   No_Child           : constant Child_Cursor;

   No_Child_Recursive : constant Child_Cursor;

   type Count_Children_Type is range 0 .. 2**31 - 1;

   procedure Query_Child
     (Position : Child_Cursor;
      Process  : not null access procedure
      (Element : in Abstract_Entity'Class))
     renames Basil.Entities.Query_Child;

   function Next_Child  (Position : in     Child_Cursor) return Child_Cursor
     renames Basil.Entities.Next_Child;

   procedure Next_Child (Position : in out Child_Cursor)
     renames Basil.Entities.Next_Child;

   function Previous_Child (Position : in     Child_Cursor) return Child_Cursor
     renames Basil.Entities.Previous_Child;

   procedure Previous_Child (Position : in out Child_Cursor)
     renames Basil.Entities.Previous_Child;

   function Has_Child   (Position : in     Child_Cursor) return Boolean
     renames Basil.Entities.Has_Child;

   ----------------------------------------------------------------------------
   --  This function takes an object representation of an entity and
   --  returns a serialized string representation. This function does
   --  some validation of the object.
   --
   --  If the content type headers do not match the private content
   --  type object, and the object type is not Unknown or Experimental
   --  it will raise Invalid_Content_Type. If the content type has
   --  Experimental components (either type or subtype) the function
   --  will ensure that the header is parsable and that the type label
   --  is formed properly. If the type is Unknown then no validation
   --  will be done.
   function To_String (Source : in MIME_Entity)
                      return String;

   ----------------------------------------------------------------------------
   --  This function takes a string containing a serialized entity at
   --  the index Start and returns an object representation. If the
   --  entity has children the function will recursivly unserialize
   --  each of them into data structures.
   function From_String (Source : in String;
                         Start  : in Positive := 1)
                        return MIME_Entity;

   ----------------------------------------------------------------------------
   --  This version of From String provides more information about the
   --  string than above. It is useful when the entity may not be the
   --  last entity in the string, and sets Index to reference the
   --  first character after the last character of the entity.
   procedure From_String (New_Entity :    out MIME_Entity;
                          Source     : in     String;
                          Index      : in out Positive);

   ----------------------------------------------------------------------------
   --  This function creates a MIME entity with the given content type
   --  and body part. The body part is an Encoded_String with the
   --  understanding that the data is seven-bit. If you have
   --  non-ASCII-text data you should encode it to Base64 or
   --  Quoted-Printable using the operations in Basil.Strings. If you
   --  want to use the 8bit or Binary transfer forms, use the function
   --  below.
   function To_MIME_Entity (Entity_Type : in Content_Type;
                            Body_Part   : in Encoded_String)
                           return MIME_Entity;

   --------------------------------------------------------------------------
   --  This function creates a MIME entity with the given content type
   --  and body part. It is a modification of the function above to
   --  permit 8-bit data in the body part. The Content_Transfer_Encoding
   --  of the type object MUST be either CTE_Binary or CTE_8bit.
   function To_MIME_Entity (Entity_Type : in Content_Type;
                            Body_Part   : in String)
                           return MIME_Entity;

   ----------------------------------------------------------------------------
   --  This is an implementation of the function that must be passed
   --  to Unserialize_Entity to handle recursive decoding of children
   --  into MIME_Entity objects (Unserialize_Entity works on
   --  Abstract_Entities). It is in the public part of this package so
   --  Basil.Entities.Messages can access it.
   procedure Get_Children (Source : in     String;
                           Index  : in out Positive;
                           Entity : in out Abstract_Entity'Class);

private

   type MIME_Entity is new Abstract_Entity with null record;

   Empty_MIME_Entity  : constant MIME_Entity
     := (Basil.Entities.Empty_Entity with null record);

   No_Child           : constant Child_Cursor
     := Basil.Entities.No_Child;

   No_Child_Recursive : constant Child_Cursor
     := Basil.Entities.No_Child_Recursive;

end Basil.Entities.MIME;
