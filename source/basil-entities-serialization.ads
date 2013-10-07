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
--  This package provides private routines to serializea and
--  unserialize entities.
--
-------------------------------------------------------------------------------

private package Basil.Entities.Serialization is

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
   function Serialize_Entity (Source : in Abstract_Entity'Class) return String;


   ----------------------------------------------------------------------------
   --  This is a recusive function used by From_String to unserialize
   --  the entity. The delimiter is the multipart delimiter of the
   --  parent entity, should one exist.
   procedure Unserialize_Entity (Source       : in     String;
                                 Delimiter    : in     String   := "";
                                 Index        : in out Positive;
                                 Entity       :    out Abstract_Entity'Class;
                                 Get_Children : not null access procedure
                                 (Source : in     String;
                                  Index  : in out Positive;
                                  Entity : in out Abstract_Entity'Class));

end Basil.Entities.Serialization;
