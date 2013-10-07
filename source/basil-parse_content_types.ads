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
--  This package implements a parser for RFC 2045 content type
--  headers. For an BNF of the language matched, please see the body
--  of this package.
--
-------------------------------------------------------------------------------

with Ada.Containers.Doubly_Linked_Lists;

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with Basil.Strings;            use Basil.Strings;

private package Basil.Parse_Content_Types is

   --  Content type parameters consist of key=value pairs. The key
   --  must be a token, the value may either be a token or a quoted
   --  text. We always store values as unquoted strings.
   type Parameter is
      record
         Key   : Unbounded_String;
         Value : Unbounded_String;
      end record;

   Empty_Parameter : constant Parameter;

   package Parameter_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Parameter);

   --  This type holds the semantically singificant parts of the
   --  Content_Type line. It is designed to hold 'raw' content type
   --  semantics, including informationo that may be thrown away when
   --  distilling the line down into the Content_Type object.
   type Content_Type_Semantics is
      record
         MIME_Type  : Unbounded_String;
         Sub_Type   : Unbounded_String;
         Parameters : Parameter_Lists.List;
      end record;

   ----------------------------------------------------------------------------
   --  This function takes the value portion of a "Content_Type"
   --  header and returns a Content_Type_Semantics object containing
   --  the semantically useful parts. If the function can not parse
   --  the input value it will raise Basil.Parse_Error.
   function Parse_Content_Type (Header_Value : in Encoded_String)
                               return Content_Type_Semantics;

private

   Empty_Parameter : constant Parameter :=
     Parameter'(To_Unbounded_String (String'("")),
                To_Unbounded_String (String'("")));

end Basil.Parse_Content_Types;
