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
--   MRCY.SPC.002
--
-------------------------------------------------------------------------------
-- PROJECT CONTEXT:
--
--  This package provides functionality for management of content type
--  information for MIME entities, including translation between
--  content type objects and the headers of an entitiy.
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Characters.Handling;   use Ada.Characters.Handling;

with Basil.Headers.Lists;       use Basil.Headers.Lists;

package Basil.Content_Types is

   -- The Content Type object type, as specified in MRCY.SPC.002
   type Content_Type (MIME_Type : Primary_Content_Type := T_Unknown) is
      record
         Content_Transfer_Encoding : Content_Transfer_Encoding_ID :=
           CTE_Unknown;
         case MIME_Type is
            when T_Application =>
               Application_Subtype  : Application_Content_Subtype
                 := ST_Unknown;
            when T_Audio =>
               Audio_Subtype        : Audio_Content_Subtype      := ST_Unknown;
            when T_Experimental =>
               Experimental_Subtype : Experimental_Content_Subtype :=
                 ST_Unknown;
            when T_Image =>
               Image_Subtype        : Image_Content_Subtype      := ST_Unknown;
            when T_Message =>
               Message_Subtype      : Message_Content_Subtype    := ST_Unknown;
            when T_Model =>
               Model_Subtype        : Model_Content_Subtype      := ST_Unknown;
            when T_Multipart =>
               Multipart_Subtype    : Multipart_Content_Subtype  := ST_Unknown;
               Multipart_Delimiter  : Unbounded_String;
            when T_Text =>
               Text_Subtype         : Text_Content_Subtype       := ST_Unknown;
               Character_Set        : Character_Set_ID           := CS_Unknown;
            when T_Unknown =>
               Unknown_Subtype      : Unknown_Content_Subtype    := ST_Unknown;
            when T_Video =>
               Video_Subtype        : Video_Content_Subtype      := ST_Unknown;
         end case;
      end record;

   No_Content_Type : constant Content_Type :=
     (MIME_Type       => T_Unknown,
      Unknown_Subtype => ST_Unknown,
      Content_Transfer_Encoding =>
        CTE_7bit);

   -- This defines a few broad classes of content type that we use for
   -- validation
   type Content_Class_Category is
     (Normal,
      Experimental,
      Unknown);

   ----------------------------------------------------------------------------
   --  This function takes the headers of a MIME entity and parses the
   --  relevent values to return a Content_Type type object
   --  representing the header.
   function From_Headers (Source : in Headers.Lists.List) return Content_Type;

   ----------------------------------------------------------------------------
   --  This function takes a content type object and uses it to
   --  generate the headers needed for a MIME entity. Add_Version
   --  controls if the MIME-Version header is added (required for
   --  Message objects but not for child MIME entities).
   function To_Headers (Source      : in Content_Type;
                        Add_Version : in Boolean)
                       return Headers.Lists.List;

   ----------------------------------------------------------------------------
   --  This function is used elsewhere in the library for validation
   --  of content types. It takes a content class object and returns
   --  its content class category (See above).
   function Get_Content_Class (Type_Object : Content_Type)
                              return Content_Class_Category;

   ----------------------------------------------------------------------------
   --  This function returns a random string suitable for use as a
   --  multipart delimiter.
   function Generate_Multipart_Delimiter return String;

end Basil.Content_Types;
