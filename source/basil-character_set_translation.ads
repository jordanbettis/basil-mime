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
--  None
--
-------------------------------------------------------------------------------
-- PROJECT CONTEXT:
--
--  This package translates the character set labels from the argument
--  to the Content-Type line of text type MIME entities into
--  Character_Set_ID enumeration identifiers.
--
--  It also does the inverse, translating enumeration identifiers into
--  labels suitable for inserting into the argument of Content-Type
--  lines.
--
-------------------------------------------------------------------------------

package Basil.Character_Set_Translation is

   --------------------------------------------------------------------------
   --  Given an identifier, this function return the IANA prefered
   --  MIME label for the character set.
   function Get_Label (Character_Set_Identifier : Character_Set_ID)
                      return String;

   ----------------------------------------------------------------------------
   --  Given an IANA registered label for a character set, this function
   --  will return a Character_Set_Id type enumeration identifier, or
   --  CS_Unknown if the character set identifier is not recognized.
   function Get_Identifier (Character_Set_Label : String)
                           return Character_Set_ID;

end Basil.Character_Set_Translation;

