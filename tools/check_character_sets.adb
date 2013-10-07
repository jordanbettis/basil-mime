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
--  This program is used by check_character_sets.py to access Basil's
--  character set registry. It reads a character set identifiers from
--  standard input and produces a diagnostic message on standard
--  output if one does not exist.
--
-------------------------------------------------------------------------------

with Ada.Text_IO;                     use Ada.Text_IO;

with Basil.Character_Set_Translation; use Basil.Character_Set_Translation;
use Basil;

-------------------------------------------------------------------------------
procedure Check_Character_Sets is

   Label      : String := Get_Line (Standard_Input);

begin -- Check_Character_Sets

   if Get_Identifier (Label) = CS_Unknown then
      Put_Line ("Unknown Character Set Label: " & Label);
   end if;

end Check_Character_Sets;
