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
--  This package contains a set of deterministic finite-state automata
--  to match tokens that are more complex than what can be described
--  using the character sets of Basil.Tokens. By using hand-made
--  automata to match these tokens, we avoid dependency on regex
--  libraries, which are not part of the standard Ada language. Also I
--  think the operation of a well-made automation is more clear than
--  that of a typical non-trivial regexp.
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;

private package Basil.Automata is

   type Automation_ID is
     (Header_Value,
      Uninteresting,
      Newline_Sequence,
      Dot_Atom_Text);

   ------------------------------------------------------------------------
   --  This procedure takes a source string, an index of the first
   --  character in the token, and the ID of the chosen automation. If
   --  the automation fails to match, the function will return zero to
   --  correspond to the behavior of its counterpart in
   --  Basil.Tokens. Otherwise, it will return the index of the last
   --  character the automation processed before matching.
   --
   --  Note that zero is returned on a failure to match regardless of
   --  the number of characters the automation processed before it
   --  rejected the input.
   function Get_Token_End (Source     : in String;
                           First      : in Positive;
                           Automation : in Automation_ID)
                          return Natural;

   --------------------------------------------------------------------------
   --  This procedure provides the common pattern of extracting a
   --  token from the front of an unbounded string and then reduce the
   --  size of the string by deleting that token from it.
   procedure Get_Token (Source     : in out Unbounded_String;
                        Token      :    out Unbounded_String;
                        Automation : in     Automation_ID);

   ----------------------------------------------------------------------------
   --  This procedure provides the common pattern of removing an
   --  unwanted token from the front of an unbounded string.
   procedure Discard_Token (Source     : in out Unbounded_String;
                            Automation : in     Automation_ID);

end Basil.Automata;
