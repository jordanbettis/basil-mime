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
--  RFC 2822 defines some syntactic contructs which are shared among
--  the parsers in this library, particularly in the more relaxed
--  obsolete definitions that we prefer to fulfill this library's
--  "good faith" translation requirement. This package is provided as
--  a common pool of constructs which any parser can use.
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

private package Basil.Parsers_Common is

   ----------------------------------------------------------------------------
   -- Matches:
   --          local-part       := word *("." word)
   --          word             := atom ; with quoted pairs
   procedure Get_Local_Part     (Value       : in out Unbounded_String;
                                 Local_Part  :    out Unbounded_String);

   ----------------------------------------------------------------------------
   -- Matches:
   --          domain           := domain-literal / domain-part
   procedure Get_Domain         (Value       : in out Unbounded_String;
                                 Domain      :    out Unbounded_String);

   ----------------------------------------------------------------------------
   -- Matches:
   --          domain-part      := atom *("." atom)
   procedure Get_Domain_Part    (Value       : in out Unbounded_String;
                                 Domain      :    out Unbounded_String);

   ----------------------------------------------------------------------------
   -- Matches:
   --          domain-literal   := "[" CWS domain-text CWS "]"
   procedure Get_Domain_Literal (Value       : in out Unbounded_String;
                                 Domain      :    out Unbounded_String);

end Basil.Parsers_Common;
