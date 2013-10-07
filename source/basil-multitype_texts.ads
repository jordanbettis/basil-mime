---------------------------------------------------------------------------------
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
--  This package provides the Multitype_Texts.List object, which
--  combines Typed_Text objects into lists for use as strings
--  containing characters in multiple character sets. It was
--  occasioned by the requirement that header values be permitted to
--  contain words in several character sets from RFC 2047
--
-------------------------------------------------------------------------------

with Basil.Typed_Texts;
with Ada.Containers.Doubly_Linked_Lists;

package Basil.Multitype_Texts is new Ada.Containers.Doubly_Linked_Lists
  (Element_Type => Basil.Typed_Texts.Typed_Text,
     "="        => Basil.Typed_Texts.Are_Equal);
