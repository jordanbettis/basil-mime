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

with Basil.Tokens;            use Basil.Tokens;
with Basil.Automata;          use Basil.Automata;
with Basil.Utils;             use Basil.Utils;
with Basil.Parsers_Common;    use Basil.Parsers_Common;

package body Basil.Parse_Message_IDs is

   --  This parser matches a modification of the EBNF in RFC 2822 p
   --  15-16. The modifications are intended to make the parser more
   --  flexible and to simplify the BNF constructions by folding in
   --  the obsolete elements from p 33.
   --
   --  This is the modified EBNF matched by this parser:
   --
   --  msg-id-list      := CWS 1*msg-id CWS
   --  msg-id           := "<" CWS local-part CWS "@" CWS domain CWS ">"
   --  local-part       := word *("." word)
   --  word             := atom ; with quoted pairs
   --  domain           := domain-literal / domain-part
   --  domain-part      := atom *("." atom)
   --  domain-literal   := "[" CWS domain-text CWS "]"
   --  CWS              := [CFWS] ; comment or folding whitespace as
   --                             ; matched by the 'uninteresting'
   --                             ; automation in Basil.Automata

   ----------------------------------------------------------------------------
   procedure Get_Message_ID   (Value       : in out Unbounded_String;
                               Message_IDs : in out Semantics_Lists.List);

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Parse_Message_IDs_List (Header_Value : in String)
                                   return Semantics_Lists.List is

      Value       : Unbounded_String := US (Header_Value);
      Message_IDs : Semantics_Lists.List;

   begin -- Parse_Message_ID_List

      Automata.Discard_Token (Value, Uninteresting);

      while Length (Value) > 0 loop
         Get_Message_ID (Value, Message_IDs);
         Automata.Discard_Token (Value, Uninteresting);
      end loop;

      return Message_IDs;

   end Parse_Message_IDs_List;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Message_ID   (Value       : in out Unbounded_String;
                               Message_IDs : in out Semantics_Lists.List) is

      Local  : Unbounded_String;
      Domain : Unbounded_String;

   begin -- Get_Message_ID

      if Length (Value) > 0 and then
        Element (Value, 1) = '<'
      then
         Tail (Value, Length (Value) - 1);
      else
         raise Parse_Error;
      end if;

      Automata.Discard_Token (Value, Uninteresting);

      Get_Local_Part (Value, Local);

      Automata.Discard_Token (Value, Uninteresting);

      if Length (Value) > 0 and then
        Element (Value, 1) = '@'
      then
         Tail (Value, Length (Value) - 1);
      else
         raise Parse_Error;
      end if;

      Automata.Discard_Token (Value, Uninteresting);

      Get_Domain (Value, Domain);

      Automata.Discard_Token (Value, Uninteresting);

      if Length (Value) > 0 and then
        Element (Value, 1) = '>'
      then
         Tail (Value, Length (Value) - 1);
      else
         raise Parse_Error;
      end if;

      if Length (Local) = 0 or Length (Domain) = 0 then
         raise Parse_Error;
      else
         Semantics_Lists.Append (Message_IDs,
                                 Message_ID_Item'(Local_Part => Local,
                                                  Domain_Part => Domain));
      end if;

   end Get_Message_ID;

end Basil.Parse_Message_IDs;
