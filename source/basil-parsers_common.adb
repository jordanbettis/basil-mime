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

with Basil.Utils;            use Basil.Utils;
with Basil.Automata;         use Basil.Automata;
with Basil.Tokens;           use Basil.Tokens;

package body Basil.Parsers_Common is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Local_Part     (Value       : in out Unbounded_String;
                                 Local_Part  :    out Unbounded_String) is

      Working_Value       : Unbounded_String := Value;
      Working_Local_Part  : Unbounded_String;
      Working_Word        : Unbounded_String;

   begin -- Get_Local_Part

      Get_Token (Source => Working_Value,
                 Token  => Working_Word,
                 Set    => RFC2822_Atom_Text,
                 Quoted => True);

      while Length (Working_Word) > 0 loop

         Append (Working_Local_Part, Working_Word);
         Working_Word := US ("");

         Automata.Discard_Token (Working_Value, Uninteresting);

         if Length (Working_Value) > 0 and then
           Element (Working_Value, 1) = '.'
         then
            Tail (Working_Value, Length (Working_Value) - 1);
            Append (Working_Local_Part, ".");
         end if;

         Automata.Discard_Token (Working_Value, Uninteresting);

         Get_Token (Source => Working_Value,
                    Token  => Working_Word,
                    Set    => RFC2822_Atom_Text,
                    Quoted => True);

      end loop;

      if Length (Working_Local_Part) > 0 then
         Value      := Working_Value;
         Local_Part := US (Unenquote_If_Needed (S (Working_Local_Part),
                                                Has_Quotation => False));
      end if;

   end Get_Local_Part;


   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Domain         (Value       : in out Unbounded_String;
                                 Domain      :    out Unbounded_String) is

      Working_Value  : Unbounded_String := Value;
      Working_Domain : Unbounded_String;

   begin -- Get_Domain

      Get_Domain_Part (Working_Value, Working_Domain);

      if Length (Working_Domain) < 1 then
         Get_Domain_Literal (Working_Value, Working_Domain);
      end if;

      if Length (Working_Domain) > 0 then
         Value    := Working_Value;
         Domain   := Working_Domain;
      end if;

   end Get_Domain;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Domain_Part    (Value       : in out Unbounded_String;
                                 Domain      :    out Unbounded_String) is

      Working_Value       : Unbounded_String := Value;
      Working_Domain_Part : Unbounded_String;
      Working_Atom        : Unbounded_String;

   begin -- Get_Domain_Part

      Get_Token (Source => Working_Value,
                 Token  => Working_Atom,
                 Set    => RFC2822_Atom_Text);

      while Length (Working_Atom) > 0 loop

         Append (Working_Domain_Part, Working_Atom);
         Working_Atom := US ("");

         Automata.Discard_Token (Working_Value, Uninteresting);

         if Length (Working_Value) > 0 and then
           Element (Working_Value, 1) = '.'
         then
            Tail (Working_Value, Length (Working_Value) - 1);
            Append (Working_Domain_Part, ".");
         end if;

         Automata.Discard_Token (Working_Value, Uninteresting);

         Get_Token (Source => Working_Value,
                    Token  => Working_Atom,
                    Set    => RFC2822_Atom_Text);

      end loop;

      if Length (Working_Domain_Part) > 0 then
         Value  := Working_Value;
         Domain := Working_Domain_Part;
      end if;

   end Get_Domain_Part;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Domain_Literal (Value       : in out Unbounded_String;
                                 Domain      :    out Unbounded_String) is

      Working_Value       : Unbounded_String := Value;
      Working_Domain_Part : Unbounded_String;

   begin -- Get_Domain_Literal

      if Length (Working_Value) > 0 and then
        Element (Working_Value, 1) = '['
      then
         Working_Value := Tail (Working_Value, Length (Working_Value) - 1);
      else
         return;
      end if;

      Automata.Discard_Token (Working_Value, Uninteresting);

      Get_Token (Source => Working_Value,
                 Token  => Working_Domain_Part,
                 Set    => RFC2822_Domain_Text,
                 Quoted => True);

      Automata.Discard_Token (Working_Value, Uninteresting);

      if Length (Working_Value) > 0 and then
        Element (Working_Value, 1) = ']'
      then
         Working_Value := Tail (Working_Value, Length (Working_Value) - 1);
      else
         return;
      end if;

      if Length (Working_Domain_Part) > 0 then
         Value  := Working_Value;
         Domain := US ("[" & Unenquote_If_Needed (S (Working_Domain_Part),
                                                  Has_Quotation => False)
                       & "]");
      end if;

   end Get_Domain_Literal;

end Basil.Parsers_Common;
