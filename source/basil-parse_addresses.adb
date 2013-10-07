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

with Ada.Strings.Maps;        use Ada.Strings.Maps;

with Basil.Tokens;            use Basil.Tokens;
with Basil.Automata;          use Basil.Automata;
with Basil.Utils;             use Basil.Utils;
with Basil.Parsers_Common;    use Basil.Parsers_Common;

package body Basil.Parse_Addresses is

   --  This parser matches a modification of the EBNF in RFC 2822 p
   --  15-16. The modifications are intended to make the parser more
   --  flexible and to simplify the BNF constructions by folding in
   --  the obsolete elements from p 33.
   --
   --  This is the modified EBNF matched by this parser:
   --
   --  address-list     := (1*([address] CWS "," CWS) [address]
   --  address          := mailbox / group
   --  mailbox          := name-addr / addr-spec
   --  name-addr        := [display-name] angle-addr
   --  angle-addr       := CWS "<" [obs-route] addr-spec ">" CWS
   --  addr-spec        := local-part "@" domain
   --  local-part       := word *("." word)
   --  word             := atom ; with quoted pairs
   --  domain           := domain-literal / domain-part
   --  domain-part      := atom *("." atom)
   --  domain-literal   := "[" CWS domain-text CWS "]"
   --  group            := display-name CWS ":" CWS [mailbox-list CWS] ";" CWS
   --  display-name     := ['"'] word *(word / "." / CWS) ['"']
   --  mailbox-list     := 1*([mailbox] CWS "," CWS) [mailbox]
   --  obs-route        := CWS obs-domain-list ":" CWS
   --  obs-domain-list  := "@" domain *( *(CWS / "," ) CWS "@" domain)
   --  CWS              := [CFWS] ; comment or folding whitespace as
   --                             ; matched by the 'uninteresting'
   --                             ; automation in Basil.Automata

   ----------------------------------------------------------------------------
   --  Loops attempting to consume addresses and groups. Failure to
   --  consume an address or a group in a loop, when there are still
   --  tokens in the string, results in a Parse_Error.
   procedure Get_Address_List     (Value     : in out Unbounded_String;
                                   Addresses : in out Semantics_Lists.List);

   ----------------------------------------------------------------------------
   --  Syntax Directed Translation:
   --
   --  * Upon successful consuming a local-part and a domain-part this
   --    procedure appends an address to the list.
   procedure Get_Mailbox          (Value     : in out Unbounded_String;
                                   Addresses : in out Semantics_Lists.List);

   ----------------------------------------------------------------------------
   procedure Get_Name_Address   (Value       : in out Unbounded_String;
                                 Name        :    out Unbounded_String;
                                 Local_Part  :    out Unbounded_String;
                                 Domain_Part :    out Unbounded_String);

   ----------------------------------------------------------------------------
   procedure Get_Angle_Address  (Value       : in out Unbounded_String;
                                 Local_Part  :    out Unbounded_String;
                                 Domain_Part :    out Unbounded_String);

   ----------------------------------------------------------------------------
   --  Failure to find both a local-part and a domain-part together
   --  results in a Parse_Error.
   procedure Get_Address_Spec   (Value       : in out Unbounded_String;
                                 Local_Part  :    out Unbounded_String;
                                 Domain_Part :    out Unbounded_String);

   ----------------------------------------------------------------------------
   --  Syntax Directed Translation:
   --
   --  * Upon sucessfully consuming the display name and the ':' this
   --    procedure appends a begin-group marker to the list.
   --  * Upon successfull consuming the ';' this procedure places an
   --    end-group marker on the list.
   --  * Failure to find a ';' after finding a display-name and ':'
   --    results in a Parse_Error
   procedure Get_Group          (Value       : in out Unbounded_String;
                                 Addresses   : in out Semantics_Lists.List);

   ----------------------------------------------------------------------------
   procedure Get_Display_Name   (Value       : in out Unbounded_String;
                                 Name        :    out Unbounded_String);

   ----------------------------------------------------------------------------
   procedure Get_Mailbox_List   (Value       : in out Unbounded_String;
                                 Addresses   : in out Semantics_Lists.List);

   ----------------------------------------------------------------------------
   --  We don't care if the user thinks he needs a route list, so we
   --  support this lexically to be conformant but we just drop the
   --  information on the floor. This procedure also handles obsolete
   --  domain lists so there's no seperate procedure for that.
   procedure Discard_Obs_Route  (Value       : in out Unbounded_String);

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Parse_Address_List (Header_Value : in String)
                               return Semantics_Lists.List is

      Value     : Unbounded_String := US (Header_Value);
      Addresses : Semantics_Lists.List;

   begin -- Parse_Address_List

      Get_Address_List (Value     => Value,
                        Addresses => Addresses);

      return Addresses;

   end Parse_Address_List;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Address_List (Value     : in out Unbounded_String;
                               Addresses : in out Semantics_Lists.List) is

      Previous_Length       : Natural := Length (Value);
      Before_Mailbox_Length : Natural;

   begin -- Get_Address_List

      --  The general meaning of the contorted BNF of the address-list
      --  is that we can have any number of addresses and address
      --  groups, seperated by commas and any assortment of comments
      --  and whitspace. So our strategy is just to loop consuming
      --  address and groups and discarding the rest until we either
      --  run out of input or encounter a parse error.

      while Length (Value) > 0 loop

         Automata.Discard_Token (Value, Uninteresting);
         Discard_Token (Value, To_Set (","));
         Automata.Discard_Token (Value, Uninteresting);

         Before_Mailbox_Length := Length (Value);
         Get_Mailbox (Value, Addresses);

         if Length (Value) = Before_Mailbox_Length then
            Get_Group (Value, Addresses);
         end if;

         --  Since both mailboxes and group are optional they'll just
         --  not consume anything if they encounter an error, however,
         --  it is an error to not consume at least one mailbox or
         --  group in a loop (or at least discard a comma), so we
         --  check to see if anything's been consumed.
         if Length (Value) = Previous_Length and Length (Value) /= 0 then
            raise Parse_Error;
         else
            Previous_Length := Length (Value);
         end if;

      end loop;

   end Get_Address_List;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Mailbox      (Value     : in out Unbounded_String;
                               Addresses : in out Semantics_Lists.List) is

      Working_Value : Unbounded_String := Value;
      Display_Name  : Unbounded_String;
      Local_Part    : Unbounded_String;
      Domain_Part   : Unbounded_String;

   begin -- Get_Mailbox

      Get_Name_Address (Working_Value,
                        Name        => Display_Name,
                        Local_Part  => Local_Part,
                        Domain_Part => Domain_Part);

      if Length (Local_Part) < 1 and Length (Domain_Part) < 1 then
         Display_Name  := US ("");
         Working_Value := Value;
         Get_Address_Spec (Working_Value,
                           Local_Part  => Local_Part,
                           Domain_Part => Domain_Part);
      end if;

      if Length (Local_Part) > 0 and Length (Domain_Part) > 0 then

         Value := Working_Value;
         Semantics_Lists.Append (Addresses,
                                 Address_Item'(Item_Type    => Address,
                                               Display_Name => Display_Name,
                                               Local_Part   => Local_Part,
                                               Domain_Part  => Domain_Part));

      end if;

      end Get_Mailbox;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Name_Address (Value       : in out Unbounded_String;
                               Name        :    out Unbounded_String;
                               Local_Part  :    out Unbounded_String;
                               Domain_Part :    out Unbounded_String) is

      Working_Value : Unbounded_String := Value;
      Display_Name  : Unbounded_String;
      Local         : Unbounded_String;
      Domain        : Unbounded_String;

   begin -- get_Name_Address

      Get_Display_Name (Working_Value,
                        Display_Name);

      Get_Angle_Address (Working_Value,
                         Local_Part => Local,
                         Domain_Part => Domain);

      if Length (Local) > 0 and Length (Domain) > 0 then

         Value       := Working_Value;
         Name        := Display_Name;
         Local_Part  := Local;
         Domain_Part := Domain;

      end if;

   end Get_Name_Address;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Angle_Address (Value       : in out Unbounded_String;
                                Local_Part  :    out Unbounded_String;
                                Domain_Part :    out Unbounded_String) is

      Working_Value         : Unbounded_String := Value;
      Working_Local_Part    : Unbounded_String;
      Working_Domain_Part   : Unbounded_String;

   begin -- Get_Angle_Address

      Automata.Discard_Token (Working_Value, Uninteresting);

      if Length (Working_Value) > 0 and then
        Element (Working_Value, 1) /= '<'
      then
         return;
      else
         Tail (Working_Value, Length (Working_Value) - 1);
      end if;

      Discard_Obs_Route (Working_Value);

      Automata.Discard_Token (Working_Value, Uninteresting);

      Get_Address_Spec (Working_Value,
                        Local_Part  => Working_Local_Part,
                        Domain_Part => Working_Domain_Part);

      Automata.Discard_Token (Working_Value, Uninteresting);

      if Length (Working_Value) > 0 and then
        Element (Working_Value, 1) /= '>'
      then
         return;
      else
         Tail (Working_Value, Length (Working_Value) - 1);
      end if;

      Automata.Discard_Token (Working_Value, Uninteresting);

      Value       := Working_Value;
      Local_Part  := Working_Local_Part;
      Domain_Part := Working_Domain_Part;

   end Get_Angle_Address;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Address_Spec   (Value       : in out Unbounded_String;
                                 Local_Part  :    out Unbounded_String;
                                 Domain_Part :    out Unbounded_String) is

      Working_Value       : Unbounded_String := Value;
      Working_Local_Part  : Unbounded_String;
      Working_Domain_Part : Unbounded_String;

   begin -- Get_Address_Spec

      Get_Local_Part (Working_Value, Working_Local_Part);

      if Length (Working_Local_Part) = 0 then
         return;
      end if;

      Automata.Discard_Token (Working_Value, Uninteresting);

      if Length (Working_Value) > 0 and then
        Element (Working_Value, 1) /= '@'
      then
         return;
      else
         Tail (Working_Value, Length (Working_Value) - 1);
      end if;

      Automata.Discard_Token (Working_Value, Uninteresting);

      Get_Domain (Working_Value, Working_Domain_Part);

      --  We consumed a local-part, we consumed an '@', but there's no
      --  domain! Oops!
      if Length (Working_Domain_Part) = 0 then
         raise Parse_Error;
      end if;

      Value       := Working_Value;
      Local_Part  := Working_Local_Part;
      Domain_Part := Working_Domain_Part;

   end Get_Address_Spec;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Group          (Value       : in out Unbounded_String;
                                 Addresses   : in out Semantics_Lists.List) is

      Working_Value : Unbounded_String := Value;
      Display_Name  : Unbounded_String;

   begin -- Get_Group

      Get_Display_Name (Working_Value, Display_Name);

      if Length (Display_Name) < 1 then
         return;
      end if;

      Automata.Discard_Token (Working_Value, Uninteresting);

      if Length (Working_Value) > 0 and then
        Element (Working_Value, 1) /= ':'
      then
         return;
      else
         Tail (Working_Value, Length (Working_Value) - 1);
      end if;

      Automata.Discard_Token (Working_Value, Uninteresting);

      Semantics_Lists.Append
        (Addresses, Address_Item'(Item_Type   => Group_Start,
                                  Group_Label => Display_Name));

      Get_Mailbox_List (Working_Value, Addresses);

      Automata.Discard_Token (Working_Value, Uninteresting);

      if Length (Working_Value) > 0 and then
        Element (Working_Value, 1) /= ';'
      then
         raise Parse_Error;
      else
         Tail (Working_Value, Length (Working_Value) - 1);
      end if;

      Semantics_Lists.Append
        (Addresses, Address_Item'(Item_Type => Group_End,
                                  End_Label => Display_Name));

      Value := Working_Value;

   end Get_Group;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Display_Name   (Value       : in out Unbounded_String;
                                 Name        :    out Unbounded_String) is

      Working_Value : Unbounded_String := Value;
      Display_Name  : Unbounded_String;
      Working_Word  : Unbounded_String;

   begin -- Get_Display_Name

      Discard_Token (Working_Value, Whitespace);
      Discard_Token (Working_Value, To_Set (""""));
      Discard_Token (Working_Value, Whitespace);

      Get_Token (Working_Value,
                 Token  => Working_Word,
                 Set    => RFC2822_Atom_Text,
                 Quoted => True);

      while Length (Working_Word) > 0 loop

         Append (Display_Name, Working_Word);
         Working_Word := US ("");

         --  For the purpose of the display name, spaces and comments
         --  are significant.
         Automata.Get_Token (Working_Value,
                             Token      => Working_Word,
                             Automation => Uninteresting);

         if Length (Working_Value) > 0 and then
           Element (Working_Value, 1) = '.'
         then
            Tail (Working_Value, Length (Working_Value) - 1);
            Append (Working_Word, ".");
         end if;

         if Length (Working_Word) < 1 then
            Get_Token (Working_Value,
                       Token  => Working_Word,
                       Set    => RFC2822_Atom_Text,
                       Quoted => True);
         end if;

      end loop;

      Discard_Token (Working_Value, Whitespace);
      Discard_Token (Working_Value, To_Set (""""));
      Discard_Token (Working_Value, Whitespace);

      Trim (Display_Name, Left => Whitespace, Right => Whitespace);

      if Length (Display_Name) > 0 then
         Value := Working_Value;
         Name  := US (Unenquote_If_Needed (S (Display_Name),
                                           Has_Quotation => False));
      end if;

   end Get_Display_Name;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Mailbox_List   (Value       : in out Unbounded_String;
                                 Addresses   : in out Semantics_Lists.List) is

      End_Of_List     : Boolean         := False;

   begin -- Get_Mailbox_List

      Automata.Discard_Token (Value, Uninteresting);

      while Length (Value) > 0 and not End_Of_List loop

         Discard_Token (Value, To_Set (","));
         Automata.Discard_Token (Value, Uninteresting);

         Get_Mailbox (Value, Addresses);

         Automata.Discard_Token (Value, Uninteresting);

         if Length (Value) > 0 and then
           Element (Value, 1) = ','
         then
            End_Of_List := False;

         else
            End_Of_List := True;
         end if;

      end loop;

   end Get_Mailbox_List;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Discard_Obs_Route  (Value       : in out Unbounded_String) is

      Working_Value : Unbounded_String := Value;
      Domain_Label  : Unbounded_String;

   begin -- Discard_Obs_Route

      Automata.Discard_Token (Working_Value, Uninteresting);

      while Length (Working_Value) > 0
        and then Element (Working_Value, 1) = '@'
      loop

         Tail (Working_Value, Length (Working_Value) - 1);

         Get_Domain (Value  => Working_Value,
                     Domain => Domain_Label);

         Automata.Discard_Token (Working_Value, Uninteresting);
         Discard_Token (Working_Value, To_Set (","));
         Automata.Discard_Token (Working_Value, Uninteresting);

      end loop;

      if Length (Working_Value) > 0 and then
        Element (Working_Value, 1) = ':'
      then

         Tail (Working_Value, Length (Working_Value) - 1);
         Automata.Discard_Token (Working_Value, Uninteresting);

         Value := Working_Value;

      end if;

   end Discard_Obs_Route;

end Basil.Parse_Addresses;
