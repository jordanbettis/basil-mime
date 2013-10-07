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

with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Numerics.Discrete_Random;
with Ada.Strings.Bounded;

with Ada.Strings.Maps;             use Ada.Strings.Maps;

with Basil.Utils;                  use Basil.Utils;
with Basil.Tokens;                 use Basil.Tokens;
with Basil.Automata;               use Basil.Automata;

package body Basil.Message_ID_Headers is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --  These items are used by the Random_String function in
   --  Generate_Message_ID. They were originally implemented at the
   --  function level but I ran into a curious problem during testing
   --  where the generator would get instantiated and primed in the
   --  same fraction of a second if this operation was called
   --  repeatedly, resulting in multiple messages with the same
   --  message ID.
   Pick_Array    : constant String :=
     "0123456789abcdefghijklmnopqrstuvwxyzABCDEFJHIJKLMNOPQRSTUVWXYZ";
   String_Length : constant Positive := 20;

   subtype Random_Index is Integer range 1 .. Pick_Array'Length;
   package Picker is new Ada.Numerics.Discrete_Random (Random_Index);
   package Bounded is
      new Ada.Strings.Bounded.Generic_Bounded_Length (String_Length);

   -- Randomer is primed in the body of this package
   Randomer   : Picker.Generator;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Generate_Message_ID (Key : in Encoded_String)
                                return Message_ID_Header is

      -------------------------------------------------------------------------
      function Random_String return String;

      function Random_String return String is

         Value : Bounded.Bounded_String;

      begin -- Random_String

         -- Picker.Reset (Gen);

         for I in 1 .. String_Length loop
            Bounded.Append (Value, Pick_Array (Picker.Random (Randomer)));
         end loop;

         return Bounded.To_String (Value);

      end Random_String;

      -------------------------------------------------------------------------
      function Get_Hostname return String;

      function Get_Hostname return String is

         use Interfaces.C;
         use Interfaces.C.Strings;

         --  gethostname is a POSIX function so hopefully this won't hurt
         --  portability too much.
         function gethostname (Name : chars_ptr; Len  : size_t) return int;
         pragma Import (C, gethostname);

         --  Unfortunatly we're now limited to platforms that support C.
         function malloc (Size : size_t) return chars_ptr;
         pragma Import (C, malloc);

         Result   : int;
         Len      : size_t := 255;
         Name     : chars_ptr;

      begin -- Get_Hostname

         Name   := malloc (Len);
         Result := gethostname (Name, Len);

         declare
            Hostname : String := Value (Name);
         begin
            Free (Name);
            if Integer (Result) = 0 and Hostname'Length /= 0 then
               return Hostname;
            else
               --  We really just need *something* for the domain.
               return "shangri.la";
            end if;
         end;

      end Get_Hostname;

   begin -- Generate_Message_ID

      return Message_ID_Header'
        (Key         => EUS (Key),
         Local_Part  => US (Random_String & ".basil.ada"),
         Domain_Part => US (Get_Hostname));

   end Generate_Message_ID;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_Message_ID_Header (Key         : in Encoded_String;
                                  Local_Part  : in Encoded_String;
                                  Domain_Part : in Encoded_String)
                                 return Message_ID_Header is

   begin -- To_Message_ID_Header

      if Domain_Part'Length /= 0 then

         if (not (Domain_Part (Domain_Part'First) = '[' and
                  Domain_Part (Domain_Part'Last) = ']'))
           and Automata.Get_Token_End (Source     => S (Domain_Part),
                                       First      => Domain_Part'First,
                                       Automation => Dot_Atom_Text)
           /= Domain_Part'Last
         then

            raise Invalid_Character;

         end if;

      end if;

      return Message_ID_Header'(Key => EUS (Key),
                                Local_Part   => US (Unenquote_If_Needed
                                                    (S (Local_Part),
                                                     Has_Quotation => False)),
                                Domain_Part  => US (Unenquote_Domain
                                                    (S (Domain_Part))));

   end To_Message_ID_Header;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Key (Header : in Message_ID_Header) return String is

   begin -- Get_Key

      return S (Header.Key);

   end Get_Key;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Local_Part  (Header : in Message_ID_Header) return String is

   begin -- Get_Local_Part

      return S (Header.Local_Part);

   end Get_Local_Part;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Domain_Part (Header : in Message_ID_Header) return String is

   begin -- Get_Domain_Part

      return S (Header.Domain_Part);

   end Get_Domain_Part;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Message_ID  (Header : in Message_ID_Header) return String is

      Local_Part   : String
        := Enquote_If_Needed (S (Header.Local_Part),
                              Requires_Quoting => not (RFC2822_Atom_Text
                                                       or To_Set (".")),
                              Add_Quotation    => False);
      Domain_Part  : String := Enquote_Domain (S (Header.Domain_Part));

   begin -- Get_Message_ID

      return "<" & Local_Part & "@" & Domain_Part & ">";

   end Get_Message_ID;

begin -- Basil.Message_ID_Headers

   -- We just need to prime Randomer here
   Picker.Reset (Randomer);

end Basil.Message_ID_Headers;
