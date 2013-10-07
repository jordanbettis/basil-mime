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

with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Maps;                use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;      use Ada.Strings.Maps.Constants;
with Ada.Characters.Handling;         use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
                                      use Ada.Strings;

with Basil.Utils;                     use Basil.Utils;
with Basil.Tokens;                    use Basil.Tokens;

package body Basil.Strings.Base64 is

   ----------------------------------------------------------------------------
   --  A quantum contains the 24 bits consisting of three unencoded
   --  characters. For encoding it is divided into four sestets of six
   --  bits each, which serve as an index to the alphabet array to
   --  form four encoded characters.
   type Quantum is mod 2**24;
   type Sestet  is mod 2**6;
   type Octet   is mod 2**8;

   -- See RFC 2045 p 25
   Base64_Alphabet : constant String(1 .. 64) :=
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

   package L renames Ada.Characters.Latin_1;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Encode (Input_String    : in String;
                    Wrap_Long_Lines : in Boolean := True)
                   return Encoded_String is

      -------------------------------------------------------------------------
      -- Pad strings whose length are not a multple of 3 with zero bytes.
      function Pad (Input_String : in String) return String;

      function Pad (Input_String : in String) return String is

      begin -- Pad

         if Input_String'Length mod 3 = 2 then
            return Input_String & L.NUL;
         elsif Input_String'Length mod 3 = 1 then
            return Input_String & L.NUL & L.NUL;
         else
            return Input_String;
         end if;

      end Pad;

      -------------------------------------------------------------------------
      procedure Wrap (Quoted_Value : in out Unbounded_String);

      procedure Wrap (Quoted_Value : in out Unbounded_String) is

         Folded_Value : Unbounded_String := US ("");
         Max_Length   : Positive         := 76;
         Fold_Count   : Natural          := 0;

      begin -- Wrap

         for I in 1 .. Length(Quoted_Value) loop
            if I mod Max_Length = 0 then
               Folded_Value := Folded_Value &
                 Slice (Quoted_Value, I - (Max_Length - 1), I) &
                 US (L.CR & L.LF);
               Fold_Count := Fold_Count + 1;
            end if;
         end loop;

         Quoted_Value := Folded_Value & Tail
           (Quoted_Value, Length (Quoted_Value) - (Fold_Count * Max_Length));

      end Wrap;

      -------------------------------------------------------------------------
      Input_Padded    : String    := Pad (Input_String);
      Input_Quantum   : Quantum   := 0;
      Output_Sestet   : Sestet    := 0;
      Index           : Natural   := 1;

      Output          : Unbounded_String := US ("");

   begin -- Encode_Base64

      while Index + 2 <= Input_Padded'Length loop

         Input_Quantum :=
           Character'Pos(Input_Padded(Index))     * 2**16 +
           Character'Pos(Input_Padded(Index + 1)) * 2**8  +
           Character'Pos(Input_Padded(Index + 2));

         for I in 1 .. 4 loop
            Output_Sestet := Sestet
              ( (Input_Quantum and 2#11_1111# * (2 ** ((4-I) * 6)))
                / (2 ** ((4-I) * 6)) );
            Output := Output & Base64_Alphabet (Integer (Output_Sestet) + 1);
         end loop;

         Index := Index + 3;

      end loop;

      if Input_String'Length mod 3 = 2 then
         Output := Head (Output, Length (Output) - 1) & US ("=");
      elsif Input_String'Length mod 3 = 1 then
         Output := Head (Output, Length (Output) - 2) & US ("==");
      end if;

      if Wrap_Long_Lines then
         Wrap (Output);
      end if;

      return ES (Output);

   end Encode;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Decode (Input_String   : in Encoded_String;
                    Reject_Badness : in Boolean := False)
                   return String is

      -------------------------------------------------------------------------
      --  This function removes CR and LF characters from the input
      --  string. If Reject_Badness is true, it raises invalid
      --  character if any other non-Base64 characters are
      --  found. Otherwise it removes them as well.
      function Cleanse (Input          : in Encoded_String;
                        Reject_Badness : in Boolean)
                       return Encoded_String;

      function Cleanse (Input          : in Encoded_String;
                        Reject_Badness : in Boolean)
                       return Encoded_String is

         Output              : Unbounded_String       := US ("");
         Base64_Alphabet_Set : constant Character_Set
           := To_Set (Base64_Alphabet & "=");

      begin -- Cleanse

         for I in Input'Range loop

            if Is_In (Input(I), Base64_Alphabet_Set) then
               Output := Output & Input(I);
            elsif Reject_Badness and Is_In (Input(I), not Newlines) then
               raise Invalid_Character;
            end if;

         end loop;

         return ES (Output);

      end Cleanse;

      -------------------------------------------------------------------------
      --  This function takes a character of an encoded value and
      --  returns the sestet that it represents.
      function Get_Value (Input : in Character) return Quantum;

      function Get_Value (Input : in Character) return Quantum is

         Value   : Integer := Character'Pos(Input);

         UC_A    : constant Integer := Character'Pos('A');
         UC_Z    : constant Integer := Character'Pos('Z');
         LC_A    : constant Integer := Character'Pos('a');
         LC_Z    : constant Integer := Character'Pos('z');
         Zero    : constant Integer := Character'Pos('0');
         Nine    : constant Integer := Character'Pos('9');
         Plus    : constant Integer := Character'Pos('+');
         Solidus : constant Integer := Character'Pos('/');
         Pad     : constant Integer := Character'Pos('=');

         -- See RFC 2045 p 25
         Base64_Pad     : constant Integer := 0;
         Base64_LC_A    : constant Integer := 26;
         Base64_Zero    : constant Integer := 52;
         Base64_Plus    : constant Integer := 62;
         Base64_Solidus : constant Integer := 63;

      begin -- Get_Value

         if Value >= UC_A and Value <= UC_Z then
            return Quantum (Value - UC_A);
         elsif Value >= LC_A and Value <= LC_Z then
            return Quantum (Value - LC_A + Base64_LC_A);
         elsif Value >= Zero and Value <= Nine then
            return Quantum (Value - Zero + Base64_Zero);
         elsif Value = Plus then
            return Quantum (Base64_Plus);
         elsif Value = Solidus then
            return Quantum (Base64_Solidus);
         elsif Value = Pad then
            return Quantum (Base64_Pad);
         else
            raise Invalid_Character;
         end if;

      end Get_Value;

      -------------------------------------------------------------------------
      --  This function takes the encoded string and returns the
      --  number of characters of padding that are on the end of it.
      function Get_Padding (Input_String : in Encoded_String)
                           return Natural;

      function Get_Padding (Input_String : in Encoded_String)
                           return Natural is

         Pad_Characters : Natural := 0;

      begin -- Get_Padding

         for I in reverse Input_String'Range loop

            exit when Input_String(I) /= '=';

            Pad_Characters := Pad_Characters + 1;

         end loop;

         return Pad_Characters;

      end Get_Padding;

      -------------------------------------------------------------------------
      Input           : Encoded_String   := Cleanse (Input_String,
                                                     Reject_Badness);
      Output          : Unbounded_String := US ("");

      Input_Quantum   : Quantum   := 0;
      Output_Octet    : Octet     := 0;
      Index           : Positive  := 1;
      Padding         : Natural   := Get_Padding (Input_String);

   begin -- Decode

      if Input'Length mod 4 /= 0 then
         raise Invalid_Quoting;
      end if;

      while Index + 3 <= Input'Length loop

         Input_Quantum :=
            Get_Value (Input(Index))     * 2**18 +
            Get_Value (Input(Index + 1)) * 2**12 +
            Get_Value (Input(Index + 2)) * 2**6  +
            Get_Value (Input(Index + 3));

         for I in 1 .. 3 loop
            Output_Octet := Octet
              ( (Input_Quantum and 2#1111_1111# * (2 ** ((3 - I) * 8)))
                / (2 ** ((3-I) * 8)) );
            Output := Output & Character'Val(Output_Octet);
         end loop;

         Index := Index + 4;

      end loop;

      return S (Head (Output, Length (Output) - Padding));

   end Decode;

end Basil.Strings.Base64;
