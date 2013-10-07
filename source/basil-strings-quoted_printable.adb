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

package body Basil.Strings.Quoted_Printable is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Encode (Input_String     : in String;
                    Quote_Additional : in Character_Set := To_Set ("");
                    Wrap_Long_Lines  : in Boolean := True)
                   return Encoded_String is

      -------------------------------------------------------------------------
      function Encode_Character (Input_Character : Character) return String;

      function Encode_Character (Input_Character : Character) return String is

         Input             : Integer := Character'Pos(Input_Character);
         Encoded_Character : String (1 .. 3);
         Hex_Digits        : constant array (0 .. 15) of Character
           := "0123456789ABCDEF";

      begin

         for I in reverse 2 .. 3 loop
            Encoded_Character (I) := Hex_Digits (Input mod 16);
            Input := Input / 16;
         end loop;

         Encoded_Character (1) := '=';

         return Encoded_Character;

      end Encode_Character;

      -------------------------------------------------------------------------
      package L renames Ada.Characters.Latin_1;

      Output_String    : Unbounded_String := US ("");
      Column_Number    : Positive         := 1;
      Max_Length       : Positive         := 72;
      Soft_Linebreak   : constant String  := '=' & L.CR & L.LF;
      Quote_Characters : Character_Set    :=
        Requires_Quoting or Quote_Additional;

   begin -- Encode_Quoted_Printable

      for I in Input_String'Range loop

         if Is_In (Input_String (I), Quote_Characters) then
            Append (Output_String,  US (Encode_Character (Input_String(I))));
            Column_Number := Column_Number + 3;
         else
            Append (Output_String, Input_String(I));
            Column_Number := Column_Number + 1;
         end if;

         if Wrap_Long_Lines and then Column_Number >= Max_Length then
            Append (Output_String, Soft_Linebreak);
            Column_Number := 1;
         end if;

         --  Substitute soft linebreaks for now encoded hard breaks.
         --  A user who doesn't want long lines wrapped won't expect any
         --  CRLFs in the output.
         if Wrap_Long_Lines and then Input_String (I) = L.LF then
            Append (Output_String, US (Soft_Linebreak));
            Column_Number := 1;
         end if;

         --  Same for CR but not when it's part of a CRLF sequence
         --  (Which will be handled above when processing gets to the LF)
         if Wrap_Long_Lines and then Input_String (I) = L.CR then
            if not (Input_String'Last > I
                    and then Input_String (I + 1) = L.LF)
            then
               Append (Output_String, US (Soft_Linebreak));
               Column_Number := 1;
            end if;
         end if;

      end loop;

      return ES (S (Output_String));

   end Encode;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Decode (Input_String   : in Encoded_String;
                    Reject_Badness : in Boolean := False)
                   return String is

      subtype Hex_Identifier is Encoded_String (1 .. 2);

      -------------------------------------------------------------------------
      -- This takes a two character string representing a two digit hex number
      -- and returns a character whose val is the number. The hex digits can
      -- be capital or lower case.
      function Decode_Character (Input : in Hex_Identifier)
                                return Character;

      function Decode_Character (Input : in Hex_Identifier)
                                return Character is

         Zero_Position : constant := Character'Pos ('0');
         A_Position    : constant := Character'Pos ('A');
         Input_Upper   : String   := To_Upper (S (Input));
         Digit_Value   : Integer  := 0;
         Value         : Integer  := 0;

      begin -- Decode_Character

         for I in Input_Upper'Range loop
            case Input_Upper(I) is
               when '0' .. '9' =>
                  Digit_Value :=
                    Character'Pos(Input_Upper (I)) - Zero_Position;
               when 'A' .. 'F' =>
                  Digit_Value :=
                    Character'Pos(Input_Upper (I)) - A_Position + 10;
               when others =>
                  raise Invalid_Character;
            end case;
            Value := 16 * Value + Digit_Value;
         end loop;

         return Character'Val (Value);

      end Decode_Character;

      -------------------------------------------------------------------------
      --  Determines if the next two characters in the string are
      --  valid hex, will raise Invalid_Character instead of returning
      --  False if Reject_Badness is true.
      function Is_Valid_Hex (Input_String   : in Encoded_String;
                             Index          : in Positive;
                             Reject_Badness : in Boolean)
                            return Boolean;

      function Is_Valid_Hex (Input_String   : in Encoded_String;
                             Index          : in Positive;
                             Reject_Badness : in Boolean)
                            return Boolean is

         -- Hex digits must be capitalized for strict RFC 2045 compliance
         Strict_Hex_Set : constant Character_Set :=
           To_Set ("0123456789ABCDEF");

      begin -- Is_Valid_Hex

         -- '=' must have at least two digits after it
         if Index >= Input_String'Length - 1 then
            if Reject_Badness then
               raise Invalid_Character;
            else
               return False;
            end if;
         end if;


         if Reject_Badness then

            if not Is_In (Input_String (Index + 1), Strict_Hex_Set) or
              not Is_In (Input_String (Index + 2), Strict_Hex_Set)
            then
               raise Invalid_Character;
            end if;

         else

            if not Is_In (Input_String (Index + 1), Hexadecimal_Digit_Set) or
              not Is_In (Input_String (Index + 2), Hexadecimal_Digit_Set)
            then
               return False;
            end if;

         end if;

         return True;

      end Is_Valid_Hex;


      -------------------------------------------------------------------------
      package L renames Ada.Characters.Latin_1;

      Output_String    : Unbounded_String       := US ("");
      I                : Positive               := Input_String'First;

   begin -- Decode_Quoted_Printable

      while I <= Input_String'Last loop

         if Reject_Badness and then
           (Is_In (Input_String (I), Requires_Quoting)
            and Input_String (I) /= '=')
         then
            raise Invalid_Character;
         end if;

         if Input_String (I) = '=' then

            if Input_String (I + 1) = L.CR and Input_String (I + 2) = L.LF then
               I := I + 3;
            elsif Is_Valid_Hex (Input_String, I, Reject_Badness) then
               Append (Output_String,
                       Decode_Character (Input_String (I + 1 .. I + 2)));
               I := I + 3;
            else
               -- Treat it like plain text, we can't get here if
               -- Reject_Badness is true because Is_Valid_Hex will
               -- have excepted.
               Append (Output_String, Input_String(I));
               I := I + 1;
            end if;

         else

            Append (Output_String, Input_String(I));
            I := I + 1;

         end if;

      end loop;

      return S (Output_String);

   end Decode;

end Basil.Strings.Quoted_Printable;
