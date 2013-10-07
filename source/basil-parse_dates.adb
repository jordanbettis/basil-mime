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

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Characters.Latin_1;

with Basil.Utils;              use Basil.Utils;
with Basil.Tokens;             use Basil.Tokens;
with Basil.Automata;           use Basil.Automata;

package body Basil.Parse_Dates is

   --  This parser matches a modification of the EBNF in RFC 2822 p
   --  14. The modifications allow for the inclusion of whitespace and
   --  comments between most tokens. Where changes have been made it
   --  is to relax the rules to better support "best effort" parsing,
   --  and to fold the 'obsolete' tokens into the main definition to
   --  simplify the rules.
   --
   --  This is the modified EBNF matched by this parser.
   --
   --  date-time   := [ CWS day-of-week CWS ',' ] CWS date CWS time
   --  day-of-week := "Mon" / "Tue" / "Wed" / "Thu" / "Fri" / "Sat" / "Sun"
   --  date        := day FWS month FWS year
   --  day         := 1*2DIGIT
   --  month       := "Jan" / "Feb" / "Mar" / "Apr" / "May" / "Jun" /
   --                 "Jul" / "Aug" / "Sep" / "Oct" / "Nov" / "Dec"
   --  year        := ( 4DIGIT / 2DIGIT )
   --  time        := time-of-day CWS zone
   --  time-of-day := hour CWS ":" CWS minute [ CWS ':' CWS Second ]
   --  hour        := 1*2DIGIT
   --  minute      := 1*2DIGIT
   --  second      := 1*2DIGIT
   --  zone        := (( "+" / "-") CWS 4DIGIT) / obs-zone
   --  obs-zone    := "UT" / "GMT" / "EST" / "EDT" / "CST" / "CDT"
   --                        "MST" / "MDT" / "PST" / "PDT" / obs-mil
   --  obs-mil     := 'A' to 'I' / 'K' to 'Z''
   --                 ; Military zones, see RFC 2822 p 32
   --  CWS         := [CFWS] ; comment or folding whitespace as
   --                        ; matched by the 'uninteresting'
   --                        ; automation in Basil.Automata
   --
   --  None of the literals are case sensitive

   ----------------------------------------------------------------------------
   --  Populates the semantics structure with the day of the week, if
   --  avaliable. If no d-o-w is found, it does not remove anything
   --  from the Value.
   procedure Get_Day_Of_Week (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics);

   ----------------------------------------------------------------------------
   --  Populates day, month and year.
   procedure Get_Date        (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics);

   ----------------------------------------------------------------------------
   --  Populates hour, minute, second and zone fields.
   procedure Get_Time        (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics);

   ----------------------------------------------------------------------------
   --  Populates zone-sign and zone
   procedure Get_Zone        (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics);

   ----------------------------------------------------------------------------
   --  Each of these extract the named token from the beginning of
   --  Value and insert it into semantics. Get_Obs_Zone gets either an
   --  obs-zone or an obs-mil.
   procedure Get_Month       (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics);
   procedure Get_Year        (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics);
   procedure Get_Obs_Zone    (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics);

   subtype Two_Digits is Encoded_String (1 .. 2);

   ----------------------------------------------------------------------------
   --  This procedure consumes the 1*2DIGIT construct. If there is
   --  only one contigious digit on the input it fills left with a
   --  zero ('0').
   procedure Get_2Digits     (Value     : in out Unbounded_String;
                              Output    :    out Two_Digits);

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Parse_Date (Header_Value : in Encoded_String)
                       return Date_Semantics is

      Working_Value     : Unbounded_String := EUS (Header_Value);
      Semantics         : Date_Semantics;

   begin -- Parse_Date

      Get_Day_Of_Week (Working_Value, Semantics);

      Automata.Discard_Token (Working_Value, Uninteresting);

      Get_Date (Working_Value, Semantics);

      Automata.Discard_Token (Working_Value, Uninteresting);

      Get_Time (Working_Value, Semantics);

      return Semantics;

   end Parse_Date;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Day_Of_Week (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics) is

      Working_Value : Unbounded_String := Value;
      Day_Of_Week   : Encoded_String (1 .. 3);

   begin -- Get_Day_Of_Week

      Automata.Discard_Token (Working_Value, Uninteresting);

      if Length(Value) < 3 then
         raise Parse_Error;
      end if;

      Day_Of_Week := ES (To_Upper (S (Head (Working_Value, 3))));
      Tail (Working_Value, Length (Working_Value) - 3);

      Automata.Discard_Token (Working_Value, Uninteresting);

      if Head (Working_Value, 1) /= "," then
         return;
      else
         Tail (Working_Value, Length (Working_Value) - 1);
      end if;

      if Day_Of_Week = "MON" or Day_Of_Week = "TUE" or Day_Of_Week = "WED"
        or Day_Of_Week = "THU" or Day_Of_Week = "FRI" or Day_Of_Week = "SAT"
        or Day_Of_Week = "SUN"
      then
         Semantics.Day_Of_Week := Day_Of_Week;
         Semantics.Has_DOW     := True;
         Value                 := Working_Value;
      else
         raise Parse_Error;
      end if;

   end Get_Day_Of_Week;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Date        (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics) is

      Working_Token : Encoded_String (1 .. 2);

   begin -- Get_Date;

      Get_2Digits (Value, Working_Token);
      Semantics.Day := Working_Token;

      Automata.Discard_Token (Value, Uninteresting);

      Get_Month (Value, Semantics);

      Automata.Discard_Token (Value, Uninteresting);

      Get_Year (Value, Semantics);

   end Get_Date;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Time        (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics) is

      Working_Token : Encoded_String (1 .. 2);

   begin -- Get_Time;

      Get_2Digits (Value, Working_Token);
      Semantics.Hour := Working_Token;

      Automata.Discard_Token (Value, Uninteresting);

      if Head (Value, 1) /= ":" then
         raise Parse_Error;
      else
         Tail (Value, Length (Value) - 1);
         Automata.Discard_Token (Value, Uninteresting);
      end if;

      Get_2Digits (Value, Working_Token);
      Semantics.Minute := Working_Token;

      Automata.Discard_Token (Value, Uninteresting);

      if Head (Value, 1) /= ":" then

         Get_Zone (Value, Semantics);

      else

         Tail (Value, Length (Value) - 1);
         Automata.Discard_Token (Value, Uninteresting);

         Get_2Digits (Value, Working_Token);
         Semantics.Second     := Working_Token;
         Semantics.Has_Second := True;

         Automata.Discard_Token (Value, Uninteresting);
         Get_Zone (Value, Semantics);

      end if;

   end Get_Time;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Zone        (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics) is

      Working_Token : Unbounded_String;

   begin -- Get_Zone

      if Head (Value, 1) = "+" or Head (Value, 1) = "-" then

         Semantics.Zone_Sign := ES (Head (Value, 1));
         Tail (Value, Length (Value) - 1);

         Automata.Discard_Token (Value, Uninteresting);

         Get_Token (Source => Value,
                    Token  => Working_Token,
                    Set    => RFC2234_Digits);

         if Length (Working_Token) = 4 then
            Semantics.Zone := ES (Working_Token);
         else
            raise Parse_Error;
         end if;

      else

         Get_Obs_Zone (Value, Semantics);

      end if;

   end Get_Zone;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Month       (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics) is

      Month : Encoded_String (1 .. 3);

   begin -- Get_Month

      if Length(Value) < 3 then
         raise Parse_Error;
      end if;

      Month := ES (To_Upper (S (Head (Value, 3))));
      Value := Tail (Value, Length (Value) - 3);

      if Month = "JAN" or Month = "FEB" or Month = "MAR" or Month = "APR"
        or Month = "MAY" or Month = "JUN" or Month = "JUL" or Month = "AUG"
        or Month = "SEP" or Month = "OCT" or Month = "NOV" or Month = "DEC"
      then

         Semantics.Month := Month;

      else

         raise Parse_Error;

      end if;

   end Get_Month;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Year        (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics) is

      Year : Unbounded_String;

   begin -- Get_Year

      Get_Token (Source => Value,
                 Token  => Year,
                 Set    => RFC2234_Digits);

      if Length (Year) = 2 then

         Semantics.Year     := "  " & ES (Year);
         Semantics.Obs_Year := True;

      elsif Length (Year) = 4 then

         Semantics.Year := ES (Year);

      else

         raise Parse_Error;

      end if;

   end Get_Year;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Obs_Zone    (Value     : in out Unbounded_String;
                              Semantics : in out Date_Semantics) is

      Obs_Zone : Unbounded_String;

   begin -- Get_Obs_Zone

      Get_Token (Source => Value,
                 Token  => Obs_Zone,
                 Set    => RFC2234_Alphas);

      Obs_Zone := US (To_Upper (S (Obs_Zone)));

      if Length (Obs_Zone) = 1 and then
        (('A' <= Element (Obs_Zone, 1)  and Element (Obs_Zone, 1) <= 'I') or
         ('K' <= Element (Obs_Zone, 1)  and Element (Obs_Zone, 1) <= 'Z'))
      then

         Semantics.Zone      := "   " & ES (Obs_Zone);
         Semantics.Zone_Type := Obsolete_Military;

      elsif Length (Obs_Zone) = 2 and then S (Obs_Zone) = "UT" then

         Semantics.Zone      := "  " & ES (Obs_Zone);
         Semantics.Zone_Type := Obsolete_Named;

      elsif Length (Obs_Zone) = 3
        and then (S (Obs_Zone) = "UTF" or S (Obs_Zone) = "GMT"
                  or S (Obs_Zone) = "EST" or S (Obs_Zone) = "EDT"
                  or S (Obs_Zone) = "CST" or S (Obs_Zone) = "CDT"
                  or S (Obs_Zone) = "MST" or S (Obs_Zone) = "MDT"
                  or S (Obs_Zone) = "PST" or S (Obs_Zone) = "PDT")
      then

         Semantics.Zone      := " " & ES (Obs_Zone);
         Semantics.Zone_Type := Obsolete_Named;

      end if;

   end Get_Obs_Zone;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_2Digits     (Value     : in out Unbounded_String;
                              Output    :    out Two_Digits) is

      Working_Token : Unbounded_String;

   begin -- Get_2Digits

      Get_Token (Source => Value,
                 Token  => Working_Token,
                 Set    => RFC2234_Digits);

      if Length (Working_Token) = 1 then

         Output := "0" & ES (Working_Token);

      elsif Length (Working_Token) = 2 then

         Output := ES (Working_Token);

      else

         --  Since none of the calls to this procedure are for optional
         --  parameters if we don't fetch out what we expect it's a parse
         --  error.
         raise Parse_Error;

      end if;

   end Get_2Digits;

end Basil.Parse_Dates;
