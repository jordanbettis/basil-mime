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
with Ada.Calendar.Formatting;  use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;  use Ada.Calendar.Time_Zones;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Strings;              use Ada.Strings;

with Basil.Utils;              use Basil.Utils;
with Basil.Parse_Dates;        use Basil.Parse_Dates;

package body Basil.Date_Headers is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Key (Source : in Date_Header) return String is

   begin -- Get_Key

      return S (Source.Key);

   end Get_Key;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Value (Source : in Date_Header) return Ada.Calendar.Time is

   begin -- Get_Value

      return Source.Value;

   end Get_Value;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_Date_Header (Key   : in Encoded_String;
                            Value : in Ada.Calendar.Time) return Date_Header is

   begin -- To_Date_Header

      return Date_Header'(Key    => EUS (Key),
                          Value  => Value);

   end To_Date_Header;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Parse (Source : in Header) return Date_Header is

   begin -- Parse

      return Date_Header'(Key   => US (Get_Key (Source)),
                          Value => Parse (ES (Get_Value (Source))));

   end Parse;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Parse (Source : in Encoded_String) return Ada.Calendar.Time is

      -------------------------------------------------------------------------
      function Get_Year (Source : in Date_Semantics) return Year_Number;

      function Get_Year (Source : in Date_Semantics) return Year_Number is

         Obs_Year : Natural;

      begin -- Get_Year

         if Source.Obs_Year then

            Obs_Year := Year_Number'Value (S (Source.Year (3 .. 4)));

            -- RFC 2822 p 32
            if Obs_Year < 50 then
               return 2000 + Obs_Year;
            else
               return 1900 + Obs_Year;
            end if;

         else

            return Year_Number'Value (S (Source.Year));

         end if;

      end Get_Year;

      -------------------------------------------------------------------------
      function Get_Month (Source : in Date_Semantics) return Month_Number;

      function Get_Month (Source : in Date_Semantics) return Month_Number is

      begin -- Get_Month

         if Source.Month = "JAN" then
            return 01;
         elsif Source.Month = "FEB" then
            return 02;
         elsif Source.Month = "MAR" then
            return 03;
         elsif Source.Month = "APR" then
            return 04;
         elsif Source.Month = "MAY" then
            return 05;
         elsif Source.Month = "JUN" then
            return 06;
         elsif Source.Month = "JUL" then
            return 07;
         elsif Source.Month = "AUG" then
            return 08;
         elsif Source.Month = "SEP" then
            return 09;
         elsif Source.Month = "OCT" then
            return 10;
         elsif Source.Month = "NOV" then
            return 11;
         elsif Source.Month = "DEC" then
            return 12;
         else
            raise Mea_Culpa;
         end if;

      end Get_Month;

      -------------------------------------------------------------------------
      function Get_Zone (Source : in Date_Semantics) return Time_Offset;

      function Get_Zone (Source : in Date_Semantics) return Time_Offset is

         Numeric_Zone : Integer;
         Hours        : Integer;
         Minutes      : Integer;

      begin -- Get_Zone

         case Source.Zone_Type is
            when Standard_Zone =>

               Numeric_Zone := Integer'Value (S (Source.Zone));

               Hours   := Integer'Value (S (Source.Zone (1 .. 2)));
               Minutes := Integer'Value (S (Source.Zone (3 .. 4)));

               Numeric_Zone := (Hours * 60) + Minutes;

               if Source.Zone_Sign = "+" then
                  return Time_Offset (Numeric_Zone);
               elsif Source.Zone_Sign = "-" then
                  return Time_Offset (0000 - Numeric_Zone);
               else
                  raise Mea_Culpa;
               end if;

            when Obsolete_Named =>

               --  Note that the offsets here are in the Ada/Unix
               --  "number of minutes" format, not in the RFC 2822
               --  HHMM format.
               if Source.Zone = "  UT" or Source.Zone = " UTC"
                 or Source.Zone = " GMT"
               then
                  return +0000;
               elsif Source.Zone = " EDT" then
                  return -0240;
               elsif Source.Zone = " EST" then
                  return -0300;
               elsif Source.Zone = " CDT" then
                  return -0300;
               elsif Source.Zone = " CST" then
                  return -0360;
               elsif Source.Zone = " MDT" then
                  return -0360;
               elsif Source.Zone = " MST" then
                  return -0420;
               elsif Source.Zone = " PDT" then
                  return -0420;
               elsif Source.Zone = " PST" then
                  return -0480;
               else
                  raise Mea_Culpa;
               end if;

            when Obsolete_Military =>

               --  RFC 2822 p 32 describes an interpertation for the
               --  military zones but then says that "they SHOULD all
               --  be considered equivalent to '-0000' unless there is
               --  out-of-band information confirming their meaning."
               return -0000;

         end case;

      end Get_Zone;

      -------------------------------------------------------------------------
      Semantics   : Date_Semantics;

      Day         : Day_Number;
      Month       : Month_Number;
      Year        : Year_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Zone        : Time_Offset;
      Leap_Second : Boolean := False;

   begin -- Parse

      Semantics := Parse_Date (Source);

      Year   := Get_Year (Semantics);
      Month  := Get_Month (Semantics);

      Day    := Day_Number'Value (S (Semantics.Day));
      Hour   := Hour_Number'Value (S (Semantics.Hour));
      Minute := Minute_Number'Value (S (Semantics.Minute));

      if Semantics.Has_Second then
         if Semantics.Second = "60" then
            Second      := 59;
            Leap_Second := True;
         else
            Second := Second_Number'Value (S (Semantics.Second));
         end if;
      else
         Second := 00;
      end if;

      Zone := Get_Zone (Semantics);

      return Time_Of (Year        => Year,
                      Month       => Month,
                      Day         => Day,
                      Hour        => Hour,
                      Minute      => Minute,
                      Second      => Second,
                      Time_Zone   => Zone,
                      Leap_Second => Leap_Second);

   end Parse;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Compile (Source : in Date_Header) return Header is

   begin -- Compile

      return To_Header (ES (Source.Key), Compile (Source.Value));

   end Compile;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Compile (Source : in Ada.Calendar.Time) return Encoded_String is

      -------------------------------------------------------------------------
      --  Right-fills a number with zeros to the specified width.
      function Fill (Number : in Natural;
                     Width  : in Positive) return String;

      function Fill (Number : in Natural;
                     Width  : in Positive) return String is

         Image       : String  := Trim (Integer'Image (Number), Both);
         Fill_Length : Integer := Width - Image'Length;

      begin -- Fill

         if Fill_Length > 0 then
            return (Fill_Length * '0') & Image;
         else
            return Image;
         end if;

      end Fill;

      -------------------------------------------------------------------------
      function Get_Day_Of_Week (Source : in Ada.Calendar.Time) return String;

      function Get_Day_Of_Week (Source : in Ada.Calendar.Time) return String is

      begin -- Get_Day_Of_Week

         case Day_Of_Week (Source) is
            when Monday =>
               return "Mon";
            when Tuesday =>
               return "Tue";
            when Wednesday =>
               return "Wed";
            when Thursday =>
               return "Thu";
            when Friday =>
               return "Fri";
            when Saturday =>
               return "Sat";
            when Sunday =>
               return "Sun";
         end case;

      end Get_Day_Of_Week;

      -------------------------------------------------------------------------
      function Get_Month (Value : Month_Number) return String;

      function Get_Month (Value : Month_Number) return String is

      begin -- Get_Month

         case Value is
            when 01 =>
               return "Jan";
            when 02 =>
               return "Feb";
            when 03 =>
               return "Mar";
            when 04 =>
               return "Apr";
            when 05 =>
               return "May";
            when 06 =>
               return "Jun";
            when 07 =>
               return "Jul";
            when 08 =>
               return "Aug";
            when 09 =>
               return "Sep";
            when 10 =>
               return "Oct";
            when 11 =>
               return "Nov";
            when 12 =>
               return "Dec";
         end case;

      end Get_Month;

      -------------------------------------------------------------------------
      function Get_Zone (Value : Time_Offset) return String;

      function Get_Zone (Value : Time_Offset) return String is

         Zone_Image : String (1 .. 4) := "0000";
         Hours      : Integer;
         Minutes    : Integer;

      begin -- Get_Zone

         Hours   := abs (Integer (Value)) / 60;
         Minutes := abs (Integer (Value)) mod 60;

         Zone_Image := Fill (Hours, 2) & Fill (Minutes, 2);

        if Value < 0000 then
           return "-" & Zone_Image;
        else
           return "+" & Zone_Image;
        end if;

      end Get_Zone;

      -------------------------------------------------------------------------
      Day         : Day_Number;
      Month       : Month_Number;
      Year        : Year_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration;
      Zone        : Time_Offset := UTC_Time_Offset (Source);
      Leap_Second : Boolean;

      MIME_Second : Integer;

   begin -- Compile

      Split (Date        => Source,
             Year        => Year,
             Month       => Month,
             Day         => Day,
             Hour        => Hour,
             Minute      => Minute,
             Second      => Second,
             Sub_Second  => Sub_Second,
             Time_Zone   => Zone,
             Leap_Second => Leap_Second);

      if Leap_Second = True then
         MIME_Second := 60;
      else
         MIME_Second := Integer (Second);
      end if;

      return
        ES (Get_Day_Of_Week (Source) & ", "
            & Fill (Day, 2)  & " " & Get_Month (Month) & " " & Fill (Year, 4)
            & " "
            & Fill (Hour, 2) & ":" & Fill (Minute, 2)  & ":"
            & Fill (MIME_Second, 2)
            & " "
            & Get_Zone (Zone));

   end Compile;

end Basil.Date_Headers;
