--
--                 Test Suite for The Basil Library
--
-- Test Cases for date-related functionality.
--

with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Strings.Maps;               use Ada.Strings.Maps;
with Ada.Containers;                 use Ada.Containers;
with Ada.Calendar;                   use Ada.Calendar;
with Ada.Calendar.Formatting;        use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;        use Ada.Calendar.Time_Zones;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;



with AUnit.Test_Cases;               use AUnit.Test_Cases;
with AUnit.Test_Cases.Registration;  use AUnit.Test_Cases.Registration;
with AUnit.Assertions;               use AUnit.Assertions;

with Ada.Text_IO;                    use Ada.Text_IO;

with Tests_Common;                   use Tests_Common;

with Basil.Strings;                  use Basil.Strings;
with Basil.Tokens;
with Basil.Headers;                  use Basil.Headers;
with Basil.Headers.Lists;            use Basil.Headers.Lists;
with Basil.Date_Headers;             use Basil.Date_Headers;

use Basil;

package body Test_Date_Headers is

   procedure Check_Date_Input (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Object : Date_Header;

   begin -- Check_Date_Input

      Object := Parse
        (To_Header ("Date", "1 Jan  71 0:0 z"));
      Assert (Image (Get_Value (Object)) = "1971-01-01 00:00:00",
              "Input test failed for Janurary Zulu");

      Object := Parse
        (To_Header ("Date", "Sun, 29 Feb  04 12 : 01 : 00 CDT  (Test)"));
      Assert (Image (Get_Value (Object)) = "2004-02-29 17:01:00",
              "Input test failed for Feburary CDT");

      Object := Parse
        (To_Header ("Date", "Fri, 4 mAR 2022 12:59:59 CST"));
      Assert (Image (Get_Value (Object)) = "2022-03-04 18:59:59",
              "Input test failed for March CST");

      Object := Parse
        (To_Header ("Date", "1 apr  2099 11():()01 MST"));
      Assert (Image (Get_Value (Object)) = "2099-04-01 18:01:00",
              "Input test failed for April MST");

      Object := Parse
        (To_Header ("Date", "08 MAY 2011 22:01:59 -0912"));
      Assert (Image (Get_Value (Object)) = "2011-05-09 07:13:59",
              "Input test failed for May -0912");

      Object := Parse
        (To_Header ("Date", "8 jUn  00 22:1:3 +1559"));
      Assert (Image (Get_Value (Object)) = "2000-06-08 06:02:03",
              "Input test failed for June +1559");

      Object := Parse
        (To_Header ("Date", "5 JUL  2179 13:13 -0000"));
      Assert (Image (Get_Value (Object)) = "2179-07-05 13:13:00",
              "Input test failed for July -0000");

      Object := Parse
        (To_Header ("Date", "21 aug  1982 04:05:33 EST"));
      Assert (Image (Get_Value (Object)) = "1982-08-21 09:05:33",
              "Input test failed for August EST");

      Object := Parse
        (To_Header ("Date", "01 Sep  2010 1:0 I"));
      Assert (Image (Get_Value (Object)) = "2010-09-01 01:00:00",
              "Input test failed for September Zulu");

      Object := Parse
        (To_Header ("Date", "Thu, 12 Oct  50 23:59:59 PST"));
      Assert (Image (Get_Value (Object)) = "1950-10-13 07:59:59",
              "Input test failed for October PST");

      Object := Parse
        (To_Header ("Date", "30 Nov  2003 13:55 UT"));
      Assert (Image (Get_Value (Object)) = "2003-11-30 13:55:00",
              "Input test failed for November Zulu");

      -- Leap Second
      Object := Parse
        (To_Header ("Date", "29 Dec  2120 05:59:60 +0500"));
      Assert (Image (Get_Value (Object)) = "2120-12-29 00:59:59",
              "Input test failed for December +0500");

   end Check_Date_Input;

   procedure Check_Date_Output
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Test    : Time;
      Compare : Time;

   begin -- Check_Date_Output

      Test := Time_Of (Year        => 2000,
                       Month       => 1,
                       Day         => 30,
                       Hour        => 12,
                       Minute      => 59,
                       Second      => 59,
                       Sub_Second  => 0.0,
                       Time_Zone   => 345);
      Compare := Parse (Compile (Test));
      Assert (Test = Compare, "Test /= Compare with 2000");

      Test := Time_Of (Year        => 2198,
                       Month       => 5,
                       Day         => 12,
                       Hour        => 22,
                       Minute      => 59,
                       Second      => 59,
                       Sub_Second  => 0.0,
                       Time_Zone   => 0);
      Compare := Parse (Compile (Test));
      Assert (Test = Compare, "Test /= Compare with 2198");

      Test := Time_Of (Year        => 1901,
                       Month       => 3,
                       Day         => 30,
                       Hour        => 12,
                       Minute      => 59,
                       Second      => 59,
                       Sub_Second  => 0.0,
                       Time_Zone   => 345);
      Compare := Parse (Compile (Test));
      Assert (Test = Compare, "Test /= Compare with 1901");

      Test := Time_Of (Year        => 2004,
                       Month       => 2,
                       Day         => 29,
                       Hour        => 12,
                       Minute      => 59,
                       Second      => 59,
                       Sub_Second  => 0.0,
                       Time_Zone   => 345);
      Compare := Parse (Compile (Test));
      Assert (Test = Compare, "Test /= Compare with 2004");

      Test := Time_Of (Year        => 1985,
                       Month       => 6,
                       Day         => 30,
                       Hour        => 19,
                       Minute      => 57,
                       Second      => 59,
                       Sub_Second  => 0.0,
                       Leap_Second => True,
                       Time_Zone   => -300);
      Compare := Parse (Compile (Test));
      Assert (Test = Compare, "Test /= Compare with Leap Second");


   end Check_Date_Output;

   ----------------------------------------------------------------------------
   procedure Register_Tests (T : in out Test_Case) is

   begin -- Register_Tests

      Register_Routine (T, Check_Date_Input'Access,
                        "Test Date Headers Parsing");
      Register_Routine (T, Check_Date_Output'Access,
                        "Test Date Headers Serialization");

   end Register_Tests;

   ----------------------------------------------------------------------------
   function Name (T : Test_Case) return String_Access is

   begin -- Name

      return new String'("Test Date Headers");

   end Name;


end Test_Date_Headers;
