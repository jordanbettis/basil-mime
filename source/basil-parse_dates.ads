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

-------------------------------------------------------------------------------
-- PROJECT CONTEXT:
--
--  This package implements a parser for RFC 2822 Date headers. For an
--  BNF of the language matched, please see the body of this package.
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with Basil.Strings;            use Basil.Strings;

private package Basil.Parse_Dates is

   --  This enumeration allows the parser to communicate with the user
   --  of the semantics the type of date that was found in the string,
   --  to avoid duplication of effort.
   type Zone_Designation_Type is
     (Standard_Zone,
      Obsolete_Named,
      Obsolete_Military);

   --  This type holds the semantically significant parts of an RFC
   --  2822 date. Since the standards regarding the names are rigid we
   --  can use standard strings.  'X's are not valid characters for
   --  any of the fields so if the output contains such then there was
   --  no information for that field in the parsed string.
   --
   --  Days of the week and months are stored in uppercase regardless
   --  of how they appeared in the string, so the valid values for
   --  Day_Of_Week are "MON" "TUE" "WED" "THU" "FRI" "SAT" and "SUN".
   --
   --  Obsolete two digit years have spaces placed in front, so they
   --  will be " YY", and Obs_Year will be set to "True". Obsolete
   --  timezone names are stored with a proceeding space(s) " GMT"
   --  "  UT", the Zone_Sign is left as an "X", and Zone_Type is set
   --  as needed.
   type Date_Semantics is
      record
         Day_Of_Week : Encoded_String (1 .. 3) := "XXX";
         Has_DOW     : Boolean                 := False;
         Day         : Encoded_String (1 .. 2) := "XX";
         Month       : Encoded_String (1 .. 3) := "XXX";
         Year        : Encoded_String (1 .. 4) := "XXXX";
         Obs_Year    : Boolean                 := False;
         Hour        : Encoded_String (1 .. 2) := "XX";
         Minute      : Encoded_String (1 .. 2) := "XX";
         Second      : Encoded_String (1 .. 2) := "XX";
         Has_Second  : Boolean                 := False;
         Zone_Sign   : Encoded_String (1 .. 1) := "X";
         Zone        : Encoded_String (1 .. 4) := "XXXX";
         Zone_Type   : Zone_Designation_Type   := Standard_Zone;
      end record;

   --------------------------------------------------------------------------
   --  This function takes the value of a date header field and
   --  returns a Date_Semantics object with as many fields populated
   --  as possible.
   function Parse_Date (Header_Value : in Encoded_String)
                       return Date_Semantics;

end Basil.Parse_Dates;
