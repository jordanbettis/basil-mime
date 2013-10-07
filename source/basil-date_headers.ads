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
-- EXTERNAL DOCUMENTATION:
--
--  MRCY.SPC.002
--
-------------------------------------------------------------------------------
-- PROJECT CONTEXT:
--
--  This package provides functionality to translate internal
--  date-time objects into RFC 2822-compliant date headers, and to
--  parse valid date headers to produce date-time objects.
--
-------------------------------------------------------------------------------

with Ada.Calendar;          use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Basil.Strings;         use Basil.Strings;
with Basil.Headers;         use Basil.Headers;

package Basil.Date_Headers is

   --  A date header object contains a header key and an
   --  Ada.Calendar.Time object representation of an RFC 2822
   --  structured date header value.
   type Date_Header is private;

   ----------------------------------------------------------------------------
   --  Accessor functions for the date header's key and value time object.
   function Get_Key (Source : in Date_Header) return String;
   function Get_Value (Source : in Date_Header) return Ada.Calendar.Time;

   ----------------------------------------------------------------------------
   --  This constructor takes a header key and an ada calendar time
   --  object to create a Date_Header object. To construct a
   --  Date_Header object from a message header, use the Parse
   --  function below.
   function To_Date_Header (Key   : in Encoded_String;
                            Value : in Ada.Calendar.Time) return Date_Header;

   ----------------------------------------------------------------------------
   --  Returns a Date_Header with the value Time object populated by
   --  parsing the structured value of the provided header.
   function Parse (Source : in Header) return Date_Header;

   ----------------------------------------------------------------------------
   --  This version of parse is a convenience function for people not
   --  dealing with headers but who need to translate between Ada and
   --  RFC 2822 dates.
   --
   --  The Source is a string containing a valid RFC 2822 structured
   --  date header value.
   function Parse (Source : in Encoded_String) return Ada.Calendar.Time;

   ----------------------------------------------------------------------------
   --  This function takes a date header object and returns a header
   --  object whose value consists of an RFC 2822 compliant structured
   --  date.
   function Compile (Source : in Date_Header) return Header;

   ----------------------------------------------------------------------------
   --  This version of compile is a convenience function for people
   --  not dealing with headers but who need to translate between Ada and
   --  RFC 2822 dates.
   --
   --  It returns an encoded string containing a valid RFC 2822
   --  structured value, given an Ada date object.
   function Compile (Source : in Ada.Calendar.Time) return Encoded_String;

private

   type Date_Header is
      record
         Key     : Unbounded_String;
         Value   : Ada.Calendar.Time;
      end record;

end Basil.Date_Headers;
