--
--                 Test Suite for The Basil Library
--
-- Common utilities for the test suite.

with Ada.Characters.Latin_1;

with Basil.Strings;

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

package Tests_Common is

  function US (Source : in String)
              return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  function S (Source : in Ada.Strings.Unbounded.Unbounded_String)
             return string
    renames Ada.Strings.Unbounded.To_String;

  function ES (Source : in String)
              return Basil.Strings.Encoded_String
    renames Basil.Strings.To_Encoded_String;

  function ES (Source : in Unbounded_String)
              return Basil.Strings.Encoded_String
    renames Basil.Strings.To_Encoded_String;

  function S  (Source : in Basil.Strings.Encoded_String)
             return String
    renames Basil.Strings.To_String;

  function EUS (Source : in Basil.Strings.Encoded_String)
             return Unbounded_String
    renames Basil.Strings.To_Unbounded_String;


   ----------------------------------------------------------------------------
   procedure Replace_All (Source_String : in out Unbounded_String;
                          Target        : in     String;
                          Replacement   : in     String);

   ----------------------------------------------------------------------------
   procedure Purge_Newlines (Source : in out Unbounded_String);

   ----------------------------------------------------------------------------
   function Purge_Newlines (Source : in String) return String;

   ----------------------------------------------------------------------------
   --  This fetches the named test entity from ./entities and returns
   --  it as a string.
   function Fetch_Entity (Name : in String) return String;

end Tests_Common;
