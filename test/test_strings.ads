--
--                 Test Suite for The Basil Library
--
-- Test Cases for Basil.Strings
--                Basil.Strings.Quoted_Printable  and
--                Basil.Strings.Quoted_Printable
--

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with AUnit.Test_Cases;             use AUnit.Test_Cases;

package Test_Strings is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Test_Case);

   function Name (T : Test_Case) return String_Access;

end Test_Strings;
