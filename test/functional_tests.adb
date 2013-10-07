--
--                 Test Suite for The Basil Library
--
-- This procedure runs the entire test suite.
--

with AUnit.Test_Suites;    use AUnit.Test_Suites;
with AUnit.Test_Runner;
with Test_Headers;
with Test_Strings;
with Test_Content_Types;
with Test_Date_Headers;
with Test_Message_ID_Headers;
with Test_Address_Headers;
with Test_MIME_Entities;
with Test_Messages;

procedure Functional_Tests is

   function Suite return Access_Test_Suite is
      Result : Access_Test_Suite := new Test_Suite;

   begin -- All_Tests

      Add_Test (Result, new Test_Headers.Test_Case);
      Add_Test (Result, new Test_Strings.Test_Case);
      Add_Test (Result, new Test_Content_Types.Test_Case);
      Add_Test (Result, new Test_Date_Headers.Test_Case);
      Add_Test (Result, new Test_Message_ID_Headers.Test_Case);
      Add_Test (Result, new Test_Address_Headers.Test_Case);
      Add_Test (Result, new Test_MIME_Entities.Test_Case);
      Add_Test (Result, new Test_Messages.Test_Case);

      return Result;

   end Suite;

   procedure Run is new Aunit.Test_Runner (Suite);

begin -- Functional_Tests

   Run;

end Functional_Tests;


