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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Basil.Utils;             use Basil.Utils;

package body Basil.Character_Set_Translation is

   -- The Character Sets array is declared in the body of this package.
   type Set_Name_List is array (Positive range <>) of Unbounded_String;
   type Set_Name_Pointer is access Set_Name_List;

   type Character_Set_Name_Array is array
     (Character_Set_ID range Character_Set_ID'First .. Character_Set_ID'Last)
     of Set_Name_Pointer;

   Character_Sets : Character_Set_Name_Array;

   ----------------------------------------------------------------------------
   --  This procedure searches one level of the arrays of character
   --  set identifiers for the Comparison_String (to allow
   --  breadth-first searching of the array, see Get_Identifier).
   --
   --  If Current_Level is One, then Deepest_Level is used to discover
   --  how far down the arrays go. It is ONLY set when Current_Level =
   --  1. If a match is found then Identifier is set to the
   --  Character_Set_ID for the match.
   procedure Search_Level (Current_Level     : in     Positive;
                           Comparison_String : in     String := "";
                           Deepest_Level     : in out Positive;
                           Identifier        :    out Character_Set_ID);

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Label (Character_Set_Identifier : Character_Set_ID)
                      return String is

   begin -- Get_Character_Set_Label

      -- In every case there is at least one label for each character
      -- set and the first label is the MIME preferred.
      return S (Character_Sets(Character_Set_Identifier)(1));

   end Get_Label;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --  This function performs a breadth-first traversal of the array of
   --  identifiers. This is an attept to make it a bit more efficient
   --  as it's more likely that the identifier "ISO-8859-1" would be
   --  found, for instance, than "IBM376" (a deep alias for US-ASCII).
   function Get_Identifier (Character_Set_Label : String)
                           return Character_Set_ID is

      --  Current level is what "level" of the arrays we're on (doing
      --  a breadth-first traversal). Deepest_Level is how deep the
      --  deepest identifier array is. It is discovered the first time
      --  Search_Level is run, and set initially to one.
      Current_Level     : Positive         := 1;
      Deepest_Level     : Positive         := 1;
      Identifier        : Character_Set_ID := CS_Unknown;
      -- All labels in MIME are case insensitive.
      Comparison_String : String := To_Lower (Character_Set_Label);

   begin -- Get_Identifier

      while Current_Level <= Deepest_Level and Identifier = CS_Unknown loop

         Search_Level (Current_Level,
                       Comparison_String,
                       Deepest_Level,
                       Identifier);

         Current_Level := Current_Level + 1;

      end loop;

      return Identifier;

   end Get_Identifier;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Search_Level (Current_Level     : in     Positive;
                           Comparison_String : in     String := "";
                           Deepest_Level     : in out Positive;
                           Identifier        :    out Character_Set_ID) is

   begin -- Search_Level

      Identifier := CS_Unknown;

      for I in Character_Set_ID'First .. Character_Set_ID'Last loop

         if Current_Level = 1 and Character_Sets (I)'Last > Deepest_Level then
            Deepest_Level := Character_Sets (I)'Last;
         end if;

         if Current_Level <= Character_Sets (I)'Last and then
           To_Lower (S
                     (Character_Sets (I) (Current_Level))) = Comparison_String
         then
            Identifier := I;
            exit; -- We've found it, exit the loop.
         end if;

      end loop;

   end Search_Level;

begin -- package Basil.MIME.Content_Types.Character_Set_Translation

   --  This populates the array of content type identifiers. I have on
   --  some authority that a destructor is not needed since this
   --  non-generic package will only be created when the program
   --  begins and destroyed when the program ends, so this will be in
   --  memory the whole time but it will never be leaked.
   Character_Sets (CS_US_ASCII) := new Set_Name_List (1 .. 11);
   Character_Sets (CS_US_ASCII) (1)  := US ("US-ASCII");
   Character_Sets (CS_US_ASCII) (2)  := US ("ANSI_X3.4-1968");
   Character_Sets (CS_US_ASCII) (3)  := US ("iso-ir-6");
   Character_Sets (CS_US_ASCII) (4)  := US ("ANSI_X3.4-1986");
   Character_Sets (CS_US_ASCII) (5)  := US ("ISO_646.irv:1991");
   Character_Sets (CS_US_ASCII) (6)  := US ("ASCII");
   Character_Sets (CS_US_ASCII) (7)  := US ("ISO646-US");
   Character_Sets (CS_US_ASCII) (8)  := US ("us");
   Character_Sets (CS_US_ASCII) (9)  := US ("IBM367");
   Character_Sets (CS_US_ASCII) (10) := US ("cp367");
   Character_Sets (CS_US_ASCII) (11) := US ("csASCII");

   Character_Sets (CS_ISO_8859_1) := new Set_Name_List (1 .. 9);
   Character_Sets (CS_ISO_8859_1) (1) := US ("ISO-8859-1");
   Character_Sets (CS_ISO_8859_1) (2) := US ("ISO_8859-1:1987");
   Character_Sets (CS_ISO_8859_1) (3) := US ("ISO_8859-1");
   Character_Sets (CS_ISO_8859_1) (4) := US ("iso-ir-100");
   Character_Sets (CS_ISO_8859_1) (5) := US ("latin1");
   Character_Sets (CS_ISO_8859_1) (6) := US ("l1");
   Character_Sets (CS_ISO_8859_1) (7) := US ("IBM819");
   Character_Sets (CS_ISO_8859_1) (8) := US ("CP819");
   Character_Sets (CS_ISO_8859_1) (9) := US ("csISOLatin1");

   Character_Sets (CS_ISO_8859_2) := new Set_Name_List (1 .. 7);
   Character_Sets (CS_ISO_8859_2) (1) := US ("ISO-8859-2");
   Character_Sets (CS_ISO_8859_2) (2) := US ("ISO_8859-2:1987");
   Character_Sets (CS_ISO_8859_2) (3) := US ("iso-ir-101");
   Character_Sets (CS_ISO_8859_2) (4) := US ("ISO_8859-2");
   Character_Sets (CS_ISO_8859_2) (5) := US ("latin2");
   Character_Sets (CS_ISO_8859_2) (6) := US ("l2");
   Character_Sets (CS_ISO_8859_2) (7) := US ("csISOLatin2");

   Character_Sets (CS_ISO_8859_3) := new Set_Name_List (1 .. 7);
   Character_Sets (CS_ISO_8859_3) (1) := US ("ISO-8859-3");
   Character_Sets (CS_ISO_8859_3) (2) := US ("ISO_8859-3:1988");
   Character_Sets (CS_ISO_8859_3) (3) := US ("iso-ir-109");
   Character_Sets (CS_ISO_8859_3) (4) := US ("ISO_8859-3");
   Character_Sets (CS_ISO_8859_3) (5) := US ("latin3");
   Character_Sets (CS_ISO_8859_3) (6) := US ("l3");
   Character_Sets (CS_ISO_8859_3) (7) := US ("csISOLatin3");

   Character_Sets (CS_ISO_8859_4) := new Set_Name_List (1 .. 7);
   Character_Sets (CS_ISO_8859_4) (1) := US ("ISO-8859-4");
   Character_Sets (CS_ISO_8859_4) (2) := US ("ISO_8859-4:1988");
   Character_Sets (CS_ISO_8859_4) (3) := US ("iso-ir-110");
   Character_Sets (CS_ISO_8859_4) (4) := US ("ISO_8859-4");
   Character_Sets (CS_ISO_8859_4) (5) := US ("latin4");
   Character_Sets (CS_ISO_8859_4) (6) := US ("l4");
   Character_Sets (CS_ISO_8859_4) (7) := US ("csISOLatin4");

   Character_Sets (CS_ISO_8859_5) := new Set_Name_List (1 .. 6);
   Character_Sets (CS_ISO_8859_5) (1) := US ("ISO-8859-5");
   Character_Sets (CS_ISO_8859_5) (2) := US ("ISO_8859-5:1988");
   Character_Sets (CS_ISO_8859_5) (3) := US ("iso-ir-144");
   Character_Sets (CS_ISO_8859_5) (4) := US ("ISO_8859-5");
   Character_Sets (CS_ISO_8859_5) (5) := US ("cyrillic");
   Character_Sets (CS_ISO_8859_5) (6) := US ("csISOLatinCyrillic");

   Character_Sets (CS_ISO_8859_6) := new Set_Name_List (1 .. 8);
   Character_Sets (CS_ISO_8859_6) (1) := US ("ISO-8859-6");
   Character_Sets (CS_ISO_8859_6) (2) := US ("ISO_8859-6:1987");
   Character_Sets (CS_ISO_8859_6) (3) := US ("iso-ir-127");
   Character_Sets (CS_ISO_8859_6) (4) := US ("ISO_8859-6");
   Character_Sets (CS_ISO_8859_6) (5) := US ("ECMA-114");
   Character_Sets (CS_ISO_8859_6) (6) := US ("ASMO-708");
   Character_Sets (CS_ISO_8859_6) (7) := US ("arabic");
   Character_Sets (CS_ISO_8859_6) (8) := US ("csISOLatinArabic");

   Character_Sets (CS_ISO_8859_7) := new Set_Name_List (1 .. 9);
   Character_Sets (CS_ISO_8859_7) (1) := US ("ISO-8859-7");
   Character_Sets (CS_ISO_8859_7) (2) := US ("ISO_8859-7:1987");
   Character_Sets (CS_ISO_8859_7) (3) := US ("iso-ir-126");
   Character_Sets (CS_ISO_8859_7) (4) := US ("ISO_8859-7");
   Character_Sets (CS_ISO_8859_7) (5) := US ("ELOT_928");
   Character_Sets (CS_ISO_8859_7) (6) := US ("ECMA-118");
   Character_Sets (CS_ISO_8859_7) (7) := US ("greek");
   Character_Sets (CS_ISO_8859_7) (8) := US ("greek8");
   Character_Sets (CS_ISO_8859_7) (9) := US ("csISOLatinGreek");

   Character_Sets (CS_ISO_8859_8) := new Set_Name_List (1 .. 6);
   Character_Sets (CS_ISO_8859_8) (1) := US ("ISO-8859-8");
   Character_Sets (CS_ISO_8859_8) (2) := US ("ISO_8859-8:1988");
   Character_Sets (CS_ISO_8859_8) (3) := US ("iso-ir-138");
   Character_Sets (CS_ISO_8859_8) (4) := US ("ISO_8859-8");
   Character_Sets (CS_ISO_8859_8) (5) := US ("hebrew");
   Character_Sets (CS_ISO_8859_8) (6) := US ("csISOLatinHebrew");

   Character_Sets (CS_ISO_8859_9) := new Set_Name_List (1 .. 8);
   Character_Sets (CS_ISO_8859_9) (1) := US ("ISO-8859-9");
   Character_Sets (CS_ISO_8859_9) (2) := US ("ISO_8859-9:1989");
   Character_Sets (CS_ISO_8859_9) (3) := US ("iso-ir-148");
   Character_Sets (CS_ISO_8859_9) (4) := US ("ISO_8859-9");
   Character_Sets (CS_ISO_8859_9) (5) := US ("latin5");
   Character_Sets (CS_ISO_8859_9) (6) := US ("l5");
   Character_Sets (CS_ISO_8859_9) (7) := US ("csISOLatin5");
   Character_Sets (CS_ISO_8859_9) (8) := US ("csISOLatin5");

   Character_Sets (CS_ISO_8859_10) := new Set_Name_List (1 .. 6);
   Character_Sets (CS_ISO_8859_10) (1) := US ("ISO-8859-10");
   Character_Sets (CS_ISO_8859_10) (2) := US ("iso-ir-157");
   Character_Sets (CS_ISO_8859_10) (3) := US ("ISO_8859-10:1992");
   Character_Sets (CS_ISO_8859_10) (4) := US ("csISOLatin6");
   Character_Sets (CS_ISO_8859_10) (5) := US ("latin6");
   Character_Sets (CS_ISO_8859_10) (6) := US ("l6");

   Character_Sets (CS_ISO_6937_2_Add) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_ISO_6937_2_Add) (1) := US ("ISO_6937-2-add");
   Character_Sets (CS_ISO_6937_2_Add) (2) := US ("iso-ir-142");
   Character_Sets (CS_ISO_6937_2_Add) (3) := US ("csISOTextComm");

   Character_Sets (CS_Jis_X0201) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_Jis_X0201) (1) := US ("JIS_X0201");
   Character_Sets (CS_Jis_X0201) (2) := US ("X0201");
   Character_Sets (CS_Jis_X0201) (3) := US ("csHalfWidthKatakana");

   Character_Sets (CS_Jis_Encoding) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Jis_Encoding) (1) := US ("JIS_Encoding");
   Character_Sets (CS_Jis_Encoding) (2) := US ("csJISEncoding");

   Character_Sets (CS_Shift_Jis) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_Shift_Jis) (1) := US ("Shift_JIS");
   Character_Sets (CS_Shift_Jis) (2) := US ("MS_Kanji");
   Character_Sets (CS_Shift_Jis) (3) := US ("csShiftJIS");

   Character_Sets (CS_Euc_Jp) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_Euc_Jp) (1) := US ("EUC-JP");
   Character_Sets (CS_Euc_Jp) (2)
     := US ("Extended_UNIX_Code_Packed_Format_for_Japanese");
   Character_Sets (CS_Euc_Jp) (3) := US ("csEUCPkdFmtJapanese");

   Character_Sets (CS_EUCFixWidJapanese) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EUCFixWidJapanese) (1)
     := US ("Extended_UNIX_Code_Fixed_Width_for_Japanese");
   Character_Sets (CS_EUCFixWidJapanese) (2) := US ("csEUCFixWidJapanese");

   Character_Sets (CS_Bs_4730) := new Set_Name_List (1 .. 6);
   Character_Sets (CS_Bs_4730) (1) := US ("BS_4730");
   Character_Sets (CS_Bs_4730) (2) := US ("iso-ir-4");
   Character_Sets (CS_Bs_4730) (3) := US ("ISO646-GB");
   Character_Sets (CS_Bs_4730) (4) := US ("gb");
   Character_Sets (CS_Bs_4730) (5) := US ("uk");
   Character_Sets (CS_Bs_4730) (6) := US ("csISO4UnitedKingdom");

   Character_Sets (CS_SEN_850200_C) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_SEN_850200_C) (1) := US ("SEN_850200_C");
   Character_Sets (CS_SEN_850200_C) (2) := US ("iso-ir-11");
   Character_Sets (CS_SEN_850200_C) (3) := US ("ISO646-SE2");
   Character_Sets (CS_SEN_850200_C) (4) := US ("se2");
   Character_Sets (CS_SEN_850200_C) (5) := US ("csISO11SwedishForNames");

   Character_Sets (CS_IT) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IT) (1) := US ("IT");
   Character_Sets (CS_IT) (2) := US ("iso-ir-15");
   Character_Sets (CS_IT) (3) := US ("ISO646-IT");
   Character_Sets (CS_IT) (4) := US ("csISO15Italian");

   Character_Sets (CS_Es) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_Es) (1) := US ("ES");
   Character_Sets (CS_Es) (2) := US ("iso-ir-17");
   Character_Sets (CS_Es) (3) := US ("ISO646-ES");
   Character_Sets (CS_Es) (4) := US ("csISO17Spanish");

   Character_Sets (CS_DIN_66003) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_DIN_66003) (1) := US ("DIN_66003");
   Character_Sets (CS_DIN_66003) (2) := US ("iso-ir-21");
   Character_Sets (CS_DIN_66003) (3) := US ("de");
   Character_Sets (CS_DIN_66003) (4) := US ("ISO646-DE");
   Character_Sets (CS_DIN_66003) (5) := US ("csISO21German");

   Character_Sets (CS_NS_4551_1) := new Set_Name_List (1 .. 6);
   Character_Sets (CS_NS_4551_1) (1) := US ("NS_4551-1");
   Character_Sets (CS_NS_4551_1) (2) := US ("iso-ir-60");
   Character_Sets (CS_NS_4551_1) (3) := US ("ISO646-NO");
   Character_Sets (CS_NS_4551_1) (4) := US ("no");
   Character_Sets (CS_NS_4551_1) (5) := US ("csISO60DanishNorwegian");
   Character_Sets (CS_NS_4551_1) (6) := US ("csISO60Norwegian1");

   Character_Sets (CS_NF_Z_62_010) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_NF_Z_62_010) (1) := US ("NF_Z_62-010");
   Character_Sets (CS_NF_Z_62_010) (2) := US ("iso-ir-69");
   Character_Sets (CS_NF_Z_62_010) (3) := US ("ISO646-FR");
   Character_Sets (CS_NF_Z_62_010) (4) := US ("fr");
   Character_Sets (CS_NF_Z_62_010) (5) := US ("csISO69French");

   Character_Sets (CS_ISO_10646_UTF_1) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_10646_UTF_1) (1) := US ("ISO-10646-UTF-1");
   Character_Sets (CS_ISO_10646_UTF_1) (2) := US ("csISO10646UTF1");

   Character_Sets (CS_ISO_646_Basic) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_ISO_646_Basic) (1) := US ("ISO_646.basic:1983");
   Character_Sets (CS_ISO_646_Basic) (2) := US ("ref");
   Character_Sets (CS_ISO_646_Basic) (3) := US ("csISO646basic1983");

   Character_Sets (CS_INVARIANT) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_INVARIANT) (1) := US ("INVARIANT");
   Character_Sets (CS_INVARIANT) (2) := US ("csINVARIANT");

   Character_Sets (CS_ISO_646_Irv) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_ISO_646_Irv) (1) := US ("ISO_646.irv:1983");
   Character_Sets (CS_ISO_646_Irv) (2) := US ("iso-ir-2");
   Character_Sets (CS_ISO_646_Irv) (3) := US ("irv");
   Character_Sets (CS_ISO_646_Irv) (4) := US ("csISO2IntlRefVersion");

   Character_Sets (CS_NATS_SEFI) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_NATS_SEFI) (1) := US ("NATS-SEFI");
   Character_Sets (CS_NATS_SEFI) (2) := US ("iso-ir-8-1");
   Character_Sets (CS_NATS_SEFI) (3) := US ("csNATSSEFI");

   Character_Sets (CS_NATS_SEFI_ADD) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_NATS_SEFI_ADD) (1) := US ("NATS-SEFI-ADD");
   Character_Sets (CS_NATS_SEFI_ADD) (2) := US ("iso-ir-8-2");
   Character_Sets (CS_NATS_SEFI_ADD) (3) := US ("csNATSSEFIADD");

   Character_Sets (CS_NATS_DANO) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_NATS_DANO) (1) := US ("NATS-DANO");
   Character_Sets (CS_NATS_DANO) (2) := US ("iso-ir-9-1");
   Character_Sets (CS_NATS_DANO) (3) := US ("csNATSDANO");

   Character_Sets (CS_NATS_DANO_ADD) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_NATS_DANO_ADD) (1) := US ("NATS-DANO-ADD");
   Character_Sets (CS_NATS_DANO_ADD) (2) := US ("iso-ir-9-2");
   Character_Sets (CS_NATS_DANO_ADD) (3) := US ("csNATSDANOADD");

   Character_Sets (CS_SEN_850200_B) := new Set_Name_List (1 .. 7);
   Character_Sets (CS_SEN_850200_B) (1) := US ("SEN_850200_B");
   Character_Sets (CS_SEN_850200_B) (2) := US ("iso-ir-10");
   Character_Sets (CS_SEN_850200_B) (3) := US ("FI");
   Character_Sets (CS_SEN_850200_B) (4) := US ("ISO646-FI");
   Character_Sets (CS_SEN_850200_B) (5) := US ("ISO646-SE");
   Character_Sets (CS_SEN_850200_B) (6) := US ("se");
   Character_Sets (CS_SEN_850200_B) (7) := US ("csISO10Swedish");

   Character_Sets (CS_KS_C_5601) := new Set_Name_List (1 .. 6);
   Character_Sets (CS_KS_C_5601) (1) := US ("KS_C_5601-1987");
   Character_Sets (CS_KS_C_5601) (2) := US ("iso-ir-149");
   Character_Sets (CS_KS_C_5601) (3) := US ("KS_C_5601-1989");
   Character_Sets (CS_KS_C_5601) (4) := US ("KSC_5601");
   Character_Sets (CS_KS_C_5601) (5) := US ("korean");
   Character_Sets (CS_KS_C_5601) (6) := US ("csKSC56011987");

   Character_Sets (CS_ISO_2022_KR) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_2022_KR) (1) := US ("ISO-2022-KR");
   Character_Sets (CS_ISO_2022_KR) (2) := US ("csISO2022KR");

   Character_Sets (CS_EUC_KR) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EUC_KR) (1) := US ("EUC-KR");
   Character_Sets (CS_EUC_KR) (2) := US ("csEUCKR");

   Character_Sets (CS_ISO_2022_JP) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_2022_JP) (1) := US ("ISO-2022-JP");
   Character_Sets (CS_ISO_2022_JP) (2) := US ("csISO2022JP");

   Character_Sets (CS_ISO_2022_JP_2) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_2022_JP_2) (1) := US ("ISO-2022-JP-2");
   Character_Sets (CS_ISO_2022_JP_2) (2) := US ("csISO2022JP2");

   Character_Sets (CS_JIS_C6220_1969_Jp) := new Set_Name_List (1 .. 6);
   Character_Sets (CS_JIS_C6220_1969_Jp) (1) := US ("JIS_C6220-1969-jp");
   Character_Sets (CS_JIS_C6220_1969_Jp) (2) := US ("JIS_C6220-1969");
   Character_Sets (CS_JIS_C6220_1969_Jp) (3) := US ("iso-ir-13");
   Character_Sets (CS_JIS_C6220_1969_Jp) (4) := US ("katakana");
   Character_Sets (CS_JIS_C6220_1969_Jp) (5) := US ("x0201-7");
   Character_Sets (CS_JIS_C6220_1969_Jp) (6) := US ("csISO13JISC6220jp");

   Character_Sets (CS_JIS_C6220_1969_Ro) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_JIS_C6220_1969_Ro) (1) := US ("JIS_C6220-1969-ro");
   Character_Sets (CS_JIS_C6220_1969_Ro) (2) := US ("iso-ir-14");
   Character_Sets (CS_JIS_C6220_1969_Ro) (3) := US ("jp");
   Character_Sets (CS_JIS_C6220_1969_Ro) (4) := US ("ISO646-JP");
   Character_Sets (CS_JIS_C6220_1969_Ro) (5) := US ("csISO14JISC6220ro");

   Character_Sets (CS_PT) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_PT) (1) := US ("PT");
   Character_Sets (CS_PT) (2) := US ("iso-ir-16");
   Character_Sets (CS_PT) (3) := US ("ISO646-PT");
   Character_Sets (CS_PT) (4) := US ("csISO16Portuguese");

   Character_Sets (CS_Greek7_Old) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_Greek7_Old) (1) := US ("greek7-old");
   Character_Sets (CS_Greek7_Old) (2) := US ("iso-ir-18");
   Character_Sets (CS_Greek7_Old) (3) := US ("csISO18Greek7Old");

   Character_Sets (CS_Latin_Greek) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_Latin_Greek) (1) := US ("Latin-greek");
   Character_Sets (CS_Latin_Greek) (2) := US ("iso-ir-19");
   Character_Sets (CS_Latin_Greek) (3) := US ("csISO19LatinGreek");

   Character_Sets (CS_NF_Z_62_010_1973) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_NF_Z_62_010_1973) (1) := US ("NF_Z_62-010_(1973)");
   Character_Sets (CS_NF_Z_62_010_1973) (2) := US ("iso-ir-25");
   Character_Sets (CS_NF_Z_62_010_1973) (3) := US ("ISO646-FR1");
   Character_Sets (CS_NF_Z_62_010_1973) (4) := US ("csISO25French");

   Character_Sets (CS_Latin_Greek_1) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_Latin_Greek_1) (1) := US ("Latin-greek-1");
   Character_Sets (CS_Latin_Greek_1) (2) := US ("iso-ir-27");
   Character_Sets (CS_Latin_Greek_1) (3) := US ("csISO27LatinGreek1");

   Character_Sets (CS_ISO_5427) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_ISO_5427) (1) := US ("ISO_5427");
   Character_Sets (CS_ISO_5427) (2) := US ("iso-ir-37");
   Character_Sets (CS_ISO_5427) (3) := US ("csISO5427Cyrillic");

   Character_Sets (CS_JIS_C6226_1978) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_JIS_C6226_1978) (1) := US ("JIS_C6226-1978");
   Character_Sets (CS_JIS_C6226_1978) (2) := US ("iso-ir-42");
   Character_Sets (CS_JIS_C6226_1978) (3) := US ("csISO42JISC62261978");

   Character_Sets (CS_BS_Viewdata) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_BS_Viewdata) (1) := US ("BS_Viewdata");
   Character_Sets (CS_BS_Viewdata) (2) := US ("iso-ir-47");
   Character_Sets (CS_BS_Viewdata) (3) := US ("csISO47BSViewdata");

   Character_Sets (CS_INIS) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_INIS) (1) := US ("INIS");
   Character_Sets (CS_INIS) (2) := US ("iso-ir-49");
   Character_Sets (CS_INIS) (3) := US ("csISO49INIS");

   Character_Sets (CS_INIS_8) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_INIS_8) (1) := US ("INIS-8");
   Character_Sets (CS_INIS_8) (2) := US ("iso-ir-50");
   Character_Sets (CS_INIS_8) (3) := US ("csISO50INIS8");

   Character_Sets (CS_INIS_Cyrillic) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_INIS_Cyrillic) (1) := US ("INIS-Cyrillic");
   Character_Sets (CS_INIS_Cyrillic) (2) := US ("iso-ir-51");
   Character_Sets (CS_INIS_Cyrillic) (3) := US ("csISO51INISCyrillic");

   Character_Sets (CS_ISO_5427_1981) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_ISO_5427_1981) (1) := US ("ISO_5427:1981");
   Character_Sets (CS_ISO_5427_1981) (2) := US ("iso-ir-54");
   Character_Sets (CS_ISO_5427_1981) (3) := US ("ISO5427Cyrillic1981");

   Character_Sets (CS_ISO_5428_1980) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_ISO_5428_1980) (1) := US ("ISO_5428:1980");
   Character_Sets (CS_ISO_5428_1980) (2) := US ("iso-ir-55");
   Character_Sets (CS_ISO_5428_1980) (3) := US ("csISO5428Greek");

   Character_Sets (CS_GB_1988_80) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_GB_1988_80) (1) := US ("GB_1988-80");
   Character_Sets (CS_GB_1988_80) (2) := US ("iso-ir-57");
   Character_Sets (CS_GB_1988_80) (3) := US ("cn");
   Character_Sets (CS_GB_1988_80) (4) := US ("ISO646-CN");
   Character_Sets (CS_GB_1988_80) (5) := US ("csISO57GB1988");

   Character_Sets (CS_GB_2312_80) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_GB_2312_80) (1) := US ("GB_2312-80");
   Character_Sets (CS_GB_2312_80) (2) := US ("iso-ir-58");
   Character_Sets (CS_GB_2312_80) (3) := US ("chinese");
   Character_Sets (CS_GB_2312_80) (4) := US ("csISO58GB231280");

   Character_Sets (CS_NS_4551_2) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_NS_4551_2) (1) := US ("NS_4551-2");
   Character_Sets (CS_NS_4551_2) (2) := US ("ISO646-NO2");
   Character_Sets (CS_NS_4551_2) (3) := US ("iso-ir-61");
   Character_Sets (CS_NS_4551_2) (4) := US ("no2");
   Character_Sets (CS_NS_4551_2) (5) := US ("csISO61Norwegian2");

   Character_Sets (CS_Videotex_Suppl) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_Videotex_Suppl) (1) := US ("videotex-suppl");
   Character_Sets (CS_Videotex_Suppl) (2) := US ("iso-ir-70");
   Character_Sets (CS_Videotex_Suppl) (3) := US ("csISO70VideotexSupp1");

   Character_Sets (CS_PT2) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_PT2) (1) := US ("PT2");
   Character_Sets (CS_PT2) (2) := US ("iso-ir-84");
   Character_Sets (CS_PT2) (3) := US ("ISO646-PT2");
   Character_Sets (CS_PT2) (4) := US ("csISO84Portuguese2");

   Character_Sets (CS_ES2) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_ES2) (1) := US ("ES2");
   Character_Sets (CS_ES2) (2) := US ("iso-ir-85");
   Character_Sets (CS_ES2) (3) := US ("ISO646-ES2");
   Character_Sets (CS_ES2) (4) := US ("csISO85Spanish2");

   Character_Sets (CS_MSZ_7795_3) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_MSZ_7795_3) (1) := US ("MSZ_7795.3");
   Character_Sets (CS_MSZ_7795_3) (2) := US ("iso-ir-86");
   Character_Sets (CS_MSZ_7795_3) (3) := US ("ISO646-HU");
   Character_Sets (CS_MSZ_7795_3) (4) := US ("hu");
   Character_Sets (CS_MSZ_7795_3) (5) := US ("csISO86Hungarian");

   Character_Sets (CS_JIS_C6226_1983) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_JIS_C6226_1983) (1) := US ("JIS_C6226-1983");
   Character_Sets (CS_JIS_C6226_1983) (2) := US ("iso-ir-87");
   Character_Sets (CS_JIS_C6226_1983) (3) := US ("x0208");
   Character_Sets (CS_JIS_C6226_1983) (4) := US ("JIS_X0208-1983");
   Character_Sets (CS_JIS_C6226_1983) (5) := US ("csISO87JISX0208");

   Character_Sets (CS_Greek7) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_Greek7) (1) := US ("greek7");
   Character_Sets (CS_Greek7) (2) := US ("iso-ir-88");
   Character_Sets (CS_Greek7) (3) := US ("csISO88Greek7");

   Character_Sets (CS_ASMO_449) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_ASMO_449) (1) := US ("ASMO_449");
   Character_Sets (CS_ASMO_449) (2) := US ("ISO_9036");
   Character_Sets (CS_ASMO_449) (3) := US ("arabic7");
   Character_Sets (CS_ASMO_449) (4) := US ("iso-ir-89");
   Character_Sets (CS_ASMO_449) (5) := US ("csISO89ASMO449");

   Character_Sets (CS_Iso_Ir_90) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Iso_Ir_90) (1) := US ("iso-ir-90");
   Character_Sets (CS_Iso_Ir_90) (2) := US ("csISO90");

   Character_Sets (CS_JIS_C6229_1984_A) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_JIS_C6229_1984_A) (1) := US ("JIS_C6229-1984-a");
   Character_Sets (CS_JIS_C6229_1984_A) (2) := US ("iso-ir-91");
   Character_Sets (CS_JIS_C6229_1984_A) (3) := US ("jp-ocr-a");
   Character_Sets (CS_JIS_C6229_1984_A) (4) := US ("csISO91JISC62291984a");

   Character_Sets (CS_JIS_C6229_1984_B) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_JIS_C6229_1984_B) (1) := US ("JIS_C6229-1984-b");
   Character_Sets (CS_JIS_C6229_1984_B) (2) := US ("iso-ir-92");
   Character_Sets (CS_JIS_C6229_1984_B) (3) := US ("ISO646-JP-OCR-B");
   Character_Sets (CS_JIS_C6229_1984_B) (4) := US ("jp-ocr-b");
   Character_Sets (CS_JIS_C6229_1984_B) (5) := US ("csISO92JISC62991984b");

   Character_Sets (CS_JIS_C6229_1984_B_Add) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_JIS_C6229_1984_B_Add) (1) := US ("JIS_C6229-1984-b-add");
   Character_Sets (CS_JIS_C6229_1984_B_Add) (2) := US ("iso-ir-93");
   Character_Sets (CS_JIS_C6229_1984_B_Add) (3) := US ("jp-ocr-b-add");
   Character_Sets (CS_JIS_C6229_1984_B_Add) (4)
     := US ("csISO93JIS62291984badd");

   Character_Sets (CS_JIS_C6229_1984_Hand) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_JIS_C6229_1984_Hand) (1) := US ("JIS_C6229-1984-hand");
   Character_Sets (CS_JIS_C6229_1984_Hand) (2) := US ("iso-ir-94");
   Character_Sets (CS_JIS_C6229_1984_Hand) (3) := US ("jp-ocr-hand");
   Character_Sets (CS_JIS_C6229_1984_Hand) (4)
     := US ("csISO94JIS62291984hand");

   Character_Sets (CS_JIS_C6229_1984_Hand_Add) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_JIS_C6229_1984_Hand_Add) (1)
     := US ("JIS_C6229-1984-hand-add");
   Character_Sets (CS_JIS_C6229_1984_Hand_Add) (2)
     := US ("iso-ir-95");
   Character_Sets (CS_JIS_C6229_1984_Hand_Add) (3)
     := US ("jp-ocr-hand-add");
   Character_Sets (CS_JIS_C6229_1984_Hand_Add) (4)
     := US ("csISO95JIS62291984handadd");

   Character_Sets (CS_JIS_C6229_1984_Kana)
     := new Set_Name_List (1 .. 3);
   Character_Sets (CS_JIS_C6229_1984_Kana) (1)
     := US ("JIS_C6229-1984-kana");
   Character_Sets (CS_JIS_C6229_1984_Kana) (2) := US ("iso-ir-96");
   Character_Sets (CS_JIS_C6229_1984_Kana) (3)
     := US ("csISO96JISC62291984kana");

   Character_Sets (CS_ISO_2033_1983) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_ISO_2033_1983) (1) := US ("ISO_2033-1983");
   Character_Sets (CS_ISO_2033_1983) (2) := US ("iso-ir-98");
   Character_Sets (CS_ISO_2033_1983) (3) := US ("e13b");
   Character_Sets (CS_ISO_2033_1983) (4) := US ("csISO2033");

   Character_Sets (CS_ANSI_X3_110_1983) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_ANSI_X3_110_1983) (1) := US ("ANSI_X3.110-1983");
   Character_Sets (CS_ANSI_X3_110_1983) (2) := US ("iso-ir-99");
   Character_Sets (CS_ANSI_X3_110_1983) (3) := US ("CSA_T500-1983");
   Character_Sets (CS_ANSI_X3_110_1983) (4) := US ("NAPLPS");
   Character_Sets (CS_ANSI_X3_110_1983) (5) := US ("csISO99NAPLPS");

   Character_Sets (CS_T_61_7BIT) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_T_61_7BIT) (1) := US ("T.61-7bit");
   Character_Sets (CS_T_61_7BIT) (2) := US ("iso-ir-102");
   Character_Sets (CS_T_61_7BIT) (3) := US ("csISO102T617bit");

   Character_Sets (CS_T_61_8BIT) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_T_61_8BIT) (1) := US ("T.61-8bit");
   Character_Sets (CS_T_61_8BIT) (2) := US ("T.61");
   Character_Sets (CS_T_61_8BIT) (3) := US ("iso-ir-103");
   Character_Sets (CS_T_61_8BIT) (4) := US ("csISO103T618bit");

   Character_Sets (CS_ECMA_Cyrillic) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_ECMA_Cyrillic) (1) := US ("ECMA-cyrillic");
   Character_Sets (CS_ECMA_Cyrillic) (2) := US ("iso-ir-111");
   Character_Sets (CS_ECMA_Cyrillic) (3) := US ("KOI8-E");
   Character_Sets (CS_ECMA_Cyrillic) (4) := US ("csISO111ECMACyrillic");

   Character_Sets (CS_CSA_Z243_4_1985_1) := new Set_Name_List (1 .. 6);
   Character_Sets (CS_CSA_Z243_4_1985_1) (1) := US ("CSA_Z243.4-1985-1");
   Character_Sets (CS_CSA_Z243_4_1985_1) (2) := US ("iso-ir-121");
   Character_Sets (CS_CSA_Z243_4_1985_1) (3) := US ("ISO646-CA");
   Character_Sets (CS_CSA_Z243_4_1985_1) (4) := US ("csa7-1");
   Character_Sets (CS_CSA_Z243_4_1985_1) (5) := US ("ca");
   Character_Sets (CS_CSA_Z243_4_1985_1) (6) := US ("csISO121Canadian1");

   Character_Sets (CS_CSA_Z243_4_1985_2) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_CSA_Z243_4_1985_2) (1) := US ("CSA_Z243.4-1985-2");
   Character_Sets (CS_CSA_Z243_4_1985_2) (2) := US ("iso-ir-122");
   Character_Sets (CS_CSA_Z243_4_1985_2) (3) := US ("ISO646-CA2");
   Character_Sets (CS_CSA_Z243_4_1985_2) (4) := US ("csa7-2");
   Character_Sets (CS_CSA_Z243_4_1985_2) (5) := US ("csISO122Canadian2");

   Character_Sets (CS_CSA_Z243_4_1985_Gr) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_CSA_Z243_4_1985_Gr) (1) := US ("CSA_Z243.4-1985-gr");
   Character_Sets (CS_CSA_Z243_4_1985_Gr) (2) := US ("iso-ir-123");
   Character_Sets (CS_CSA_Z243_4_1985_Gr) (3) := US ("csISO123CSAZ24341985gr");

   Character_Sets (CS_ISO_8859_6_E) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_ISO_8859_6_E) (1) := US ("ISO-8859-6-E");
   Character_Sets (CS_ISO_8859_6_E) (2) := US ("ISO_8859-6-E");
   Character_Sets (CS_ISO_8859_6_E) (3) := US ("csISO88596E");

   Character_Sets (CS_ISO_8859_6_I) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_ISO_8859_6_I) (1) := US ("ISO-8859-6-I");
   Character_Sets (CS_ISO_8859_6_I) (2) := US ("ISO_8859-6-I");
   Character_Sets (CS_ISO_8859_6_I) (3) := US ("csISO88596I");

   Character_Sets (CS_T_101_G2) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_T_101_G2) (1) := US ("T.101-G2");
   Character_Sets (CS_T_101_G2) (2) := US ("iso-ir-128");
   Character_Sets (CS_T_101_G2) (3) := US ("csISO128T101G2");

   Character_Sets (CS_ISO_8859_8_E) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_ISO_8859_8_E) (1) := US ("ISO-8859-8-E");
   Character_Sets (CS_ISO_8859_8_E) (2) := US ("ISO_8859-8-E");
   Character_Sets (CS_ISO_8859_8_E) (3) := US ("csISO88598E");

   Character_Sets (CS_ISO_8859_8_I) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_ISO_8859_8_I) (1) := US ("ISO_8859-8-I");
   Character_Sets (CS_ISO_8859_8_I) (2) := US ("ISO-8859-8-I");
   Character_Sets (CS_ISO_8859_8_I) (3) := US ("csISO88598I");

   Character_Sets (CS_CSN_369103) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_CSN_369103) (1) := US ("CSN_369103");
   Character_Sets (CS_CSN_369103) (2) := US ("iso-ir-139");
   Character_Sets (CS_CSN_369103) (3) := US ("csISO139CSN369103");

   Character_Sets (CS_JUS_I_B1_002) := new Set_Name_List (1 .. 6);
   Character_Sets (CS_JUS_I_B1_002) (1) := US ("JUS_I.B1.002");
   Character_Sets (CS_JUS_I_B1_002) (2) := US ("iso-ir-141");
   Character_Sets (CS_JUS_I_B1_002) (3) := US ("ISO646-YU");
   Character_Sets (CS_JUS_I_B1_002) (4) := US ("js");
   Character_Sets (CS_JUS_I_B1_002) (5) := US ("yu");
   Character_Sets (CS_JUS_I_B1_002) (6) := US ("csISO141JUSIB1002");

   Character_Sets (CS_IEC_P27_1) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_IEC_P27_1) (1) := US ("IEC_P27-1");
   Character_Sets (CS_IEC_P27_1) (2) := US ("iso-ir-143");
   Character_Sets (CS_IEC_P27_1) (3) := US ("csISO143IECP271");

   Character_Sets (CS_JUS_I_B1_003_Serb) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_JUS_I_B1_003_Serb) (1) := US ("JUS_I.B1.003-serb");
   Character_Sets (CS_JUS_I_B1_003_Serb) (2) := US ("iso-ir-146");
   Character_Sets (CS_JUS_I_B1_003_Serb) (3) := US ("serbian");
   Character_Sets (CS_JUS_I_B1_003_Serb) (4) := US ("csISO146Serbian");

   Character_Sets (CS_JUS_I_B1_003_Mac) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_JUS_I_B1_003_Mac) (1) := US ("JUS_I.B1.003-mac");
   Character_Sets (CS_JUS_I_B1_003_Mac) (2) := US ("macedonian");
   Character_Sets (CS_JUS_I_B1_003_Mac) (3) := US ("iso-ir-147");
   Character_Sets (CS_JUS_I_B1_003_Mac) (4) := US ("csISO147Macedonian");

   Character_Sets (CS_Greek_Ccitt) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_Greek_Ccitt) (1) := US ("greek-ccitt");
   Character_Sets (CS_Greek_Ccitt) (2) := US ("iso-ir-150");
   Character_Sets (CS_Greek_Ccitt) (3) := US ("csISO150");
   Character_Sets (CS_Greek_Ccitt) (4) := US ("csISO150GreekCCITT");

   Character_Sets (CS_NC_NC00_10_81) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_NC_NC00_10_81) (1) := US ("NC_NC00-10:81");
   Character_Sets (CS_NC_NC00_10_81) (2) := US ("cuba");
   Character_Sets (CS_NC_NC00_10_81) (3) := US ("iso-ir-151");
   Character_Sets (CS_NC_NC00_10_81) (4) := US ("ISO646-CU");
   Character_Sets (CS_NC_NC00_10_81) (5) := US ("csISO151Cuba");

   Character_Sets (CS_ISO_6937_2_25) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_ISO_6937_2_25) (1) := US ("ISO_6937-2-25");
   Character_Sets (CS_ISO_6937_2_25) (2) := US ("iso-ir-152");
   Character_Sets (CS_ISO_6937_2_25) (3) := US ("csISO6937Add");

   Character_Sets (CS_GOST_19768_74) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_GOST_19768_74) (1) := US ("GOST_19768-74");
   Character_Sets (CS_GOST_19768_74) (2) := US ("ST_SEV_358-88");
   Character_Sets (CS_GOST_19768_74) (3) := US ("iso-ir-153");
   Character_Sets (CS_GOST_19768_74) (4) := US ("csISO153GOST1976874");

   Character_Sets (CS_ISO_8859_SUPP) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_ISO_8859_SUPP) (1) := US ("ISO_8859-supp");
   Character_Sets (CS_ISO_8859_SUPP) (2) := US ("iso-ir-154");
   Character_Sets (CS_ISO_8859_SUPP) (3) := US ("latin1-2-5");
   Character_Sets (CS_ISO_8859_SUPP) (4) := US ("csISO8859Supp");

   Character_Sets (CS_ISO_10367_BOX) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_ISO_10367_BOX) (1) := US ("ISO_10367-box");
   Character_Sets (CS_ISO_10367_BOX) (2) := US ("iso-ir-155");
   Character_Sets (CS_ISO_10367_BOX) (3) := US ("csISO10367Box");

   Character_Sets (CS_Latin_Lap) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_Latin_Lap) (1) := US ("latin-lap");
   Character_Sets (CS_Latin_Lap) (2) := US ("lap");
   Character_Sets (CS_Latin_Lap) (3) := US ("iso-ir-158");
   Character_Sets (CS_Latin_Lap) (4) := US ("csISO158Lap");

   Character_Sets (CS_JIS_X0212_1990) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_JIS_X0212_1990) (1) := US ("JIS_X0212-1990");
   Character_Sets (CS_JIS_X0212_1990) (2) := US ("x0212");
   Character_Sets (CS_JIS_X0212_1990) (3) := US ("iso-ir-159");
   Character_Sets (CS_JIS_X0212_1990) (4) := US ("csISO159JISX02121990");

   Character_Sets (CS_DS_2089) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_DS_2089) (1) := US ("DS_2089");
   Character_Sets (CS_DS_2089) (2) := US ("DS2089");
   Character_Sets (CS_DS_2089) (3) := US ("ISO646-DK");
   Character_Sets (CS_DS_2089) (4) := US ("dk");
   Character_Sets (CS_DS_2089) (5) := US ("csISO646Danish");

   Character_Sets (CS_Us_Dk) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Us_Dk) (1) := US ("us-dk");
   Character_Sets (CS_Us_Dk) (2) := US ("csUSDK");

   Character_Sets (CS_Dk_Us) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Dk_Us) (1) := US ("dk-us");
   Character_Sets (CS_Dk_Us) (2) := US ("csDKUS");

   Character_Sets (CS_KSC5636) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_KSC5636) (1) := US ("KSC5636");
   Character_Sets (CS_KSC5636) (2) := US ("ISO646-KR");
   Character_Sets (CS_KSC5636) (3) := US ("csKSC5636");

   Character_Sets (CS_UNICODE_1_1_UTF_7) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_UNICODE_1_1_UTF_7) (1) := US ("UNICODE-1-1-UTF-7");
   Character_Sets (CS_UNICODE_1_1_UTF_7) (2) := US ("csUnicode11UTF7");

   Character_Sets (CS_ISO_2022_CN) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_ISO_2022_CN) (1) := US ("ISO-2022-CN");

   Character_Sets (CS_ISO_2022_CN_EXT) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_ISO_2022_CN_EXT) (1) := US ("ISO-2022-CN-EXT");

   Character_Sets (CS_UTF_8) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_UTF_8) (1) := US ("UTF-8");

   Character_Sets (CS_ISO_8859_13) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_ISO_8859_13) (1) := US ("ISO-8859-13");

   Character_Sets (CS_ISO_8859_14) := new Set_Name_List (1 .. 7);
   Character_Sets (CS_ISO_8859_14) (1) := US ("ISO-8859-14");
   Character_Sets (CS_ISO_8859_14) (2) := US ("iso-ir-199");
   Character_Sets (CS_ISO_8859_14) (3) := US ("ISO_8859-14:1998");
   Character_Sets (CS_ISO_8859_14) (4) := US ("ISO_8859-14");
   Character_Sets (CS_ISO_8859_14) (5) := US ("latin8");
   Character_Sets (CS_ISO_8859_14) (6) := US ("iso-celtic");
   Character_Sets (CS_ISO_8859_14) (7) := US ("l8");

   Character_Sets (CS_ISO_8859_15) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_ISO_8859_15) (1) := US ("ISO-8859-15");
   Character_Sets (CS_ISO_8859_15) (2) := US ("ISO_8859-15");
   Character_Sets (CS_ISO_8859_15) (3) := US ("Latin-9");
   Character_Sets (CS_ISO_8859_15) (4) := US ("l9");

   Character_Sets (CS_ISO_8859_16) := new Set_Name_List (1 .. 6);
   Character_Sets (CS_ISO_8859_16) (1) := US ("ISO-8859-16");
   Character_Sets (CS_ISO_8859_16) (2) := US ("iso-ir-226");
   Character_Sets (CS_ISO_8859_16) (3) := US ("ISO_8859-16:2001");
   Character_Sets (CS_ISO_8859_16) (4) := US ("ISO_8859-16");
   Character_Sets (CS_ISO_8859_16) (5) := US ("latin10");
   Character_Sets (CS_ISO_8859_16) (6) := US ("l10");

   Character_Sets (CS_GBK) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_GBK) (1) := US ("GBK");
   Character_Sets (CS_GBK) (2) := US ("CP936");
   Character_Sets (CS_GBK) (3) := US ("MS936");
   Character_Sets (CS_GBK) (4) := US ("windows-936");

   Character_Sets (CS_GB18030) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_GB18030) (1) := US ("GB18030");

   Character_Sets (CS_OSD_EBCDIC_DF04_15) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_OSD_EBCDIC_DF04_15) (1) := US ("OSD_EBCDIC_DF04_15");

   Character_Sets (CS_OSD_EBCDIC_DF03_IRV) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_OSD_EBCDIC_DF03_IRV) (1) := US ("OSD_EBCDIC_DF03_IRV");

   Character_Sets (CS_OSD_EBCDIC_DF04_1) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_OSD_EBCDIC_DF04_1) (1) := US ("OSD_EBCDIC_DF04_1");

   Character_Sets (CS_ISO_11548_1) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_ISO_11548_1) (1) := US ("ISO-11548-1");
   Character_Sets (CS_ISO_11548_1) (2) := US ("ISO_11548-1");
   Character_Sets (CS_ISO_11548_1) (3) := US ("ISO_TR_11548-1");
   Character_Sets (CS_ISO_11548_1) (4) := US ("csISO115481");

   Character_Sets (CS_KZ_1048) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_KZ_1048) (1) := US ("KZ-1048");
   Character_Sets (CS_KZ_1048) (2) := US ("STRK1048-2002");
   Character_Sets (CS_KZ_1048) (3) := US ("RK1048");
   Character_Sets (CS_KZ_1048) (4) := US ("csKZ1048");

   Character_Sets (CS_ISO_10646_UCS_2) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_10646_UCS_2) (1) := US ("ISO-10646-UCS-2");
   Character_Sets (CS_ISO_10646_UCS_2) (2) := US ("csUnicode");

   Character_Sets (CS_ISO_10646_UCS_4) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_10646_UCS_4) (1) := US ("ISO-10646-UCS-4");
   Character_Sets (CS_ISO_10646_UCS_4) (2) := US ("csUCS4");

   Character_Sets (CS_ISO_10646_UCS_Basic) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_10646_UCS_Basic) (1) := US ("ISO-10646-UCS-Basic");
   Character_Sets (CS_ISO_10646_UCS_Basic) (2) := US ("csUnicodeASCII");

   Character_Sets (CS_ISO_10646_Unicode_Latin1) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_ISO_10646_Unicode_Latin1) (1)
     := US ("ISO-10646-Unicode-Latin1");
   Character_Sets (CS_ISO_10646_Unicode_Latin1) (2) := US ("csUnicodeLatin1");
   Character_Sets (CS_ISO_10646_Unicode_Latin1) (3) := US ("ISO-10646");

   Character_Sets (CS_ISO_10646_J_1) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_ISO_10646_J_1) (1) := US ("ISO-10646-J-1");

   Character_Sets (CS_ISO_UNICODE_IBM_1261) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_UNICODE_IBM_1261) (1) := US ("ISO-Unicode-IBM-1261");
   Character_Sets (CS_ISO_UNICODE_IBM_1261) (2) := US ("csUnicodeIBM1261");

   Character_Sets (CS_ISO_UNICODE_IBM_1268) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_UNICODE_IBM_1268) (1) := US ("ISO-Unicode-IBM-1268");
   Character_Sets (CS_ISO_UNICODE_IBM_1268) (2) := US ("csUnicodeIBM1268");

   Character_Sets (CS_ISO_UNICODE_IBM_1276) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_UNICODE_IBM_1276) (1) := US ("ISO-Unicode-IBM-1276");
   Character_Sets (CS_ISO_UNICODE_IBM_1276) (2) := US ("csUnicodeIBM1276");

   Character_Sets (CS_ISO_UNICODE_IBM_1264) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_UNICODE_IBM_1264) (1) := US ("ISO-Unicode-IBM-1264");
   Character_Sets (CS_ISO_UNICODE_IBM_1264) (2) := US ("csUnicodeIBM1264");

   Character_Sets (CS_ISO_UNICODE_IBM_1265) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_UNICODE_IBM_1265) (1) := US ("ISO-Unicode-IBM-1265");
   Character_Sets (CS_ISO_UNICODE_IBM_1265) (2) := US ("csUnicodeIBM1265");

   Character_Sets (CS_UNICODE_1_1) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_UNICODE_1_1) (1) := US ("UNICODE-1-1");
   Character_Sets (CS_UNICODE_1_1) (2) := US ("csUnicode11");

   Character_Sets (CS_SCSU) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_SCSU) (1) := US ("SCSU");

   Character_Sets (CS_UTF_7) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_UTF_7) (1) := US ("UTF-7");

   Character_Sets (CS_UTF_16BE) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_UTF_16BE) (1) := US ("UTF-16BE");

   Character_Sets (CS_UTF_16LE) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_UTF_16LE) (1) := US ("UTF-16LE");

   Character_Sets (CS_UTF_16) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_UTF_16) (1) := US ("UTF-16");

   Character_Sets (CS_CESU_8) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_CESU_8) (1) := US ("CESU-8");
   Character_Sets (CS_CESU_8) (2) := US ("csCESU-8");

   Character_Sets (CS_UTF_32) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_UTF_32) (1) := US ("UTF-32");

   Character_Sets (CS_UTF_32BE) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_UTF_32BE) (1) := US ("UTF-32BE");

   Character_Sets (CS_UTF_32LE) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_UTF_32LE) (1) := US ("UTF-32LE");

   Character_Sets (CS_BOCU_1) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_BOCU_1) (1) := US ("BOCU-1");
   Character_Sets (CS_BOCU_1) (2) := US ("csBOCU-1");

   Character_Sets (CS_ISO_8859_1_Windows_3_0_Latin_1)
     := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_8859_1_Windows_3_0_Latin_1) (1)
     := US ("ISO-8859-1-Windows-3.0-Latin-1");
   Character_Sets (CS_ISO_8859_1_Windows_3_0_Latin_1) (2)
     := US ("csWindows30Latin1");

   Character_Sets (CS_ISO_8859_1_Windows_3_1_Latin_1)
     := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_8859_1_Windows_3_1_Latin_1) (1)
     := US ("ISO-8859-1-Windows-3.1-Latin-1");
   Character_Sets (CS_ISO_8859_1_Windows_3_1_Latin_1) (2)
     := US ("csWindows31Latin1");

   Character_Sets (CS_ISO_8859_2_Windows_Latin_2)
     := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_8859_2_Windows_Latin_2) (1)
     := US ("ISO-8859-2-Windows-Latin-2");
   Character_Sets (CS_ISO_8859_2_Windows_Latin_2) (2)
     := US ("csWindows31Latin2");

   Character_Sets (CS_ISO_8859_9_Windows_Latin_5)
     := new Set_Name_List (1 .. 2);
   Character_Sets (CS_ISO_8859_9_Windows_Latin_5) (1)
     := US ("ISO-8859-9-Windows-Latin-5");
   Character_Sets (CS_ISO_8859_9_Windows_Latin_5) (2)
     := US ("csWindows31Latin5");

   Character_Sets (CS_Hp_Roman8) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_Hp_Roman8) (1) := US ("hp-roman8");
   Character_Sets (CS_Hp_Roman8) (2) := US ("roman8");
   Character_Sets (CS_Hp_Roman8) (3) := US ("r8");
   Character_Sets (CS_Hp_Roman8) (4) := US ("csHPRoman8");

   Character_Sets (CS_Adobe_Standard_Encoding)
     := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Adobe_Standard_Encoding) (1)
     := US ("Adobe-Standard-Encoding");
   Character_Sets (CS_Adobe_Standard_Encoding) (2)
     := US ("csAdobeStandardEncoding");

   Character_Sets (CS_Ventura_US) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Ventura_US) (1) := US ("Ventura-US");
   Character_Sets (CS_Ventura_US) (2) := US ("csVenturaUS");

   Character_Sets (CS_Ventura_International)
     := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Ventura_International) (1)
     := US ("Ventura-International");
   Character_Sets (CS_Ventura_International) (2)
     := US ("csVenturaInternational");

   Character_Sets (CS_DEC_MCS) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_DEC_MCS) (1) := US ("DEC-MCS");
   Character_Sets (CS_DEC_MCS) (2) := US ("dec");
   Character_Sets (CS_DEC_MCS) (3) := US ("csDECMCS");

   Character_Sets (CS_IBM850) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM850) (1) := US ("IBM850");
   Character_Sets (CS_IBM850) (2) := US ("cp850");
   Character_Sets (CS_IBM850) (3) := US ("850");
   Character_Sets (CS_IBM850) (4) := US ("csPC850Multilingual");

   Character_Sets (CS_Pc8_Danish_Norwegian) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Pc8_Danish_Norwegian) (1) := US ("PC8-Danish-Norwegian");
   Character_Sets (CS_Pc8_Danish_Norwegian) (2) := US ("csPC8DanishNorwegian");

   Character_Sets (CS_IBM862) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM862) (1) := US ("IBM862");
   Character_Sets (CS_IBM862) (2) := US ("cp862");
   Character_Sets (CS_IBM862) (3) := US ("862");
   Character_Sets (CS_IBM862) (4) := US ("csPC862LatinHebrew");

   Character_Sets (CS_PC8_Turkish) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_PC8_Turkish) (1) := US ("PC8-Turkish");
   Character_Sets (CS_PC8_Turkish) (2) := US ("csPC8Turkish");

   Character_Sets (CS_IBM_Symbols) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_IBM_Symbols) (1) := US ("IBM-Symbols");
   Character_Sets (CS_IBM_Symbols) (2) := US ("csIBMSymbols");

   Character_Sets (CS_IBM_Thai) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_IBM_Thai) (1) := US ("IBM-Thai");
   Character_Sets (CS_IBM_Thai) (2) := US ("csIBMThai");

   Character_Sets (CS_HP_Legal) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_HP_Legal) (1) := US ("HP-Legal");
   Character_Sets (CS_HP_Legal) (2) := US ("csHPLegal");

   Character_Sets (CS_HP_Pi_Font) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_HP_Pi_Font) (1) := US ("HP-Pi-font");
   Character_Sets (CS_HP_Pi_Font) (2) := US ("csHPPiFont");

   Character_Sets (CS_HP_Math8) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_HP_Math8) (1) := US ("HP-Math8");
   Character_Sets (CS_HP_Math8) (2) := US ("csHPMath8");

   Character_Sets (CS_Adobe_Symbol_Encoding) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Adobe_Symbol_Encoding) (1)
     := US ("Adobe-Symbol-Encoding");
   Character_Sets (CS_Adobe_Symbol_Encoding) (2) := US ("csHPPSMath");

   Character_Sets (CS_HP_DeskTop) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_HP_DeskTop) (1) := US ("HP-DeskTop");
   Character_Sets (CS_HP_DeskTop) (2) := US ("csHPDesktop");

   Character_Sets (CS_Ventura_Math) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Ventura_Math) (1) := US ("Ventura-Math");
   Character_Sets (CS_Ventura_Math) (2) := US ("csVenturaMath");

   Character_Sets (CS_Microsoft_Publishing) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Microsoft_Publishing) (1) := US ("Microsoft-Publishing");
   Character_Sets (CS_Microsoft_Publishing) (2)
     := US ("csMicrosoftPublishing");

   Character_Sets (CS_Windows_31j) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Windows_31j) (1) := US ("Windows-31J");
   Character_Sets (CS_Windows_31j) (2) := US ("csWindows31J");

   Character_Sets (CS_GB2312) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_GB2312) (1) := US ("GB2312");
   Character_Sets (CS_GB2312) (2) := US ("csGB2312");

   Character_Sets (CS_Big5) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_Big5) (1) := US ("Big5");
   Character_Sets (CS_Big5) (2) := US ("csBig5");

   Character_Sets (CS_Macintosh) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_Macintosh) (1) := US ("macintosh");
   Character_Sets (CS_Macintosh) (2) := US ("mac");
   Character_Sets (CS_Macintosh) (3) := US ("csMacintosh");

   Character_Sets (CS_IBM037) := new Set_Name_List (1 .. 7);
   Character_Sets (CS_IBM037) (1) := US ("IBM037");
   Character_Sets (CS_IBM037) (2) := US ("cp037");
   Character_Sets (CS_IBM037) (3) := US ("ebcdic-cp-us");
   Character_Sets (CS_IBM037) (4) := US ("ebcdic-cp-ca");
   Character_Sets (CS_IBM037) (5) := US ("ebcdic-cp-wt");
   Character_Sets (CS_IBM037) (6) := US ("ebcdic-cp-nl");
   Character_Sets (CS_IBM037) (7) := US ("csIBM037");

   Character_Sets (CS_IBM038) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM038) (1) := US ("IBM038");
   Character_Sets (CS_IBM038) (2) := US ("EBCDIC-INT");
   Character_Sets (CS_IBM038) (3) := US ("cp038");
   Character_Sets (CS_IBM038) (4) := US ("csIBM038");

   Character_Sets (CS_IBM273) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_IBM273) (1) := US ("IBM273");
   Character_Sets (CS_IBM273) (2) := US ("CP273");
   Character_Sets (CS_IBM273) (3) := US ("csIBM273");

   Character_Sets (CS_IBM274) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM274) (1) := US ("IBM274");
   Character_Sets (CS_IBM274) (2) := US ("EBCDIC-BE");
   Character_Sets (CS_IBM274) (3) := US ("CP274");
   Character_Sets (CS_IBM274) (4) := US ("csIBM274");

   Character_Sets (CS_IBM275) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM275) (1) := US ("IBM275");
   Character_Sets (CS_IBM275) (2) := US ("EBCDIC-BR");
   Character_Sets (CS_IBM275) (3) := US ("cp275");
   Character_Sets (CS_IBM275) (4) := US ("csIBM275");

   Character_Sets (CS_IBM277) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM277) (1) := US ("IBM277");
   Character_Sets (CS_IBM277) (2) := US ("EBCDIC-CP-DK");
   Character_Sets (CS_IBM277) (3) := US ("EBCDIC-CP-NO");
   Character_Sets (CS_IBM277) (4) := US ("csIBM277");

   Character_Sets (CS_IBM278) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_IBM278) (1) := US ("IBM278");
   Character_Sets (CS_IBM278) (2) := US ("CP278");
   Character_Sets (CS_IBM278) (3) := US ("ebcdic-cp-fi");
   Character_Sets (CS_IBM278) (4) := US ("ebcdic-cp-se");
   Character_Sets (CS_IBM278) (5) := US ("csIBM278");

   Character_Sets (CS_IBM280) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM280) (1) := US ("IBM280");
   Character_Sets (CS_IBM280) (2) := US ("CP280");
   Character_Sets (CS_IBM280) (3) := US ("ebcdic-cp-it");
   Character_Sets (CS_IBM280) (4) := US ("csIBM280");

   Character_Sets (CS_IBM281) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM281) (1) := US ("IBM281");
   Character_Sets (CS_IBM281) (2) := US ("EBCDIC-JP-E");
   Character_Sets (CS_IBM281) (3) := US ("cp281");
   Character_Sets (CS_IBM281) (4) := US ("csIBM281");

   Character_Sets (CS_IBM284) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM284) (1) := US ("IBM284");
   Character_Sets (CS_IBM284) (2) := US ("CP284");
   Character_Sets (CS_IBM284) (3) := US ("ebcdic-cp-es");
   Character_Sets (CS_IBM284) (4) := US ("csIBM284");

   Character_Sets (CS_IBM285) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM285) (1) := US ("IBM285");
   Character_Sets (CS_IBM285) (2) := US ("CP285");
   Character_Sets (CS_IBM285) (3) := US ("ebcdic-cp-gb");
   Character_Sets (CS_IBM285) (4) := US ("csIBM285");

   Character_Sets (CS_IBM290) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM290) (1) := US ("IBM290");
   Character_Sets (CS_IBM290) (2) := US ("cp290");
   Character_Sets (CS_IBM290) (3) := US ("EBCDIC-JP-kana");
   Character_Sets (CS_IBM290) (4) := US ("csIBM290");

   Character_Sets (CS_IBM297) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM297) (1) := US ("IBM297");
   Character_Sets (CS_IBM297) (2) := US ("cp297");
   Character_Sets (CS_IBM297) (3) := US ("ebcdic-cp-fr");
   Character_Sets (CS_IBM297) (4) := US ("csIBM297");

   Character_Sets (CS_IBM420) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM420) (1) := US ("IBM420");
   Character_Sets (CS_IBM420) (2) := US ("cp420");
   Character_Sets (CS_IBM420) (3) := US ("ebcdic-cp-ar1");
   Character_Sets (CS_IBM420) (4) := US ("csIBM420");

   Character_Sets (CS_IBM423) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM423) (1) := US ("IBM423");
   Character_Sets (CS_IBM423) (2) := US ("cp423");
   Character_Sets (CS_IBM423) (3) := US ("ebcdic-cp-gr");
   Character_Sets (CS_IBM423) (4) := US ("csIBM423");

   Character_Sets (CS_IBM424) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM424) (1) := US ("IBM424");
   Character_Sets (CS_IBM424) (2) := US ("cp424");
   Character_Sets (CS_IBM424) (3) := US ("ebcdic-cp-he");
   Character_Sets (CS_IBM424) (4) := US ("csIBM424");

   Character_Sets (CS_IBM437) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM437) (1) := US ("IBM437");
   Character_Sets (CS_IBM437) (2) := US ("cp437");
   Character_Sets (CS_IBM437) (3) := US ("437");
   Character_Sets (CS_IBM437) (4) := US ("csPC8CodePage437");

   Character_Sets (CS_IBM500) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_IBM500) (1) := US ("IBM500");
   Character_Sets (CS_IBM500) (2) := US ("CP500");
   Character_Sets (CS_IBM500) (3) := US ("ebcdic-cp-be");
   Character_Sets (CS_IBM500) (4) := US ("ebcdic-cp-ch");
   Character_Sets (CS_IBM500) (5) := US ("csIBM500");

   Character_Sets (CS_IBM851) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM851) (1) := US ("IBM851");
   Character_Sets (CS_IBM851) (2) := US ("cp851");
   Character_Sets (CS_IBM851) (3) := US ("851");
   Character_Sets (CS_IBM851) (4) := US ("csIBM851");

   Character_Sets (CS_IBM852) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM852) (1) := US ("IBM852");
   Character_Sets (CS_IBM852) (2) := US ("cp852");
   Character_Sets (CS_IBM852) (3) := US ("852");
   Character_Sets (CS_IBM852) (4) := US ("csPCp852");

   Character_Sets (CS_IBM855) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM855) (1) := US ("IBM855");
   Character_Sets (CS_IBM855) (2) := US ("cp855");
   Character_Sets (CS_IBM855) (3) := US ("855");
   Character_Sets (CS_IBM855) (4) := US ("csIBM855");

   Character_Sets (CS_IBM857) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM857) (1) := US ("IBM857");
   Character_Sets (CS_IBM857) (2) := US ("cp857");
   Character_Sets (CS_IBM857) (3) := US ("857");
   Character_Sets (CS_IBM857) (4) := US ("csIBM857");

   Character_Sets (CS_IBM860) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM860) (1) := US ("IBM860");
   Character_Sets (CS_IBM860) (2) := US ("cp860");
   Character_Sets (CS_IBM860) (3) := US ("860");
   Character_Sets (CS_IBM860) (4) := US ("csIBM860");

   Character_Sets (CS_IBM861) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_IBM861) (1) := US ("IBM861");
   Character_Sets (CS_IBM861) (2) := US ("cp861");
   Character_Sets (CS_IBM861) (3) := US ("861");
   Character_Sets (CS_IBM861) (4) := US ("cp-is");
   Character_Sets (CS_IBM861) (5) := US ("csIBM861");

   Character_Sets (CS_IBM863) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM863) (1) := US ("IBM863");
   Character_Sets (CS_IBM863) (2) := US ("cp863");
   Character_Sets (CS_IBM863) (3) := US ("863");
   Character_Sets (CS_IBM863) (4) := US ("csIBM863");

   Character_Sets (CS_IBM864) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_IBM864) (1) := US ("IBM864");
   Character_Sets (CS_IBM864) (2) := US ("cp864");
   Character_Sets (CS_IBM864) (3) := US ("csIBM864");

   Character_Sets (CS_IBM865) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM865) (1) := US ("IBM865");
   Character_Sets (CS_IBM865) (2) := US ("cp865");
   Character_Sets (CS_IBM865) (3) := US ("865");
   Character_Sets (CS_IBM865) (4) := US ("csIBM865");

   Character_Sets (CS_IBM868) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM868) (1) := US ("IBM868");
   Character_Sets (CS_IBM868) (2) := US ("CP868");
   Character_Sets (CS_IBM868) (3) := US ("cp-ar");
   Character_Sets (CS_IBM868) (4) := US ("csIBM868");

   Character_Sets (CS_IBM869) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_IBM869) (1) := US ("IBM869");
   Character_Sets (CS_IBM869) (2) := US ("cp869");
   Character_Sets (CS_IBM869) (3) := US ("869");
   Character_Sets (CS_IBM869) (4) := US ("cp-gr");
   Character_Sets (CS_IBM869) (5) := US ("csIBM869");

   Character_Sets (CS_IBM870) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_IBM870) (1) := US ("IBM870");
   Character_Sets (CS_IBM870) (2) := US ("CP870");
   Character_Sets (CS_IBM870) (3) := US ("ebcdic-cp-roece");
   Character_Sets (CS_IBM870) (4) := US ("ebcdic-cp-yu");
   Character_Sets (CS_IBM870) (5) := US ("csIBM870");

   Character_Sets (CS_IBM871) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM871) (1) := US ("IBM871");
   Character_Sets (CS_IBM871) (2) := US ("CP871");
   Character_Sets (CS_IBM871) (3) := US ("ebcdic-cp-is");
   Character_Sets (CS_IBM871) (4) := US ("csIBM871");

   Character_Sets (CS_IBM880) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM880) (1) := US ("IBM880");
   Character_Sets (CS_IBM880) (2) := US ("cp880");
   Character_Sets (CS_IBM880) (3) := US ("EBCDIC-Cyrillic");
   Character_Sets (CS_IBM880) (4) := US ("csIBM880");

   Character_Sets (CS_IBM891) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_IBM891) (1) := US ("IBM891");
   Character_Sets (CS_IBM891) (2) := US ("cp891");
   Character_Sets (CS_IBM891) (3) := US ("csIBM891");

   Character_Sets (CS_IBM903) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_IBM903) (1) := US ("IBM903");
   Character_Sets (CS_IBM903) (2) := US ("cp903");
   Character_Sets (CS_IBM903) (3) := US ("csIBM903");

   Character_Sets (CS_IBM904) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM904) (1) := US ("IBM904");
   Character_Sets (CS_IBM904) (2) := US ("cp904");
   Character_Sets (CS_IBM904) (3) := US ("904");
   Character_Sets (CS_IBM904) (4) := US ("csIBBM904");

   Character_Sets (CS_IBM905) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM905) (1) := US ("IBM905");
   Character_Sets (CS_IBM905) (2) := US ("CP905");
   Character_Sets (CS_IBM905) (3) := US ("ebcdic-cp-tr");
   Character_Sets (CS_IBM905) (4) := US ("csIBM905");

   Character_Sets (CS_IBM918) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM918) (1) := US ("IBM918");
   Character_Sets (CS_IBM918) (2) := US ("CP918");
   Character_Sets (CS_IBM918) (3) := US ("ebcdic-cp-ar2");
   Character_Sets (CS_IBM918) (4) := US ("csIBM918");

   Character_Sets (CS_IBM1026) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_IBM1026) (1) := US ("IBM1026");
   Character_Sets (CS_IBM1026) (2) := US ("CP1026");
   Character_Sets (CS_IBM1026) (3) := US ("csIBM1026");

   Character_Sets (CS_EBCDIC_AT_DE) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_AT_DE) (1) := US ("EBCDIC-AT-DE");
   Character_Sets (CS_EBCDIC_AT_DE) (2) := US ("csIBMEBCDICATDE");

   Character_Sets (CS_EBCDIC_AT_DE_A) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_AT_DE_A) (1) := US ("EBCDIC-AT-DE-A");
   Character_Sets (CS_EBCDIC_AT_DE_A) (2) := US ("csEBCDICATDEA");

   Character_Sets (CS_EBCDIC_CA_FR) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_CA_FR) (1) := US ("EBCDIC-CA-FR");
   Character_Sets (CS_EBCDIC_CA_FR) (2) := US ("csEBCDICCAFR");

   Character_Sets (CS_EBCDIC_DK_NO) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_DK_NO) (1) := US ("EBCDIC-DK-NO");
   Character_Sets (CS_EBCDIC_DK_NO) (2) := US ("csEBCDICDKNO");

   Character_Sets (CS_EBCDIC_DK_NO_A) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_DK_NO_A) (1) := US ("EBCDIC-DK-NO-A");
   Character_Sets (CS_EBCDIC_DK_NO_A) (2) := US ("csEBCDICDKNOA");

   Character_Sets (CS_EBCDIC_FI_SE) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_FI_SE) (1) := US ("EBCDIC-FI-SE");
   Character_Sets (CS_EBCDIC_FI_SE) (2) := US ("csEBCDICFISE");

   Character_Sets (CS_EBCDIC_FI_SE_A) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_FI_SE_A) (1) := US ("EBCDIC-FI-SE-A");
   Character_Sets (CS_EBCDIC_FI_SE_A) (2) := US ("csEBCDICFISEA");

   Character_Sets (CS_EBCDIC_FR) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_FR) (1) := US ("EBCDIC-FR");
   Character_Sets (CS_EBCDIC_FR) (2) := US ("csEBCDICFR");

   Character_Sets (CS_EBCDIC_IT) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_IT) (1) := US ("EBCDIC-IT");
   Character_Sets (CS_EBCDIC_IT) (2) := US ("csEBCDICIT");

   Character_Sets (CS_EBCDIC_PT) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_PT) (1) := US ("EBCDIC-PT");
   Character_Sets (CS_EBCDIC_PT) (2) := US ("csEBCDICPT");

   Character_Sets (CS_EBCDIC_ES) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_ES) (1) := US ("EBCDIC-ES");
   Character_Sets (CS_EBCDIC_ES) (2) := US ("csEBCDICES");

   Character_Sets (CS_EBCDIC_ES_A) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_ES_A) (1) := US ("EBCDIC-ES-A");
   Character_Sets (CS_EBCDIC_ES_A) (2) := US ("csEBCDICESA");

   Character_Sets (CS_EBCDIC_ES_S) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_ES_S) (1) := US ("EBCDIC-ES-S");
   Character_Sets (CS_EBCDIC_ES_S) (2) := US ("csEBCDICESS");

   Character_Sets (CS_EBCDIC_UK) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_UK) (1) := US ("EBCDIC-UK");
   Character_Sets (CS_EBCDIC_UK) (2) := US ("csEBCDICUK");

   Character_Sets (CS_EBCDIC_US) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_EBCDIC_US) (1) := US ("EBCDIC-US");
   Character_Sets (CS_EBCDIC_US) (2) := US ("csEBCDICUS");

   Character_Sets (CS_Unknown) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_Unknown) (1) := US ("Basil_Unknown");

   Character_Sets (CS_UNKNOWN_8BIT) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_UNKNOWN_8BIT) (1) := US ("UNKNOWN-8BIT");
   Character_Sets (CS_UNKNOWN_8BIT) (2) := US ("csUnknown8BiT");

   Character_Sets (CS_MNEMONIC) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_MNEMONIC) (1) := US ("MNEMONIC");
   Character_Sets (CS_MNEMONIC) (2) := US ("csMnemonic");

   Character_Sets (CS_MNEM) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_MNEM) (1) := US ("MNEM");
   Character_Sets (CS_MNEM) (2) := US ("csMnem");

   Character_Sets (CS_VISCII) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_VISCII) (1) := US ("VISCII");
   Character_Sets (CS_VISCII) (2) := US ("csVISCII");

   Character_Sets (CS_VIQR) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_VIQR) (1) := US ("VIQR");
   Character_Sets (CS_VIQR) (2) := US ("csVIQR");

   Character_Sets (CS_KOI8_R) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_KOI8_R) (1) := US ("KOI8-R");
   Character_Sets (CS_KOI8_R) (2) := US ("csKOI8R");

   Character_Sets (CS_HZ_GB_2312) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_HZ_GB_2312) (1) := US ("HZ-GB-2312");

   Character_Sets (CS_IBM866) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM866) (1) := US ("IBM866");
   Character_Sets (CS_IBM866) (2) := US ("cp866");
   Character_Sets (CS_IBM866) (3) := US ("866");
   Character_Sets (CS_IBM866) (4) := US ("csIBM866");

   Character_Sets (CS_IBM775) := new Set_Name_List (1 .. 3);
   Character_Sets (CS_IBM775) (1) := US ("IBM775");
   Character_Sets (CS_IBM775) (2) := US ("cp775");
   Character_Sets (CS_IBM775) (3) := US ("csPC775Baltic");

   Character_Sets (CS_KOI8_U) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_KOI8_U) (1) := US ("KOI8-U");

   Character_Sets (CS_IBM00858) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM00858) (1) := US ("IBM00858");
   Character_Sets (CS_IBM00858) (2) := US ("CCSID00858");
   Character_Sets (CS_IBM00858) (3) := US ("CP00858");
   Character_Sets (CS_IBM00858) (4) := US ("PC-Multilingual-850+euro");

   Character_Sets (CS_IBM00924) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM00924) (1) := US ("IBM00924");
   Character_Sets (CS_IBM00924) (2) := US ("CCSID00924");
   Character_Sets (CS_IBM00924) (3) := US ("CP00924");
   Character_Sets (CS_IBM00924) (4) := US ("ebcdic-Latin9--euro");

   Character_Sets (CS_IBM01140) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM01140) (1) := US ("IBM01140");
   Character_Sets (CS_IBM01140) (2) := US ("CCSID01140");
   Character_Sets (CS_IBM01140) (3) := US ("CP01140");
   Character_Sets (CS_IBM01140) (4) := US ("ebcdic-us-37+euro");

   Character_Sets (CS_IBM01141) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM01141) (1) := US ("IBM01141");
   Character_Sets (CS_IBM01141) (2) := US ("CCSID01141");
   Character_Sets (CS_IBM01141) (3) := US ("CP01141");
   Character_Sets (CS_IBM01141) (4) := US ("ebcdic-de-273+euro");

   Character_Sets (CS_IBM01142) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_IBM01142) (1) := US ("IBM01142");
   Character_Sets (CS_IBM01142) (2) := US ("CCSID01142");
   Character_Sets (CS_IBM01142) (3) := US ("CP01142");
   Character_Sets (CS_IBM01142) (4) := US ("ebcdic-dk-277+euro");
   Character_Sets (CS_IBM01142) (5) := US ("ebcdic-no-277+euro");

   Character_Sets (CS_IBM01143) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_IBM01143) (1) := US ("IBM01143");
   Character_Sets (CS_IBM01143) (2) := US ("CCSID01143");
   Character_Sets (CS_IBM01143) (3) := US ("CP01143");
   Character_Sets (CS_IBM01143) (4) := US ("ebcdic-fi-278+euro");
   Character_Sets (CS_IBM01143) (5) := US ("ebcdic-se-278+euro");

   Character_Sets (CS_IBM01144) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM01144) (1) := US ("IBM01144");
   Character_Sets (CS_IBM01144) (2) := US ("CCSID01144");
   Character_Sets (CS_IBM01144) (3) := US ("CP01144");
   Character_Sets (CS_IBM01144) (4) := US ("ebcdic-it-280+euro");

   Character_Sets (CS_IBM01145) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM01145) (1) := US ("IBM01145");
   Character_Sets (CS_IBM01145) (2) := US ("CCSID01145");
   Character_Sets (CS_IBM01145) (3) := US ("CP01145");
   Character_Sets (CS_IBM01145) (4) := US ("ebcdic-es-284+euro");

   Character_Sets (CS_IBM01146) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM01146) (1) := US ("IBM01146");
   Character_Sets (CS_IBM01146) (2) := US ("CCSID01146");
   Character_Sets (CS_IBM01146) (3) := US ("CP01146");
   Character_Sets (CS_IBM01146) (4) := US ("ebcdic-gb-285+euro");

   Character_Sets (CS_IBM01147) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM01147) (1) := US ("IBM01147");
   Character_Sets (CS_IBM01147) (2) := US ("CCSID01147");
   Character_Sets (CS_IBM01147) (3) := US ("CP01147");
   Character_Sets (CS_IBM01147) (4) := US ("ebcdic-fr-297+euro");

   Character_Sets (CS_IBM01148) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM01148) (1) := US ("IBM01148");
   Character_Sets (CS_IBM01148) (2) := US ("CCSID01148");
   Character_Sets (CS_IBM01148) (3) := US ("CP01148");
   Character_Sets (CS_IBM01148) (4) := US ("ebcdic-international-500+euro");

   Character_Sets (CS_IBM01149) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_IBM01149) (1) := US ("IBM01149");
   Character_Sets (CS_IBM01149) (2) := US ("CCSID01149");
   Character_Sets (CS_IBM01149) (3) := US ("CP01149");
   Character_Sets (CS_IBM01149) (4) := US ("ebcdic-is-871+euro");

   Character_Sets (CS_Big5_HKSCS) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_Big5_HKSCS) (1) := US ("Big5-HKSCS");

   Character_Sets (CS_IBM1047) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_IBM1047) (1) := US ("IBM1047");
   Character_Sets (CS_IBM1047) (2) := US ("IBM-1047");

   Character_Sets (CS_PTCP154) := new Set_Name_List (1 .. 5);
   Character_Sets (CS_PTCP154) (1) := US ("PTCP154");
   Character_Sets (CS_PTCP154) (2) := US ("cs44PTCP154");
   Character_Sets (CS_PTCP154) (3) := US ("PT154");
   Character_Sets (CS_PTCP154) (4) := US ("CP154");
   Character_Sets (CS_PTCP154) (5) := US ("Cyrillic-Asian");

   Character_Sets (CS_Amiga_1251) := new Set_Name_List (1 .. 4);
   Character_Sets (CS_Amiga_1251) (1) := US ("Amiga-1251");
   Character_Sets (CS_Amiga_1251) (2) := US ("Ami1251");
   Character_Sets (CS_Amiga_1251) (3) := US ("Amiga1251");
   Character_Sets (CS_Amiga_1251) (4) := US ("Ami-1251");

   Character_Sets (CS_KOI7_Switched) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_KOI7_Switched) (1) := US ("KOI7-switched");

   Character_Sets (CS_BRF) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_BRF) (1) := US ("BRF");
   Character_Sets (CS_BRF) (2) := US ("csBRF");

   Character_Sets (CS_TSCII) := new Set_Name_List (1 .. 2);
   Character_Sets (CS_TSCII) (1) := US ("TSCII");
   Character_Sets (CS_TSCII) (2) := US ("csTSCII");

   Character_Sets (CS_Windows_1250) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_Windows_1250) (1) := US ("windows-1250");

   Character_Sets (CS_Windows_1251) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_Windows_1251) (1) := US ("windows-1251");

   Character_Sets (CS_Windows_1252) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_Windows_1252) (1) := US ("windows-1252");

   Character_Sets (CS_Windows_1253) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_Windows_1253) (1) := US ("windows-1253");

   Character_Sets (CS_Windows_1254) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_Windows_1254) (1) := US ("windows-1254");

   Character_Sets (CS_Windows_1255) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_Windows_1255) (1) := US ("windows-1255");

   Character_Sets (CS_Windows_1256) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_Windows_1256) (1) := US ("windows-1256");

   Character_Sets (CS_Windows_1257) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_Windows_1257) (1) := US ("windows-1257");

   Character_Sets (CS_Windows_1258) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_Windows_1258) (1) := US ("windows-1258");

   Character_Sets (CS_TIS_620) := new Set_Name_List (1 .. 1);
   Character_Sets (CS_TIS_620) (1) := US ("TIS-620");


end Basil.Character_Set_Translation;
