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
--  This file contains the library-defined exceptions and the
--  enumerations for content types, content transfer encodings, MIME
--  types, and character sets that are used throughout the library.
--
-------------------------------------------------------------------------------

with Basil_Generated.Application_Subtypes;
with Basil_Generated.Audio_Subtypes;
with Basil_Generated.Image_Subtypes;
with Basil_Generated.Message_Subtypes;
with Basil_Generated.Model_Subtypes;
with Basil_Generated.Multipart_Subtypes;
with Basil_Generated.Text_Subtypes;
with Basil_Generated.Video_Subtypes;

package Basil is

   pragma Pure;

   ----------------------------------------------------------------------------
   --  Exceptions specified for Basil.MIME.Content_Type subprograms
   --  from MRCY.SPC.002
   Invalid_Message_Or_Entity : exception;
   Invalid_Content_Type      : exception;
   Invalid_Encoding          : exception;
   Invalid_Character_Set     : exception;
   Invalid_Delimiter         : exception;
   Invalid_Header            : exception;
   Invalid_Character         : exception;
   Invalid_Quoting           : exception;
   Illegal_Child_Operation   : exception;
   Premature_End             : exception;
   Parse_Error               : exception;

   --  This exception is for abstract procedures (That must be
   --  overloaded) of non-abstract types (such that the 'abstract'
   --  keyword can't be used).
   Derivatives_Must_Overload : exception;

   --  This exception will only be raised by 'can't happen'
   --  situations caused by flaws in the library.
   Mea_Culpa                 : exception;

   --  This type is defined to provide counts of children of messages
   --  and MIME entities.
   type Count_Children_Type is range 0 .. 2**31 - 1;

   --  This type is defined to allow selection of a strategy for
   --  cursors used to traverse children of messages and MIME
   --  entities.
   type Cursor_Strategy is
     (Strategy_Simple,
      Strategy_Recursive);

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   -- ENUMERATION DEFINITIONS
   --
   -- These are the enumerations required by MRCY.SPC.002
   -- They consist of:
   --
   --   * Primary_Content_Type for identification of the MIME content types.
   --   * Content_Transfer_Encoding_Id for definition of the content transfer
   --       encodings specified by RFC 2047.
   --   * The nine MIME Content_Subtype enumerations for each of the MIME
   --       types.
   --   * Character_Set_Id for identification of the IANA recognized
   --       character sets.
   type Primary_Content_Type is
     (T_Application,
      T_Audio,
      T_Experimental,
      T_Image,
      T_Message,
      T_Model,
      T_Multipart,
      T_Text,
      T_Unknown,
      T_Video);

   --------------------------------------------------------------------------
   --  RFC 2047 permits but STRONGLY DISCOURAGES the creation of
   --  experimental content transfer encodings.
   type Content_Transfer_Encoding_ID is
     (CTE_7bit,
      CTE_8bit,
      CTE_Binary,
      CTE_Quoted_Printable,
      CTE_Base64,
      CTE_Unknown,
      CTE_Experimental);

   ------------------------------------------------------------------------
   --  These are the subtype enumeration definitions.
   --
   --  ST_Experimental exists in each subtype to support x- style
   --  experimental subtypes, which should be treated by users of this
   --  library as application/binary per RFC 2047. ST_Unknown exists
   --  to permit handling of content types which do not have IANA
   --  designation but which also do not begin with "x-" or "X-" (a
   --  violation of the standards).
   type Application_Content_Subtype is
     new Basil_Generated.Application_Subtypes.Application_Content_Subtype;

   type Audio_Content_Subtype is
     new Basil_Generated.Audio_Subtypes.Audio_Content_Subtype;

   --  Per MRCY-SPC-002 this enumeration is defined for MIME types
   --  beginning with X- or x-. Per RFC 2045, users of this package
   --  should treat unknown encoding types as application/binary.
   type Experimental_Content_Subtype is
     (ST_Experimental,
      ST_Unknown);

   type Image_Content_Subtype is
     new Basil_Generated.Image_Subtypes.Image_Content_Subtype;

   type Message_Content_Subtype is
     new Basil_Generated.Image_Subtypes.Image_Content_Subtype;

   type Model_Content_Subtype is
     new Basil_Generated.Model_Subtypes.Model_Content_Subtype;

   type Multipart_Content_Subtype is
     new Basil_Generated.Multipart_Subtypes.Multipart_Content_Subtype;

   type Text_Content_Subtype is
     new Basil_Generated.Text_Subtypes.Text_Content_Subtype;

   --  Types will end up here if they violate the rule from RFC 2047
   --  that encodings MUST be registered with IANA or begin with "X-"
   --  or "x-", or because they are using an IANA registered MIME type
   --  of which Basil is not aware for some reason.
   type Unknown_Content_Subtype is
     (ST_Unknown);

   type Video_Content_Subtype is
     new Basil_Generated.Video_Subtypes.Video_Content_Subtype;

   ----------------------------------------------------------------------------
   --  This is an enumeration of all the character sets from the IANA
   --  at http://www.iana.org/assignments/character-sets
   --
   --  Only a few of these sets are actually supported by the Unicode
   --  translation routines of this library in Basil.Strings.
   --
   type Character_Set_ID is
     -- Basil Name                       -- IANA Name
     --------------------------------------------------------------------------
     (CS_US_ASCII,                       -- ANSI_X3.4-1968
      CS_ISO_8859_1,                     -- ISO_8859-1:1987
      CS_ISO_8859_2,                     -- ISO_8859-2:1987
      CS_ISO_8859_3,                     -- ISO_8859-3:1984
      CS_ISO_8859_4,                     -- ISO_8859-4:1988
      CS_ISO_8859_5,                     -- ISO_8859-5:1988
      CS_ISO_8859_6,                     -- ISO_8859-6:1987
      CS_ISO_8859_7,                     -- ISO_8859-7:1987
      CS_ISO_8859_8,                     -- ISO_8859-8:1988
      CS_ISO_8859_9,                     -- ISO_8859-9:1989
      CS_ISO_8859_10,                    -- ISO_8859-10
      CS_ISO_6937_2_Add,                 -- ISO_6937-2-add
      CS_Jis_X0201,                      -- JIS_X0201
      CS_Jis_Encoding,                   -- JIS_Encoding
      CS_Shift_Jis,                      -- Shift JIS
      CS_Euc_Jp,                         -- Extended_UNIX_Code_Packed_Format...
      CS_EUCFixWidJapanese,              -- Extended_UNIX_Code_Fixed_Width_F...
      CS_Bs_4730,                        -- BS_4730
      CS_SEN_850200_C,                   -- SEN_850200_C
      CS_IT,                             -- IT
      CS_Es,                             -- ES
      CS_DIN_66003,                      -- DIN_66003
      CS_NS_4551_1,                      -- NS_4551-1
      CS_NF_Z_62_010,                    -- NF_Z_62-010
      CS_ISO_10646_UTF_1,                -- ISO-10646-UTF-1
      CS_ISO_646_Basic,                  -- ISO_646.basic:1983
      CS_INVARIANT,                      -- INVARIANT
      CS_ISO_646_Irv,                    -- ISO_646.irv:1983
      CS_NATS_SEFI,                      -- NATS-SEFI
      CS_NATS_SEFI_ADD,                  -- NATS-SEFI-ADD
      CS_NATS_DANO,                      -- NATS-DANO
      CS_NATS_DANO_ADD,                  -- NATS-DANO-ADD
      CS_SEN_850200_B,                   -- SEN_850200_B
      CS_KS_C_5601,                      -- KS_C_5601-1987
      CS_ISO_2022_KR,                    -- ISO_2022_KR
      CS_EUC_KR,                         -- EUC_KR
      CS_ISO_2022_JP,                    -- ISO_2022_JP
      CS_ISO_2022_JP_2,                  -- ISO-2022-JP-2
      CS_JIS_C6220_1969_Jp,              -- JIS_C6220-1969-jp
      CS_JIS_C6220_1969_Ro,              -- JIS_C6220-1969-ro
      CS_PT,                             -- PT
      CS_Greek7_Old,                     -- greek7-old
      CS_Latin_Greek,                    -- Latin-greek
      CS_NF_Z_62_010_1973,               -- NF_Z_62-010_(1973)
      CS_Latin_Greek_1,                  -- Latin-greek-1
      CS_ISO_5427,                       -- ISO_5427
      CS_JIS_C6226_1978,                 -- JIS_C6226-1978
      CS_BS_Viewdata,                    -- BS_Viewdata
      CS_INIS,                           -- INIS
      CS_INIS_8,                         -- INIS-8
      CS_INIS_Cyrillic,                  -- INIS-Cyrillic
      CS_ISO_5427_1981,                  -- ISO_5427_1981
      CS_ISO_5428_1980,                  -- ISO_5428:1980
      CS_GB_1988_80,                     -- GB_1988-80
      CS_GB_2312_80,                     -- GB_2312-80
      CS_NS_4551_2,                      -- NS_4551-2
      CS_Videotex_Suppl,                 -- videotex-suppl
      CS_PT2,                            -- PT2
      CS_ES2,                            -- ES2
      CS_MSZ_7795_3,                     -- MSZ_7795.3
      CS_JIS_C6226_1983,                 -- JIS_C6226-1983
      CS_Greek7,                         -- greek7
      CS_ASMO_449,                       -- ASMO_449
      CS_Iso_Ir_90,                      -- iso-ir-90
      CS_JIS_C6229_1984_A,               -- JIS_C6229-1984-a
      CS_JIS_C6229_1984_B,               -- JIS_C6229-1984-b
      CS_JIS_C6229_1984_B_Add,           -- JIS_C6229-1984-b-add
      CS_JIS_C6229_1984_Hand,            -- JIS_C6229-1984-hand
      CS_JIS_C6229_1984_Hand_Add,        -- JIS_C6229-1984-hand-add
      CS_JIS_C6229_1984_Kana,            -- JIS_C6229-1984-kana
      CS_ISO_2033_1983,                  -- ISO_2033-1983
      CS_ANSI_X3_110_1983,               -- ANSI_X3.110-1983
      CS_T_61_7BIT,                      -- T.61-7bit
      CS_T_61_8BIT,                      -- T.61-8bit
      CS_ECMA_Cyrillic,                  -- ECMA-cyrillic
      CS_CSA_Z243_4_1985_1,              -- CSA_Z243.4-1985-1
      CS_CSA_Z243_4_1985_2,              -- CSA_Z243.4-1985-2
      CS_CSA_Z243_4_1985_Gr,             -- CSA_Z243.4-1985-gr
      CS_ISO_8859_6_E,                   -- ISO_8859-6-E
      CS_ISO_8859_6_I,                   -- ISO_8859-6-I
      CS_T_101_G2,                       -- T.101-G2
      CS_ISO_8859_8_E,                   -- ISO_8859-8-E
      CS_ISO_8859_8_I,                   -- ISO_8859-8-I
      CS_CSN_369103,                     -- CSN_369103
      CS_JUS_I_B1_002,                   -- JUS_I.B1.002
      CS_IEC_P27_1,                      -- IEC_P27-1
      CS_JUS_I_B1_003_Serb,              -- JUS_I.B1.003-serb
      CS_JUS_I_B1_003_Mac,               -- JUS_I.B1.003-mac
      CS_Greek_Ccitt,                    -- greek-ccitt
      CS_NC_NC00_10_81,                  -- NC_NC00-10:81
      CS_ISO_6937_2_25,                  -- ISO_6937-2-25
      CS_GOST_19768_74,                  -- GOST_19768-74
      CS_ISO_8859_SUPP,                  -- ISO_8859-supp
      CS_ISO_10367_BOX,                  -- ISO_10367-box
      CS_Latin_Lap,                      -- latin-lap
      CS_JIS_X0212_1990,                 -- JIS_X0212-1990
      CS_DS_2089,                        -- DS_2089
      CS_Us_Dk,                          -- us-dk
      CS_Dk_Us,                          -- dk-us
      CS_KSC5636,                        -- KSC5636
      CS_UNICODE_1_1_UTF_7,              -- UNICODE-1-1-UTF-7
      CS_ISO_2022_CN,                    -- ISO-2022-CN
      CS_ISO_2022_CN_EXT,                -- ISO-2022-CN-EXT
      CS_UTF_8,                          -- UTF-8
      CS_ISO_8859_13,                    -- ISO-8859-13
      CS_ISO_8859_14,                    -- ISO-8859-14
      CS_ISO_8859_15,                    -- ISO-8859-15
      CS_ISO_8859_16,                    -- ISO-8859-16
      CS_GBK,                            -- GBK
      CS_GB18030,                        -- GB18030
      CS_OSD_EBCDIC_DF04_15,             -- OSD_EBCDIC_DF04_15
      CS_OSD_EBCDIC_DF03_IRV,            -- OSD_EBCDIC_DF03_IRV
      CS_OSD_EBCDIC_DF04_1,              -- OSD_EBCDIC_DF04_1
      CS_ISO_11548_1,                    -- ISO-11548-1
      CS_KZ_1048,                        -- KZ-1048
      CS_ISO_10646_UCS_2,                -- ISO-10646-UCS-2
      CS_ISO_10646_UCS_4,                -- ISO-10646-UCS-4
      CS_ISO_10646_UCS_Basic,            -- ISO-10646-UCS-Basic
      CS_ISO_10646_Unicode_Latin1,       -- ISO-10646-Unicode-Latin1
      CS_ISO_10646_J_1,                  -- ISO-10646-J-1
      CS_ISO_UNICODE_IBM_1261,           -- ISO-Unicode-IBM-1261
      CS_ISO_UNICODE_IBM_1268,           -- ISO-Unicode-IBM-1268
      CS_ISO_UNICODE_IBM_1276,           -- ISO-Unicode-IBM-1276
      CS_ISO_UNICODE_IBM_1264,           -- ISO-Unicode-IBM-1264
      CS_ISO_UNICODE_IBM_1265,           -- ISO-Unicode-IBM-1265
      CS_UNICODE_1_1,                    -- UNICODE-1-1
      CS_SCSU,                           -- SCSU
      CS_UTF_7,                          -- UTF-7
      CS_UTF_16BE,                       -- UTF-16BE
      CS_UTF_16LE,                       -- UTF-16LE
      CS_UTF_16,                         -- UTF-16
      CS_CESU_8,                         -- CESU-8
      CS_UTF_32,                         -- UTF-32
      CS_UTF_32BE,                       -- UTF-32BE
      CS_UTF_32LE,                       -- UTF-32LE
      CS_BOCU_1,                         -- BOCU-1
      CS_ISO_8859_1_Windows_3_0_Latin_1, -- ISO-8859-1-Windows-3.0-Latin-1
      CS_ISO_8859_1_Windows_3_1_Latin_1, -- ISO-8859-1-Windows-3.1-Latin-1
      CS_ISO_8859_2_Windows_Latin_2,     -- ISO-8859-2-Windows-Latin-2
      CS_ISO_8859_9_Windows_Latin_5,     -- ISO-8859-9-Windows-Latin-5
      CS_Hp_Roman8,                      -- hp-roman8
      CS_Adobe_Standard_Encoding,        -- Adobe-Standard-Encoding
      CS_Ventura_US,                     -- Ventura-US
      CS_Ventura_International,          -- Ventura-International
      CS_DEC_MCS,                        -- DEC-MCS
      CS_IBM850,                         -- IBM850
      CS_Pc8_Danish_Norwegian,           -- PC8-Danish-Norwegian
      CS_IBM862,                         -- IBM862
      CS_PC8_Turkish,                    -- PC8-Turkish
      CS_IBM_Symbols,                    -- IBM-Symbols
      CS_IBM_Thai,                       -- IBM-Thai
      CS_HP_Legal,                       -- HP-Legal
      CS_HP_Pi_Font,                     -- HP-Pi-font
      CS_HP_Math8,                       -- HP-Math8
      CS_Adobe_Symbol_Encoding,          -- Adobe-Symbol-Encoding
      CS_HP_DeskTop,                     -- HP-DeskTop
      CS_Ventura_Math,                   -- Ventura-Math
      CS_Microsoft_Publishing,           -- Microsoft-Publishing
      CS_Windows_31j,                    -- Windows-31J
      CS_GB2312,                         -- GB2312
      CS_Big5,                           -- Big5
      CS_Macintosh,                      -- macintosh
      CS_IBM037,                         -- IBM037
      CS_IBM038,                         -- IBM038
      CS_IBM273,                         -- IBM273
      CS_IBM274,                         -- IBM274
      CS_IBM275,                         -- IBM275
      CS_IBM277,                         -- IBM277
      CS_IBM278,                         -- IBM278
      CS_IBM280,                         -- IBM280
      CS_IBM281,                         -- IBM281
      CS_IBM284,                         -- IBM284
      CS_IBM285,                         -- IBM285
      CS_IBM290,                         -- IBM290
      CS_IBM297,                         -- IBM297
      CS_IBM420,                         -- IBM420
      CS_IBM423,                         -- IBM423
      CS_IBM424,                         -- IBM424
      CS_IBM437,                         -- IBM437
      CS_IBM500,                         -- IBM500
      CS_IBM851,                         -- IBM851
      CS_IBM852,                         -- IBM852
      CS_IBM855,                         -- IBM855
      CS_IBM857,                         -- IBM857
      CS_IBM860,                         -- IBM860
      CS_IBM861,                         -- IBM861
      CS_IBM863,                         -- IBM863
      CS_IBM864,                         -- IBM864
      CS_IBM865,                         -- IBM865
      CS_IBM868,                         -- IBM868
      CS_IBM869,                         -- IBM869
      CS_IBM870,                         -- IBM870
      CS_IBM871,                         -- IBM871
      CS_IBM880,                         -- IBM880
      CS_IBM891,                         -- IBM891
      CS_IBM903,                         -- IBM903
      CS_IBM904,                         -- IBM904
      CS_IBM905,                         -- IBM905
      CS_IBM918,                         -- IBM918
      CS_IBM1026,                        -- IBM1026
      CS_EBCDIC_AT_DE,                   -- EBCDIC-AT-DE
      CS_EBCDIC_AT_DE_A,                 -- EBCDIC-AT-DE-A
      CS_EBCDIC_CA_FR,                   -- EBCDIC-CA-FR
      CS_EBCDIC_DK_NO,                   -- EBCDIC-DK-NO
      CS_EBCDIC_DK_NO_A,                 -- EBCDIC-DK-NO-A
      CS_EBCDIC_FI_SE,                   -- EBCDIC-FI-SE
      CS_EBCDIC_FI_SE_A,                 -- EBCDIC-FI-SE-A
      CS_EBCDIC_FR,                      -- EBCDIC-FR
      CS_EBCDIC_IT,                      -- EBCDIC-IT
      CS_EBCDIC_PT,                      -- EBCDIC-PT
      CS_EBCDIC_ES,                      -- EBCDIC-ES
      CS_EBCDIC_ES_A,                    -- EBCDIC-ES-A
      CS_EBCDIC_ES_S,                    -- EBCDIC-ES-S
      CS_EBCDIC_UK,                      -- EBCDIC-UK
      CS_EBCDIC_US,                      -- EBCDIC-US
      CS_Unknown,                        -- Basil Code for Unrecognized Chsets
      CS_UNKNOWN_8BIT,                   -- UNKNOWN-8BIT
      CS_MNEMONIC,                       -- MNEMONIC
      CS_MNEM,                           -- MNEM
      CS_VISCII,                         -- VISCII
      CS_VIQR,                           -- VIQR
      CS_KOI8_R,                         -- KOI8-R
      CS_HZ_GB_2312,                     -- HZ-GB-2312
      CS_IBM866,                         -- IBM866
      CS_IBM775,                         -- IBM775
      CS_KOI8_U,                         -- KOI8-U
      CS_IBM00858,                       -- IBM00858
      CS_IBM00924,                       -- IBM00924
      CS_IBM01140,                       -- IBM01140
      CS_IBM01141,                       -- IBM01141
      CS_IBM01142,                       -- IBM01142
      CS_IBM01143,                       -- IBM01143
      CS_IBM01144,                       -- IBM01144
      CS_IBM01145,                       -- IBM01145
      CS_IBM01146,                       -- IBM01146
      CS_IBM01147,                       -- IBM01147
      CS_IBM01148,                       -- IBM01148
      CS_IBM01149,                       -- IBM01149
      CS_Big5_HKSCS,                     -- Big5-HKSCS
      CS_IBM1047,                        -- IBM1047
      CS_PTCP154,                        -- PTCP154
      CS_Amiga_1251,                     -- Amiga-1251
      CS_KOI7_Switched,                  -- KOI7-switched
      CS_BRF,                            -- BRF
      CS_TSCII,                          -- TSCII
      CS_Windows_1250,                   -- windows-1250
      CS_Windows_1251,                   -- windows-1251
      CS_Windows_1252,                   -- windows-1252
      CS_Windows_1253,                   -- windows-1253
      CS_Windows_1254,                   -- windows-1254
      CS_Windows_1255,                   -- windows-1255
      CS_Windows_1256,                   -- windows-1256
      CS_Windows_1257,                   -- windows-1257
      CS_Windows_1258,                   -- windows-1258
      CS_TIS_620);                       -- TIS-620

end Basil;
