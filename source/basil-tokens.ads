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
--   This package provides message lexical analysis by providing
--    1) A set of relevent character maps.
--    2) A procedure to identify a token consisting of a given character
--         set from the beginning of a string.
--    3) A 'token' object which can be made into a list
--    4) A set of routines to consume various types of message
--         components into lists of token objects.
--
-------------------------------------------------------------------------------

with Ada.Strings.Maps;            use Ada.Strings.Maps;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

package Basil.Tokens is

   --  Definition of whitespace that includes \t, \n, \r, etc.
   --  Used for "Best Effort" matching. For a stricter definition
   --  use RFC2234_Whitespace
   Whitespace                : constant Character_Set;
   Newlines                  : constant Character_Set;

   --  Characters that should be quoted in Quoted-Printable text
   --  according to RFC 2045.
   Requires_Quoting          : constant Character_Set;

   --  The set of characters with the high bit set in Latin 1 and we
   --  consider 'printable.' This excludes all US-ASCII and the
   --  NO-BREAK SPACE (16#A0#);
   Highbit_Printable         : constant Character_Set;

   --  The standards make frequrent reference to these primitave
   --  constructs defined in RFC 2234
   RFC2234_Alphas            : constant Character_Set;
   RFC2234_Digits            : constant Character_Set;
   RFC2234_Controls          : constant Character_Set;
   RFC2234_Hexdigits         : constant Character_Set;
   RFC2234_Visibles          : constant Character_Set;
   RFC2234_Whitespace        : constant Character_Set;

   --  RFC 2045 set definitions
   RFC2045_Tspecials         : constant Character_Set;
   RFC2045_Token             : constant Character_Set;

   --  RFC 2047 header encapsulation definitions
   RFC2047_Especials         : constant Character_Set;
   RFC2047_Token             : constant Character_Set;
   RFC2047_Encoded_Text      : constant Character_Set;

   --  RFC 2822 set definitions
   RFC2822_No_WS_Control     : constant Character_Set;
   RFC2822_Text              : constant Character_Set;
   RFC2822_Specials          : constant Character_Set;
   RFC2822_Atom_Text         : constant Character_Set;
   RFC2822_Quoted_Text       : constant Character_Set;
   RFC2822_Domain_Text       : constant Character_Set;


   ----------------------------------------------------------------------------
   --  This procedure takes a source string and an index of the first
   --  character in the token. It returns the index of the last
   --  CONTIGUOUS character in the string which matches the set, or
   --  zero if the first character does not match.
   --
   --  If quoted is True the function will consider quoted pairs
   --  ('\' + char) as being part of the token.
   function Get_Token_End (Source : in String;
                           First  : in Positive;
                           Set    : in Character_Set;
                           Quoted : in Boolean := False)
                          return Natural;

   --------------------------------------------------------------------------
   --  This procedure provides the common pattern of extracting a
   --  token from the front of an unbounded string and then reduce the
   --  size of the string by deleting that token from it.
   --
   --  If quoted is True the function will consider quoted pairs
   --  ('\' + char) as being part of the token.
   procedure Get_Token (Source : in out Unbounded_String;
                        Token  :    out Unbounded_String;
                        Set    : in     Character_Set;
                        Quoted : in     Boolean := False);

   ----------------------------------------------------------------------------
   --  This procedure provides the common pattern of removing an
   --  unwanted token from the front of an unbounded string.
   procedure Discard_Token (Source : in out Unbounded_String;
                            Set    : in     Character_Set;
                            Quoted : in     Boolean := False);

private

   package L renames Ada.Characters.Latin_1;

   ----------------------------------------------------------------------
   --  This, our definition of whitespace, is more expansive than in
   --  the standard and is used for "Best Effort" parsing when finding
   --  word boundaries or eating whitespace. For other uses of
   --  whitespace use RFC2234_Whitespace
   Whitespace                   : constant Character_Set := To_Set
     (Ranges =>
        ((L.HT                  , L.CR                   ), -- "\t\n\v\f\r"
         (L.Space               , L.Space                )));

   ----------------------------------------------------------------------------
   --  This is our definition but it also matches RFC 2234
   Newlines                     : constant Character_Set := To_Set
     (Ranges =>
        ((L.LF                  , L.LF                   ),   -- "\n"
         (L.CR                  , L.CR                   ))); -- "\r"

   ----------------------------------------------------------------------------
   --  See RFC 2045 p. 19 Section 6.7.1
   Requires_Quoting             : constant Character_Set := To_Set
     (Ranges =>
        ((L.NUL                 , L.US                   ),
         (L.Equals_Sign         , L.Equals_Sign          ),
         (L.DEL                 , Character'Val (255)    )));

   ----------------------------------------------------------------------------
   --  Everything > US-ASCII except the NO-BREAK SPACE
   Highbit_Printable            : constant Character_Set := To_Set
     (Span =>
        (L.Inverted_Exclamation , L.LC_Y_Diaeresis       ));

   ----------------------------------------------------------------------------
   --  See RFC 2234 p 11
   --
   --  Hex digits are used for most of these definitions to match RFC 2234.
   RFC2234_Alphas               : constant Character_Set := To_Set
     (Ranges =>
        ((Character'Val(16#41#) , Character'Val(16#5A#)  ),   -- A through Z
         (Character'Val(16#61#) , Character'Val(16#7A#)  ))); -- a through z

   ----------------------------------------------------------------------------
   RFC2234_Digits               : constant Character_Set := To_Set
     (Span =>
        (Character'Val(16#30#)  , Character'Val(16#39#)  ));  -- 0 through 9

   ----------------------------------------------------------------------------
   RFC2234_Controls             : constant Character_Set := To_Set
     (Span =>
        (Character'Val(16#00#)  , Character'Val(16#1F#)  ));  -- \0 through US

   ----------------------------------------------------------------------------
   RFC2234_Hexdigits            : constant Character_Set :=
     RFC2234_Digits or To_Set("ABCDEF");

   ----------------------------------------------------------------------------
   --  See RFC 2234 p 12
   RFC2234_Visibles             : constant Character_Set := To_Set
     (Span =>
        (Character'Val(16#21#)  , Character'Val(16#7E#)  ));  -- ! through ~

   ----------------------------------------------------------------------------
   RFC2234_Whitespace           : constant Character_Set := To_Set
     (Ranges =>
        ((Character'Val(16#09#) , Character'Val(16#09#)  ),   -- "\t"
         (Character'Val(16#20#) , Character'Val(16#20#)  ))); -- " "

   ----------------------------------------------------------------------------
   --  See RFC 2047 p. 4
   --
   --  Underscores '_' are not listed as especials by RFC 2047, but
   --  later in the document it states that underscores in quoted text
   --  should be treated as spaces, so to preserve an underscore as
   --  such it must be encoded. This is as near as I can tell a bug in
   --  the standard, and this character set has '_' defined as an
   --  especial to ensure preservation of that character.
   RFC2045_Tspecials            : constant Character_Set := To_Set
     (Ranges =>
        ((L.Left_Parenthesis    , L.Right_Parenthesis    ),   -- "()"
         (L.Quotation           , L.Quotation            ),   -- '"'
         (L.Comma               , L.Comma                ),   -- ","
         (L.Solidus             , L.Solidus              ),   -- "/"
         (L.Colon               , L.Commercial_At        ),   -- ":;<=>?@"
         (L.Left_Square_Bracket , L.Right_Square_Bracket ))); -- "[\]"

   ----------------------------------------------------------------------------
   --  Per the spec "1*<Any CHAR except SPACE, CTLs, and especials>"
   RFC2045_Token                : constant Character_Set :=
     (not RFC2234_Controls) and (not Whitespace) and (not RFC2045_Tspecials);

   ----------------------------------------------------------------------------
   --  See RFC 2047 p. 4
   --
   --  Underscores '_' are not listed as especials by RFC 2047, but
   --  later in the document it states that underscores in quoted text
   --  should be treated as spaces, so to preserve an underscore as
   --  such it must be encoded. This is as near as I can tell a bug in
   --  the standard, and this character set has '_' defined as an
   --  especial to ensure preservation of that character.
   RFC2047_Especials            : constant Character_Set := To_Set
     (Ranges =>
        ((L.Left_Parenthesis    , L.Right_Parenthesis    ),   -- "()"
         (L.Quotation           , L.Quotation            ),   -- '"'
         (L.Comma               , L.Comma                ),   -- ","
         (L.Full_Stop           , L.Solidus              ),   -- "./"
         (L.Colon               , L.Commercial_At        ),   -- ":;<=>?@"
         (L.Left_Square_Bracket , L.Left_Square_Bracket  ),   -- "["
         (L.Right_Square_Bracket, L.Right_Square_Bracket ),   -- "]"
         (L.Low_Line            , L.Low_Line             ))); -- "_"

   ----------------------------------------------------------------------------
   --  Per the spec "1*<Any CHAR except SPACE, CTLs, and especials>"
   RFC2047_Token                : constant Character_Set :=
     (not Requires_Quoting) and (not Whitespace) and (not RFC2047_Especials);

   ----------------------------------------------------------------------------
   --  Per the spec "1*<Any printable ASCII character other than "?" or Space"
   RFC2047_Encoded_Text         : constant Character_Set := To_Set
     (Ranges =>
        ((L.Exclamation         , L.Greater_Than_Sign    ),   -- ! through >
         (L.Commercial_At       , L.Tilde                ))); -- @ through ~

   ----------------------------------------------------------------------------
   --  See RFC 2822 p 10 (We include illegal chars NUL and >127 as CTLs)
   --    to support MRCY.SPC.002's requirement that we handle high bits.
   --  These are defined with decmels to match RFC 2822's definition.
   RFC2822_No_WS_Control        : constant Character_Set := To_Set
     (Ranges =>
        ((Character'Val(0)      , Character'Val(8)       ),   -- \0 through \b
         (Character'Val(11)     , Character'Val(12)      ),   -- \v through \f
         (Character'Val(14)     , Character'Val(31)      ),   -- SO through US
         (Character'Val(127)    , Character'Val(255)     ))); -- DEL and up

   ----------------------------------------------------------------------------
   --  Everything except \n and \r.
   RFC2822_Text                 : constant Character_Set := To_Set
     (Ranges =>
        ((Character'Val(1)      , Character'Val(9)       ),   -- SOH through \t
         (Character'Val(11)     , Character'Val(12)      ),   -- \v through \f
         (Character'Val(14)     , Character'Val(127)     ))); -- SO through DEL

   ----------------------------------------------------------------------------
   --  See RFC 2822 p 10
   RFC2822_Specials             : constant Character_Set := To_Set
     (Ranges =>
        ((L.Left_Parenthesis    , L.Right_Parenthesis    ),   -- "()"
         (L.Quotation           , L.Quotation            ),   -- '"'
         (L.Comma               , L.Comma                ),   -- ","
         (L.Full_Stop           , L.Full_Stop            ),   -- "."
         (L.Colon               , L.Less_Than_Sign       ),   -- ":;<"
         (L.Greater_Than_Sign   , L.Greater_Than_Sign    ),   -- ">"
         (L.Commercial_At       , L.Commercial_At        ),   -- "@"
         (L.Left_Square_Bracket , L.Right_Square_Bracket ))); -- "[\]"

   ----------------------------------------------------------------------------
   RFC2822_Atom_Text            : constant Character_Set := To_Set
     (Ranges =>
        ((L.Exclamation         , L.Exclamation          ),   -- "!"
         (L.Number_Sign         , L.Apostrophe           ),   -- "#$%&'"
         (L.Asterisk            , L.Plus_Sign            ),   -- "*+"
         (L.Hyphen              , L.Hyphen               ),   -- "-"
         (L.Solidus             , L.Solidus              ),   -- "/"
         (L.Equals_Sign         , L.Equals_Sign          ),   -- "="
         (L.Question            , L.Question             ),   -- "?"
         (L.Circumflex          , L.Low_Line             ),   -- "^_"
         (L.Grave               , L.Grave                ),   -- "`"
         (L.Left_Curly_Bracket  , L.Tilde                )))  -- "{|}~"
     or RFC2234_Alphas
     or RFC2234_Digits;

   ----------------------------------------------------------------------------
   RFC2822_Quoted_Text          : constant Character_Set := To_Set
     (Ranges =>
        ((Character'Val(33)     , Character'Val(33)      ),   -- "!"
         (Character'Val(35)     , Character'Val(91)      ),   -- # through [
         (Character'Val(93)     , Character'Val(126)     )))  -- ] through ~
     or RFC2822_No_WS_Control;

   ----------------------------------------------------------------------------
   --  RFC 2822 p 17
   RFC2822_Domain_Text          : constant Character_Set :=
     RFC2822_Quoted_Text and (not To_Set ("[\]"));

end Basil.Tokens;
