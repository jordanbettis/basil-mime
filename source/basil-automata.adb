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

with Basil.Utils;             use Basil.Utils;
with Basil.Tokens;            use Basil.Tokens;

with Ada.Strings.Maps;        use Ada.Strings.Maps;
with Ada.Characters.Latin_1;

with Ada.Text_IO;             use Ada.Text_IO;

package body Basil.Automata is

   --  These automations have at any one time a particular named
   --  state. The next input character is a reqest to transition to a
   --  different state. Depending on the rules of the particular
   --  automation, it may choose to either transition to that state or
   --  it may choose to stop in either an accept or a non-accept
   --  state.

   --  They terminate with either an Accept_Stop or Nonaccept_Stop and
   --  set the Index to the last position processed before the
   --  automation decided to stop.

   ----------------------------------------------------------------------------
   --  This state machine matches a header value. It is designed to
   --  match folded whitespace with CR, LF, or CRLF sequences. This is
   --  the transition table:
   --
   --              ----------- Requested Transition --------------
   --  State       Space     CR        LF       Character  Stop
   ---------------------------------------------------------------
   --  Start       Accept    Accept    Accept   Accept     NA_Stop
   --  Space       Accept    Accept    Accept   Accept     NA_Stop
   --  CR          Accept    A_Stop    Accept   A_Stop     A_Stop
   --  LF          Accept    A_Stop    A_Stop   A_Stop     A_Stop
   --  Character   Accept    Accept    Accept   Accept     NA_Stop
   --
   --  Note that the only thing that causes a Nonaccept_Stop is coming
   --  to the end of the input while not in an Accept_Stop state.
   procedure Header_Value (Source   : in     String;
                           Index    : in out Positive;
                           Accepted :    out Boolean);

   ----------------------------------------------------------------------------
   --  This state machine matches tokens we don't care about,
   --  including spaces and comments. This is a bit more complicated
   --  than typical automata because we have to deal with nested
   --  comments. I considered using a recursive parser for it but I
   --  think this is a lighter and more efficient way of handling it
   --  while still being robust.
   --
   --  In addition to our normal 'state' we also have a comment depth
   --  count. Certian events cause the counter to increment (cd+1) and
   --  some to decrement. We have two state tables, one for when the
   --  counter is 0 and one for when it is greater than 0. Note that
   --  it is only possible to be in start when CD = 0, and Comment,
   --  Character, and Escape states are only possible when CD > 0;
   --
   --  Comment_Depth = 0:
   --              ----------- Requested Transition ----------
   --  State       Space  Comment Uncomment Character Stop
   -----------------------------------------------------------
   --  Start       Accept A_CD+1  NA_Stop   NA_Stop   NA_Stop
   --  Space       Accept A_CD+1  A_Stop    A_Stop    A_Stop
   --  Uncomment   Accept A_CD+1  A_Stop    A_Stop    A_Stop
   --
   --  Comment_Depth > 0:
   --              ----------- Requested Transition -----------------
   --  State       Space  Comment Uncomment Character Escape  Stop
   ------------------------------------------------------------------
   --  Space       Accept A_CD+1  A_CD-1    Accept    Accept  NA_Stop
   --  Comment     Accept A_CD+1  A_CD-1    Accept    Accept  NA_Stop
   --  Uncomment   Accept A_CD+1  A_CD-1    Accept    Accept  NA_Stop
   --  Character   Accept A_CD+1  A_CD-1    Accept    Accept  NA_Stop
   --  Escape      Accept Char    Char      Accept    Char    NA_Stop
   --
   --  Note that coming to the end of input while Comment_Depth > 0
   --  always results in a Nonaccept_Stop
   procedure Uninteresting (Source   : in     String;
                            Index    : in out Positive;
                            Accepted :    out Boolean);

   ----------------------------------------------------------------------------
   --  This state machine matches a single newline sequence, \r, \n,
   --  or \r\n. The advantage over Basil.Tokens is that this one will
   --  match only one such sequence, rather than arbitrarily many.
   --
   --                    ---- Requested Transition -------
   --  State             Linefeed  Carriage-Return Stop
   -------------------------------------------------------
   --  Start             Accept    Accept          NA_Stop
   --  Carriage-Return   Accept    A_Stop          A_Stop
   --  Linefeed          A_Stop    A_Stop          A_Stop
   procedure Newline_Sequence (Source   : in     String;
                               Index    : in out Positive;
                               Accepted :    out Boolean);

   ----------------------------------------------------------------------------
   --  This state machine matches a dot-atom-text construct, as
   --  defined in RFC 2822 p 12. The dot-atom is simply an atom that
   --  contains dot characters '.' in its interior, but does not begin
   --  or end with a dot and contains no consecutive dots.
   --
   --  A stop request is caused by either the end of the string or a
   --  character that is not atom-text or a dot.
   --
   --                --- Requested Transition ----
   --  State         Atom-Text Dot          Stop
   -------------------------------------------------------
   --  Start         Accept    NA_Stop      NA_Stop
   --  Atom-Text     Accept    Accept       A_Stop
   --  Dot           Accept    NA_Stop      NA_Stop
   procedure Dot_Atom_Text (Source   : in     String;
                            Index    : in out Positive;
                            Accepted :    out Boolean);

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Token_End (Source     : in String;
                           First      : in Positive;
                           Automation : in Automation_ID)
                          return Natural is

      Accepted : Boolean;
      Index    : Natural := First;

   begin -- Get_Toke_End

      if First > Source'Last then
         return 0;
      end if;

      case Automation is
         when Header_Value =>
            Header_Value (Source     => Source,
                          Index      => Index,
                          Accepted   => Accepted);
         when Uninteresting =>
            Uninteresting (Source   => Source,
                           Index    => Index,
                           Accepted => Accepted);

         when Newline_Sequence =>
            Newline_Sequence (Source   => Source,
                              Index    => Index,
                              Accepted => Accepted);

         when Dot_Atom_Text =>
            Dot_Atom_Text (Source   => Source,
                           Index    => Index,
                           Accepted => Accepted);

      end case;

      if Accepted then
         return Index;
      else
         return 0;
      end if;

   end Get_Token_End;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Token (Source     : in out Unbounded_String;
                        Token      :    out Unbounded_String;
                        Automation : in     Automation_ID) is

      Index_End : Natural := Get_Token_End (S (Source),
                                            First      => 1,
                                            Automation => Automation);

   begin -- Get_Token

      Token := Head (Source, Index_End);

      Tail (Source, Length (Source) - Index_End);

   end Get_Token;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Discard_Token (Source     : in out Unbounded_String;
                            Automation : in     Automation_ID) is

      Index_End : Natural := Get_Token_End (S (Source),
                                            First      => 1,
                                            Automation => Automation);

   begin -- Discard_Token

      Tail (Source, Length (Source) - Index_End);

   end Discard_Token;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Header_Value (Source   : in     String;
                           Index    : in out Positive;
                           Accepted :    out Boolean) is

      type States is
        (State_Start,
         State_Nonaccept_Stop,
         State_Accept_Stop,
         State_Space,
         State_Carriage_Return,
         State_Linefeed,
         State_Character);

      subtype Transitions is States range State_Accept_Stop .. State_Character;

      -------------------------------------------------------------------------
      --  This function is given the next character in the string and
      --  considers it to return the requested transition.
      function Get_Transition (Next_Char : in Standard.Character)
                              return Transitions;

      function Get_Transition (Next_Char : in Standard.Character)
                              return Transitions is

         package L renames Ada.Characters.Latin_1;

      begin -- Get_Transition

         if Is_In (Next_Char, RFC2234_Whitespace) then
            return State_Space;
         elsif Next_Char = L.CR then
            return State_Carriage_Return;
         elsif Next_Char = L.LF then
            return State_Linefeed;
         else
            return State_Character;
         end if;

      end Get_Transition;

      -------------------------------------------------------------------------
      Transition : Transitions := Get_Transition (Source(Index));
      State      : States      := State_Start;

   begin -- Header_Value

      while State /= State_Accept_Stop and State /= State_Nonaccept_Stop loop

         case State is

            when State_Start =>
               case Transition is
                  when State_Accept_Stop =>
                     State := State_Nonaccept_Stop;
                  when others =>
                     State := Transition;
               end case;

            when State_Nonaccept_Stop .. State_Accept_Stop =>
               raise Mea_Culpa;

            when State_Space =>
               case Transition is
                  when State_Accept_Stop =>
                     State := State_Nonaccept_Stop;
                  when others =>
                     State := Transition;
               end case;

            when State_Carriage_Return =>
               case Transition is
                  when State_Space =>
                     State := Transition;
                  when State_Carriage_Return =>
                     State := State_Accept_Stop;
                  when State_Linefeed =>
                     State := Transition;
                  when State_Character =>
                     State := State_Accept_Stop;
                  when State_Accept_Stop =>
                     State := Transition;
               end case;

            when State_Linefeed =>
               case Transition is
                  when State_Space =>
                     State := Transition;
                  when State_Carriage_Return .. State_Character =>
                     State := State_Accept_Stop;
                  when State_Accept_Stop =>
                     State := Transition;
               end case;

            when State_Character =>
               case Transition is
                  when State_Accept_Stop =>
                     State := State_Nonaccept_Stop;
                  when others =>
                     State := Transition;
               end case;

         end case;

         Index := Index + 1;

         if Index <= Source'Last then
            Transition := Get_Transition (Source(Index));
         elsif Index > Source'Last then
            Transition := State_Accept_Stop;
         end if;

      end loop;

      if State = State_Accept_Stop then
         Accepted := True;
         -- We need to decrement to handle two things:
         --    1) The last state transition occured on a non-matching
         --       character.
         --    2) The loop while the stop transition was being requested
         --       incremented the index again.
         Index := Index - 2;
      else
         Accepted := False;
      end if;

   end Header_Value;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Uninteresting (Source   : in     String;
                            Index    : in out Positive;
                            Accepted :    out Boolean) is

      type States is
        (State_Start,
         State_Nonaccept_Stop,
         State_Accept_Stop,
         State_Space,
         State_Character,
         State_Comment,
         State_Uncomment,
         State_Escape);

      subtype Transitions is States range State_Accept_Stop .. State_Escape;

      -------------------------------------------------------------------------
      --  This function is given the next character in the string and
      --  the current comment depth count to determine the next
      --  transition.
      function Get_Transition (Next_Char     : in Standard.Character;
                               Comment_Depth : in Natural)
                              return Transitions;

      function Get_Transition (Next_Char     : in Standard.Character;
                               Comment_Depth : in Natural)
                              return Transitions is

         package L renames Ada.Characters.Latin_1;

      begin -- Get_Transition

         if Comment_Depth = 0 then

            if Is_In (Next_Char, Whitespace) then
               return State_Space;
            elsif Next_Char = '(' then
               return State_Comment;
            else
               return State_Character;
            end if;

         else

            if Is_In (Next_Char, Whitespace) then
               return State_Space;
            elsif Next_Char = '(' then
               return State_Comment;
            elsif Next_Char = ')' then
               return State_Uncomment;
            elsif Next_Char = '\' then
               return State_Escape;
            else
               return State_Character;
            end if;

         end if;

      end Get_Transition;

      -------------------------------------------------------------------------
      Comment_Depth : Natural     := 0;
      State         : States      := State_Start;
      Transition    : Transitions := Get_Transition (Source(Index),
                                                     Comment_Depth);

   begin -- Uninteresting

      while State /= State_Accept_Stop and State /= State_Nonaccept_Stop loop

         if Comment_Depth = 0 then

            case State is

               when State_Start =>
                  case Transition is
                     when State_Space =>
                        State := Transition;
                     when State_Comment =>
                        Comment_Depth := Comment_Depth + 1;
                        State         := Transition;
                     when State_Character =>
                        State := State_Nonaccept_Stop;
                     when State_Accept_Stop =>
                        State := State_Nonaccept_Stop;
                     when others =>
                        raise Mea_Culpa;
                  end case;

               when State_Space =>
                  case Transition is
                     when State_Space =>
                        State := Transition;
                     when State_Comment =>
                        Comment_Depth := Comment_Depth + 1;
                        State         := Transition;
                     when State_Character =>
                        State := State_Accept_Stop;
                     when State_Accept_Stop =>
                        State := Transition;
                     when others =>
                        raise Mea_Culpa;
                  end case;

               when State_Uncomment =>
                  case Transition is
                     when State_Space =>
                        State := Transition;
                     when State_Comment =>
                        Comment_Depth := Comment_Depth + 1;
                        State         := Transition;
                     when State_Character =>
                        State := State_Accept_Stop;
                     when State_Accept_Stop =>
                        State := Transition;
                     when others =>
                        raise Mea_Culpa;
                  end case;

               when others =>
                  raise Mea_Culpa;

            end case;

         elsif Comment_Depth > 0 then

            case State is

               when State_Space .. State_Uncomment =>
                  case Transition is
                     when State_Space =>
                        State := Transition;
                     when State_Comment =>
                        Comment_Depth := Comment_Depth + 1;
                        State         := Transition;
                     when State_Uncomment =>
                        Comment_Depth := Comment_Depth - 1;
                        State         := Transition;
                     when State_Character =>
                        State := Transition;
                     when State_Escape =>
                        State := Transition;
                     when State_Accept_Stop =>
                        State := State_Nonaccept_Stop;
                     when others =>
                        raise Mea_Culpa;
                  end case;

               when State_Escape =>
                  case Transition is
                     when State_Space =>
                        State := Transition;
                     when State_Comment =>
                        State := State_Character;
                     when State_Uncomment =>
                        State := State_Character;
                     when State_Character =>
                        State := Transition;
                     when State_Escape =>
                        State := State_Character;
                     when State_Accept_Stop =>
                        State := State_Nonaccept_Stop;
                     when others =>
                        raise Mea_Culpa;
                  end case;

               when others =>
                  raise Mea_Culpa;

            end case;

         end if;

         Index := Index + 1;

         if Index <= Source'Last then
            Transition := Get_Transition (Source(Index),
                                          Comment_Depth);
         elsif Index > Source'Last then
            Transition := State_Accept_Stop;
         end if;

      end loop;

      if State = State_Accept_Stop then
         Accepted := True;
         -- We need to decrement to handle two things:
         --    1) The last state transition occured on a non-matching
         --       character.
         --    2) The loop while the stop transition was being requested
         --       incremented the index again.
         Index := Index - 2;
      else
         Accepted := False;
      end if;

   end Uninteresting;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Newline_Sequence (Source   : in     String;
                               Index    : in out Positive;
                               Accepted :    out Boolean) is

      type States is
        (State_Start,
         State_Nonaccept_Stop,
         State_Accept_Stop,
         State_Character,
         State_Linefeed,
         State_Carriage_Return);

      subtype Transitions is States
        range State_Accept_Stop .. State_Carriage_Return;

      -------------------------------------------------------------------------
      --  This function is given the next character in the string and
      --  the current comment depth count to determine the next
      --  transition.
      function Get_Transition (Next_Char     : in Standard.Character)
                              return Transitions;

      function Get_Transition (Next_Char     : in Standard.Character)
                              return Transitions is

         package L renames Ada.Characters.Latin_1;

      begin -- Get_Transition

         if Next_Char = L.LF then
            return State_Linefeed;
         elsif Next_Char = L.CR then
            return State_Carriage_Return;
         else
            return State_Character;
         end if;

      end Get_Transition;

      -------------------------------------------------------------------------
      State         : States      := State_Start;
      Transition    : Transitions := Get_Transition (Source(Index));

   begin -- Newline_Sequence

      while State /= State_Accept_Stop and State /= State_Nonaccept_Stop loop

         case State is

            when State_Start =>
               case Transition is
                  when State_Character =>
                     State := State_Nonaccept_Stop;
                  when State_Linefeed =>
                     State := Transition;
                  when State_Carriage_Return =>
                     State := Transition;
                  when State_Accept_Stop =>
                     State := State_Nonaccept_Stop;
               end case;

            when State_Carriage_Return =>
               case Transition is
                  when State_Character =>
                     State := State_Accept_Stop;
                  when State_Linefeed =>
                     State := Transition;
                  when State_Carriage_Return =>
                     State := State_Accept_Stop;
                  when State_Accept_Stop =>
                     State := State_Accept_Stop;
               end case;

            when State_Linefeed =>
               case Transition is
                  when State_Accept_Stop .. State_Carriage_Return =>
                     State := State_Accept_Stop;
               end case;

            when others =>
               raise Mea_Culpa;

         end case;

         Index := Index + 1;

         if Index <= Source'Last then
            Transition := Get_Transition (Source(Index));
         elsif Index > Source'Last then
            Transition := State_Accept_Stop;
         end if;

      end loop;

      if State = State_Accept_Stop then
         Accepted := True;
         -- We need to decrement to handle two things:
         --    1) The last state transition occured on a non-matching
         --       character.
         --    2) The loop while the stop transition was being requested
         --       incremented the index again.
         Index := Index - 2;
      else
         Accepted := False;
      end if;

   end Newline_Sequence;

   ----------------------------------------------------------------------------
   --  This state machine matches a dot-atom-text construct, as
   --  defined in RFC 2822 p 12. The dot-atom is simply an atom that
   --  contains dot characters '.' in its interior, but does not begin
   --  or end with a dot and contains no consecutive dots.
   --
   --  A stop request is caused by either the end of the string or a
   --  character that is not atom-text or a dot.
   --
   --                --- Requested Transition ----
   --  State         Atom-Text Dot          Stop
   -------------------------------------------------------
   --  Start         Accept    NA_Stop      NA_Stop
   --  Atom-Text     Accept    Accept       A_Stop
   --  Dot           Accept    NA_Stop      NA_Stop
   procedure Dot_Atom_Text (Source   : in     String;
                            Index    : in out Positive;
                            Accepted :    out Boolean) is

      type States is
        (State_Start,
         State_Nonaccept_Stop,
         State_Accept_Stop,
         State_Atom_Text,
         State_Dot);

      subtype Transitions is States
        range State_Accept_Stop .. State_Dot;

      -------------------------------------------------------------------------
      --  This function is given the next character in the string and
      --  the current comment depth count to determine the next
      --  transition.
      function Get_Transition (Next_Char     : in Standard.Character)
                              return Transitions;

      function Get_Transition (Next_Char     : in Standard.Character)
                              return Transitions is

         package L renames Ada.Characters.Latin_1;

      begin -- Get_Transition

         if Is_In (Next_Char, RFC2822_Atom_Text) then
            return State_Atom_Text;
         elsif Next_Char = '.' then
            return State_Dot;
         else
            return State_Accept_Stop;
         end if;

      end Get_Transition;

      -------------------------------------------------------------------------
      State         : States      := State_Start;
      Transition    : Transitions := Get_Transition (Source(Index));

   begin -- Dot_Atom_Text

      while State /= State_Accept_Stop and State /= State_Nonaccept_Stop loop

         case State is

            when State_Start =>
               case Transition is
                  when State_Atom_Text =>
                     State := Transition;
                  when State_Dot =>
                     State := State_Nonaccept_Stop;
                  when State_Accept_Stop =>
                     State := State_Nonaccept_Stop;
                  when others =>
                     raise Mea_Culpa;
               end case;

            when State_Atom_Text =>
               case Transition is
                  when State_Atom_Text =>
                     State := Transition;
                  when State_Dot =>
                     State := Transition;
                  when State_Accept_Stop =>
                     State := State_Accept_Stop;
                  when others =>
                     raise Mea_Culpa;
               end case;

            when State_Dot =>
               case Transition is
                  when State_Atom_Text =>
                     State := Transition;
                  when State_Dot =>
                     State := State_Nonaccept_Stop;
                  when State_Accept_Stop =>
                     State := State_Nonaccept_Stop;
                  when others =>
                     raise Mea_Culpa;
               end case;

            when others =>
               raise Mea_Culpa;

         end case;

               Index := Index + 1;

         if Index <= Source'Last then
            Transition := Get_Transition (Source(Index));
         elsif Index > Source'Last then
            Transition := State_Accept_Stop;
         end if;

      end loop;

      if State = State_Accept_Stop then
         Accepted := True;
         -- We need to decrement to handle two things:
         --    1) The last state transition occured on a non-matching
         --       character.
         --    2) The loop while the stop transition was being requested
         --       incremented the index again.
         Index := Index - 2;
      else
         Accepted := False;
      end if;

   end Dot_Atom_Text;

end Basil.Automata;
