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
--  This package provides an entity object from which Messages and
--  MIME_Entities can be derived, and a set of operations that are
--  common for both objects.
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;
with Ada.Finalization;                   use Ada.Finalization;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Basil.Headers.Lists;
with Basil.Content_Types;                use Basil.Content_Types;
with Basil.Strings;                      use Basil.Strings;

package Basil.Entities is

   --  The Abstract_Entity has both public and private components.
   --
   --  Some caveats about this data structure,
   --
   --  1) The object uses a private component to keep track of if it is
   --     multipart or not. It is updated when you use Set_Content_Type to
   --     change the content type of the object. However, since you have
   --     access to the headers list itself there is no stopping you from
   --     changing the Content-Type or Content-Transfer-Encoding lines
   --     yourself. To verify integrity the To_String operation ensures
   --     that there is sanity between the headers of the message and
   --     what the object thinks its content type information is, and
   --     will raise Invalid_Content_Type if they differ.
   --
   --     If you wish to make an object with an experimental type or
   --     subtype you may do so by setting the Content Type to
   --     T_Experimental/ST_Experimental (or whatever combination
   --     of those you need) and then set the CT and CTE lines yourself.
   --     This will not cause To_String to raise an exception provided
   --     the lines are properly formed.
   --
   --  2) I suspect there might be some confusion as to the purpose of
   --     the body. For non-Multipart entities, it is exactly as
   --     you would expect: the body of the message. However, the way the
   --     MIME standards are written, child entites are *embedded* in
   --     the body of the entitiy. The standard allows (and somewhat
   --     encourages) text to be placed between child entities, but then
   --     goes on to specify that the user will never see it.
   --
   --     Since the intermedial text is encouraged, and must be preserved
   --     in transit, this type uses the body and some other private
   --     components to store the text. The body itself is simply any text
   --     before the first child MIME entity. You should, therefore, not
   --     try to inject text into it thinking that the user will see it,
   --     but rather create a text/plain child entity and put the text
   --     in its body.
   --
   --  3) If the entity is not multipart any operation that applies to
   --     a multipart object (operations on the child entities list) will
   --     raise Illegal_Child_Operation. This is true even if you set the
   --     headers to something else without doing Set_Content_Type (see
   --     caveat #1).

   type Public_Components is new Controlled with
      record
         Headers_List : Headers.Lists.List;
      end record;

   type Abstract_Entity is new Public_Components with private;

   Empty_Entity : constant Abstract_Entity;

   ----------------------------------------------------------------------------
   --  Content type accessor functions. Set_Content_Type updates the
   --  headers list to reflect the new content type unless it has
   --  Experimental or Unknown components (in which case you're on
   --  your own managing the content type headers).
   --
   --  PLEASE NOTE: According to RFC 2045 p 16 entities of type
   --  multipart may not have a content transfer encoding of
   --  quoted-printable or base64. If Set_Content_Type is used to
   --  apply a Content Type with those properties it will raise
   --  Invalid_Content_Type.
   procedure Set_Content_Type (Entity : in out Abstract_Entity;
                               Value  : in     Content_Type);

   function Get_Content_Type  (Entity : in Abstract_Entity)
                              return Content_Type;

   ----------------------------------------------------------------------------
   --  This function allows you to set the body part of a message to
   --  the value provided. Note that the value is restricted to the
   --  characters allowed in an Encoded_String. If you have an 8Bit or
   --  Binary type entity and wish to have unrestricted access to set
   --  the value, use the function below.
   procedure Set_Body_Part (Entity : in out Abstract_Entity;
                            Value  : in Encoded_String);

   ----------------------------------------------------------------------------
   --  This version of Set_Body_Type allows you to set arbitrary data
   --  to entities with a content transfer encoding of 8Bit or Binary.
   --
   --  Please note that if the CTE of the entity is not CTE_8bit or
   --  CTE_Binary this function will raise an exception.
   procedure Set_Body_Part (Entity : in out Abstract_Entity;
                            Value  : in     String);

   ----------------------------------------------------------------------------
   --  Body part accessor functions. If the message is multipart,
   --  please see the caveats above about how the body part gets used.
   --
   --  PLEASE NOTE: Set_Body_Part does *NOT* encode the data if the
   --  content transfer encoding of the entity is CTE_Quoted_Printable
   --  or CTE_Base64, so you MUST do that yourself. However, if the
   --  CTE is anything *but* CTE_8Bit or CTE_Binary, characters not in
   --  0 < Char < 128 will cause the function to raise an exception.
   function Get_Body_Part  (Entity : in Abstract_Entity)
                           return String;

   ----------------------------------------------------------------------------
   --  This function tests if the provided Entity is multipart (such
   --  that accessing child elements is permitted).
   function Is_Multipart (Entity : in Abstract_Entity) return Boolean;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --  CHILD ENTITY MANAGEMENT
   --
   --  The Child_Cursor behaves on the child entities just as a
   --  Doubly_Lined_List cursor behaves on a list when the strategy is
   --  simple.  A cursor with the recursive strategy differs in that
   --  it will recursively descend into the child entity tree as it
   --  iterates, ensuring that *all* children of any given entity will
   --  be visited.
   type Child_Cursor
     (Strategy : Cursor_Strategy := Strategy_Simple)
   is private;

   No_Child           : constant Child_Cursor;

   No_Child_Recursive : constant Child_Cursor;

   --  Returns the number of direct children of the entity. This
   --  function is not recursive
   function Count_Children  (Entity   : Abstract_Entity) return
     Count_Children_Type;

   --  Analogous to Is_Empty
   function Has_No_Children (Entity   : Abstract_Entity) return Boolean;

   --  Analogous to Element
   function  Get_Child      (Position : Child_Cursor)
                            return Abstract_Entity;

   procedure Query_Child
     (Position : in Child_Cursor;
      Process  : not null access procedure (Child :
                                            in Abstract_Entity'Class));

   procedure Update_Child
     (Parent    : in out Abstract_Entity;
      Position  : Child_Cursor;
      Process   : not null access procedure (Child :
                                             in out Abstract_Entity'Class));

   ----------------------------------------------------------------------------
   --  Child Entity List Manipulation
   procedure Clear_Children (Entity   : in Abstract_Entity);

   procedure Move_Children    (Target : in out Abstract_Entity;
                               Source : in out Abstract_Entity);

   procedure Replace_Child (Parent    : in out Abstract_Entity;
                            Position  : in     Child_Cursor;
                            New_Child : in     Abstract_Entity'Class);

   procedure Insert_Child  (Parent    : in out Abstract_Entity ;
                            Before    : in     Child_Cursor;
                            New_Child : in     Abstract_Entity'Class;
                            Count     : in     Count_Children_Type := 1);

   procedure Insert_Child  (Parent    : in out Abstract_Entity;
                            Before    : in     Child_Cursor;
                            New_Child : in     Abstract_Entity'Class;
                            Position  :    out Child_Cursor;
                            Count     : in     Count_Children_Type := 1);

   procedure Prepend_Child (Parent    : in out Abstract_Entity;
                            New_Child : in     Abstract_Entity'Class;
                            Count     : in     Count_Children_Type := 1);

   procedure Append_Child  (Parent    : in out Abstract_Entity;
                            New_Child : in     Abstract_Entity'Class;
                            Count     : in     Count_Children_Type := 1);

   procedure Delete_Child  (Parent    : in out Abstract_Entity;
                            Position  : in out Child_Cursor;
                            Count     : in     Count_Children_Type := 1);

   procedure Delete_First_Child (Parent : in out Abstract_Entity;
                                 Count  : in     Count_Children_Type := 1);

   procedure Delete_Last_Child  (Parent : in out Abstract_Entity;
                                 Count  : in     Count_Children_Type := 1);

   procedure Reverse_Children   (Parent : in out Abstract_Entity);

   ----------------------------------------------------------------------------
   --  Swap_Children will swap any two descendents of the provided
   --  parent. Swap_Child_Links will only work on two immediate
   --  children and WILL FAIL IF I OR J ARE RECURSIVE CURSORS.
   procedure Swap_Children      (Parent : in out Abstract_Entity;
                                 I, J   : in     Child_Cursor);

   procedure Swap_Child_Links   (Parent : in out Abstract_Entity;
                                 I, J   : in     Child_Cursor);

   ----------------------------------------------------------------------------
   --  For the Splice operations to succeed, both target and source
   --  must be multipart entities with the same subtype and content
   --  transfer encoding.
   procedure Splice_Children (Target   : in out Abstract_Entity;
                              Before   : in     Child_Cursor;
                              Source   : in out Abstract_Entity);

   procedure Splice_Children (Target   : in out Abstract_Entity;
                              Before   : in     Child_Cursor;
                              Source   : in out Abstract_Entity;
                              Position : in out Child_Cursor);

   ----------------------------------------------------------------------------
   --  Operations on Cursors
   --
   --  First_Child and Last_Child return new cursors so you must
   --  specify the strategy. the Next_Child and Previous_Child
   --  functions return a cursor with the same strategy as the one
   --  passed in.
   --
   --  Get_First_Child doesn't need a strategy as the first child is
   --  the same regardless, however, Get_Last_Child does because the
   --  last child in the list may itself have children, which will
   --  affect the ultimate last child.

   function First_Child     (Parent   : in Abstract_Entity;
                             Strategy : in Cursor_Strategy)
                            return Child_Cursor;

   function Get_First_Child (Parent   : in Abstract_Entity)
                            return Abstract_Entity;

   function Last_Child      (Parent   : in Abstract_Entity;
                             Strategy : in Cursor_Strategy)
                            return Child_Cursor;

   function Get_Last_Child  (Parent   : in Abstract_Entity;
                             Strategy : in Cursor_Strategy)
                            return Abstract_Entity;

   procedure Next_Child     (Position : in out Child_Cursor);

   function  Next_Child     (Position : in     Child_Cursor)
                            return Child_Cursor;

   procedure Previous_Child (Position : in out Child_Cursor);

   function  Previous_Child (Position : in     Child_Cursor)
                            return Child_Cursor;

   function Get_Strategy      (Object : in     Child_Cursor)
                              return Cursor_Strategy;

   function Has_Child       (Position : Child_Cursor) return Boolean;

   ----------------------------------------------------------------------------
   --  Find operations
   --
   --  The normal Find operations for lists aren't particularly
   --  relevent to entities, so they're not included here. Instead,
   --  the following two operations are provided which search for
   --  entities with a given content type.
   --
   --  The Position cursor is not optional because the function
   --  determins the iteration strategy from it. If you wish to search
   --  from beginning to end or vice versa, use No_Child.
   function Find_With_Type (Parent      : in Abstract_Entity;
                            Entity_Type : in Content_Type;
                            Position    : in Child_Cursor)
                           return Child_Cursor;

   function Reverse_Find_With_Type
     (Parent      : in Abstract_Entity;
      Entity_Type : in Content_Type;
      Position    : in Child_Cursor)
     return Child_Cursor;

   ----------------------------------------------------------------------------
   --  Iteration Operations.
   --
   --  Please note that these operations that these functions both
   --  traverse the tree depth-first if you choose the recursive
   --  strategy. This reduces complications from operations deleting
   --  elements.

   procedure Iterate_Children
     (Parent    : in Abstract_Entity;
      Strategy  : in Cursor_Strategy;
      Process   : not null access procedure (Position : Child_Cursor));

   procedure Reverse_Iterate_Children
     (Parent    : in Abstract_Entity;
      Strategy  : in Cursor_Strategy;
      Process   : not null access procedure (Position : Child_Cursor));

private

   type Children_List;

   type Children_List_Pointer is access all Children_List;

   type Abstract_Entity_Pointer is access constant Abstract_Entity'Class;

   type Abstract_Entity is new Public_Components
     with record
        Entity_Type   : Content_Type;
        Body_Part     : Unbounded_String;
        Trailing_Junk : Unbounded_String;
        Children      : Children_List_Pointer;
   end record;

   overriding procedure Initialize (Entity: in out Abstract_Entity);
   overriding procedure Adjust     (Entity: in out Abstract_Entity);
   overriding procedure Finalize   (Entity: in out Abstract_Entity);

   package Children_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Element_Type => Abstract_Entity'Class);

   type Children_List is new Children_Lists.List with null record;

   Empty_Children_List : aliased Children_List
     := (Children_Lists.Empty_List with null record);

   type Cursor_Stack_Node;

   type Stack_Node_Pointer is access Cursor_Stack_Node;

   type Child_Stack is new Controlled with
      record
         Pointer : Stack_Node_Pointer;
      end record;

   overriding procedure Initialize (Object: in out Child_Stack);
   overriding procedure Adjust     (Object: in out Child_Stack);
   overriding procedure Finalize   (Object: in out Child_Stack);

   type Child_Cursor (Strategy : Cursor_Strategy := Strategy_Simple) is
      record
         Element_Cursor  : Children_Lists.Cursor;
         Parent          : Abstract_Entity_Pointer;
         case Strategy is
            when Strategy_Simple =>
               null;
            when Strategy_Recursive =>
               Stack     : Child_Stack;
         end case;
      end record;

   Empty_Entity      : constant Abstract_Entity :=
     (Controlled with
      Headers_List  => Headers.Lists.Empty_List,
      Entity_Type   => No_Content_Type,
      Body_Part     => Null_Unbounded_String,
      Trailing_Junk => Null_Unbounded_String,
      Children      => Empty_Children_List'Access);

   No_Child           : constant Child_Cursor :=
     (Strategy       => Strategy_Simple,
      Element_Cursor => Children_Lists.No_Element,
      Parent         => null);

   No_Child_Recursive : constant Child_Cursor :=
     (Strategy       => Strategy_Recursive,
      Element_Cursor => Children_Lists.No_Element,
      Parent         => null,
      Stack          => (Controlled with
                         Pointer => null));

   type Cursor_Stack_Node is
      record
         Element_Cursor : Children_Lists.Cursor;
         Parent         : Abstract_Entity_Pointer;
         Next           : Stack_Node_Pointer := null;
      end record;

end Basil.Entities;
