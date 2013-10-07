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

with Basil.Utils;            use Basil.Utils;
with Basil.Headers;          use Basil.Headers;
with Basil.Automata;         use Basil.Automata;

with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Containers;         use Ada.Containers;
with Ada.Characters.Latin_1;

with Ada.Unchecked_Deallocation;

with System.Address_Image;

package body Basil.Entities is

   ----------------------------------------------------------------------------
   --  This procedure is used by the operations on child entities to
   --  raise an error if the user calls the procedure on a
   --  non-multipart entity.
   procedure Assert_Multipart (Entity : in Abstract_Entity'Class);

   --------------------------------------------------------------------------
   --  This procedure is used by the operations entities for which
   --  there must be at least one child. It will raise an error if the
   --  number of children = 0.
   procedure Assert_Non_Empty (Entity : in Abstract_Entity'Class);

   ----------------------------------------------------------------------------
   --  This procedure raises an error if there is a problem with the
   --  relationship between the parent and the child. It is not
   --  necessary to call Assert_Multitype on the parent if one of
   --  these functions are used, as they verify that the parent and
   --  all potential links between are multitype.
   procedure Assert_Relationship (Parent : in Abstract_Entity'Class;
                                  Child  : in Child_Cursor);

   ----------------------------------------------------------------------------
   --  This procedure verifies that the provided cursor is still
   --  valid. For a recursive cursor it verifies that every cursor in
   --  the stack is still valid.
   procedure Assert_Valid_Cursor (Object : in Child_Cursor);

   ----------------------------------------------------------------------------
   --  Operations against the stack of child cursors in the
   --  Recursive_Child_Cursor. Push adds a new item to the stack, Pop
   --  removes one, setting the current value of the cursor to that of
   --  the removed item, and truncate deletes all items on the stack.
   procedure Push_Stack (Object        : in out Child_Cursor;
                         New_Parent    : in     Abstract_Entity_Pointer;
                         New_Reference : in     Children_Lists.Cursor);

   procedure Pop_Stack  (Object        : in out Child_Cursor);

   procedure Truncate_Stack (Object    : in out Child_Stack);

   ----------------------------------------------------------------------------
   --  This function is used by the Find_With_Type class of functions,
   --  which are simply wrappers which define the search direction and
   --  position (if not supplied). The search strategy (simple or
   --  recursive) is determined by the type of cursor supplied in
   --  position
   function Find_With_Type (Parent      : in Abstract_Entity;
                            Entity_Type : in Content_Type;
                            Position    : in Child_Cursor;
                            Forward     : in Boolean)
                           return Child_Cursor;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Set_Content_Type (Entity : in out Abstract_Entity;
                               Value  : in     Content_Type) is

      -------------------------------------------------------------------------
      --  Ensures that there are no experimental or unknown components
      --  that prevent serialization of the content type object. If
      --  there are, it's up to the user to make sure there are valid
      --  content type headers in the entity.
      function Is_Serializable (Value : in Content_Type) return Boolean;

      function Is_Serializable (Value : in Content_Type) return Boolean is

      begin -- Is_Serializable

         if Value.Content_Transfer_Encoding = CTE_Unknown or
           Value.Content_Transfer_Encoding = CTE_Experimental
         then
            return False;
         end if;

         if Get_Content_Class (Value) /= Normal then
            return False;
         end if;

         return True;

      end Is_Serializable;

      -------------------------------------------------------------------------
      Type_Headers   : Headers.Lists.List;

   begin -- Set_Content_Type

      if Value.MIME_Type = T_Multipart and
        (Value.Content_Transfer_Encoding = CTE_Quoted_Printable or
         Value.Content_Transfer_Encoding = CTE_Base64)
      then
         raise Invalid_Content_Type;
      end if;

      Entity.Entity_Type := Value;

      if Is_Serializable (Value) then

         Type_Headers := To_Headers (Source      => Value,
                                     Add_Version => False);

         Headers.Lists.Purge_Header (Entity.Headers_List, "CONTENT-TYPE");
         Headers.Lists.Purge_Header
           (Entity.Headers_List, "CONTENT-TRANSFER-ENCODING");

         Headers.Lists.Splice (Target => Entity.Headers_List,
                               Before => Headers.Lists.No_Element,
                               Source => Type_Headers);

      end if;

   end Set_Content_Type;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Content_Type (Entity : in Abstract_Entity)
                             return Content_Type is

   begin -- Get_Content_Type

      return Entity.Entity_Type;

   end Get_Content_Type;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Set_Body_Part (Entity : in out Abstract_Entity;
                            Value  : in Encoded_String) is

   begin -- Set_Body_Part

      Entity.Body_Part := EUS (Value);

   end Set_Body_Part;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Set_Body_Part (Entity : in out Abstract_Entity;
                            Value  : in     String) is

   begin -- Set_Body_Part


      if Entity.Entity_Type.Content_Transfer_Encoding = CTE_7bit or
        Entity.Entity_Type.Content_Transfer_Encoding = CTE_Base64 or
        Entity.Entity_Type.Content_Transfer_Encoding = CTE_Quoted_Printable
      then

         raise Invalid_Content_Type;

      else

         Entity.Body_Part := US (Value);

      end if;


   end Set_Body_Part;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Body_Part  (Entity : in Abstract_Entity)
                           return String is

   begin -- Get_Body_Part

      return S (Entity.Body_Part);

   end Get_Body_Part;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Is_Multipart (Entity : in Abstract_Entity) return Boolean is

   begin -- Is_Multipart

      if Entity.Entity_Type.MIME_Type = T_Multipart then
         return True;
      else
         return False;
      end if;

   end Is_Multipart;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --         SUBPROGRAMS FOR OPERATIONS ON CHILD ENTITIES                   --
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Count_Children  (Entity   : Abstract_Entity)
                            return Count_Children_Type is

   begin -- Count_Children_Type

      Assert_Multipart (Entity);

      return Count_Children_Type (Length (Entity.Children.all));

   end Count_Children;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Has_No_Children (Entity   : Abstract_Entity) return Boolean is

   begin -- Has_No_Children

      Assert_Multipart (Entity);

      return Is_Empty (Entity.Children.all);

   end Has_No_Children;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function  Get_Child      (Position : Child_Cursor)
                            return Abstract_Entity is

   begin -- Get_Child

      Assert_Valid_Cursor (Position);

      return
        Abstract_Entity (Children_Lists.Element (Position.Element_Cursor));

   end Get_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Query_Child
     (Position : in Child_Cursor;
      Process  : not null access procedure (Child  :
                                            in Abstract_Entity'Class)) is

   begin -- Query_Child

      Assert_Valid_Cursor (Position);

      Children_Lists.Query_Element (Position => Position.Element_Cursor,
                                    Process  => Process);

   end Query_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Update_Child
     (Parent    : in out Abstract_Entity;
      Position  : Child_Cursor;
      Process   : not null access procedure (Child :
                                             in out Abstract_Entity'Class)) is

   begin -- Update_Child

      Assert_Multipart (Parent);
      Assert_Valid_Cursor (Position);

      Update_Element (Container => Position.Parent.all.Children.all,
                      Position  => Position.Element_Cursor,
                      Process   => Process);

   end Update_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Clear_Children (Entity   : in Abstract_Entity) is

   begin -- Clear_Children

      Assert_Multipart (Entity);

      Clear (Entity.Children.all);

   end Clear_Children;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Move_Children    (Target : in out Abstract_Entity;
                               Source : in out Abstract_Entity) is

   begin -- Move_Children

      Assert_Multipart (Target);
      Assert_Multipart (Source);

      if Target.Entity_Type /= Source.Entity_Type then
         raise Invalid_Content_Type;
      end if;

      Move (Target => Target.Children.all,
            Source => Source.Children.all);

   end Move_Children;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Replace_Child (Parent    : in out Abstract_Entity;
                            Position  : in     Child_Cursor;
                            New_Child : in     Abstract_Entity'Class) is

   begin -- Replace_Child

      Assert_Relationship (Parent, Position);

      Replace_Element (Container => Position.Parent.all.Children.all,
                       Position  => Position.Element_Cursor,
                       New_Item  => New_Child);

   end Replace_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Insert_Child  (Parent    : in out Abstract_Entity;
                            Before    : in     Child_Cursor;
                            New_Child : in     Abstract_Entity'Class;
                            Count     : in     Count_Children_Type := 1) is

   begin -- Insert_Child

      Assert_Relationship (Parent, Before);

      Insert (Container => Before.Parent.all.Children.all,
              Before    => Before.Element_Cursor,
              New_Item  => New_Child,
              Count     => Count_Type (Count));

   end Insert_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Insert_Child  (Parent    : in out Abstract_Entity;
                            Before    : in     Child_Cursor;
                            New_Child : in     Abstract_Entity'Class;
                            Position  :    out Child_Cursor;
                            Count     : in     Count_Children_Type := 1) is

   begin -- Insert_Child

      Assert_Relationship (Parent, Before);

      Insert (Container => Before.Parent.all.Children.all,
              Before    => Before.Element_Cursor,
              New_Item  => New_Child,
              Position  => Position.Element_Cursor,
              Count     => Count_Type (Count));

   end Insert_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Prepend_Child (Parent    : in out Abstract_Entity;
                            New_Child : in     Abstract_Entity'Class;
                            Count     : in     Count_Children_Type := 1) is

   begin -- Prepend_Child

      Assert_Multipart (Parent);

      Prepend (Container => Parent.Children.all,
               New_Item  => New_Child,
               Count     => Count_Type (Count));

   end Prepend_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Append_Child  (Parent    : in out Abstract_Entity;
                            New_Child : in     Abstract_Entity'Class;
                            Count     : in     Count_Children_Type := 1) is

   begin -- Append_Child

      Assert_Multipart (Parent);

      Append (Container => Parent.Children.all,
              New_Item  => New_Child,
              Count     => Count_Type (Count));

   end Append_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Delete_Child  (Parent    : in out Abstract_Entity;
                            Position  : in out Child_Cursor;
                            Count     : in     Count_Children_Type := 1) is

   begin -- Delete_Child

      Assert_Relationship (Parent, Position);

      Delete (Container => Position.Parent.all.Children.all,
              Position  => Position.Element_Cursor,
              Count     => Count_Type (Count));

      if Position.Strategy = Strategy_Recursive then
         Truncate_Stack (Position.Stack);
      end if;

   end Delete_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Delete_First_Child (Parent : in out Abstract_Entity;
                                 Count  : in     Count_Children_Type := 1) is

   begin -- Delete_First_Child

      Assert_Multipart (Parent);

      Delete_First (Container => Parent.Children.all,
                    Count     => Count_Type (Count));

   end Delete_First_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Delete_Last_Child  (Parent : in out Abstract_Entity;
                                 Count  : in     Count_Children_Type := 1) is

   begin -- Delete_Last_Child

      Assert_Multipart (Parent);

      Delete_Last (Container => Parent.Children.all,
                   Count     => Count_Type (Count));

   end Delete_Last_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Reverse_Children   (Parent : in out Abstract_Entity) is

   begin -- Reverse_Children

      Assert_Multipart (Parent);

      Reverse_Elements (Parent.Children.all);

   end Reverse_Children;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Swap_Children      (Parent : in out Abstract_Entity;
                                 I, J   : in     Child_Cursor) is

   begin -- Swap_Children

      Assert_Relationship (Parent, I);
      Assert_Relationship (Parent, J);

      if Children_Lists.Has_Element(I.Element_Cursor)
        and Children_Lists.Has_Element(J.Element_Cursor)
      then

         declare
            I_Copy : Abstract_Entity'Class
              := Children_Lists.Element (I.Element_Cursor);
         begin

            Replace_Element (Container => I.Parent.all.Children.all,
                             Position  => I.Element_Cursor,
                             New_Item  =>
                               Children_Lists.Element (J.Element_Cursor));

            Replace_Element (Container => J.Parent.all.Children.all,
                             Position  => J.Element_Cursor,
                             New_Item  => I_Copy);

         end;

      end if;

   end Swap_Children;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Swap_Child_Links   (Parent : in out Abstract_Entity;
                                 I, J   : in     Child_Cursor) is

   begin -- Swap_Child_Links;

      Assert_Multipart (Parent);

      if I.Strategy = Strategy_Recursive
        or J.Strategy = Strategy_Recursive then
         raise Illegal_Child_Operation;
      end if;

      Swap_Links (Parent.Children.all,
                  I.Element_Cursor,
                  J.Element_Cursor);

   end Swap_Child_Links;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Splice_Children  (Target : in out Abstract_Entity;
                               Before : in     Child_Cursor;
                               Source : in out Abstract_Entity) is

   begin -- Splice_Children

      if Before.Parent /= null
        and Children_Lists."/="(Before.Element_Cursor,
                                Children_Lists.No_Element)
      then

         Assert_Relationship (Target, Before);

      end if;

      Assert_Multipart (Source);

      if Target.Entity_Type /= Source.Entity_Type then
         raise Invalid_Content_Type;
      end if;

      Splice (Target => Target.Children.all,
              Before => Before.Element_Cursor,
              Source => Source.Children.all);

   end Splice_Children;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Splice_Children (Target   : in out Abstract_Entity;
                              Before   : in     Child_Cursor;
                              Source   : in out Abstract_Entity;
                              Position : in out Child_Cursor) is


   begin -- Splice_Children


      if Before.Parent /= null
        and Children_Lists."/="(Before.Element_Cursor,
                                Children_Lists.No_Element)
      then

         Assert_Relationship (Target, Before);

      end if;

      Assert_Relationship (Source, Position);

      if Target.Entity_Type /= Source.Entity_Type then
         raise Invalid_Content_Type;
      end if;

      Splice (Target   => Target.Children.all,
              Before   => Before.Element_Cursor,
              Source   => Source.Children.all,
              Position => Position.Element_Cursor);

   end Splice_Children;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function First_Child (Parent   : in Abstract_Entity;
                         Strategy : in Cursor_Strategy)
                        return Child_Cursor is

   begin -- First_Child

      Assert_Multipart (Parent);

      case Strategy is
         when Strategy_Simple =>
            return Child_Cursor'(Strategy       => Strategy_Simple,
                                 Element_Cursor => First (Parent.Children.all),
                                 Parent         => Parent'Unchecked_Access);

         when Strategy_Recursive =>
            return Child_Cursor'
              (Strategy       => Strategy_Recursive,
               Element_Cursor => First (Parent.Children.all),
               Parent         => Parent'Unchecked_Access,
               Stack          => Child_Stack'(Controlled with
                                              Pointer => null));
      end case;

   end First_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_First_Child   (Parent   : in Abstract_Entity)
                              return Abstract_Entity is

   begin -- Get_First_Child

      Assert_Multipart (Parent);

      return Abstract_Entity (First_Element (Parent.Children.all));

   end Get_First_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Last_Child (Parent   : in Abstract_Entity;
                        Strategy : in Cursor_Strategy)
                       return Child_Cursor is

      Last_Recursive : Child_Cursor :=
        Child_Cursor'(Strategy       => Strategy_Recursive,
                      Element_Cursor => Last (Parent.Children.all),
                      Parent         => Parent'Unchecked_Access,
                      Stack          => Child_Stack'(Controlled with
                                                     Pointer => null));

      -------------------------------------------------------------------------
      procedure Recurse (Current : in Abstract_Entity'Class);

      procedure Recurse (Current : in Abstract_Entity'Class) is

      begin -- Recurse

         if Is_Multipart (Current) and not Is_Empty (Current.Children.all) then

            Push_Stack (Object        => Last_Recursive,
                        New_Reference => Last (Current.Children.all),
                        New_Parent    => Current'Unchecked_Access);

            Children_Lists.Query_Element
              (Position => Last (Current.Children.all),
               Process  => Recurse'Access);

         end if;

      end Recurse;

   begin -- Last_Child

      Assert_Multipart (Parent);

      case Strategy is

         when Strategy_Simple =>

            return Child_Cursor'(Strategy       => Strategy_Simple,
                                 Element_Cursor => Last (Parent.Children.all),
                                 Parent         => Parent'Unchecked_Access);

         when Strategy_Recursive =>

            Last_Recursive :=
              Child_Cursor'(Strategy  => Strategy_Recursive,
                            Element_Cursor => Last (Parent.Children.all),
                            Parent         => Parent'Unchecked_Access,
                            Stack          => Child_Stack'(Controlled with
                                                           Pointer => null));

            Children_Lists.Query_Element
              (Position => Last (Parent.Children.all),
               Process  => Recurse'Access);

            return Last_Recursive;

      end case;

   end Last_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Last_Child  (Parent   : in Abstract_Entity;
                             Strategy : in Cursor_Strategy)
                            return Abstract_Entity is

   begin -- Get_Last_Child

      Assert_Multipart (Parent);

      case Strategy is

         when Strategy_Simple =>

            return Abstract_Entity (Last_Element (Parent.Children.all));

         when Strategy_Recursive =>

            return Abstract_Entity
              (Get_Child (Last_Child (Parent, Strategy_Recursive)));

      end case;

   end Get_Last_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Next_Child     (Position : in out Child_Cursor) is

      Has_Sub_List    : Boolean := False;
      First_Sub_Child : Children_Lists.Cursor;
      Child_Access    : Abstract_Entity_Pointer;

      -------------------------------------------------------------------------
      procedure Get_Sub_Child (Child : in Abstract_Entity'Class);

      procedure Get_Sub_Child (Child : in Abstract_Entity'Class) is

      begin -- Get_Sub_Child

         if Is_Multipart (Child) and not Is_Empty (Child.Children.all) then

            Has_Sub_List    := True;
            First_Sub_Child := First (Child.Children.all);
            Child_Access    := Child'Unchecked_Access;
         end if;

      end Get_Sub_Child;

      -------------------------------------------------------------------------
   begin -- Next_Child

      Assert_Valid_Cursor (Position);

      case Position.Strategy is

         when Strategy_Simple =>

            Children_Lists.Next (Position.Element_Cursor);

         when Strategy_Recursive =>

            Children_Lists.Query_Element
              (Position.Element_Cursor, Get_Sub_Child'Access);

            if Has_Sub_List = True then

               Push_Stack (Object        => Position,
                           New_Parent    => Child_Access,
                           New_Reference => First_Sub_Child);

            else

               Children_Lists.Next (Position.Element_Cursor);

               --  If we come to the end of the current list we start
               --  poping and advancing until we either run out of
               --  recursion levels or we find a next child.
               while not Children_Lists.Has_Element
                 (Position.Element_Cursor)
               loop

                  Pop_Stack (Position);

                  if not Children_Lists.Has_Element
                    (Position.Element_Cursor)
                  then
                     exit;
                  else
                     Children_Lists.Next (Position.Element_Cursor);
                  end if;

               end loop;

            end if;

      end case;

   end Next_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function  Next_Child     (Position : in     Child_Cursor)
                            return Child_Cursor is

      Next_Position : Child_Cursor := Position;

   begin -- Next_Child

      Assert_Multipart (Position.Parent.all);

      case Position.Strategy is

         when Strategy_Simple =>

            return Child_Cursor'
              (Strategy       => Strategy_Simple,
               Element_Cursor => Children_Lists.Next (Position.Element_Cursor),
               Parent         => Position.Parent);

         when Strategy_Recursive =>

            Next_Child (Next_Position);

            return Next_Position;

      end case;

   end Next_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Previous_Child (Position : in out Child_Cursor) is

      Has_Sub_List    : Boolean := False;
      Last_Sub_Child  : Children_Lists.Cursor;
      Child_Access    : Abstract_Entity_Pointer;

      -------------------------------------------------------------------------
      procedure Get_Sub_Child (Child : in Abstract_Entity'Class);

      procedure Get_Sub_Child (Child : in Abstract_Entity'Class) is

      begin -- Get_Sub_Child

         if Is_Multipart (Child) and not Is_Empty (Child.Children.all) then
            Has_Sub_List   := True;
            Last_Sub_Child := Last (Child.Children.all);
            Child_Access   := Child'Unchecked_Access;
         else
            Has_Sub_List := False;
         end if;

      end Get_Sub_Child;

      -------------------------------------------------------------------------
   begin -- Previous_Child

      Assert_Valid_Cursor (Position);

      case Position.Strategy is

         when Strategy_Simple =>

            Children_Lists.Previous (Position.Element_Cursor);

         when Strategy_Recursive =>

            Children_Lists.Previous (Position.Element_Cursor);

            if not Children_Lists.Has_Element (Position.Element_Cursor) then

               Pop_Stack (Position);

            else

               Children_Lists.Query_Element
                 (Position.Element_Cursor, Get_Sub_Child'Access);

               while Has_Sub_List = True loop

                  Push_Stack (Object        => Position,
                              New_Parent    => Child_Access,
                              New_Reference => Last_Sub_Child);

                  Children_Lists.Query_Element
                    (Position.Element_Cursor, Get_Sub_Child'Access);

                  --  Gnat doesn't like it that Has_Sub_List isn't
                  --  directly modified while inside the loop.
                  Has_Sub_List := Has_Sub_List;

               end loop;

            end if;

      end case;

   end Previous_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function  Previous_Child     (Position : in     Child_Cursor)
                            return Child_Cursor is

      Previous_Position : Child_Cursor := Position;

   begin -- Previous_Child

      Assert_Multipart (Position.Parent.all);

      case Position.Strategy is

         when Strategy_Simple =>

            return Child_Cursor'
              (Strategy       => Strategy_Simple,
               Element_Cursor =>
                 Children_Lists.Previous (Position.Element_Cursor),
               Parent         => Position.Parent);

         when Strategy_Recursive =>

            Previous_Child (Previous_Position);

            return Previous_Position;

      end case;

   end Previous_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Strategy (Object : in     Child_Cursor)
                         return Cursor_Strategy is

   begin -- Get_Strategy

      return Object.Strategy;

   end Get_Strategy;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Has_Child (Position : Child_Cursor) return Boolean is

   begin -- Has_Child

      return Children_Lists.Has_Element (Position.Element_Cursor);

   end Has_Child;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Find_With_Type (Parent      : in Abstract_Entity;
                            Entity_Type : in Content_Type;
                            Position    : in Child_Cursor)
                           return Child_Cursor is

   begin -- Find_With_Type

      return Find_With_Type (Parent      => Parent,
                             Entity_Type => Entity_Type,
                             Position    => Position,
                             Forward     => True);


   end Find_With_Type;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Reverse_Find_With_Type
     (Parent      : in Abstract_Entity;
      Entity_Type : in Content_Type;
      Position    : in Child_Cursor)
     return Child_Cursor is

   begin -- Reverse_Find_With_Type

      return Find_With_Type (Parent      => Parent,
                             Entity_Type => Entity_Type,
                             Position    => Position,
                             Forward     => False);

   end Reverse_Find_With_Type;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Iterate_Children
     (Parent    : in Abstract_Entity;
      Process   : not null access procedure (Position : Child_Cursor));

   procedure Iterate_Children
     (Parent    : in Abstract_Entity;
      Process   : not null access procedure (Position : Child_Cursor)) is

      List_Cursor : Children_Lists.Cursor;
      Next_Cursor : Children_Lists.Cursor;

   begin -- Iterate_Children

      Assert_Multipart (Parent);

      List_Cursor := First (Parent.Children.all);

      while Children_Lists.Has_Element (List_Cursor) loop

         Next_Cursor := Children_Lists.Next (List_Cursor);

         Process (Child_Cursor'(Strategy       => Strategy_Simple,
                                Element_Cursor => List_Cursor,
                                Parent         => Parent'Unchecked_Access));

         List_Cursor := Next_Cursor;

      end loop;

   end Iterate_Children;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Iterate_Children
     (Parent    : in Abstract_Entity;
      Strategy  : in Cursor_Strategy;
      Process   : not null access procedure (Position :
                                             Child_Cursor)) is

      -------------------------------------------------------------------------
      procedure Recurse (Current_Element : in out Abstract_Entity'Class);

      procedure Recurse (Current_Element : in out Abstract_Entity'Class) is

      begin -- Recurse

         if Is_Multipart (Current_Element) then

            Iterate_Children (Parent   => Current_Element,
                              Strategy => Strategy,
                              Process  => Process);

         end if;

      end Recurse;

      -------------------------------------------------------------------------
      List_Cursor : Children_Lists.Cursor;
      Next_Cursor : Children_Lists.Cursor;

   begin -- Iterate_Children

      Assert_Multipart (Parent);

      List_Cursor := First (Parent.Children.all);

      while Children_Lists.Has_Element (List_Cursor) loop

         Next_Cursor := Children_Lists.Next (List_Cursor);

         case Strategy is

            when Strategy_Simple =>

              Process
                (Child_Cursor'(Strategy       => Strategy_Simple,
                               Element_Cursor => List_Cursor,
                               Parent         => Parent'Unchecked_Access));

            when Strategy_Recursive =>

              Update_Element (Container => Parent.Children.all,
                              Position  => List_Cursor,
                              Process   => Recurse'Access);

              Process (Child_Cursor'
                       (Strategy       => Strategy_Recursive,
                        Element_Cursor => List_Cursor,
                        Parent         => Parent'Unchecked_Access,
                        Stack          => Child_Stack'(Controlled with
                                                       Pointer => null)));

         end case;

         List_Cursor := Next_Cursor;

      end loop;

   end Iterate_Children;


   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Reverse_Iterate_Children
     (Parent    : in Abstract_Entity;
      Strategy  : in Cursor_Strategy;
      Process   : not null access procedure (Position :
                                             Child_Cursor)) is

      -------------------------------------------------------------------------
      procedure Recurse (Current_Element : in out Abstract_Entity'Class);

      procedure Recurse (Current_Element : in out Abstract_Entity'Class) is

      begin -- Recurse

         if Is_Multipart (Current_Element) then

            Reverse_Iterate_Children (Parent   => Current_Element,
                                      Strategy => Strategy,
                                      Process  => Process);

         end if;

      end Recurse;

      -------------------------------------------------------------------------
      List_Cursor     : Children_Lists.Cursor;
      Previous_Cursor : Children_Lists.Cursor;

   begin -- Reverse_Iterate_Children

      Assert_Multipart (Parent);

      List_Cursor := Last (Parent.Children.all);

      while Children_Lists.Has_Element (List_Cursor) loop

         Previous_Cursor := Children_Lists.Previous (List_Cursor);

         case Strategy is

            when Strategy_Simple =>

              Process
                (Child_Cursor'(Strategy       => Strategy_Simple,
                               Element_Cursor => List_Cursor,
                               Parent         => Parent'Unchecked_Access));

            when Strategy_Recursive =>

              Update_Element (Container => Parent.Children.all,
                              Position  => List_Cursor,
                              Process   => Recurse'Access);

              Process
                (Child_Cursor'(Strategy       => Strategy_Recursive,
                               Element_Cursor => List_Cursor,
                               Parent         => Parent'Unchecked_Access,
                               Stack          => Child_Stack'(Controlled with
                                                       Pointer => null)));

         end case;

         List_Cursor := Previous_Cursor;

      end loop;

   end Reverse_Iterate_Children;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --           OVERRIDING PROCEDURES FOR ABSTRACT ENTITY                    --
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Initialize (Entity: in out Abstract_Entity) is

   begin -- Initialize

      if Entity.Children = null then
         Entity.Children := new Children_List;
      end if;

   end Initialize;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Adjust (Entity: in out Abstract_Entity) is

      New_Children_List : Children_List_Pointer := new Children_List;
      Current_Cursor    : Children_Lists.Cursor := First (Entity.Children.all);

   begin -- Adjust

      while Children_Lists.Has_Element (Current_Cursor) loop

         Entities.Append (New_Children_List.all,
                          Children_Lists.Element (Current_Cursor));

         Children_Lists.Next (Current_Cursor);

      end loop;

      Entity.Children   := New_Children_List;
      New_Children_List := null;

   end Adjust;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Finalize (Entity: in out Abstract_Entity) is

      procedure Free is
         new Ada.Unchecked_Deallocation
        (Object => Children_List, Name => Children_List_Pointer);

   begin -- Finalize

      if Entity.Children /= null
        and Entity.Children /= Empty_Children_List'Access
      then
         Free (Entity.Children);
      end if;

      Entity.Children := null;

   end Finalize;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --                 OVERRIDING PROCEDURES FOR CURSOR STACK                 --
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Initialize (Object: in out Child_Stack) is

   begin -- Initialize

      Object.Pointer := null;

   end Initialize;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Adjust     (Object: in out Child_Stack) is

      Old_Stack : Stack_Node_Pointer := Object.Pointer;
      New_Stack : Stack_Node_Pointer := null;

   begin -- Adjust

      if Old_Stack /= null then

         New_Stack :=
           new Cursor_Stack_Node'
           (Element_Cursor => Old_Stack.all.Element_Cursor,
            Parent         => Old_Stack.all.Parent,
            Next           => null);

         Old_Stack := Old_Stack.all.Next;

      end if;

      while Old_Stack /= null loop

         New_Stack.all.Next :=
           new Cursor_Stack_Node'
           (Element_Cursor => Old_Stack.all.Element_Cursor,
            Parent         => Old_Stack.all.Parent,
            Next           => null);

         New_Stack := New_Stack.all.Next;
         Old_Stack := Old_Stack.all.Next;

      end loop;

      Object.Pointer := New_Stack;

   end Adjust;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Finalize   (Object: in out Child_Stack) is

   begin -- Finalize

      Truncate_Stack (Object);

   end Finalize;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --         PRIVATE SUBPROGRAMS DEFINED AT THE TOP OF THE BODY             --
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Assert_Multipart (Entity : Abstract_Entity'Class) is

   begin -- Assert_Multipart

      if Entity.Entity_Type.MIME_Type /= T_Multipart then
         raise Illegal_Child_Operation;
      end if;

   end Assert_Multipart;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Assert_Non_Empty (Entity : Abstract_Entity'Class) is

   begin -- Assert_Non_Empty

      if Length (Entity.Children.all) = 0 then
         raise Illegal_Child_Operation;
      end if;

   end Assert_Non_Empty;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Assert_Relationship (Parent : in Abstract_Entity'Class;
                                  Child  : in Child_Cursor) is

      Current_Pointer : Abstract_Entity_Pointer := Child.Parent;
      Stack_Pointer   : Stack_Node_Pointer;

   begin -- Assert_Relationship

      case Child.Strategy is

         when Strategy_Simple =>

            if not Is_Multipart (Parent)
              or Child.Parent /= Parent'Unchecked_Access
            then
               raise Illegal_Child_Operation;
            end if;

         when Strategy_Recursive =>

            Stack_Pointer := Child.Stack.Pointer;

            if Child.Parent = null then
               raise Illegal_Child_Operation;
            else
               Assert_Multipart (Child.Parent.all);
               Assert_Non_Empty (Child.Parent.all);
            end if;

            while Current_Pointer /= Parent'Unchecked_Access loop

               if not Children_Lists.Has_Element
                 (Stack_Pointer.all.Element_Cursor)
               then
                  raise Illegal_Child_Operation;
               end if;

               if Stack_Pointer.all.Parent = null then
                  raise Illegal_Child_Operation;
               else
                  Assert_Multipart (Stack_Pointer.Parent.all);
                  Assert_Non_Empty (Stack_Pointer.Parent.all);
                  exit when Stack_Pointer.Parent = Parent'Unchecked_Access;
               end if;

               if Stack_Pointer.all.Next = null then
                  raise Illegal_Child_Operation;
               else
                  Stack_Pointer := Stack_Pointer.all.Next;
               end if;

               Current_Pointer := Stack_Pointer.all.Parent;

            end loop;

      end case;

   end Assert_Relationship;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Assert_Valid_Cursor (Object : in Child_Cursor) is

      Stack_Pointer   : Stack_Node_Pointer;

   begin -- Assert_Valid_Cursor

      if Object.Parent = null then
         raise Illegal_Child_Operation;
      else
         Assert_Multipart (Object.Parent.all);
         Assert_Non_Empty (Object.Parent.all);
      end if;

      if not Children_Lists.Has_Element (Object.Element_Cursor) then
         raise Illegal_Child_Operation;
      end if;

      case Object.Strategy is

         when Strategy_Simple =>
           null;

         when Strategy_Recursive =>

            Stack_Pointer := Object.Stack.Pointer;

            while Stack_Pointer /= null loop

               if not Children_Lists.Has_Element
                 (Stack_Pointer.all.Element_Cursor)
               then
                  raise Illegal_Child_Operation;
               end if;

               if Stack_Pointer.all.Parent = null then
                  raise Illegal_Child_Operation;
               else
                  Assert_Multipart (Object.Parent.all);
                  Assert_Non_Empty (Object.Parent.all);
               end if;

               Stack_Pointer := Stack_Pointer.all.Next;

            end loop;

      end case;

   end Assert_Valid_Cursor;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Push_Stack (Object        : in out Child_Cursor;
                         New_Parent    : in     Abstract_Entity_Pointer;
                         New_Reference : in     Children_Lists.Cursor) is

      Current_Top : Stack_Node_Pointer := Object.Stack.Pointer;
      New_Item    : Stack_Node_Pointer
        := new Cursor_Stack_Node'(Element_Cursor => Object.Element_Cursor,
                                  Parent         => Object.Parent,
                                  Next           => Current_Top);

   begin -- Push_Stack

      Object.Element_Cursor := New_Reference;
      Object.Parent         := New_Parent;
      Object.Stack.Pointer  := New_Item;

   end Push_Stack;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Pop_Stack  (Object        : in out Child_Cursor) is

      New_Top : Stack_Node_Pointer;

      procedure Free is new Ada.Unchecked_Deallocation
        (Cursor_Stack_Node, Stack_Node_Pointer);

   begin -- Pop_Stack

      if Object.Stack.Pointer = null then

         Object.Element_Cursor := Children_Lists.No_Element;
         Object.Parent         := null;

      else

         New_Top               := Object.Stack.Pointer.all.Next;
         Object.Element_Cursor := Object.Stack.Pointer.all.Element_Cursor;
         Object.Parent         := Object.Stack.Pointer.all.Parent;

         Object.Stack.Pointer.all.Next := null;
         Free (Object.Stack.Pointer);
         Object.Stack.Pointer          := New_Top;

      end if;

   end Pop_Stack;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Truncate_Stack (Object    : in out Child_Stack) is

      New_Top : Stack_Node_Pointer;

      procedure Free is new Ada.Unchecked_Deallocation
        (Cursor_Stack_Node, Stack_Node_Pointer);

   begin -- Truncate_Stack

      while Object.Pointer /= null loop

         New_Top := Object.Pointer.all.Next;

         Object.Pointer.all.Next    := null;
         Free (Object.Pointer);
         Object.Pointer             := New_Top;

      end loop;


   end Truncate_Stack;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Find_With_Type (Parent      : in Abstract_Entity;
                            Entity_Type : in Content_Type;
                            Position    : in Child_Cursor;
                            Forward     : in Boolean)
                           return Child_Cursor is

      Are_Equal : Boolean := False;
      Entity    : Child_Cursor := Position;

      -------------------------------------------------------------------------
      procedure Consider_Entity (Current : in Abstract_Entity'Class);

      procedure Consider_Entity (Current : in Abstract_Entity'Class) is

      begin -- Consider_Entity

         if Current.Entity_Type = Entity_Type then
            Are_Equal := True;
         end if;

      end Consider_Entity;

      -------------------------------------------------------------------------
      Current : Child_Cursor := Position;

   begin -- Simple_Find_With_Type

      if Current.Parent /= null
        and Children_Lists."/="(Current.Element_Cursor,
                                Children_Lists.No_Element)
      then

         Assert_Relationship (Parent, Position);

      else

         Assert_Multipart (Parent);

         if Forward then
            Current := First_Child (Parent, Current.Strategy);
         else
            Current := Last_Child (Parent, Current.Strategy);
         end if;

      end if;

      if Children_Lists.Has_Element (Current.Element_Cursor) then
         Children_Lists.Query_Element (Position => Current.Element_Cursor,
                                       Process  => Consider_Entity'Access);
      end if;

      while Has_Child (Current)
        and not Are_Equal
      loop

         if Forward then
            Next_Child (Current);
         else
            Previous_Child (Current);
         end if;

         if Children_Lists.Has_Element (Current.Element_Cursor) then
            Children_Lists.Query_Element (Position => Current.Element_Cursor,
                                          Process  => Consider_Entity'Access);
         end if;

      end loop;

      return Current;

   end Find_With_Type;

end Basil.Entities;
