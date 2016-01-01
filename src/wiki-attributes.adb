-----------------------------------------------------------------------
--  wiki-attributes -- Wiki document attributes
--  Copyright (C) 2015, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Characters.Conversions;
with Ada.Unchecked_Deallocation;

package body Wiki.Attributes is

   use Ada.Characters;

   --  ------------------------------
   --  Get the attribute name.
   --  ------------------------------
   function Get_Name (Position : in Cursor) return String is
      Attr : constant Attribute_Access := Attribute_Vectors.Element (Position.Pos);
   begin
      return Attr.Name;
   end Get_Name;

   --  ------------------------------
   --  Get the attribute value.
   --  ------------------------------
   function Get_Value (Position : in Cursor) return String is
      Attr : constant Attribute_Access := Attribute_Vectors.Element (Position.Pos);
   begin
      return Ada.Characters.Conversions.To_String (Attr.Value);
   end Get_Value;

   --  ------------------------------
   --  Get the attribute wide value.
   --  ------------------------------
   function Get_Wide_Value (Position : in Cursor) return Wide_Wide_String is
      Attr : constant Attribute_Access := Attribute_Vectors.Element (Position.Pos);
   begin
      return Attr.Value;
   end Get_Wide_Value;

   --  ------------------------------
   --  Get the attribute wide value.
   --  ------------------------------
   function Get_Unbounded_Wide_Value (Position : in Cursor) return Unbounded_Wide_Wide_String is
      Attr : constant Attribute_Access := Attribute_Vectors.Element (Position.Pos);
   begin
      return To_Unbounded_Wide_Wide_String (Attr.Value);
   end Get_Unbounded_Wide_Value;

   --  ------------------------------
   --  Returns True if the cursor has a valid attribute.
   --  ------------------------------
   function Has_Element (Position : in Cursor) return Boolean is
   begin
      return Attribute_Vectors.Has_Element (Position.Pos);
   end Has_Element;

   --  ------------------------------
   --  Move the cursor to the next attribute.
   --  ------------------------------
   procedure Next (Position : in out Cursor) is
   begin
      Attribute_Vectors.Next (Position.Pos);
   end Next;

   --  ------------------------------
   --  Find the attribute with the given name.
   --  ------------------------------
   function Find (List : in Attribute_List_Type;
                  Name : in String) return Cursor is
      Iter : Attribute_Vectors.Cursor := List.List.First;
   begin
      while Attribute_Vectors.Has_Element (Iter) loop
         declare
            Attr : constant Attribute_Access := Attribute_Vectors.Element (Iter);
         begin
            if Attr.Name = Name then
               return Cursor '(Pos => Iter);
            end if;
         end;
         Attribute_Vectors.Next (Iter);
      end loop;
      return Cursor '(Pos => Iter);
   end Find;

   --  ------------------------------
   --  Find the attribute with the given name and return its value.
   --  ------------------------------
   function Get_Attribute (List : in Attribute_List_Type;
                           Name : in String) return Unbounded_Wide_Wide_String is
      Attr : constant Cursor := Find (List, Name);
   begin
      if Has_Element (Attr) then
         return Get_Unbounded_Wide_Value (Attr);
      else
         return Null_Unbounded_Wide_Wide_String;
      end if;
   end Get_Attribute;

   --  ------------------------------
   --  Append the attribute to the attribute list.
   --  ------------------------------
   procedure Append (List  : in out Attribute_List_Type;
                     Name  : in Wide_Wide_String;
                     Value : in Wide_Wide_String) is
      Attr : constant Attribute_Access
        := new Attribute '(Name_Length  => Name'Length,
                           Value_Length => Value'Length,
                           Name         => Conversions.To_String (Name),
                           Value        => Value);
   begin
      List.List.Append (Attr);
   end Append;

   --  ------------------------------
   --  Append the attribute to the attribute list.
   --  ------------------------------
   procedure Append (List  : in out Attribute_List_Type;
                     Name  : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
                     Value : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String) is
   begin
      Append (List, Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (Name),
              Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (Value));
   end Append;

   --  ------------------------------
   --  Get the cursor to get access to the first attribute.
   --  ------------------------------
   function First (List : in Attribute_List_Type) return Cursor is
   begin
      return Cursor '(Pos => List.List.First);
   end First;

   --  ------------------------------
   --  Get the number of attributes in the list.
   --  ------------------------------
   function Length (List : in Attribute_List_Type) return Natural is
   begin
      return Natural (List.List.Length);
   end Length;

   --  ------------------------------
   --  Clear the list and remove all existing attributes.
   --  ------------------------------
   procedure Clear (List : in out Attribute_List_Type) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Attribute,
                                        Name   => Attribute_Access);
      Item : Attribute_Access;
   begin
      while not List.List.Is_Empty loop
         Item := List.List.Last_Element;
         List.List.Delete_Last;
         Free (Item);
      end loop;
   end Clear;

   --  ------------------------------
   --  Finalize the attribute list releasing any storage.
   --  ------------------------------
   overriding
   procedure Finalize (List : in out Attribute_List_Type) is
   begin
      List.Clear;
   end Finalize;

end Wiki.Attributes;
