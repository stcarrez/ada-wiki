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

package body Wiki.Attributes is

   use Ada.Characters;

   --  ------------------------------
   --  Get the attribute name.
   --  ------------------------------
   function Get_Name (Position : in Cursor) return String is
      Attr : constant Attribute_Ref := Attribute_Vectors.Element (Position.Pos);
   begin
      return Attr.Value.Name;
   end Get_Name;

   --  ------------------------------
   --  Get the attribute value.
   --  ------------------------------
   function Get_Value (Position : in Cursor) return String is
      Attr : constant Attribute_Ref := Attribute_Vectors.Element (Position.Pos);
   begin
      return Ada.Characters.Conversions.To_String (Attr.Value.Value);
   end Get_Value;

   --  ------------------------------
   --  Get the attribute wide value.
   --  ------------------------------
   function Get_Wide_Value (Position : in Cursor) return Wiki.Strings.WString is
      Attr : constant Attribute_Ref := Attribute_Vectors.Element (Position.Pos);
   begin
      return Attr.Value.Value;
   end Get_Wide_Value;

   --  ------------------------------
   --  Get the attribute wide value.
   --  ------------------------------
   function Get_Unbounded_Wide_Value (Position : in Cursor) return Unbounded_Wide_Wide_String is
      Attr : constant Attribute_Ref := Attribute_Vectors.Element (Position.Pos);
   begin
      return To_Unbounded_Wide_Wide_String (Attr.Value.Value);
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
   function Find (List : in Attribute_List;
                  Name : in String) return Cursor is
      Iter : Attribute_Vectors.Cursor := List.List.First;
   begin
      while Attribute_Vectors.Has_Element (Iter) loop
         declare
            Attr : constant Attribute_Ref := Attribute_Vectors.Element (Iter);
         begin
            if Attr.Value.Name = Name then
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
   function Get_Attribute (List : in Attribute_List;
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
   --  Find the attribute with the given name and return its value.
   --  ------------------------------
   function Get_Attribute (List : in Attribute_List;
                           Name : in String) return Wiki.Strings.WString is
      Attr : constant Cursor := Find (List, Name);
   begin
      if Has_Element (Attr) then
         return Get_Wide_Value (Attr);
      else
         return "";
      end if;
   end Get_Attribute;

   --  ------------------------------
   --  Append the attribute to the attribute list.
   --  ------------------------------
   procedure Append (List  : in out Attribute_List;
                     Name  : in Wiki.Strings.WString;
                     Value : in Wiki.Strings.WString) is
      Attr : constant Attribute_Access
        := new Attribute '(Util.Refs.Ref_Entity with
                           Name_Length  => Name'Length,
                           Value_Length => Value'Length,
                           Name         => Conversions.To_String (Name),
                           Value        => Value);
   begin
      List.List.Append (Attribute_Refs.Create (Attr));
   end Append;

   --  ------------------------------
   --  Append the attribute to the attribute list.
   --  ------------------------------
   procedure Append (List  : in out Attribute_List;
                     Name  : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
                     Value : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String) is
   begin
      Append (List, Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (Name),
              Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (Value));
   end Append;

   --  ------------------------------
   --  Append the attribute to the attribute list.
   --  ------------------------------
   procedure Append (List  : in out Attribute_List;
                     Name  : in String;
                     Value : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String) is
      Val  : constant Wiki.Strings.WString := To_Wide_Wide_String (Value);
      Attr : constant Attribute_Access
        := new Attribute '(Util.Refs.Ref_Entity with
                           Name_Length  => Name'Length,
                           Value_Length => Val'Length,
                           Name         => Name,
                           Value        => Val);
   begin
      List.List.Append (Attribute_Refs.Create (Attr));
   end Append;

   --  ------------------------------
   --  Get the cursor to get access to the first attribute.
   --  ------------------------------
   function First (List : in Attribute_List) return Cursor is
   begin
      return Cursor '(Pos => List.List.First);
   end First;

   --  ------------------------------
   --  Get the number of attributes in the list.
   --  ------------------------------
   function Length (List : in Attribute_List) return Natural is
   begin
      return Natural (List.List.Length);
   end Length;

   --  ------------------------------
   --  Clear the list and remove all existing attributes.
   --  ------------------------------
   procedure Clear (List : in out Attribute_List) is
   begin
      List.List.Clear;
   end Clear;

   --  ------------------------------
   --  Iterate over the list attributes and call the <tt>Process</tt> procedure.
   --  ------------------------------
   procedure Iterate (List    : in Attribute_List;
                      Process : not null access procedure (Name  : in String;
                                                           Value : in Wiki.Strings.WString)) is
      Iter : Attribute_Vectors.Cursor := List.List.First;
      Item : Attribute_Ref;
   begin
      while Attribute_Vectors.Has_Element (Iter) loop
         Item := Attribute_Vectors.Element (Iter);
         Process (Item.Value.Name, Item.Value.Value);
         Attribute_Vectors.Next (Iter);
      end loop;
   end Iterate;

   --  ------------------------------
   --  Finalize the attribute list releasing any storage.
   --  ------------------------------
   overriding
   procedure Finalize (List : in out Attribute_List) is
   begin
      List.Clear;
   end Finalize;

end Wiki.Attributes;
