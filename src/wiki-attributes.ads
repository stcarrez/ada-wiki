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
with Ada.Strings.Wide_Wide_Unbounded;
private with Ada.Containers.Vectors;
private with Ada.Finalization;

--  == Attributes ==
--  The  <tt>Attributes</tt> package defines a simple management of attributes for
--  the wiki document parser.  Attribute lists are described by the <tt>Attribute_List_Type</tt>
--  with some operations to append or query for an attribute.
package Wiki.Attributes is

   pragma Preelaborate;

   use Ada.Strings.Wide_Wide_Unbounded;

   type Cursor is private;

   --  Get the attribute name.
   function Get_Name (Position : in Cursor) return String;

   --  Get the attribute value.
   function Get_Value (Position : in Cursor) return String;

   --  Get the attribute wide value.
   function Get_Wide_Value (Position : in Cursor) return Wide_Wide_String;

   --  Get the attribute wide value.
   function Get_Unbounded_Wide_Value (Position : in Cursor) return Unbounded_Wide_Wide_String;

   --  Returns True if the cursor has a valid attribute.
   function Has_Element (Position : in Cursor) return Boolean;

   --  Move the cursor to the next attribute.
   procedure Next (Position : in out Cursor);

   --  A list of attributes.
   type Attribute_List_Type is limited private;

   --  Find the attribute with the given name.
   function Find (List : in Attribute_List_Type;
                  Name : in String) return Cursor;

   --  Find the attribute with the given name and return its value.
   function Get_Attribute (List : in Attribute_List_Type;
                           Name : in String) return Unbounded_Wide_Wide_String;

   --  Append the attribute to the attribute list.
   procedure Append (List  : in out Attribute_List_Type;
                     Name  : in Wide_Wide_String;
                     Value : in Wide_Wide_String);

   --  Append the attribute to the attribute list.
   procedure Append (List  : in out Attribute_List_Type;
                     Name  : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
                     Value : in Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String);

   --  Get the cursor to get access to the first attribute.
   function First (List : in Attribute_List_Type) return Cursor;

   --  Get the number of attributes in the list.
   function Length (List : in Attribute_List_Type) return Natural;

   --  Clear the list and remove all existing attributes.
   procedure Clear (List : in out Attribute_List_Type);

private

   type Attribute (Name_Length, Value_Length : Natural) is limited record
      Name   : String (1 .. Name_Length);
      Value  : Wide_Wide_String (1 .. Value_Length);
   end record;
   type Attribute_Access is access all Attribute;

   package Attribute_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Attribute_Access);

   subtype Attribute_Vector is Attribute_Vectors.Vector;

   type Cursor is record
      Pos : Attribute_Vectors.Cursor;
   end record;

   type Attribute_List_Type is limited new Ada.Finalization.Limited_Controlled with record
      List    : Attribute_Vector;
   end record;

   --  Finalize the attribute list releasing any storage.
   overriding
   procedure Finalize (List : in out Attribute_List_Type);

end Wiki.Attributes;
