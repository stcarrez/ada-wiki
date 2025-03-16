-----------------------------------------------------------------------
--  wiki-attributes -- Wiki document attributes
--  Copyright (C) 2015, 2016, 2020, 2022, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Strings;
private with Ada.Containers.Vectors;
private with Ada.Finalization;
private with Util.Refs;

--  == Attributes ==
--  The `Attributes` package defines a simple management of attributes for
--  the wiki document parser.  Attribute lists are described by the `Attribute_List`
--  with some operations to append or query for an attribute.  Attributes are used for
--  the Wiki document representation to describe the HTML attributes that were parsed and
--  several parameters that describe Wiki content (links, ...).
--
--  The Wiki filters and Wiki plugins have access to the attributes before they are added
--  to the Wiki document.  They can check them or modify them according to their needs.
--
--  The Wiki renderers use the attributes to render the final HTML content.
package Wiki.Attributes is

   pragma Preelaborate;

   type Cursor is private;

   --  Get the attribute name.
   function Get_Name (Position : in Cursor) return String;

   --  Get the attribute value.
   function Get_Value (Position : in Cursor) return String;

   --  Get the attribute wide value.
   function Get_Wide_Value (Position : in Cursor) return Wiki.Strings.WString;

   --  Returns True if the attribute was defined with a value.
   function Has_Value (Position : in Cursor) return Boolean;

   --  Returns True if the cursor has a valid attribute.
   function Has_Element (Position : in Cursor) return Boolean;

   --  Move the cursor to the next attribute.
   procedure Next (Position : in out Cursor);

   --  A list of attributes.
   type Attribute_List is private;

   --  Find the attribute with the given name.
   function Find (List : in Attribute_List;
                  Name : in String) return Cursor;

   --  Find the attribute with the given name and return its value.
   function Get_Attribute (List : in Attribute_List;
                           Name : in String) return Wiki.Strings.WString;

   --  Append the attribute to the attribute list.
   procedure Append (List  : in out Attribute_List;
                     Name  : in Wiki.Strings.WString;
                     Value : in Wiki.Strings.WString);
   procedure Append (List  : in out Attribute_List;
                     Name  : in String;
                     Value : in Wiki.Strings.WString);
   procedure Append (List  : in out Attribute_List;
                     Name  : in String;
                     Value : in Wiki.Strings.UString);
   procedure Append (List  : in out Attribute_List;
                     Name  : in String;
                     Value : in Wiki.Strings.BString);
   procedure Append (List  : in out Attribute_List;
                     Name  : in Wiki.Strings.WString);

   --  Get the cursor to get access to the first attribute.
   function First (List : in Attribute_List) return Cursor;

   --  Get the number of attributes in the list.
   function Length (List : in Attribute_List) return Natural;

   --  Clear the list and remove all existing attributes.
   procedure Clear (List : in out Attribute_List);

   --  Iterate over the list attributes and call the <tt>Process</tt> procedure.
   procedure Iterate (List    : in Attribute_List;
                      Process : not null access procedure (Name  : in String;
                                                           Value : in Wiki.Strings.WString));

private

   type Attribute (Name_Length, Value_Length : Natural) is limited
   new Util.Refs.Ref_Entity with record
      Name   : String (1 .. Name_Length);
      Value  : Wiki.Strings.WString (1 .. Value_Length);
   end record;
   type Attribute_Access is access all Attribute'Class;

   --  Special type to differentiate an empty value from no value at all
   --  Example: <input hidden>
   --  Since this is a common case we don't want to optimize this case, just have
   --  a way to identify it from <input hidden="">
   type Attribute_No_Value (Name_Length : Natural) is limited
   new Attribute (Name_Length, 0) with null record;
   type Attribute_No_Value_Access is access all Attribute_No_Value'Class;

   package Attribute_Refs is
     new Util.Refs.Indefinite_References (Attribute'Class, Attribute_Access);

   use Attribute_Refs;

   subtype Attribute_Ref is Attribute_Refs.Ref;

   package Attribute_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Attribute_Ref);

   subtype Attribute_Vector is Attribute_Vectors.Vector;

   type Cursor is record
      Pos : Attribute_Vectors.Cursor;
   end record;

   type Attribute_List is new Ada.Finalization.Controlled with record
      List    : Attribute_Vector;
   end record;

   --  Finalize the attribute list releasing any storage.
   overriding
   procedure Finalize (List : in out Attribute_List);

end Wiki.Attributes;
