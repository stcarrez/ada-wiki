-----------------------------------------------------------------------
--  wiki-writers-builders -- Wiki writer to a string builder
--  Copyright (C) 2011, 2012, 2013, 2015 Stephane Carrez
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
with Util.Texts.Builders;

--  == Writer interfaces ==
--  The <tt>Wiki.Writers</tt> package defines the interfaces used by the renderer to write
--  their outputs.
--
package Wiki.Writers.Builders is

   package Wide_Wide_Builders is new Util.Texts.Builders (Element_Type => Wide_Wide_Character,
                                                          Input        => Wide_Wide_String,
                                                          Chunk_Size   => 512);

   type Writer_Builder_Type is limited new Wiki.Writers.Writer_Type with private;
   type Writer_Builder_Type_Access is access all Writer_Builder_Type'Class;

   --  Write the content to the string builder.
   overriding
   procedure Write (Writer  : in out Writer_Builder_Type;
                    Content : in Wide_Wide_String);

   --  Write the content to the string builder.
   overriding
   procedure Write (Writer  : in out Writer_Builder_Type;
                    Content : in Unbounded_Wide_Wide_String);

   --  Write a single character to the string builder.
   overriding
   procedure Write (Writer : in out Writer_Builder_Type;
                    Char   : in Wide_Wide_Character);

   --  Write a single character to the string builder.
   procedure Write_Char (Writer : in out Writer_Builder_Type;
                    Char   : in Character);

   --  Write the string to the string builder.
   procedure Write_String (Writer : in out Writer_Builder_Type;
                           Content : in String);

   --  Iterate over the buffer content calling the <tt>Process</tt> procedure with each
   --  chunk.
   procedure Iterate (Source  : in Writer_Builder_Type;
                      Process : not null access procedure (Chunk : in Wide_Wide_String));

   --  Convert what was collected in the writer builder to a string and return it.
   function To_String (Source : in Writer_Builder_Type) return String;

   type Html_Writer_Type is limited new Writer_Builder_Type
     and Wiki.Writers.Html_Writer_Type with private;
   type Html_Writer_Type_Access is access all Html_Writer_Type'Class;

   overriding
   procedure Write_Wide_Element (Writer  : in out Html_writer_Type;
                                 Name    : in String;
                                 Content : in Unbounded_Wide_Wide_String);

   overriding
   procedure Write_Wide_Attribute (Writer  : in out Html_writer_Type;
                                   Name    : in String;
                                   Content : in Unbounded_Wide_Wide_String);

   overriding
   procedure Start_Element (Writer : in out Html_Writer_Type;
                            Name   : in String);

   overriding
   procedure End_Element (Writer : in out Html_Writer_Type;
                          Name   : in String);

   overriding
   procedure Write_Wide_Text (Writer  : in out Html_Writer_Type;
                              Content : in Unbounded_Wide_Wide_String);

private

   BLOCK_SIZE : constant Positive := 512;

   type Writer_Builder_Type is limited new Writer_Type with record
      Content : Wide_Wide_Builders.Builder (BLOCK_SIZE);
   end record;

   type Html_Writer_Type is limited new Writer_Builder_Type
     and Wiki.Writers.Html_Writer_Type with record
      --  Whether an XML element must be closed (that is a '>' is necessary)
      Close_Start : Boolean := False;
   end record;

end Wiki.Writers.Builders;
