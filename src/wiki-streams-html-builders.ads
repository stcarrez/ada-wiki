-----------------------------------------------------------------------
--  wiki-streams-html-builders -- Wiki writer to a string builder
--  Copyright (C) 2011, 2012, 2013, 2015, 2016 Stephane Carrez
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
with Wiki.Streams.Builders;

--  == Writer interfaces ==
--  The <tt>Wiki.Writers</tt> package defines the interfaces used by the renderer to write
--  their outputs.
--
package Wiki.Streams.Html.Builders is

   type Html_Output_Builder_Stream is limited new Wiki.Streams.Builders.Output_Builder_Stream
     and Wiki.Streams.Html.Html_Output_Stream with private;
   type Html_Output_Builder_Stream_Access is access all Html_Output_Builder_Stream'Class;

   overriding
   procedure Write_Wide_Element (Stream  : in out Html_Output_Builder_Stream;
                                 Name    : in String;
                                 Content : in Wiki.Strings.WString);

   overriding
   procedure Write_Wide_Attribute (Stream  : in out Html_Output_Builder_Stream;
                                   Name    : in String;
                                   Content : in Unbounded_Wide_Wide_String);

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   overriding
   procedure Write_Wide_Attribute (Stream  : in out Html_Output_Builder_Stream;
                                   Name    : in String;
                                   Content : in Wiki.Strings.WString);

   overriding
   procedure Start_Element (Stream : in out Html_Output_Builder_Stream;
                            Name   : in String);

   overriding
   procedure End_Element (Stream : in out Html_Output_Builder_Stream;
                          Name   : in String);

   overriding
   procedure Write_Wide_Text (Stream  : in out Html_Output_Builder_Stream;
                              Content : in Wiki.Strings.WString);

private

   type Html_Output_Builder_Stream is limited new Wiki.Streams.Builders.Output_Builder_Stream
     and Wiki.Streams.Html.Html_Output_Stream with record
      --  Whether an XML element must be closed (that is a '>' is necessary)
      Close_Start : Boolean := False;
   end record;

end Wiki.Streams.Html.Builders;
