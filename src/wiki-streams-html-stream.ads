-----------------------------------------------------------------------
--  wiki-streams-html-stream -- Generic Wiki HTML output stream
--  Copyright (C) 2016, 2020, 2022 Stephane Carrez
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
with Wiki.Strings;

generic
   type Output_Stream is limited new Wiki.Streams.Output_Stream with private;
package Wiki.Streams.Html.Stream is

   type Html_Output_Stream is limited new Output_Stream
     and Html.Html_Output_Stream with private;

   --  Set the indentation level for HTML output stream.
   overriding
   procedure Set_Indent_Level (Writer : in out Html_Output_Stream;
                               Indent : in Natural);

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   overriding
   procedure Write_Wide_Attribute (Stream  : in out Html_Output_Stream;
                                   Name    : in String;
                                   Content : in Wiki.Strings.UString);

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   overriding
   procedure Write_Wide_Attribute (Stream  : in out Html_Output_Stream;
                                   Name    : in String;
                                   Content : in Wide_Wide_String);

   --  Start an XML element with the given name.
   overriding
   procedure Start_Element (Stream : in out Html_Output_Stream;
                            Name   : in String);

   --  Closes an XML element of the given name.
   overriding
   procedure End_Element (Stream : in out Html_Output_Stream;
                          Name   : in String);

   --  Write a text escaping any character as necessary.
   overriding
   procedure Write_Wide_Text (Stream  : in out Html_Output_Stream;
                              Content : in Wiki.Strings.WString);

   --  Write an optional newline or space.
   overriding
   procedure Newline (Writer : in out Html_Output_Stream);

private

   type Html_Output_Stream is limited new Output_Stream
     and Html.Html_Output_Stream with record
      --  Whether an XML element must be closed (that is a '>' is necessary)
      Close_Start  : Boolean := False;
      Empty_Line   : Boolean := True;
      Indent_Level : Natural := 0;
      Indent_Pos   : Natural := 0;
      Text_Length  : Natural := 0;
   end record;

end Wiki.Streams.Html.Stream;
