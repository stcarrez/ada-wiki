-----------------------------------------------------------------------
--  wiki-streams-html-text_io -- Wiki HTML output stream on Ada Text_IO
--  Copyright (C) 2016 Stephane Carrez
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
with Wiki.Streams.Text_IO;

--  === HTML Output Stream ===
--  The <tt>Wiki.Writers</tt> package defines the interfaces used by the renderer to write
--  their outputs.
--
--  The <tt>Input_Stream</tt> interface defines the interface that must be implemented to
--  read the source Wiki content.  The <tt>Read</tt> procedure is called by the parser
--  repeatedly while scanning the Wiki content.
package Wiki.Streams.Html.Text_IO is

   type Html_File_Output_Stream is limited new Wiki.Streams.Text_IO.File_Output_Stream
     and Html.Html_Output_Stream with private;

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   overriding
   procedure Write_Wide_Attribute (Stream  : in out Html_File_Output_Stream;
                                   Name    : in String;
                                   Content : in Wiki.Strings.UString);

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   overriding
   procedure Write_Wide_Attribute (Stream  : in out Html_File_Output_Stream;
                                   Name    : in String;
                                   Content : in Wide_Wide_String);

   --  Start an XML element with the given name.
   overriding
   procedure Start_Element (Stream : in out Html_File_Output_Stream;
                            Name   : in String);

   --  Closes an XML element of the given name.
   overriding
   procedure End_Element (Stream : in out Html_File_Output_Stream;
                          Name   : in String);

   --  Write a text escaping any character as necessary.
   overriding
   procedure Write_Wide_Text (Stream  : in out Html_File_Output_Stream;
                              Content : in Wiki.Strings.WString);

   --  Write the string to the stream.
   procedure Write_String (Stream  : in out Html_File_Output_Stream'Class;
                           Content : in String);

private

   type Html_File_Output_Stream is limited new Wiki.Streams.Text_IO.File_Output_Stream
     and Html.Html_Output_Stream with record
      --  Whether an XML element must be closed (that is a '>' is necessary)
      Close_Start : Boolean := False;
   end record;

end Wiki.Streams.Html.Text_IO;
