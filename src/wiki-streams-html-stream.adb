-----------------------------------------------------------------------
--  wiki-streams-html-stream -- Generic Wiki HTML output stream
--  Copyright (C) 2016, 2020 Stephane Carrez
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

package body Wiki.Streams.Html.Stream is

   --  Close the current XML entity if an entity was started
   procedure Close_Current (Stream : in out Html_Output_Stream'Class);

   --  Write the string to the stream.
   procedure Write_String (Stream  : in out Html_Output_Stream'Class;
                           Content : in String);

   --  ------------------------------
   --  Close the current XML entity if an entity was started
   --  ------------------------------
   procedure Close_Current (Stream : in out Html_Output_Stream'Class) is
   begin
      if Stream.Close_Start then
         Stream.Write ('>');
         Stream.Close_Start := False;
      end if;
   end Close_Current;

   --  ------------------------------
   --  Write the string to the stream.
   --  ------------------------------
   procedure Write_String (Stream  : in out Html_Output_Stream'Class;
                           Content : in String) is
   begin
      for I in Content'Range loop
         Stream.Write (Wiki.Strings.To_WChar (Content (I)));
      end loop;
   end Write_String;

   --  ------------------------------
   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   --  ------------------------------
   overriding
   procedure Write_Wide_Attribute (Stream  : in out Html_Output_Stream;
                                   Name    : in String;
                                   Content : in Wiki.Strings.UString) is
   begin
      if Stream.Close_Start then
         Html.Write_Escape_Attribute (Stream, Name, Wiki.Strings.To_WString (Content));
      end if;
   end Write_Wide_Attribute;

   --  ------------------------------
   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   --  ------------------------------
   overriding
   procedure Write_Wide_Attribute (Stream  : in out Html_Output_Stream;
                                   Name    : in String;
                                   Content : in Wide_Wide_String) is
   begin
      if Stream.Close_Start then
         Stream.Write_Escape_Attribute (Name, Content);
      end if;
   end Write_Wide_Attribute;

   --  ------------------------------
   --  Start an XML element with the given name.
   --  ------------------------------
   overriding
   procedure Start_Element (Stream : in out Html_Output_Stream;
                            Name   : in String) is
   begin
      Close_Current (Stream);
      Stream.Write ('<');
      Stream.Write_String (Name);
      Stream.Close_Start := True;
   end Start_Element;

   --  ------------------------------
   --  Closes an XML element of the given name.
   --  ------------------------------
   overriding
   procedure End_Element (Stream : in out Html_Output_Stream;
                          Name   : in String) is
   begin
      if Stream.Close_Start then
         Stream.Write (" />");
         Stream.Close_Start := False;
      else
         Close_Current (Stream);
         Stream.Write ("</");
         Stream.Write_String (Name);
         Stream.Write ('>');
      end if;
   end End_Element;

   --  ------------------------------
   --  Write a text escaping any character as necessary.
   --  ------------------------------
   overriding
   procedure Write_Wide_Text (Stream  : in out Html_Output_Stream;
                              Content : in Wiki.Strings.WString) is
   begin
      Close_Current (Stream);
      Stream.Write_Escape (Content);
   end Write_Wide_Text;

end Wiki.Streams.Html.Stream;
