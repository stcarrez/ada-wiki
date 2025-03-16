-----------------------------------------------------------------------
--  wiki-streams-html-stream -- Generic Wiki HTML output stream
--  Copyright (C) 2016, 2020, 2022, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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

   --  Enable/disable strict XML generation.  When disabled, the <br>, <hr>,
   --  <img> elements are not closed.
   overriding
   procedure Set_Strict_XML (Writer : in out Html_Output_Stream;
                             Strict : in Boolean);

   --  Enable/disable indentation temporarily.
   overriding
   procedure Set_Enable_Indent (Writer : in out Html_Output_Stream;
                                Enable : in Boolean);

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
      Close_Start   : Boolean := False;
      Empty_Line    : Boolean := True;
      Strict        : Boolean := True;
      Enable_Indent : Boolean := True;
      Indent_Level  : Natural := 0;
      Indent_Pos    : Natural := 0;
      Text_Length   : Natural := 0;
   end record;

end Wiki.Streams.Html.Stream;
