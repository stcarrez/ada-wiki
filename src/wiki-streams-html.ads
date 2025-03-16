-----------------------------------------------------------------------
--  wiki-streams-html -- Wiki HTML output stream
--  Copyright (C) 2011, 2012, 2013, 2015, 2016, 2020, 2022, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Strings;

--  === HTML Output Stream ===
--  The `Wiki.Writers` package defines the interfaces used by the renderer to write
--  their outputs.
--
--  The `Input_Stream` interface defines the interface that must be implemented to
--  read the source Wiki content.  The `Read` procedure is called by the parser
--  repeatedly while scanning the Wiki content.
package Wiki.Streams.Html is

   type Html_Output_Stream is limited interface and Output_Stream;
   type Html_Output_Stream_Access is access all Html_Output_Stream'Class;

   --  Set the indentation level for HTML output stream.
   procedure Set_Indent_Level (Writer : in out Html_Output_Stream;
                               Indent : in Natural) is null;

   --  Enable/disable strict XML generation.  When disabled, the <br>, <hr>,
   --  <img> elements are not closed.
   procedure Set_Strict_XML (Writer : in out Html_Output_Stream;
                             Strict : in Boolean) is abstract;

   --  Enable/disable indentation temporarily.
   procedure Set_Enable_Indent (Writer : in out Html_Output_Stream;
                                Enable : in Boolean) is abstract;

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   procedure Write_Wide_Attribute (Writer  : in out Html_Output_Stream;
                                   Name    : in String;
                                   Content : in Wiki.Strings.UString) is abstract;

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   procedure Write_Wide_Attribute (Writer  : in out Html_Output_Stream;
                                   Name    : in String;
                                   Content : in Wide_Wide_String) is abstract;

   --  Start an XML element with the given name.
   procedure Start_Element (Writer : in out Html_Output_Stream;
                            Name   : in String) is abstract;

   --  Closes an XML element of the given name.
   procedure End_Element (Writer : in out Html_Output_Stream;
                          Name   : in String) is abstract;

   --  Write a text escaping any character as necessary.
   procedure Write_Wide_Text (Writer  : in out Html_Output_Stream;
                              Content : in Wiki.Strings.WString) is abstract;

   --  Write an optional newline or space.
   procedure Newline (Writer : in out Html_Output_Stream) is null;

   --  Write a character on the response stream and escape that character as necessary.
   procedure Write_Escape (Stream : in out Html_Output_Stream'Class;
                           Char   : in Wiki.Strings.WChar);

   --  Write a string on the response stream and escape the characters as necessary.
   procedure Write_Escape (Stream  : in out Html_Output_Stream'Class;
                           Content : in Wiki.Strings.WString);

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   procedure Write_Escape_Attribute (Stream  : in out Html_Output_Stream'Class;
                                     Name    : in String;
                                     Content : in Wiki.Strings.WString);

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   procedure Write_Attribute (Writer  : in out Html_Output_Stream'Class;
                              Name    : in String;
                              Content : in String);

end Wiki.Streams.Html;
