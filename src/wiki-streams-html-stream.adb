-----------------------------------------------------------------------
--  wiki-streams-html-stream -- Generic Wiki HTML output stream
--  Copyright (C) 2016, 2020, 2022, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Helpers;
package body Wiki.Streams.Html.Stream is

   --  Close the current XML entity if an entity was started
   procedure Close_Current (Stream : in out Html_Output_Stream'Class);

   --  Write the string to the stream.
   procedure Write_String (Stream  : in out Html_Output_Stream'Class;
                           Content : in String);

   --  ------------------------------
   --  Set the indentation level for HTML output stream.
   --  ------------------------------
   overriding
   procedure Set_Indent_Level (Writer : in out Html_Output_Stream;
                               Indent : in Natural) is
   begin
      Writer.Indent_Level := Indent;
   end Set_Indent_Level;

   --  ------------------------------
   --  Enable/disable strict XML generation.  When disabled, the <br>, <hr>,
   --  <img> elements are not closed.
   --  ------------------------------
   overriding
   procedure Set_Strict_XML (Writer : in out Html_Output_Stream;
                             Strict : in Boolean) is
   begin
      Writer.Strict := Strict;
   end Set_Strict_XML;

   --  ------------------------------
   --  Enable/disable indentation temporarily.
   --  ------------------------------
   overriding
   procedure Set_Enable_Indent (Writer : in out Html_Output_Stream;
                                Enable : in Boolean) is
   begin
      Writer.Enable_Indent := Enable;
   end Set_Enable_Indent;

   --  ------------------------------
   --  Write an optional newline or space.
   --  ------------------------------
   overriding
   procedure Newline (Writer : in out Html_Output_Stream) is
   begin
      if Writer.Indent_Level > 0 and then Writer.Text_Length > 0 then
         Writer.Write (Wiki.Helpers.LF);
         Writer.Empty_Line := True;
      end if;
      Writer.Text_Length := 0;
   end Newline;

   --  ------------------------------
   --  Close the current XML entity if an entity was started
   --  ------------------------------
   procedure Close_Current (Stream : in out Html_Output_Stream'Class) is
   begin
      if Stream.Close_Start then
         Stream.Write ('>');
         Stream.Close_Start := False;
         Stream.Empty_Line := False;
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
      if Content'Length > 0 then
         Stream.Text_Length := Stream.Text_Length + Content'Length;
         Stream.Empty_Line := False;
      end if;
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
      if Stream.Enable_Indent and then Stream.Indent_Pos > 1 then
         if not Stream.Empty_Line then
            Stream.Write (Helpers.LF);
         end if;
         for I in 1 .. Stream.Indent_Pos loop
            Stream.Write (' ');
         end loop;
      end if;
      Stream.Write ('<');
      Stream.Write_String (Name);
      Stream.Close_Start := True;
      Stream.Text_Length := 0;
      Stream.Empty_Line := False;
      Stream.Indent_Pos := Stream.Indent_Pos + Stream.Indent_Level;
   end Start_Element;

   --  ------------------------------
   --  Closes an XML element of the given name.
   --  ------------------------------
   overriding
   procedure End_Element (Stream : in out Html_Output_Stream;
                          Name   : in String) is
      Close_Start : constant Boolean := Stream.Close_Start;
   begin
      if Stream.Indent_Pos >= Stream.Indent_Level then
         Stream.Indent_Pos := Stream.Indent_Pos - Stream.Indent_Level;
      end if;
      if Close_Start and then (Name in "br" | "img" | "hr") then
         if Stream.Strict then
            Stream.Write (" /");
         end if;
         Stream.Write ('>');
         Stream.Close_Start := False;
      else
         Close_Current (Stream);
         if Stream.Enable_Indent and then Stream.Text_Length = 0 and then not Close_Start then
            if not Stream.Empty_Line and then Stream.Indent_Level > 0 then
               Stream.Write (Wiki.Helpers.LF);
            end if;
            if Stream.Indent_Pos > 1 then
               Stream.Write (Helpers.LF);
               for I in 1 .. Stream.Indent_Pos loop
                  Stream.Write (' ');
               end loop;
            end if;
         end if;
         Stream.Write ("</");
         Stream.Write_String (Name);
         Stream.Write ('>');
      end if;
      if Stream.Enable_Indent and then Stream.Indent_Level > 0 then
         Stream.Write (Wiki.Helpers.LF);
         Stream.Empty_Line := True;
      end if;
      Stream.Text_Length := 0;
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
      if Content'Length > 0 then
         Stream.Text_Length := Stream.Text_Length + Content'Length;
         Stream.Empty_Line := False;
      else
         Stream.Write (' ');
      end if;
   end Write_Wide_Text;

end Wiki.Streams.Html.Stream;
