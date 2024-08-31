-----------------------------------------------------------------------
--  wiki-streams-html -- Wiki HTML output stream
--  Copyright (C) 2011, 2012, 2013, 2015, 2016, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package body Wiki.Streams.Html is

   type Unicode_Char is mod 2**31;

   --  Write the string to the stream.
   procedure Write_String (Stream  : in out Html_Output_Stream'Class;
                           Content : in String);

   --  ------------------------------
   --  Write a character on the response stream and escape that character as necessary.
   --  ------------------------------
   procedure Write_Escape (Stream : in out Html_Output_Stream'Class;
                           Char   : in Wiki.Strings.WChar) is
      Code : constant Unicode_Char := Wiki.Strings.WChar'Pos (Char);
   begin
      --  If "?" or over, no escaping is needed (this covers
      --  most of the Latin alphabet)
      if Code > 16#3F# or else Code <= 16#20# then
         Stream.Write (Char);
      elsif Char = '<' then
         Stream.Write ("&lt;");
      elsif Char = '>' then
         Stream.Write ("&gt;");
      elsif Char = '&' then
         Stream.Write ("&amp;");
      else
         Stream.Write (Char);
      end if;
   end Write_Escape;

   --  ------------------------------
   --  Write a string on the response stream and escape the characters as necessary.
   --  ------------------------------
   procedure Write_Escape (Stream  : in out Html_Output_Stream'Class;
                           Content : in Wiki.Strings.WString) is
   begin
      for I in Content'Range loop
         Stream.Write_Escape (Content (I));
      end loop;
   end Write_Escape;

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
   procedure Write_Escape_Attribute (Stream  : in out Html_Output_Stream'Class;
                                     Name    : in String;
                                     Content : in Wiki.Strings.WString) is
   begin
      Stream.Write (' ');
      Stream.Write_String (Name);
      Stream.Write ('=');
      Stream.Write ('"');
      for I in Content'Range loop
         declare
            C : constant Wiki.Strings.WChar := Content (I);
         begin
            if C = '"' then
               Stream.Write ("&quot;");
            else
               Stream.Write_Escape (C);
            end if;
         end;
      end loop;
      Stream.Write ('"');
   end Write_Escape_Attribute;

   --  ------------------------------
   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   --  ------------------------------
   procedure Write_Attribute (Writer  : in out Html_Output_Stream'Class;
                              Name    : in String;
                              Content : in String) is
   begin
      Writer.Write_Wide_Attribute (Name, Wiki.Strings.To_WString (Content));
   end Write_Attribute;

end Wiki.Streams.Html;
