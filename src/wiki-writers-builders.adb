-----------------------------------------------------------------------
--  wiki-writers-builders -- Wiki writer to a string builder
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
with Ada.Characters.Conversions;
with GNAT.Encode_UTF8_String;
package body Wiki.Writers.Builders is

   --  Close the current XML entity if an entity was started
   procedure Close_Current (Stream : in out Html_Writer_Type'Class);

   --  Internal method to write a character on the response stream
   --  and escape that character as necessary.  Unlike 'Write_Char',
   --  this operation does not closes the current XML entity.
   procedure Write_Escape (Stream : in out Html_Writer_Type'Class;
                           Char   : in Wide_Wide_Character);

   --  ------------------------------
   --  Write the content to the string builder.
   --  ------------------------------
   overriding
   procedure Write (Writer  : in out Writer_Builder_Type;
                    Content : in Wide_Wide_String) is
   begin
      Wide_Wide_Builders.Append (Writer.Content, Content);
   end Write;

   --  ------------------------------
   --  Write the content to the string builder.
   --  ------------------------------
   overriding
   procedure Write (Writer  : in out Writer_Builder_Type;
                    Content : in Unbounded_Wide_Wide_String) is
   begin
      Wide_Wide_Builders.Append (Writer.Content, To_Wide_Wide_String (Content));
   end Write;

   --  ------------------------------
   --  Iterate over the buffer content calling the <tt>Process</tt> procedure with each
   --  chunk.
   --  ------------------------------
   procedure Iterate (Source  : in Writer_Builder_Type;
                      Process : not null access procedure (Chunk : in Wide_Wide_String)) is
   begin
      Wide_Wide_Builders.Iterate (Source.Content, Process);
   end Iterate;

   --  ------------------------------
   --  Convert what was collected in the writer builder to a string and return it.
   --  ------------------------------
   function To_String (Source : in Writer_Builder_Type) return String is
      procedure Convert (Chunk : in Wide_Wide_String);

      Pos    : Natural := 1;
      Result : String (1 .. 5 * Wide_Wide_Builders.Length (Source.Content));

      procedure Convert (Chunk : in Wide_Wide_String) is
      begin
         for I in Chunk'Range loop
            GNAT.Encode_UTF8_String.Encode_Wide_Wide_Character (Char   => Chunk (I),
                                                                Result => Result,
                                                                Ptr    => Pos);
         end loop;
      end Convert;

   begin
      Wide_Wide_Builders.Iterate (Source.Content, Convert'Access);
      return Result (1 .. Pos - 1);
   end To_String;

   --  ------------------------------
   --  Write a single character to the string builder.
   --  ------------------------------
   procedure Write (Writer : in out Writer_Builder_Type;
                    Char   : in Wide_Wide_Character) is
   begin
      Wide_Wide_Builders.Append (Writer.Content, Char);
   end Write;

   --  ------------------------------
   --  Write a single character to the string builder.
   --  ------------------------------
   procedure Write_Char (Writer : in out Writer_Builder_Type;
                         Char   : in Character) is
   begin
      Wide_Wide_Builders.Append (Writer.Content,
                                 Ada.Characters.Conversions.To_Wide_Wide_Character (Char));
   end Write_Char;

   --  ------------------------------
   --  Write the string to the string builder.
   --  ------------------------------
   procedure Write_String (Writer : in out Writer_Builder_Type;
                           Content : in String) is
   begin
      for I in Content'Range loop
         Writer.Write_Char (Content (I));
      end loop;
   end Write_String;

   type Unicode_Char is mod 2**31;

   --  ------------------------------
   --  Internal method to write a character on the response stream
   --  and escape that character as necessary.  Unlike 'Write_Char',
   --  this operation does not closes the current XML entity.
   --  ------------------------------
   procedure Write_Escape (Stream : in out Html_Writer_Type'Class;
                           Char   : in Wide_Wide_Character) is
      Code : constant Unicode_Char := Wide_Wide_Character'Pos (Char);
   begin
      --  If "?" or over, no escaping is needed (this covers
      --  most of the Latin alphabet)
      if Code > 16#3F# or Code <= 16#20# then
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
   --  Close the current XML entity if an entity was started
   --  ------------------------------
   procedure Close_Current (Stream : in out Html_Writer_Type'Class) is
   begin
      if Stream.Close_Start then
         Stream.Write_Char ('>');
         Stream.Close_Start := False;
      end if;
   end Close_Current;

   procedure Write_Wide_Element (Writer  : in out Html_Writer_Type;
                                 Name    : in String;
                                 Content : in Unbounded_Wide_Wide_String) is

   begin
      Writer.Start_Element (Name);
      Writer.Write_Wide_Text (Content);
      Writer.End_Element (Name);
   end Write_Wide_Element;

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   procedure Write_Wide_Attribute (Writer  : in out Html_Writer_Type;
                                   Name    : in String;
                                   Content : in Wide_Wide_String) is
   begin
      if Writer.Close_Start then
         Writer.Write_Char (' ');
         Writer.Write_String (Name);
         Writer.Write_Char ('=');
         Writer.Write_Char ('"');
         for I in Content'Range loop
            declare
               C : constant Wide_Wide_Character := Content (I);
            begin
               if C = '"' then
                  Writer.Write_String ("&quot;");
               else
                  Writer.Write_Escape (C);
               end if;
            end;
         end loop;
         Writer.Write_Char ('"');
      end if;
   end Write_Wide_Attribute;

   procedure Write_Wide_Attribute (Writer  : in out Html_Writer_Type;
                                   Name    : in String;
                                   Content : in Unbounded_Wide_Wide_String) is
      Count : constant Natural := Length (Content);
   begin
      if Writer.Close_Start then
         Writer.Write_Char (' ');
         Writer.Write_String (Name);
         Writer.Write_Char ('=');
         Writer.Write_Char ('"');
         for I in 1 .. Count loop
            declare
               C : constant Wide_Wide_Character := Element (Content, I);
            begin
               if C = '"' then
                  Writer.Write_String ("&quot;");
               else
                  Writer.Write_Escape (C);
               end if;
            end;
         end loop;
         Writer.Write_Char ('"');
      end if;
   end Write_Wide_Attribute;

   procedure Start_Element (Writer : in out Html_Writer_Type;
                            Name   : in String) is
   begin
      Close_Current (Writer);
      Writer.Write_Char ('<');
      Writer.Write_String (Name);
      Writer.Close_Start := True;
   end Start_Element;

   procedure End_Element (Writer : in out Html_Writer_Type;
                          Name   : in String) is
   begin
      if Writer.Close_Start then
         Writer.Write_String (" />");
         Writer.Close_Start := False;
      else
         Close_Current (Writer);
         Writer.Write_String ("</");
         Writer.Write_String (Name);
         Writer.Write_Char ('>');
      end if;
   end End_Element;

   procedure Write_Wide_Text (Writer  : in out Html_Writer_Type;
                              Content : in Unbounded_Wide_Wide_String) is
      Count : constant Natural := Length (Content);
   begin
      Close_Current (Writer);
      if Count > 0 then
         for I in 1 .. Count loop
            Html_Writer_Type'Class (Writer).Write_Escape (Element (Content, I));
         end loop;
      end if;
   end Write_Wide_Text;

end Wiki.Writers.Builders;
