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

package body Wiki.Streams.Html.Builders is

   --  Close the current XML entity if an entity was started
   procedure Close_Current (Stream : in out Html_Output_Builder_Stream'Class);

   type Unicode_Char is mod 2**31;

   --  ------------------------------
   --  Close the current XML entity if an entity was started
   --  ------------------------------
   procedure Close_Current (Stream : in out Html_Output_Builder_Stream'Class) is
   begin
      if Stream.Close_Start then
         Stream.Write ('>');
         Stream.Close_Start := False;
      end if;
   end Close_Current;

   --  ------------------------------
   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   --  ------------------------------
   procedure Write_Wide_Attribute (Stream  : in out Html_Output_Builder_Stream;
                                   Name    : in String;
                                   Content : in Wiki.Strings.WString) is
   begin
      if Stream.Close_Start then
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
      end if;
   end Write_Wide_Attribute;

   procedure Write_Wide_Attribute (Stream  : in out Html_Output_Builder_Stream;
                                   Name    : in String;
                                   Content : in Wiki.Strings.UString) is
      Count : constant Natural := Wiki.Strings.Length (Content);
   begin
      if Stream.Close_Start then
         Stream.Write (' ');
         Stream.Write_String (Name);
         Stream.Write ('=');
         Stream.Write ('"');
         for I in 1 .. Count loop
            declare
               C : constant Wiki.Strings.WChar := Wiki.Strings.Element (Content, I);
            begin
               if C = '"' then
                  Stream.Write ("&quot;");
               else
                  Stream.Write_Escape (C);
               end if;
            end;
         end loop;
         Stream.Write ('"');
      end if;
   end Write_Wide_Attribute;

   procedure Start_Element (Stream : in out Html_Output_Builder_Stream;
                            Name   : in String) is
   begin
      Close_Current (Stream);
      Stream.Write ('<');
      Stream.Write_String (Name);
      Stream.Close_Start := True;
   end Start_Element;

   procedure End_Element (Stream : in out Html_Output_Builder_Stream;
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

   procedure Write_Wide_Text (Stream  : in out Html_Output_Builder_Stream;
                              Content : in Wiki.Strings.WString) is
   begin
      Close_Current (Stream);
      for I in Content'Range loop
         Html_Output_Builder_Stream'Class (Stream).Write_Escape (Content (I));
      end loop;
   end Write_Wide_Text;

end Wiki.Streams.Html.Builders;
