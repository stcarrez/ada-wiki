-----------------------------------------------------------------------
--  wiki-parsers-html -- Wiki HTML parser
--  Copyright (C) 2015, 2016 Stephane Carrez
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

with Wiki.Helpers;
with Wiki.Parsers.Html.Entities;
package body Wiki.Parsers.Html is

   --  Parse an HTML attribute
   procedure Parse_Attribute_Name (P    : in out Parser;
                                   Name : in out Wiki.Strings.BString);

   --  Parse a HTML/XML comment to strip it.
   procedure Parse_Comment (P : in out Parser);

   --  Parse a simple DOCTYPE declaration and ignore it.
   procedure Parse_Doctype (P : in out Parser);

   procedure Collect_Attributes (P     : in out Parser);
   function Is_Letter (C : in Wiki.Strings.WChar) return Boolean;
   procedure Collect_Attribute_Value (P     : in out Parser;
                                      Value : in out Wiki.Strings.BString);

   function Is_Letter (C : in Wiki.Strings.WChar) return Boolean is
   begin
      return C > ' ' and C /= ':' and C /= '>' and C /= ''' and C /= '"'
        and C /= '/' and C /= '=' and C /= '<';
   end Is_Letter;

   --  ------------------------------
   --  Parse an HTML attribute
   --  ------------------------------
   procedure Parse_Attribute_Name (P    : in out Parser;
                                   Name : in out Wiki.Strings.BString) is
      C : Wiki.Strings.WChar;
   begin
      Skip_Spaces (P);
      while not P.Is_Eof loop
         Peek (P, C);
         if not Is_Letter (C) then
            Put_Back (P, C);
            return;
         end if;
         Wiki.Strings.Wide_Wide_Builders.Append (Name, C);
      end loop;
   end Parse_Attribute_Name;

   procedure Collect_Attribute_Value (P     : in out Parser;
                                      Value : in out Wiki.Strings.BString) is
      C     : Wiki.Strings.WChar;
      Token : Wiki.Strings.WChar;
   begin
--      Value := Wiki.Strings.To_UString ("");
      Peek (P, Token);
      if Wiki.Helpers.Is_Space (Token) then
         return;
      elsif Token = '>' then
         Put_Back (P, Token);
         return;
      elsif Token /= ''' and Token /= '"' then
         Wiki.Strings.Wide_Wide_Builders.Append (Value, Token);
         while not P.Is_Eof loop
            Peek (P, C);
            if C = ''' or C = '"' or C = ' ' or C = '=' or C = '>' or C = '<' or C = '`' then
               Put_Back (P, C);
               return;
            end if;
            Wiki.Strings.Wide_Wide_Builders.Append (Value, C);
         end loop;
      else
         while not P.Is_Eof loop
            Peek (P, C);
            if C = Token then
               return;
            end if;
            Wiki.Strings.Wide_Wide_Builders.Append (Value, C);
         end loop;
      end if;
   end Collect_Attribute_Value;

   --  ------------------------------
   --  Parse a list of HTML attributes up to the first '>'.
   --  attr-name
   --  attr-name=
   --  attr-name=value
   --  attr-name='value'
   --  attr-name="value"
   --  <name name='value' ...>
   --  ------------------------------
   procedure Collect_Attributes (P     : in out Parser) is
      procedure Append_Attribute (Name : in Wiki.Strings.WString);

      C     : Wiki.Strings.WChar;
      Name  : Wiki.Strings.BString (64);
      Value : Wiki.Strings.BString (256);

      procedure Append_Attribute (Name : in Wiki.Strings.WString) is

         procedure Attribute_Value (Value : in Wiki.Strings.WString);

         procedure Attribute_Value (Value : in Wiki.Strings.WString) is
         begin
            Attributes.Append (P.Attributes, Name, Value);
         end Attribute_Value;

         procedure Attribute_Value is
           new Wiki.Strings.Wide_Wide_Builders.Get (Attribute_Value);
         pragma Inline (Attribute_Value);

      begin
         Attribute_Value (Value);
      end Append_Attribute;
      pragma Inline (Append_Attribute);

      procedure Append_Attribute is
        new Wiki.Strings.Wide_Wide_Builders.Get (Append_Attribute);

   begin
      Wiki.Attributes.Clear (P.Attributes);
      while not P.Is_Eof loop
         Parse_Attribute_Name (P, Name);
         Skip_Spaces (P);
         Peek (P, C);
         if C = '>' or C = '<' or C = '/' then
            Put_Back (P, C);
            exit;
         end if;
         if C = '=' then
            Collect_Attribute_Value (P, Value);
            Append_Attribute (Name);
            Wiki.Strings.Wide_Wide_Builders.Clear (Name);
            Wiki.Strings.Wide_Wide_Builders.Clear (Value);
         elsif Wiki.Strings.Wide_Wide_Builders.Length (Name) > 0 then
            Put_Back (P, C);
            Append_Attribute (Name);
            Wiki.Strings.Wide_Wide_Builders.Clear (Name);
         end if;
      end loop;

      --  Add any pending attribute.
      if Wiki.Strings.Wide_Wide_Builders.Length (Name) > 0 then
         Append_Attribute (Name);
      end if;
   end Collect_Attributes;

   --  ------------------------------
   --  Parse a HTML/XML comment to strip it.
   --  ------------------------------
   procedure Parse_Comment (P : in out Parser) is
      C     : Wiki.Strings.WChar;
   begin
      Peek (P, C);
      while not P.Is_Eof loop
         Peek (P, C);
         if C = '-' then
            declare
               Count : Natural := 1;
            begin
               Peek (P, C);
               while not P.Is_Eof and C = '-' loop
                  Peek (P, C);
                  Count := Count + 1;
               end loop;
               exit when C = '>' and Count >= 2;
            end;
         end if;
      end loop;
   end Parse_Comment;

   --  ------------------------------
   --  Parse a simple DOCTYPE declaration and ignore it.
   --  ------------------------------
   procedure Parse_Doctype (P : in out Parser) is
      C : Wiki.Strings.WChar;
   begin
      while not P.Is_Eof loop
         Peek (P, C);
         exit when C = '>';
      end loop;
   end Parse_Doctype;

   --  ------------------------------
   --  Parse a HTML element <XXX attributes>
   --  or parse an end of HTML element </XXX>
   --  ------------------------------
   procedure Parse_Element (P : in out Parser) is
      C    : Wiki.Strings.WChar;

      procedure Add_Element (Token : in Wiki.Strings.WString);

      procedure Add_Element (Token : in Wiki.Strings.WString) is
         Tag  : Wiki.Html_Tag;
      begin
         Tag := Wiki.Find_Tag (Token);
         if C = '/' then
            Skip_Spaces (P);
            Peek (P, C);
            if C /= '>' then
               Put_Back (P, C);
            end if;
            Flush_Text (P);
            if Tag = Wiki.UNKNOWN_TAG then
               if Token = "noinclude" then
                  P.Is_Hidden := False;
               end if;
            else
               End_Element (P, Tag);
            end if;
         else
            Collect_Attributes (P);
            Peek (P, C);
            if C = '/' then
               Peek (P, C);
               if C = '>' then
                  Start_Element (P, Tag, P.Attributes);
                  End_Element (P, Tag);
                  return;
               end if;
            elsif C /= '>' then
               Put_Back (P, C);
            end if;
            if Tag = UNKNOWN_TAG then
               if Token = "noinclude" then
                  P.Is_Hidden := True;
               end if;
            else
               Start_Element (P, Tag, P.Attributes);
            end if;
         end if;
      end Add_Element;
      pragma Inline_Always (Add_Element);

      procedure Add_Element is
        new Wiki.Strings.Wide_Wide_Builders.Get (Add_Element);
      pragma Inline_Always (Add_Element);

      Name : Wiki.Strings.BString (64);
   begin
      Peek (P, C);
      if C = '!' then
         Peek (P, C);
         if C = '-' then
            Parse_Comment (P);
         else
            Parse_Doctype (P);
         end if;
         return;
      end if;
      if C /= '/' then
         Put_Back (P, C);
      end if;
      Parse_Attribute_Name (P, Name);
      Add_Element (Name);
   end Parse_Element;

   --  ------------------------------
   --  Parse an HTML entity such as &nbsp; and replace it with the corresponding code.
   --  ------------------------------
   procedure Parse_Entity (P     : in out Parser;
                           Token : in Wiki.Strings.WChar) is
      pragma Unreferenced (Token);

      Name : String (1 .. 10);
      Len  : Natural := 0;
      High : Natural := Wiki.Parsers.Html.Entities.Keywords'Last;
      Low  : Natural := Wiki.Parsers.Html.Entities.Keywords'First;
      Pos  : Natural;
      C    : Wiki.Strings.WChar;
   begin
      while Len < Name'Last loop
         Peek (P, C);
         exit when C = ';' or else P.Is_Eof;
         Len := Len + 1;
         Name (Len) := Wiki.Strings.To_Char (C);
      end loop;
      while Low <= High loop
         Pos := (Low + High) / 2;
         if Wiki.Parsers.Html.Entities.Keywords (Pos).all = Name (1 .. Len) then
            Parse_Text (P, Entities.Mapping (Pos));
            return;
         elsif Entities.Keywords (Pos).all < Name (1 .. Len) then
            Low := Pos + 1;
         else
            High := Pos - 1;
         end if;
      end loop;

      --  The HTML entity is not recognized: we must treat it as plain wiki text.
      Parse_Text (P, '&');
      for I in 1 .. Len loop
         Parse_Text (P, Wiki.Strings.To_WChar (Name (I)));
      end loop;
   end Parse_Entity;

end Wiki.Parsers.Html;
