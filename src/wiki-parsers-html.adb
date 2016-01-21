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
with Ada.Characters.Conversions;

with Wiki.Helpers;
with Wiki.Parsers.Html.Entities;
package body Wiki.Parsers.Html is

   --  Parse a HTML/XML comment to strip it.
   procedure Parse_Comment (P : in out Parser);

   --  Parse a simple DOCTYPE declaration and ignore it.
   procedure Parse_Doctype (P : in out Parser);

   procedure Collect_Attributes (P     : in out Parser);
   function Is_Letter (C : in Wide_Wide_Character) return Boolean;
   procedure Collect_Attribute_Value (P     : in out Parser;
                                      Value : in out Unbounded_Wide_Wide_String);

   function Is_Letter (C : in Wide_Wide_Character) return Boolean is
   begin
      return C > ' ' and C /= ':' and C /= '>' and C /= ''' and C /= '"'
        and C /= '/' and C /= '=' and C /= '<';
   end Is_Letter;

   --  ------------------------------
   --  Parse an HTML attribute
   --  ------------------------------
   procedure Parse_Attribute_Name (P    : in out Parser;
                                   Name : in out Unbounded_Wide_Wide_String) is
      C : Wide_Wide_Character;
   begin
      Name := To_Unbounded_Wide_Wide_String ("");
      Skip_Spaces (P);
      while not P.Is_Eof loop
         Peek (P, C);
         if not Is_Letter (C) then
            Put_Back (P, C);
            return;
         end if;
         Ada.Strings.Wide_Wide_Unbounded.Append (Name, C);
      end loop;
   end Parse_Attribute_Name;

   procedure Collect_Attribute_Value (P     : in out Parser;
                                      Value : in out Unbounded_Wide_Wide_String) is
      C     : Wide_Wide_Character;
      Token : Wide_Wide_Character;
   begin
      Value := To_Unbounded_Wide_Wide_String ("");
      Peek (P, Token);
      if Wiki.Helpers.Is_Space (Token) then
         return;
      elsif Token = '>' then
         Put_Back (P, Token);
         return;
      elsif Token /= ''' and Token /= '"' then
         Append (Value, Token);
         while not P.Is_Eof loop
            Peek (P, C);
            if C = ''' or C = '"' or C = ' ' or C = '=' or C = '>' or C = '<' or C = '`' then
               Put_Back (P, C);
               return;
            end if;
            Append (Value, C);
         end loop;
      else
         while not P.Is_Eof loop
            Peek (P, C);
            if C = Token then
               return;
            end if;
            Append (Value, C);
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
      C     : Wide_Wide_Character;
      Name  : Unbounded_Wide_Wide_String;
      Value : Unbounded_Wide_Wide_String;
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
            Attributes.Append (P.Attributes, Name, Value);
         elsif Wiki.Helpers.Is_Space (C) and Length (Name) > 0 then
            Attributes.Append (P.Attributes, Name, Null_Unbounded_Wide_Wide_String);
         end if;
      end loop;
      --  Peek (P, C);

      --  Add any pending attribute.
      if Length (Name) > 0 then
         Attributes.Append (P.Attributes, Name, Null_Unbounded_Wide_Wide_String);
      end if;
   end Collect_Attributes;

   --  ------------------------------
   --  Parse a HTML/XML comment to strip it.
   --  ------------------------------
   procedure Parse_Comment (P : in out Parser) is
      C     : Wide_Wide_Character;
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
      C : Wide_Wide_Character;
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
      Name : Unbounded_Wide_Wide_String;
      C    : Wide_Wide_Character;
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
      if C = '/' then
         Skip_Spaces (P);
         Peek (P, C);
         if C /= '>' then
            Put_Back (P, C);
         end if;
         Flush_Text (P);
         End_Element (P, Name);
      else
         Collect_Attributes (P);
         Peek (P, C);
         if C = '/' then
            Peek (P, C);
            if C = '>' then
               Start_Element (P, Name, P.Attributes);
               End_Element (P, Name);
               return;
            end if;
         elsif C /= '>' then
            Put_Back (P, C);
         end if;
         Start_Element (P, Name, P.Attributes);
      end if;
   end Parse_Element;

   --  ------------------------------
   --  Parse an HTML entity such as &nbsp; and replace it with the corresponding code.
   --  ------------------------------
   procedure Parse_Entity (P     : in out Parser;
                           Token : in Wide_Wide_Character) is
      Name : String (1 .. 10);
      Len  : Natural := 0;
      High : Natural := Wiki.Parsers.Html.Entities.Keywords'Last;
      Low  : Natural := Wiki.Parsers.Html.Entities.Keywords'First;
      Pos  : Natural;
      C    : Wide_Wide_Character;
   begin
      loop
         Peek (P, C);
         exit when C = ';';
         if Len < Name'Last then
            Len := Len + 1;
         end if;
         Name (Len) := Ada.Characters.Conversions.To_Character (C);
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
   end Parse_Entity;

end Wiki.Parsers.Html;
