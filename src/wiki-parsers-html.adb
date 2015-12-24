-----------------------------------------------------------------------
--  wiki-parsers-html -- Wiki HTML parser
--  Copyright (C) 2015 Stephane Carrez
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
with Ada.Strings.Wide_Wide_Unbounded;

package body Wiki.Parsers.Html is

   use Ada.Strings.Wide_Wide_Unbounded;

   procedure Collect_Attributes (P     : in out Parser);
   function Is_Space (C : in Wide_Wide_Character) return Boolean;
   function Is_Letter (C : in Wide_Wide_Character) return Boolean;
   procedure Collect_Attribute_Value (P     : in out Parser;
                                      Value : in out Unbounded_Wide_Wide_String);

   function Is_Space (C : in Wide_Wide_Character) return Boolean is
   begin
      return C = ' ' or C = LF or C = CR;
   end Is_Space;

   function Is_Letter (C : in Wide_Wide_Character) return Boolean is
   begin
      return C > ' ' and C /= ':' and C /= '>' and C /= ''' and C /= '"'
        and C /= '/' and C /= '=';
   end Is_Letter;

   procedure Skip_Spaces (P : in out Parser) is
      C : Wide_Wide_Character;
   begin
      while not P.Is_Eof loop
         Peek (P, C);
         if not Is_Space (C) then
            Put_Back (P, C);
            return;
         end if;
      end loop;
   end Skip_Spaces;

   --  Parse an HTML attribute
   procedure Parse_Attribute_Name (P    : in out Parser;
                                   Name : in out Unbounded_Wide_Wide_String) is
      C : Wide_Wide_Character;
   begin
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
                                      Quote : in Wide_Wide_Character;
                                      Value : in out Unbounded_Wide_Wide_String) is
      C : Wide_Wide_Character;
   begin
      while not P.Is_Eof loop
         Peek (P, C);
         exit when C = Quote;
         Append (Value, C);
      end loop;
   end Collect_Attribute_Value;

   procedure Collect_Attribute_Value (P     : in out Parser;
                                      Value : in out Unbounded_Wide_Wide_String) is
      C : Wide_Wide_Character;
   begin
      while not P.Is_Eof loop
         Peek (P, C);
         if C = ''' or C = '"' or C = ' ' or C = '=' or C = '>' or C = '<' or C = '`' then
            Put_Back (P, C);
            return;
         end if;
         Append (Value, C);
      end loop;
   end Collect_Attribute_Value;

   --  Parse a list of HTML attributes up to the first '>'.
   --  attr-name
   --  attr-name=
   --  attr-name=value
   --  attr-name='value'
   --  attr-name="value"
   --  <name name='value' ...>
   procedure Collect_Attributes (P     : in out Parser) is
      C     : Wide_Wide_Character;
      Name  : Unbounded_Wide_Wide_String;
      Value : Unbounded_Wide_Wide_String;
   begin
      loop
         Parse_Attribute_Name (P, Name);
         Skip_Spaces (P);
         Peek (P, C);
         exit when C = '>';
         if C = '=' then
            Collect_Attribute_Value (P, Value);
            Attributes.Append (P.Attributes, Name, Value);
         elsif Is_Space (C) then
            Attributes.Append (P.Attributes, Name, Null_Unbounded_Wide_Wide_String);
         end if;
      end loop;
      --  Peek (P, C);

      --  Add any pending attribute.
      if Length (Name) > 0 then
         Attributes.Append (P.Attributes, Name, Null_Unbounded_Wide_Wide_String);
      end if;
   end Collect_Attributes;

   --  Parse a HTML element <XXX attributes>
   --  or parse an end of HTML element </XXX>
   procedure Parse_Element (P : in out Parser) is
      Name : Unbounded_Wide_Wide_String;
      C    : Wide_Wide_Character;
   begin
      Peek (P, C);
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
         P.Document.End_Element (Name);
      else
         Collect_Attributes (P);
         P.Document.Start_Element (Name, P.Attributes);
      end if;
   end Parse_Element;

end Wiki.Parsers.Html;
