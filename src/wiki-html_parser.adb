-----------------------------------------------------------------------
--  wiki-html_parser -- Wiki HTML parser
--  Copyright (C) 2015, 2016, 2018, 2020, 2021, 2022, 2023 Stephane Carrez
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

with Interfaces;
with Wiki.Helpers;
with Wiki.Html_Parser.Entities;
package body Wiki.Html_Parser is

   use Wiki.Strings;

   --  Parse an HTML element name.
   procedure Parse_Element_Name (Parser : in out Parser_Type;
                                 Text   : in Wiki.Strings.WString;
                                 From   : in Positive;
                                 Last   : out Positive);

   --  Parse an HTML attribute name.
   procedure Parse_Attribute_Name (Parser : in out Parser_Type;
                                   Text   : in Wiki.Strings.WString;
                                   From   : in Positive;
                                   Last   : out Positive);

   procedure Parse_Attribute_Value (Parser : in out Parser_Type;
                                    Text   : in Wiki.Strings.WString;
                                    From   : in Positive;
                                    Last   : out Positive);

   --  Parse a HTML/XML comment to strip it.
   procedure Parse_Comment (Parser : in out Parser_Type;
                            Text   : in Wiki.Strings.WString;
                            From   : in Positive;
                            Last   : out Positive);

   --  Parse a simple DOCTYPE declaration and ignore it.
   procedure Parse_Doctype (Parser : in out Parser_Type;
                            Text   : in Wiki.Strings.WString;
                            From   : in Positive;
                            Last   : out Positive);

   function Is_Letter (C : in Wiki.Strings.WChar) return Boolean;
   function From_Hex (Value : in String) return Wiki.Strings.WChar;

   --  Check if the character is valid for a name attribute.
   function Is_Valid_For_Name (C : in Wiki.Strings.WChar) return Boolean
      is (C > ' ' and then C not in '/' | ''' | '"' | '>' | '=');

   --  Check if the character is valid for a value attribute without quotes.
   function Is_Valid_For_Value (C : in Wiki.Strings.WChar) return Boolean
      is (C > ' ' and then C not in '/' | ''' | '"' | '>'
           | '=' | '<' | '`');

   function Is_Letter (C : in Wiki.Strings.WChar) return Boolean is
   begin
      return C > ' ' and then C not in ':' | '>' | ''' | '"'
        | '/' | '=' | '<';
   end Is_Letter;

   function Is_Empty (Parser : in Parser_Type) return Boolean is
   begin
      return Parser.State = State_None;
   end Is_Empty;

   --  ------------------------------
   --  Parse an HTML element name.
   --  ------------------------------
   procedure Parse_Element_Name (Parser : in out Parser_Type;
                                 Text   : in Wiki.Strings.WString;
                                 From   : in Positive;
                                 Last   : out Positive) is
      C   : Wiki.Strings.WChar;
      Pos : Positive := From;
   begin
      if Wiki.Strings.Length (Parser.Elt_Name) = 0 then
         Pos := Wiki.Helpers.Skip_Spaces (Text, From);
      end if;
      while Pos <= Text'Last loop
         C := Text (Pos);
         if not Is_Letter (C) then
            if Parser.State = State_End_Element then
               Parser.State := State_Expect_End_Element;
            else
               Parser.State := State_Check_Attribute;
            end if;
            exit;
         end if;
         Pos := Pos + 1;
         Wiki.Strings.Wide_Wide_Builders.Append (Parser.Elt_Name, C);
      end loop;
      Last := Pos;
   end Parse_Element_Name;

   --  ------------------------------
   --  Parse an HTML attribute name.
   --  ------------------------------
   procedure Parse_Attribute_Name (Parser : in out Parser_Type;
                                   Text   : in Wiki.Strings.WString;
                                   From   : in Positive;
                                   Last   : out Positive) is
      C   : Wiki.Strings.WChar;
      Pos : Positive := From;
   begin
      Parser.State := State_Parse_Attribute;
      if Wiki.Strings.Length (Parser.Attr_Name) = 0 then
         Pos := Wiki.Helpers.Skip_Spaces (Text, From);
      end if;
      while Pos <= Text'Last loop
         C := Text (Pos);
         if not Is_Valid_For_Name (C) then
            Parser.State := State_Check_Attribute_Value;
            exit;
         end if;
         Pos := Pos + 1;
         Wiki.Strings.Wide_Wide_Builders.Append (Parser.Attr_Name, C);
      end loop;
      Last := Pos;
   end Parse_Attribute_Name;

   --  ------------------------------
   --  Parse an attribute value after the '=' separting the attribute name.
   --  attr-name=value
   --  attr-name='value'
   --  attr-name="value"
   --  ------------------------------
   procedure Parse_Attribute_Value (Parser : in out Parser_Type;
                                    Text   : in Wiki.Strings.WString;
                                    From   : in Positive;
                                    Last   : out Positive) is
      C     : Wiki.Strings.WChar;
      Token : Wiki.Strings.WChar := Parser.Separator;
      Pos   : Positive := From;
   begin
      if Parser.State /= State_Parse_Attribute_Value or else Token = '=' then
         Parser.State := State_Parse_Attribute_Value;
         Token := Text (Pos);
         if Wiki.Helpers.Is_Space_Or_Newline (Token) then
            Pos := Wiki.Helpers.Skip_Spaces (Text, Pos);
            if Pos > Text'Last then
               Last := Pos;
               return;
            end if;
            Token := Text (Pos);
         end if;
         if Token = ''' or else Token = '"' then
            Parser.Separator := Token;
            Pos := Pos + 1;
         else
            Parser.Separator := ' ';
         end if;
      end if;

      if Token /= ''' and then Token /= '"' then
         while Pos <= Text'Last loop
            C := Text (Pos);
            if not Is_Valid_For_Value (C) then
               Parser.State := State_Valid_Attribute_Value;
               exit;
            end if;
            Pos := Pos + 1;
            Wiki.Strings.Wide_Wide_Builders.Append (Parser.Attr_Value, C);
         end loop;
      else
         while Pos <= Text'Last loop
            C := Text (Pos);
            Pos := Pos + 1;
            if C = Token then
               Parser.State := State_Valid_Attribute_Value;
               exit;
            end if;
            Wiki.Strings.Wide_Wide_Builders.Append (Parser.Attr_Value, C);
         end loop;
      end if;
      Last := Pos;
   end Parse_Attribute_Value;

   --  ------------------------------
   --  Parse a HTML/XML comment to strip it.
   --  ------------------------------
   procedure Parse_Comment (Parser : in out Parser_Type;
                            Text   : in Wiki.Strings.WString;
                            From   : in Positive;
                            Last   : out Positive) is
      C    : Wiki.Strings.WChar;
      Pos  : Positive := From;
   begin
      if Parser.State /= State_Comment then
         Parser.State := State_Comment;
         Parser.Counter := 0;
      end if;

      while Pos <= Text'Last loop
         C := Text (Pos);
         Pos := Pos + 1;
         if C = '-' then
            Parser.Counter := Parser.Counter + 1;

         elsif C = '>' then
            if Parser.Counter >= 2 then
               Parser.State := State_None;
               exit;
            end if;
            Parser.Counter := 0;

         else
            Parser.Counter := 0;
         end if;
      end loop;
      Last := Pos;
   end Parse_Comment;

   --  ------------------------------
   --  Parse a simple DOCTYPE declaration and ignore it.
   --  ------------------------------
   procedure Parse_Doctype (Parser : in out Parser_Type;
                            Text   : in Wiki.Strings.WString;
                            From   : in Positive;
                            Last   : out Positive) is
      C   : Wiki.Strings.WChar;
      Pos : Positive := From;
   begin
      Parser.State := State_Doctype;
      while Pos <= Text'Last loop
         C := Text (Pos);
         Pos := Pos + 1;
         if C = '>' then
            Parser.State := State_None;
            exit;
         end if;
      end loop;
      Last := Pos;
   end Parse_Doctype;

   --  ------------------------------
   --  Parse a HTML element <XXX attributes>
   --  or parse an end of HTML element </XXX>
   --  ------------------------------
   procedure Parse_Element (Parser : in out Parser_Type;
                            Text   : in Wiki.Strings.WString;
                            From   : in Positive;
                            Process : not null access
                             procedure (Kind  : in State_Type;
                                        Name  : in Wiki.Strings.WString;
                                        Attr  : in out Wiki.Attributes.Attribute_List);
                            Last   : out Positive) is
      procedure Append_Attribute (Name : in Wiki.Strings.WString);
      procedure Append_Attribute (Name : in Wiki.Strings.WString) is

         procedure Attribute_Value (Value : in Wiki.Strings.WString);

         procedure Attribute_Value (Value : in Wiki.Strings.WString) is
         begin
            Attributes.Append (Parser.Attributes, Name, Value);
         end Attribute_Value;

         procedure Attribute_Value is
           new Wiki.Strings.Wide_Wide_Builders.Get (Attribute_Value);
         pragma Inline (Attribute_Value);

      begin
         Attribute_Value (Parser.Attr_Value);
      end Append_Attribute;
      pragma Inline (Append_Attribute);

      procedure Append_Attribute is
        new Wiki.Strings.Wide_Wide_Builders.Get (Append_Attribute);

      C   : Wiki.Strings.WChar;
      Pos : Positive := From;
   begin
      loop
         case Parser.State is
            when State_None | State_Start =>
               --  The '<' was found, decide what's next.
               if Pos > Text'Last then
                  Parser.State := State_Start;
                  Last := Pos;
                  return;
               end if;
               C := Text (Pos);
               if C = '!' then
                  Pos := Pos + 1;
                  Parser.State := State_Comment_Or_Doctype;
                  if Pos > Text'Last then
                     Last := Pos;
                     return;
                  end if;
               elsif C = '/' then
                  Parser.State := State_End_Element;
                  Pos := Pos + 1;
                  Wiki.Strings.Clear (Parser.Elt_Name);
               else
                  Parser.State := State_Element;
                  Wiki.Strings.Clear (Parser.Elt_Name);
                  Wiki.Attributes.Clear (Parser.Attributes);
               end if;

            when State_Comment_Or_Doctype =>
               --  Decide whether we are parsing a <!DOCTYPE> or <!-- ... -->
               C := Text (Pos);
               if C = '-' then
                  Parse_Comment (Parser, Text, Pos + 1, Last);
               else
                  Parse_Doctype (Parser, Text, Pos + 1, Last);
               end if;
               return;

            when State_Doctype =>
               --  Parse the <!DOCTYPE ...> element.
               Parse_Doctype (Parser, Text, Pos, Last);
               return;

            when State_Comment =>
               --  Parse an XML comment <!-- ... -->
               Parse_Comment (Parser, Text, Pos, Last);
               return;

            when State_Element =>
               --  Parse an HTML start element such as <pre> or <pre attr>
               Parse_Element_Name (Parser, Text, Pos, Last);
               if Last > Text'Last then
                  return;
               end if;
               Pos := Last;

            when State_End_Element =>
               --  Parse an HTML end element such as </pre>
               Parse_Element_Name (Parser, Text, Pos, Last);
               if Last > Text'Last then
                  return;
               end if;
               Pos := Last;
               if Parser.State = State_Check_Attribute and then Text (Pos) = '>' then
                  Last := Pos + 1;
                  Parser.State := State_None;
                  Process (HTML_END, To_WString (Parser.Elt_Name), Parser.Attributes);
                  return;
               end if;

            when State_Check_Attribute =>
               C := Text (Pos);
               if C = '/' then
                  Parser.State := State_Expect_Start_End_Element;
                  Pos := Pos + 1;
                  if Pos > Text'Last then
                     Last := Pos;
                     return;
                  end if;
               elsif C = '>' then
                  Process (HTML_START, To_WString (Parser.Elt_Name), Parser.Attributes);
                  Last := Pos + 1;
                  Parser.State := State_None;
                  return;
               else
                  Parser.State := State_Parse_Attribute;
               end if;

            when State_Expect_End_Element =>
               Pos := Wiki.Helpers.Skip_Spaces (Text, Pos);
               if Pos > Text'Last then
                  Last := Pos;
                  return;
               end if;
               C := Text (Pos);
               if C = '>' then
                  Process (HTML_END, To_WString (Parser.Elt_Name), Parser.Attributes);
                  Last := Pos + 1;
                  Parser.State := State_None;
                  return;
               else
                  Process (HTML_ERROR, To_WString (Parser.Elt_Name), Parser.Attributes);
                  Last := Pos;
                  Parser.State := State_None;
                  return;
               end if;

            when State_Expect_Start_End_Element =>
               C := Text (Pos);
               if C = '>' then
                  Process (HTML_START_END, To_WString (Parser.Elt_Name), Parser.Attributes);
                  Last := Pos + 1;
                  Parser.State := State_None;
                  return;
               else
                  Process (HTML_ERROR, To_WString (Parser.Elt_Name), Parser.Attributes);
                  Last := Pos;
                  Parser.State := State_None;
                  return;
               end if;

            when State_Parse_Attribute =>
               Parse_Attribute_Name (Parser, Text, Pos, Last);
               if Last > Text'Last then
                  return;
               end if;
               Pos := Last;

            when State_Check_Attribute_Value | State_No_Attribute_Value =>
               C := Text (Pos);
               if C = '>' then
                  if Length (Parser.Attr_Name) > 0 then
                     Append_Attribute (Parser.Attr_Name);
                  end if;
                  Process (HTML_START, To_WString (Parser.Elt_Name), Parser.Attributes);
                  Last := Pos + 1;
                  Parser.State := State_None;
                  return;

               elsif C = '/' then
                  Pos := Pos + 1;
                  Parser.State := State_Expect_Start_End_Element;
                  if Pos > Text'Last then
                     Last := Pos;
                     return;
                  end if;

               elsif Wiki.Strings.Length (Parser.Attr_Name) = 0 then
                  Last := Pos;
                  Parser.State := State_None;
                  Process (HTML_ERROR, To_WString (Parser.Elt_Name), Parser.Attributes);
                  return;

               elsif C = '=' then
                  Parser.State := State_Parse_Attribute_Value;
                  Parser.Separator := '=';
                  Pos := Pos + 1;
                  if Pos > Text'Last then
                     Last := Pos;
                     return;
                  end if;

               else
                  Append_Attribute (Parser.Attr_Name);
                  Wiki.Strings.Wide_Wide_Builders.Clear (Parser.Attr_Name);
                  Wiki.Strings.Wide_Wide_Builders.Clear (Parser.Attr_Value);
                  Parser.State := State_Check_Attribute;
               end if;

            when State_Parse_Attribute_Value =>
               Parse_Attribute_Value (Parser, Text, Pos, Last);
               if Last > Text'Last then
                  return;
               end if;
               Pos := Last;

            when State_Valid_Attribute_Value =>
               Append_Attribute (Parser.Attr_Name);
               Wiki.Strings.Wide_Wide_Builders.Clear (Parser.Attr_Name);
               Wiki.Strings.Wide_Wide_Builders.Clear (Parser.Attr_Value);
               Parser.State := State_Check_Attribute;

         end case;
      end loop;
   end Parse_Element;

   use Interfaces;

   function From_Hex (C : in Character) return Interfaces.Unsigned_8 is
      (if C in '0' .. '9' then Character'Pos (C) - Character'Pos ('0')
      elsif C in 'A' .. 'F' then Character'Pos (C) - Character'Pos ('A') + 10
      elsif C in 'a' .. 'f' then Character'Pos (C) - Character'Pos ('a') + 10
      else raise Constraint_Error);

   function From_Hex (Value : in String) return Wiki.Strings.WChar is
      Result : Interfaces.Unsigned_32 := 0;
   begin
      for C of Value loop
         Result := Interfaces.Shift_Left (Result, 4);
         Result := Result + Interfaces.Unsigned_32 (From_Hex (C));
      end loop;
      return Wiki.Strings.WChar'Val (Result);
   end From_Hex;

   --  ------------------------------
   --  Parse an HTML entity such as `&nbsp;` and return its value with the last position
   --  if it was correct.  The first `&` is assumed to have been already verified.
   --  When `Entity` is not 0, the parsing is finished.  When `Last` exceeds the input
   --  text position, it is necessary to call `Parse_Entity` with the next input chunk.
   --  ------------------------------
   procedure Parse_Entity (Parser  : in out Parser_Type;
                           Text    : in Wiki.Strings.WString;
                           From    : in Positive;
                           Status  : in out Entity_State_Type;
                           Entity  : out Wiki.Strings.WChar;
                           Last    : out Natural) is
      Len  : Natural := Parser.Counter;
      C    : Wiki.Strings.WChar;
      Pos  : Positive := From;
   begin
      if Status /= ENTITY_MIDDLE then
         Len := 0;
      end if;
      while Len < MAX_ENTITY_LENGTH loop
         if Pos > Text'Last then
            Parser.Counter := Len;
            Status := ENTITY_MIDDLE;
            Entity := NUL;
            Last := Pos;
            return;
         end if;
         C := Text (Pos);
         Pos := Pos + 1;
         exit when C = ';';
         if Wiki.Helpers.Is_Newline (C) then
            Parser.Counter := 0;
            Entity := NUL;
            Status := ENTITY_NONE;
            Last := From;
            return;
         end if;
         Len := Len + 1;
         Parser.Entity_Name (Len) := Wiki.Strings.To_Char (C);
      end loop;

      Parser.Counter := 0;
      declare
         High : Natural := Wiki.Html_Parser.Entities.Keywords'Last;
         Low  : Natural := Wiki.Html_Parser.Entities.Keywords'First;
      begin
         while Low <= High loop
            declare
               I : constant Natural := (Low + High) / 2;
            begin
               if Wiki.Html_Parser.Entities.Keywords (I).all = Parser.Entity_Name (1 .. Len) then
                  Entity := Entities.Mapping (I);
                  Last := Pos;
                  Status := ENTITY_VALID;
                  return;
               elsif Entities.Keywords (I).all < Parser.Entity_Name (1 .. Len) then
                  Low := I + 1;
               else
                  High := I - 1;
               end if;
            end;
         end loop;
      end;

      if Len > 0 and then Parser.Entity_Name (1) = '#' then
         if Parser.Entity_Name (2) >= '0' and then Parser.Entity_Name (2) <= '9' then
            begin
               C := Wiki.Strings.WChar'Val (Natural'Value (Parser.Entity_Name (2 .. Len)));
               if C = NUL then
                  C := Wiki.Strings.WChar'Val (16#0fffd#);
               end if;
               Entity := C;
               Last := Pos;
               Status := ENTITY_VALID;
               return;

            exception
               when Constraint_Error =>
                  null;
            end;
         elsif Parser.Entity_Name (2) in 'x' | 'X' then
            begin
               C := From_Hex (Parser.Entity_Name (3 .. Len));
               if C = NUL then
                  C := Wiki.Strings.WChar'Val (16#0fffd#);
               end if;
               Entity := C;
               Last := Pos;
               Status := ENTITY_VALID;
               return;

            exception
               when Constraint_Error =>
                  null;
            end;

         end if;
      end if;

      --  The HTML entity is not recognized: we must treat it as plain wiki text.
      Status := ENTITY_NONE;
      Entity := NUL;
      Last := From;
   end Parse_Entity;

end Wiki.Html_Parser;
