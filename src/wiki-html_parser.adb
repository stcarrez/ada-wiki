-----------------------------------------------------------------------
--  wiki-html_parser -- Wiki HTML parser
--  Copyright (C) 2015 - 2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
   function From_Hex (Value : in String) return Interfaces.Unsigned_32;

   --  Check if the character is valid for a name attribute.
   function Is_Valid_For_Name (C : in Wiki.Strings.WChar) return Boolean
      is (C > ' ' and then C not in '/' | ''' | '"' | '>' | '=' | '<');

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
         Pos := Wiki.Helpers.Skip_Spaces_Or_Newline (Text, From);
         if Pos <= Text'Last and then Text (Pos) = ':' then
            Last := Pos;
            Parser.State := State_Error;
            return;
         end if;
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
            Pos := Wiki.Helpers.Skip_Spaces_Or_Newline (Text, Pos);
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

   procedure Replace_Entities (Parser : in out Parser_Type;
                               From   : in Wiki.Strings.WString;
                               Into   : in out Wiki.Strings.WString;
                               Last   : out Natural) is
      Pos        : Natural := From'First;
      Target_Pos : Natural := Into'First - 1;
      C          : Wiki.Strings.WChar;
      Status     : Entity_State_Type := ENTITY_NONE;
   begin
      while Pos <= From'Last loop
         Target_Pos := Target_Pos + 1;
         C := From (Pos);
         if C = '&' then
            declare
               Amp_Pos : constant Natural := Pos;
            begin
               Parse_Entity (Parser, From, Amp_Pos + 1, Status, C, Pos);
               if Status = ENTITY_VALID then
                  Into (Target_Pos) := C;
               else
                  --  Not a valid entity: keep the '&' and reprocess what follows it.
                  Into (Target_Pos) := '&';
                  Pos := Amp_Pos + 1;
               end if;
            end;
         else
            Into (Target_Pos) := C;
            Pos := Pos + 1;
         end if;
      end loop;
      Last := Target_Pos;
   end Replace_Entities;

   --  ------------------------------
   --  Parse a HTML element <XXX attributes>
   --  or parse an end of HTML element </XXX>
   --  ------------------------------
   procedure Parse_Element (Parser : in out Parser_Type;
                            Text   : in Wiki.Strings.WString;
                            From   : in Positive;
                            Result : out State_Type;
                            Last   : out Positive) is
      procedure Append_Attribute (Name : in Wiki.Strings.WString);
      procedure Append_Attribute (Name : in Wiki.Strings.WString) is

         procedure Attribute_Value (Value : in Wiki.Strings.WString);

         procedure Attribute_Value (Value : in Wiki.Strings.WString) is
         begin
            if (for some C of Value => C = '&') then
               declare
                  New_Value : Wiki.Strings.WString (1 .. Value'Length);
                  Last : Natural;
               begin
                  Replace_Entities (Parser, Value, New_Value, Last);
                  Attributes.Append (Parser.Attributes, Name, New_Value (1 .. Last));
               end;
            else
               Attributes.Append (Parser.Attributes, Name, Value);
            end if;
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

      procedure Append_Attribute_No_Value (Name : in Wiki.Strings.WString);
      procedure Append_Attribute_No_Value (Name : in Wiki.Strings.WString) is
      begin
         Attributes.Append (Parser.Attributes, Name);
      end Append_Attribute_No_Value;
      pragma Inline (Append_Attribute_No_Value);

      procedure Append_Attribute_No_Value is
        new Wiki.Strings.Wide_Wide_Builders.Get (Append_Attribute_No_Value);

      C   : Wiki.Strings.WChar;
      Pos : Positive := From;
   begin
      Result := HTML_NONE;
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
               elsif C in 'a' .. 'z' | 'A' .. 'Z' then
                  --  Only an ASCII letter can start a HTML element name.
                  Parser.State := State_Element;
                  Wiki.Strings.Clear (Parser.Elt_Name);
                  Wiki.Attributes.Clear (Parser.Attributes);
               else
                  --  The '<' is not followed by a letter: it is a literal text
                  --  character (HTML5 tag open state).  Only the '<' is consumed.
                  Parser.State := State_None;
                  Wiki.Strings.Clear (Parser.Elt_Name);
                  Wiki.Attributes.Clear (Parser.Attributes);
                  Result := HTML_ERROR;
                  Last := Pos;
                  return;
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
                  Result := HTML_END;
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
                  Last := Pos + 1;
                  Parser.State := State_None;
                  Result := HTML_START;
                  return;
               else
                  Parser.State := State_Parse_Attribute;
               end if;

            when State_Expect_End_Element =>
               Pos := Wiki.Helpers.Skip_Spaces_Or_Newline (Text, Pos);
               if Pos > Text'Last then
                  Last := Pos;
                  return;
               end if;
               C := Text (Pos);
               if C = '>' then
                  Last := Pos + 1;
                  Parser.State := State_None;
                  Result := HTML_END;
                  return;
               else
                  Last := Pos;
                  Parser.State := State_None;
                  Result := HTML_ERROR;
                  return;
               end if;

            when State_Expect_Start_End_Element =>
               C := Text (Pos);
               if C = '>' then
                  Last := Pos + 1;
                  Parser.State := State_None;
                  Result := HTML_START_END;
                  return;
               else
                  Last := Pos;
                  Parser.State := State_None;
                  Result := HTML_ERROR;
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
                     Append_Attribute_No_Value (Parser.Attr_Name);
                  end if;
                  Last := Pos + 1;
                  Parser.State := State_None;
                  Result := HTML_START;
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
                  Result := HTML_ERROR;
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

            when State_Error =>
               Last := Pos;
               Parser.State := State_None;
               Result := HTML_ERROR;
               return;

         end case;
      end loop;
   end Parse_Element;

   use Interfaces;

   function From_Hex (C : in Character) return Interfaces.Unsigned_8 is
      (if C in '0' .. '9' then Character'Pos (C) - Character'Pos ('0')
      elsif C in 'A' .. 'F' then Character'Pos (C) - Character'Pos ('A') + 10
      elsif C in 'a' .. 'f' then Character'Pos (C) - Character'Pos ('a') + 10
      else raise Constraint_Error);

   function From_Hex (Value : in String) return Interfaces.Unsigned_32 is
      Result : Interfaces.Unsigned_32 := 0;
   begin
      for C of Value loop
         Result := Interfaces.Shift_Left (Result, 4);
         Result := Result + Interfaces.Unsigned_32 (From_Hex (C));
      end loop;
      return Result;
   end From_Hex;

   --  ------------------------------
   --  Find a named character reference in the sorted keyword table.
   --  Returns the index of the entry, or 0 when not found.
   --  ------------------------------
   function Find_Keyword (Name : in String) return Natural is
      High : Natural := Wiki.Html_Parser.Entities.Keywords'Last;
      Low  : Natural := Wiki.Html_Parser.Entities.Keywords'First;
   begin
      while Low <= High loop
         declare
            I : constant Natural := (Low + High) / 2;
         begin
            if Wiki.Html_Parser.Entities.Keywords (I).all = Name then
               return I;
            elsif Entities.Keywords (I).all < Name then
               Low := I + 1;
            else
               High := I - 1;
            end if;
         end;
      end loop;
      return 0;
   end Find_Keyword;

   --  ------------------------------
   --  Check whether the name is a legacy named character reference that HTML5
   --  also recognizes without the ';' terminator.
   --  ------------------------------
   function Is_Legacy_Keyword (Name : in String) return Boolean is
      High : Natural := Wiki.Html_Parser.Entities.Legacy_Keywords'Last;
      Low  : Natural := Wiki.Html_Parser.Entities.Legacy_Keywords'First;
   begin
      while Low <= High loop
         declare
            I : constant Natural := (Low + High) / 2;
         begin
            if Wiki.Html_Parser.Entities.Legacy_Keywords (I).all = Name then
               return True;
            elsif Entities.Legacy_Keywords (I).all < Name then
               Low := I + 1;
            else
               High := I - 1;
            end if;
         end;
      end loop;
      return False;
   end Is_Legacy_Keyword;

   --  ------------------------------
   --  Map a numeric character reference value to a Unicode code point following
   --  the HTML5 rules: NULL, surrogates and values above 0x10FFFF are replaced by
   --  U+FFFD, and the 0x80-0x9F range is mapped through the Windows-1252 table.
   --  ------------------------------
   function Numeric_Reference (Value : in Interfaces.Unsigned_32) return Wiki.Strings.WChar is
      use type Interfaces.Unsigned_32;
   begin
      if Value = 0 or else Value > 16#10FFFF#
        or else (Value >= 16#D800# and then Value <= 16#DFFF#)
      then
         return Wiki.Strings.WChar'Val (16#FFFD#);
      elsif Value >= 16#80# and then Value <= 16#9F# then
         case Value is
            when 16#80# => return Wiki.Strings.WChar'Val (16#20AC#);
            when 16#82# => return Wiki.Strings.WChar'Val (16#201A#);
            when 16#83# => return Wiki.Strings.WChar'Val (16#0192#);
            when 16#84# => return Wiki.Strings.WChar'Val (16#201E#);
            when 16#85# => return Wiki.Strings.WChar'Val (16#2026#);
            when 16#86# => return Wiki.Strings.WChar'Val (16#2020#);
            when 16#87# => return Wiki.Strings.WChar'Val (16#2021#);
            when 16#88# => return Wiki.Strings.WChar'Val (16#02C6#);
            when 16#89# => return Wiki.Strings.WChar'Val (16#2030#);
            when 16#8A# => return Wiki.Strings.WChar'Val (16#0160#);
            when 16#8B# => return Wiki.Strings.WChar'Val (16#2039#);
            when 16#8C# => return Wiki.Strings.WChar'Val (16#0152#);
            when 16#8E# => return Wiki.Strings.WChar'Val (16#017D#);
            when 16#91# => return Wiki.Strings.WChar'Val (16#2018#);
            when 16#92# => return Wiki.Strings.WChar'Val (16#2019#);
            when 16#93# => return Wiki.Strings.WChar'Val (16#201C#);
            when 16#94# => return Wiki.Strings.WChar'Val (16#201D#);
            when 16#95# => return Wiki.Strings.WChar'Val (16#2022#);
            when 16#96# => return Wiki.Strings.WChar'Val (16#2013#);
            when 16#97# => return Wiki.Strings.WChar'Val (16#2014#);
            when 16#98# => return Wiki.Strings.WChar'Val (16#02DC#);
            when 16#99# => return Wiki.Strings.WChar'Val (16#2122#);
            when 16#9A# => return Wiki.Strings.WChar'Val (16#0161#);
            when 16#9B# => return Wiki.Strings.WChar'Val (16#203A#);
            when 16#9C# => return Wiki.Strings.WChar'Val (16#0153#);
            when 16#9E# => return Wiki.Strings.WChar'Val (16#017E#);
            when 16#9F# => return Wiki.Strings.WChar'Val (16#0178#);
            when others => return Wiki.Strings.WChar'Val (Value);
         end case;
      else
         return Wiki.Strings.WChar'Val (Value);
      end if;
   end Numeric_Reference;

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
      Len        : Natural := Parser.Counter;
      C          : Wiki.Strings.WChar;
      Pos        : Positive := From;
      Terminated : Boolean := False;
   begin
      if Status /= ENTITY_MIDDLE then
         Len := 0;
      end if;
      --  Scan the entity name: it is made of ASCII letters, digits and '#'.
      while Len < MAX_ENTITY_LENGTH loop
         if Pos > Text'Last then
            Parser.Counter := Len;
            Status := ENTITY_MIDDLE;
            Entity := NUL;
            Last := Pos;
            return;
         end if;
         C := Text (Pos);
         if C = ';' then
            Terminated := True;
            Pos := Pos + 1;
            exit;
         end if;
         if Wiki.Helpers.Is_Newline (C) then
            Parser.Counter := 0;
            Entity := NUL;
            Status := ENTITY_NONE;
            Last := From;
            return;
         end if;
         if C not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '#' then
            --  End of the entity name without the ';' terminator.
            exit;
         end if;
         Len := Len + 1;
         Parser.Entity_Name (Len) := Wiki.Strings.To_Char (C);
         Pos := Pos + 1;
      end loop;

      Parser.Counter := 0;
      if Len = 0 then
         Status := ENTITY_NONE;
         Entity := NUL;
         Last := From;
         return;
      end if;

      if Terminated then
         --  With the ';' terminator the name must match a named character reference.
         declare
            I : constant Natural := Find_Keyword (Parser.Entity_Name (1 .. Len));
         begin
            if I > 0 then
               Entity := Entities.Mapping (I);
               Last := Pos;
               Status := ENTITY_VALID;
               return;
            end if;
         end;
      elsif Pos > Text'Last
        or else Text (Pos) not in '=' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
      then
         --  Without the ';' terminator, only the legacy named character references
         --  are recognized, and only when not followed by '=' or an alphanumeric
         --  character (HTML5 named character reference state).  Use the longest match.
         for L in reverse 1 .. Len loop
            if Is_Legacy_Keyword (Parser.Entity_Name (1 .. L)) then
               declare
                  I : constant Natural := Find_Keyword (Parser.Entity_Name (1 .. L));
               begin
                  if I > 0 then
                     Entity := Entities.Mapping (I);
                     Last := Pos;
                     Status := ENTITY_VALID;
                     return;
                  end if;
               end;
            end if;
         end loop;
      end if;

      --  Numeric character references &#123; and &#x1F600; (the ';' is optional).
      if Parser.Entity_Name (1) = '#' then
         declare
            Value : Interfaces.Unsigned_32 := 0;
            Valid : Boolean := True;
         begin
            if Len >= 2 and then Parser.Entity_Name (2) in '0' .. '9' and then Len <= 8 then
               --  Decimal reference with up to 7 digits.
               begin
                  Value := Interfaces.Unsigned_32
                    (Natural'Value (Parser.Entity_Name (2 .. Len)));
               exception
                  when Constraint_Error =>
                     Valid := False;
               end;
            elsif Len >= 3 and then Parser.Entity_Name (2) in 'x' | 'X' and then Len <= 8 then
               --  Hexadecimal reference with up to 6 digits.
               begin
                  Value := From_Hex (Parser.Entity_Name (3 .. Len));
               exception
                  when Constraint_Error =>
                     Valid := False;
               end;
            else
               Valid := False;
            end if;
            if Valid then
               Entity := Numeric_Reference (Value);
               Last := Pos;
               Status := ENTITY_VALID;
               return;
            end if;
         end;
      end if;

      --  The HTML entity is not recognized: we must treat it as plain wiki text.
      Status := ENTITY_NONE;
      Entity := NUL;
      Last := From;
   end Parse_Entity;

end Wiki.Html_Parser;
