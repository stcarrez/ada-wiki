-----------------------------------------------------------------------
--  wiki-parsers -- Wiki parser
--  Copyright (C) 2011 - 2022 Stephane Carrez
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
with Ada.Strings.Wide_Wide_Fixed;
with Interfaces;

with Wiki.Parsers.Html;
with Wiki.Helpers;
with Wiki.Helpers.Parser;
with Wiki.Nodes;
with Wiki.Parsers.Common;
with Wiki.Parsers.Creole;
with Wiki.Parsers.Dotclear;
with Wiki.Parsers.Markdown;
with Wiki.Parsers.Textile;
with Wiki.Parsers.Google;
with Wiki.Parsers.MediaWiki;
package body Wiki.Parsers is

   use Wiki.Nodes;
   use Wiki.Helpers;

   --  ------------------------------
   --  Peek the next character from the wiki text buffer.
   --  ------------------------------
   procedure Peek (P     : in out Parser'Class;
                   Token : out Wiki.Strings.WChar) is
   begin
      if not P.Has_Pending then
         --  Get the next character.
         P.Reader.Read (Token, P.Is_Eof);
         if P.Is_Eof then
            --  Return a \n on end of file (this simplifies the implementation).
            Token := LF;
            P.Pending := LF;
            P.Has_Pending := True;
         end if;
      else
         --  Return the pending character.
         Token := P.Pending;
         if not P.Is_Eof then
            P.Has_Pending := False;
         end if;
      end if;
   end Peek;

   --  ------------------------------
   --  Put back the character so that it will be returned by the next call to Peek.
   --  ------------------------------
   procedure Put_Back (P     : in out Parser;
                       Token : in Wiki.Strings.WChar) is
   begin
      P.Pending     := Token;
      P.Has_Pending := True;
   end Put_Back;

   --  ------------------------------
   --  Flush the wiki text that was collected in the text buffer.
   --  ------------------------------
   procedure Flush_Text (P : in out Parser) is

      procedure Add_Text (Content : in Wiki.Strings.WString);

      procedure Add_Text (Content : in Wiki.Strings.WString) is
      begin
         P.Context.Filters.Add_Text (P.Document, Content, P.Format);
      end Add_Text;
      pragma Inline (Add_Text);

      procedure Add_Text is
         new Wiki.Strings.Wide_Wide_Builders.Get (Add_Text);
      pragma Inline (Add_Text);

   begin
      if Length (P.Text) > 0 then
         if P.Previous_Tag /= UNKNOWN_TAG and then No_End_Tag (P.Previous_Tag) then
            if not P.Context.Is_Hidden then
               P.Context.Filters.Pop_Node (P.Document, P.Previous_Tag);
            end if;
            P.Previous_Tag := UNKNOWN_TAG;
         end if;
         if not P.Context.Is_Hidden then
            Add_Text (P.Text);
         end if;
         Clear (P.Text);
      end if;
   end Flush_Text;

   --  ------------------------------
   --  Flush the wiki dl/dt/dd definition list.
   --  ------------------------------
   procedure Flush_List (P : in out Parser) is
   begin
      if P.In_List then
         if not P.Context.Is_Hidden then
            P.Context.Filters.Pop_Node (P.Document, Wiki.DL_TAG);
         end if;
         P.In_List := False;
      end if;
   end Flush_List;

   --  ------------------------------
   --  Skip white spaces and tabs.
   --  ------------------------------
   procedure Skip_Spaces (P : in out Parser) is
      C : Wiki.Strings.WChar;
   begin
      while not P.Is_Eof loop
         Peek (P, C);
         if not Wiki.Helpers.Is_Space_Or_Newline (C) then
            Put_Back (P, C);
            return;
         end if;
      end loop;
   end Skip_Spaces;

   --  ------------------------------
   --  Append a character to the wiki text buffer.
   --  ------------------------------
   procedure Parse_Text (P     : in out Parser;
                         Token : in Wiki.Strings.WChar) is
   begin
      if Token /= CR then
         Append (P.Text, Token);
      end if;
      P.Empty_Line := False;
   end Parse_Text;

   --  ------------------------------
   --  Skip all the spaces and tabs as well as end of the current line (CR+LF).
   --  ------------------------------
   procedure Skip_End_Of_Line (P : in out Parser) is
      C : Wiki.Strings.WChar;
   begin
      loop
         Peek (P, C);
         exit when C /= ' ' and C /= HT;
      end loop;
      if C = CR then
         Peek (P, C);
         if C /= LF then
            Put_Back (P, C);
         end if;
      elsif C = LF then
         Peek (P, C);
         if C /= CR then
            Put_Back (P, C);
         end if;
      end if;
   end Skip_End_Of_Line;

   --  ------------------------------
   --  Check if the link refers to an image and must be rendered as an image.
   --  Returns a positive index of the start the the image link.
   --  ------------------------------
   function Is_Image (P    : in Parser;
                      Link : in Wide_Wide_String) return Natural is
      Pos : Natural;
   begin
      if not P.Check_Image_Link then
         return 0;
      elsif Wiki.Helpers.Is_Url (Link) then
         Pos := Ada.Strings.Wide_Wide_Fixed.Index (Link, ".", Ada.Strings.Backward);
         if Pos = 0 then
            return 0;
         elsif Wiki.Helpers.Is_Image_Extension (Link (Pos .. Link'Last)) then
            return Link'First;
         else
            return 0;
         end if;
      elsif Link'Length <= 5 then
         return 0;
      elsif Link (Link'First .. Link'First + 5) = "Image:" then
         return Link'First + 6;
      elsif Link (Link'First .. Link'First + 4) /= "File:" and
        Link (Link'First .. Link'First + 5) /= "Image:"
      then
         return 0;
      else
         Pos := Ada.Strings.Wide_Wide_Fixed.Index (Link, ".", Ada.Strings.Backward);
         if Pos = 0 then
            return 0;
         elsif Wiki.Helpers.Is_Image_Extension (Link (Pos .. Link'Last)) then
            return Link'First;
         else
            return 0;
         end if;
      end if;
   end Is_Image;

   --  ------------------------------
   --  Returns true if we are included from another wiki content.
   --  ------------------------------
   function Is_Included (P : in Parser) return Boolean is
   begin
      return P.Context.Is_Included;
   end Is_Included;

   --  ------------------------------
   --  Find the plugin with the given name.
   --  Returns null if there is no such plugin.
   --  ------------------------------
   function Find (P    : in Parser;
                  Name : in Wiki.Strings.WString) return Wiki.Plugins.Wiki_Plugin_Access is
      use type Wiki.Plugins.Plugin_Factory_Access;
   begin
      if P.Context.Factory = null then
         return null;
      else
         return P.Context.Factory.Find (Wiki.Strings.To_String (Name));
      end if;
   end Find;

   procedure Toggle_Format (P      : in out Parser;
                            Format : in Format_Type) is
   begin
      Flush_Text (P);
      P.Format (Format) := not P.Format (Format);
   end Toggle_Format;

   --  ------------------------------
   --  Parse the beginning or the end of a single character sequence.  This procedure
   --  is instantiated for several format types (bold, italic, superscript, subscript, code).
   --  Example:
   --    _name_    *bold*   `code`
   --  ------------------------------
   procedure Parse_Single_Format (P     : in out Parser;
                                  Token : in Wiki.Strings.WChar) is
      pragma Unreferenced (Token);
   begin
      Toggle_Format (P, Format);
   end Parse_Single_Format;

   --  ------------------------------
   --  Parse the beginning or the end of a double character sequence.  This procedure
   --  is instantiated for several format types (bold, italic, superscript, subscript, code).
   --  Example:
   --    --name--  **bold** ~~strike~~
   --  ------------------------------
   procedure Parse_Double_Format (P     : in out Parser;
                                  Token : in Wiki.Strings.WChar) is
      C : Wiki.Strings.WChar;
   begin
      Peek (P, C);
      if C = Token then
         Toggle_Format (P, Format);
      else
         Parse_Text (P, Token);
         Put_Back (P, C);
      end if;
   end Parse_Double_Format;

   --  ------------------------------
   --  Parse a space and take necessary formatting actions.
   --  Example:
   --    item1 item2   => add space in text buffer
   --    ' * item'     => start a bullet list (Google)
   --    ' # item'     => start an ordered list (Google)
   --    ' item'       => preformatted text (Google, Creole)
   --  ------------------------------
   procedure Parse_Space (P     : in out Parser;
                          Token : in Wiki.Strings.WChar) is
      C     : Wiki.Strings.WChar;
   begin
      if P.Empty_Line then
         --  Skip spaces for multi-space preformatted blocks or when we are within HTML elements.
         if P.Preformat_Column > 1 or not P.Document.Is_Root_Node then
            loop
               Peek (P, C);
               exit when C /= ' ' and C /= HT;
            end loop;
         else
            Peek (P, C);
         end if;
         if C = '*' or C = '#' then
            Common.Parse_List (P, C);
         elsif C = '-' then
            Put_Back (P, C);
         elsif C = CR or C = LF then
            Parse_End_Line (P, C);
         else
            Put_Back (P, C);
            Common.Parse_Preformatted (P, Token);
         end if;
      else
         Append (P.Text, Token);
      end if;
   end Parse_Space;

   procedure Parse_End_Line (P     : in out Parser;
                             Token : in Wiki.Strings.WChar) is
      C     : Wiki.Strings.WChar := Token;
      Count : Positive := 1;
   begin
      if P.Is_Eof then
         return;
      end if;
      loop
         Peek (P, C);
         exit when P.Is_Eof;
         if C = Token then
            Count := Count + 1;
         elsif C /= CR and C /= LF then
            Put_Back (P, C);
            exit;
         end if;
      end loop;
      if Count >= 2 then
         Flush_Text (P);
         Flush_List (P);

         --  Finish the active table.
         if P.In_Table then
            P.Context.Filters.Finish_Table (P.Document);
            P.In_Table := False;
         end if;

         --  Finish the active blockquotes if a new paragraph is started on an empty line.
         if P.Quote_Level > 0 then
            if not P.Context.Is_Hidden then
               P.Context.Filters.Add_Blockquote (P.Document, 0);
            end if;
            P.Quote_Level := 0;
         end if;
         if not P.Context.Is_Hidden then
            P.Context.Filters.Add_Node (P.Document, Wiki.Nodes.N_PARAGRAPH);
         end if;
         P.In_Paragraph := True;
      elsif (Length (P.Text) > 0 or not P.Empty_Line) and then not P.Is_Eof then
         Flush_Text (P);
         if not P.Context.Is_Hidden then
            P.Context.Filters.Add_Node (P.Document, Wiki.Nodes.N_NEWLINE);
         end if;
      end if;

      --  Finish the active blockquotes if a new paragraph is started immediately after
      --  the blockquote.
      if P.Quote_Level > 0 and C /= '>' then
         Flush_Text (P);
         if not P.Context.Is_Hidden then
            P.Context.Filters.Add_Blockquote (P.Document, 0);
         end if;
         P.Quote_Level := 0;
      end if;
      P.Empty_Line := True;
   end Parse_End_Line;

   --  ------------------------------
   --  Parse a HTML component.
   --  Example:
   --     <b> or </b>
   --  ------------------------------
   procedure Parse_Maybe_Html (P     : in out Parser;
                               Token : in Wiki.Strings.WChar) is
   begin
      --  Don't parse HTML if a formatting text mode is setup.
      if (for some Mode of P.Format => Mode) then
         Parse_Text (P, Token);
         return;
      end if;
      Wiki.Parsers.Html.Parse_Element (P);
   end Parse_Maybe_Html;

   Misc_Wiki_Table : aliased constant Parser_Table
     := (
         16#0A# => Parse_End_Line'Access,
         16#0D# => Parse_End_Line'Access,
         Character'Pos (' ') => Parse_Space'Access,
         Character'Pos ('=') => Common.Parse_Header'Access,
         Character'Pos ('*') => Common.Parse_Single_Bold'Access,
         Character'Pos ('_') => Common.Parse_Single_Italic'Access,
         Character'Pos ('`') => Common.Parse_Single_Code'Access,
         Character'Pos ('^') => Common.Parse_Single_Superscript'Access,
         Character'Pos ('~') => Common.Parse_Double_Strikeout'Access,
         Character'Pos (',') => Common.Parse_Double_Subscript'Access,
         Character'Pos ('[') => Common.Parse_Link'Access,
         Character'Pos ('\') => Common.Parse_Line_Break'Access,
         Character'Pos ('#') => Common.Parse_List'Access,
         Character'Pos ('@') => Common.Parse_Double_Code'Access,
         Character'Pos ('<') => Parse_Maybe_Html'Access,
         others => Parse_Text'Access
        );

   Html_Table : aliased constant Parser_Table
     := (
         Character'Pos ('<') => Parse_Maybe_Html'Access,
         Character'Pos ('&') => Html.Parse_Entity'Access,
         others => Parse_Text'Access
        );

   type Syntax_Parser_Tables is array (Wiki_Syntax) of Parser_Table_Access;

   Syntax_Tables : constant Syntax_Parser_Tables
     := (
         SYNTAX_GOOGLE     => Google.Wiki_Table'Access,
         SYNTAX_CREOLE     => Creole.Wiki_Table'Access,
         SYNTAX_DOTCLEAR   => Dotclear.Wiki_Table'Access,
         SYNTAX_PHPBB      => MediaWiki.Wiki_Table'Access,
         SYNTAX_MEDIA_WIKI => MediaWiki.Wiki_Table'Access,
         SYNTAX_MARKDOWN   => Markdown.Wiki_Table'Access,
         SYNTAX_TEXTILE    => Textile.Wiki_Table'Access,
         SYNTAX_MIX        => Misc_Wiki_Table'Access,
         SYNTAX_HTML       => Html_Table'Access
        );

   procedure Start_Element (P          : in out Parser;
                            Tag        : in Wiki.Html_Tag;
                            Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      if P.Previous_Tag /= UNKNOWN_TAG and then No_End_Tag (P.Previous_Tag) then
         if not P.Context.Is_Hidden then
            P.Context.Filters.Pop_Node (P.Document, P.Previous_Tag);
         end if;
         P.Previous_Tag := UNKNOWN_TAG;
      end if;
      Flush_Text (P);
      if not P.Context.Is_Hidden then
         P.Context.Filters.Push_Node (P.Document, Tag, Attributes);
      end if;

      --  When we are within a <pre> HTML element, switch to HTML to emit the text as is.
      if Tag = PRE_TAG and P.Context.Syntax /= SYNTAX_HTML then
         P.Previous_Syntax := P.Context.Syntax;
         P.Set_Syntax (SYNTAX_HTML);
      end if;
      P.Previous_Tag := Tag;
   end Start_Element;

   procedure End_Element (P    : in out Parser;
                          Tag  : in Wiki.Html_Tag) is
      Previous_Tag : constant Wiki.Html_Tag := P.Previous_Tag;
   begin
      P.Previous_Tag := UNKNOWN_TAG;
      if Previous_Tag /= UNKNOWN_TAG
        and then Previous_Tag /= Tag
        and then No_End_Tag (Previous_Tag)
      then
         End_Element (P, Previous_Tag);
      end if;

      Flush_Text (P);
      if not P.Context.Is_Hidden then
         P.Context.Filters.Pop_Node (P.Document, Tag);
      end if;

      --  Switch back to the previous syntax when we reached the </pre> HTML element.
      if P.Previous_Syntax /= P.Context.Syntax and Tag = PRE_TAG then
         P.Set_Syntax (P.Previous_Syntax);
      end if;
   end End_Element;

   --  ------------------------------
   --  Set the plugin factory to find and use plugins.
   --  ------------------------------
   procedure Set_Plugin_Factory (Engine  : in out Parser;
                                 Factory : in Wiki.Plugins.Plugin_Factory_Access) is
   begin
      Engine.Context.Factory := Factory;
   end Set_Plugin_Factory;

   --  ------------------------------
   --  Set the wiki syntax that the wiki engine must use.
   --  ------------------------------
   procedure Set_Syntax (Engine : in out Parser;
                         Syntax : in Wiki_Syntax := SYNTAX_MIX) is
   begin
      Engine.Context.Syntax := Syntax;
      Engine.Table  := Syntax_Tables (Syntax);
   end Set_Syntax;

   --  ------------------------------
   --  Add a filter in the wiki engine.
   --  ------------------------------
   procedure Add_Filter (Engine : in out Parser;
                         Filter : in Wiki.Filters.Filter_Type_Access) is
   begin
      Engine.Context.Filters.Add_Filter (Filter);
   end Add_Filter;

   --  ------------------------------
   --  Set the plugin context.
   --  ------------------------------
   procedure Set_Context (Engine  : in out Parser;
                          Context : in Wiki.Plugins.Plugin_Context) is
   begin
      Engine.Context.Previous := Context.Previous;
      Engine.Context.Filters.Set_Chain (Context.Filters);
      Engine.Context.Factory   := Context.Factory;
      Engine.Context.Variables := Context.Variables;
      Engine.Context.Is_Included := Context.Is_Included;
      Engine.Context.Ident := Context.Ident;
      Engine.Set_Syntax (Context.Syntax);
   end Set_Context;

   --  ------------------------------
   --  Parse the wiki text contained in <b>Text</b> according to the wiki syntax
   --  defined on the parser. The string is assumed to be in UTF-8 format.
   --  ------------------------------
   procedure Parse (Engine : in out Parser;
                    Text   : in String;
                    Doc    : in out Wiki.Documents.Document) is
      procedure Element (Content : in String;
                         Pos     : in out Natural;
                         Char    : out Wiki.Strings.WChar);
      function Length (Content : in String) return Natural;

      procedure Element (Content : in String;
                         Pos     : in out Natural;
                         Char    : out Wiki.Strings.WChar) is
         use Interfaces;

         Val : Unsigned_32;
      begin
         Val := Character'Pos (Content (Pos));
         Pos := Pos + 1;

         --  UTF-8 conversion
         --  7  U+0000   U+007F   1  0xxxxxxx
         --  11 U+0080   U+07FF   2  110xxxxx 10xxxxxx
         --  16 U+0800   U+FFFF   3  1110xxxx 10xxxxxx 10xxxxxx
         --  21 U+10000  U+1FFFFF 4  11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
         if Val >= 16#80# and Pos <= Content'Last then
            Val := Shift_Left (Val, 6);
            Val := Val or (Character'Pos (Content (Pos)) and 16#3F#);
            Pos := Pos + 1;
            if Val <= 16#37FF# or Pos > Content'Last then
               Val := Val and 16#07ff#;
            else
               Val := Shift_Left (Val, 6);
               Val := Val or (Character'Pos (Content (Pos)) and 16#3F#);
               Pos := Pos + 1;
               if Val <= 16#EFFFF# or Pos > Content'Last then
                  Val := Val and 16#FFFF#;
               else
                  Val := Shift_Left (Val, 6);
                  Val := Val or (Character'Pos (Content (Pos)) and 16#3F#);
                  Val := Val and 16#1FFFFF#;
                  Pos := Pos + 1;
               end if;
            end if;
         end if;
         Char := Wiki.Strings.WChar'Val (Val);
      end Element;
      pragma Inline (Element);

      function Length (Content : in String) return Natural is
      begin
         return Content'Length;
      end Length;
      pragma Inline_Always (Length);

      procedure Parse_Text is
        new Wiki.Helpers.Parser (Engine_Type  => Parser,
                                 Element_Type => String);
      pragma Inline (Parse_Text);

   begin
      Parse_Text (Engine, Text, Doc);
   end Parse;

   --  ------------------------------
   --  Parse the wiki text contained in <b>Text</b> according to the wiki syntax
   --  defined on the parser.
   --  ------------------------------
   procedure Parse (Engine : in out Parser;
                    Text   : in Wide_Wide_String;
                    Doc    : in out Wiki.Documents.Document) is
      procedure Element (Content : in Wide_Wide_String;
                         Pos     : in out Natural;
                         Char    : out Wiki.Strings.WChar);
      function Length (Content : in Wide_Wide_String) return Natural;

      procedure Element (Content : in Wide_Wide_String;
                         Pos     : in out Natural;
                         Char    : out Wiki.Strings.WChar) is
      begin
         Char := Content (Pos);
         Pos := Pos + 1;
      end Element;
      pragma Inline (Element);

      function Length (Content : in Wide_Wide_String) return Natural is
      begin
         return Content'Length;
      end Length;
      pragma Inline_Always (Length);

      procedure Parse_Text is
        new Wiki.Helpers.Parser (Engine_Type  => Parser,
                                 Element_Type => Wiki.Strings.WString);
      pragma Inline (Parse_Text);

   begin
      Parse_Text (Engine, Text, Doc);
   end Parse;

   --  ------------------------------
   --  Parse the wiki text contained in <b>Text</b> according to the wiki syntax
   --  defined on the parser.
   --  ------------------------------
   procedure Parse (Engine : in out Parser;
                    Text   : in Wiki.Strings.UString;
                    Doc    : in out Wiki.Documents.Document) is
      procedure Element (Content : in Wiki.Strings.UString;
                         Pos     : in out Natural;
                         Char    : out Wiki.Strings.WChar);

      procedure Element (Content : in Wiki.Strings.UString;
                         Pos     : in out Natural;
                         Char    : out Wiki.Strings.WChar) is
      begin
         Char := Wiki.Strings.Element (Content, Pos);
         Pos := Pos + 1;
      end Element;
      pragma Inline_Always (Element);

      procedure Parse_Text is
        new Wiki.Helpers.Parser (Engine_Type  => Parser,
                                 Element_Type => Wiki.Strings.UString,
                                 Length       => Wiki.Strings.Length);
      pragma Inline (Parse_Text);
   begin
      Parse_Text (Engine, Text, Doc);
   end Parse;

   --  ------------------------------
   --  Parse the wiki stream managed by <tt>Stream</tt> according to the wiki syntax configured
   --  on the wiki engine.
   --  ------------------------------
   procedure Parse (Engine : in out Parser;
                    Stream : in Wiki.Streams.Input_Stream_Access;
                    Doc    : in out Wiki.Documents.Document) is
      Main : constant Boolean := Doc.Is_Empty;
   begin
      Engine.Document   := Doc;
      Engine.Previous_Syntax := Engine.Context.Syntax;
      Engine.Empty_Line := True;
      Engine.Format     := (others => False);
      Engine.Is_Eof     := False;
      Engine.In_Paragraph := True;
      Engine.Has_Pending := False;
      Engine.Reader      := Stream;
      Engine.Link_Double_Bracket := False;
      Engine.Escape_Char := '~';
      Engine.Param_Char := Wiki.Strings.WChar'Last;
      if Main then
         Engine.Context.Filters.Add_Node (Engine.Document, Wiki.Nodes.N_PARAGRAPH);
      end if;
      case Engine.Context.Syntax is
         when SYNTAX_DOTCLEAR =>
            Engine.Is_Dotclear := True;
            Engine.Escape_Char := '\';
            Engine.Header_Offset := -6;
            Engine.Link_Title_First := True;

         when SYNTAX_CREOLE =>
            Engine.Link_Double_Bracket := True;
            Engine.Param_Char := '<';

         when SYNTAX_MEDIA_WIKI =>
            Engine.Link_Double_Bracket := True;
            Engine.Check_Image_Link := True;
            Engine.Param_Char := '{';

         when SYNTAX_TEXTILE =>
            Engine.Link_Double_Bracket := True;
            Engine.Check_Image_Link := True;
            Engine.Param_Char := '{';

         when SYNTAX_MIX =>
            Engine.Is_Dotclear := True;

         when SYNTAX_GOOGLE =>
            Engine.Link_No_Space := True;

         when SYNTAX_MARKDOWN =>
            Engine.Preformat_Column := 4;
            Engine.Escape_Char := '\';

         when others =>
            null;

      end case;
      Parse_Token (Engine);
      Flush_Text (Engine);
      if Main then
         Engine.Context.Filters.Finish (Engine.Document);
      end if;
      Doc := Engine.Document;
   end Parse;

   procedure Parse_Token (P : in out Parser) is
      C : Wiki.Strings.WChar;
   begin
      loop
         Peek (P, C);
         exit when P.Is_Eof;
         if C > '~' then
            Parse_Text (P, C);
         else
            P.Table (Wiki.Strings.WChar'Pos (C)).all (P, C);
         end if;
      end loop;
   end Parse_Token;

end Wiki.Parsers;
