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
with Wiki.Parsers.Creole;
with Wiki.Parsers.Dotclear;
with Wiki.Parsers.Markdown;
with Wiki.Parsers.Textile;
with Wiki.Parsers.Google;
with Wiki.Parsers.MediaWiki;
package body Wiki.Parsers is

   use Wiki.Helpers;
   use Wiki.Buffers;
   use type Wiki.Nodes.Node_Kind;

   procedure Append_Preformatted (P      : in out Parser;
                                  Format : in Wiki.Strings.WString);

   procedure Next (Content : in out Content_Access;
                   Pos     : in out Positive) is
   begin
      if Pos + 1 > Content.Last then
         Content := Content.Next_Block;
         Pos := 1;
      else
         Pos := Pos + 1;
      end if;
   end Next;

   procedure Append_Preformatted (P      : in out Parser;
                                  Format : in Wiki.Strings.WString) is
      procedure Add_Preformatted (Content : in Wiki.Strings.WString);

      procedure Add_Preformatted (Content : in Wiki.Strings.WString) is
      begin
         P.Context.Filters.Add_Preformatted (P.Document, Content, Format);
      end Add_Preformatted;

      procedure Add_Preformatted is
         new Wiki.Strings.Wide_Wide_Builders.Get (Add_Preformatted);
      pragma Inline (Add_Preformatted);

   begin
      Add_Preformatted (P.Text);
   end Append_Preformatted;

   procedure Append_Text (P    : in out Parser;
                          Text : in Wiki.Strings.BString;
                          From : in Positive;
                          To   : in Positive) is
      procedure Add_Text (Content : in Wiki.Strings.WString);

      procedure Add_Text (Content : in Wiki.Strings.WString) is
      begin
         P.Context.Filters.Add_Text (P.Document, Content (From .. To), P.Format);
      end Add_Text;

      procedure Add_Text is
         new Wiki.Strings.Wide_Wide_Builders.Get (Add_Text);
      pragma Inline (Add_Text);

   begin
      Add_Text (P.Text);
   end Append_Text;

   function Is_List_Item (P     : in Parser;
                          Level : in Natural) return Boolean is
   begin
      if P.Current_Node not in Nodes.N_LIST_ITEM | Nodes.N_LIST_START then
         return False;
      end if;
      declare
         Top : constant Block_Access := Block_Stack.Current (P.Blocks);
      begin
         return Top.Level = Level;
      end;
   end Is_List_Item;

   procedure Pop_List (P      : in out Parser;
                       Level  : in Natural;
                       Marker : in Wiki.Strings.WChar) is
   begin
      while not Block_Stack.Is_Empty (P.Blocks) loop
         declare
            Top : constant Block_Access := Block_Stack.Current (P.Blocks);
         begin
            exit when Top.Kind in Nodes.N_LIST_START | Nodes.N_NUM_LIST_START | Nodes.N_LIST_ITEM
              and then Top.Level <= Level and then Top.Marker = Marker;
            exit when Top.Kind not in Nodes.N_LIST_ITEM
              | Nodes.N_LIST_START | Nodes.N_NUM_LIST_START;
            Pop_Block (P);
         end;
      end loop;
   end Pop_List;

   procedure Pop_List (P      : in out Parser) is
   begin
      while not Block_Stack.Is_Empty (P.Blocks) loop
         declare
            Top : constant Block_Access := Block_Stack.Current (P.Blocks);
         begin
            exit when Top.Kind not in Nodes.N_LIST_ITEM
              | Nodes.N_LIST_START | Nodes.N_NUM_LIST_START | Nodes.N_DEFINITION;
            Flush_Text (P, Trim => Right);
            Pop_Block (P);
         end;
      end loop;
   end Pop_List;

   procedure Add_Header (P : in out Parser;
                         Level : in Natural) is
      procedure Add_Header (Content : in Wiki.Strings.WString);

      procedure Add_Header (Content : in Wiki.Strings.WString) is
         Last         : Natural := Wiki.Helpers.Trim_Spaces (Content, Content'Last);
         Ignore_Token : Boolean := True;
         Seen_Token   : Boolean := False;
      begin
         --  Remove the spaces and '=' at end of header string.
         while Last > Content'First loop
            if Content (Last) = '=' then
               exit when not Ignore_Token;
               Seen_Token := True;
            elsif Content (Last) = ' ' or Content (Last) = HT then
               Ignore_Token := not Seen_Token;
            else
               exit;
            end if;
            Last := Last - 1;
         end loop;
         P.Context.Filters.Add_Header (P.Document, Content (Content'First .. Last), Level);
      end Add_Header;

      procedure Add_Header is
         new Wiki.Strings.Wide_Wide_Builders.Get (Add_Header);

   begin
      if not P.Context.Is_Hidden then
         Add_Header (P.Text);
      end if;
   end Add_Header;

   --  ------------------------------
   --  Push a new block kind on the block stack.
   --  ------------------------------
   procedure Flush_Block (P     : in out Parser) is
      Top : constant Block_Access := Block_Stack.Current (P.Blocks);
   begin
      if Top /= null then
         if Top.Kind = Nodes.N_PREFORMAT then
            Append_Preformatted (P, Strings.To_WString (P.Preformat_Format));
         elsif Top.Kind = Nodes.N_PARAGRAPH then
            if P.Parse_Inline /= null then
               P.Parse_Inline (P, P.Text_Buffer.First'Unchecked_Access);
            end if;
         elsif Top.Kind = Nodes.N_LIST_ITEM then
            if P.Parse_Inline /= null then
               P.Parse_Inline (P, P.Text_Buffer.First'Unchecked_Access);
            end if;
            Clear (P.Text);
         elsif Top.Kind = Nodes.N_HEADER then
            Add_Header (P, P.Header_Level);
         else
            Flush_Text (P);
         end if;
      else
         if P.Parse_Inline /= null then
            P.Parse_Inline (P, P.Text_Buffer.First'Unchecked_Access);
         end if;
         Clear (P.Text);
      end if;
   end Flush_Block;

   --  ------------------------------
   --  Push a new block kind on the block stack.
   --  ------------------------------
   procedure Push_Block (P      : in out Parser;
                         Kind   : in Wiki.Nodes.Node_Kind;
                         Level  : in Integer := 0;
                         Marker : in Wiki.Strings.WChar := ' ';
                         Number : in Integer := 0) is
      Empty : constant Boolean := Block_Stack.Is_Empty (P.Blocks);
   begin
      Flush_Block (P);
      Block_Stack.Push (P.Blocks);
      declare
         Top : constant Block_Access := Block_Stack.Current (P.Blocks);
      begin
         Top.Kind  := Kind;
         Top.Level := Level;
         Top.Marker := Marker;
         Top.Number := Number;
         P.Current_Node := Kind;
         if Kind = Nodes.N_LIST_START then
            P.Context.Filters.Add_List (P.Document, 1, False);
         elsif Kind = Nodes.N_NUM_LIST_START then
            P.Context.Filters.Add_List (P.Document, Number, True);
         elsif Kind = Nodes.N_LIST_ITEM then
            P.Context.Filters.Add_Node (P.Document, Kind);
         elsif Empty and Kind = Nodes.N_PARAGRAPH then
            P.Context.Filters.Add_Node (P.Document, Kind);
            P.Is_Empty_Paragraph := True;
         end if;
      end;
   end Push_Block;

   procedure Pop_Block_Until (P     : in out Parser;
                              Kind  : in Wiki.Nodes.Node_Kind;
                              Level : in Integer) is
   begin
      null;
   end Pop_Block_Until;

   procedure Pop_All (P : in out Parser) is
   begin
      while not Block_Stack.Is_Empty (P.Blocks) loop
         Pop_Block (P);
      end loop;
   end Pop_All;

   --  ------------------------------
   --  Pop the current block stack.
   --  ------------------------------
   procedure Pop_Block (P    : in out Parser) is
   begin
      Flush_Block (P);
      if not Block_Stack.Is_Empty (P.Blocks) then
         declare
            Top : Block_Access := Block_Stack.Current (P.Blocks);
         begin
            case Top.Kind is
               when Nodes.N_LIST_START =>
                  P.Context.Filters.Add_Node (P.Document, Nodes.N_LIST_END);

               when Nodes.N_NUM_LIST_START =>
                  P.Context.Filters.Add_Node (P.Document, Nodes.N_NUM_LIST_END);

               when Nodes.N_LIST_ITEM =>
                  P.Context.Filters.Add_Node (P.Document, Nodes.N_LIST_ITEM_END);

               when Nodes.N_BLOCKQUOTE =>
                  P.Context.Filters.Add_Blockquote (P.Document, 0);

               when Nodes.N_DEFINITION =>
                  P.Context.Filters.Add_Node (P.Document, Nodes.N_END_DEFINITION);

               when others =>
                  null;

            end case;
            Block_Stack.Pop (P.Blocks);
            if not Block_Stack.Is_Empty (P.Blocks) then
               Top := Block_Stack.Current (P.Blocks);
               P.Current_Node := Top.Kind;
            else
               P.Current_Node := Nodes.N_NONE;
            end if;
         end;
      else
         P.Current_Node := Nodes.N_NONE;
      end if;
      Clear (P.Text);
   end Pop_Block;

   --  ------------------------------
   --  Read the next wiki input line in the line buffer.
   --  ------------------------------
   procedure Read_Line (Parser : in out Parser_Type'Class;
                        Buffer : out Wiki.Buffers.Buffer_Access) is
      procedure Read (Into : in out Wiki.Strings.WString;
                      Last : out Natural);

      procedure Read (Into : in out Wiki.Strings.WString;
                      Last : out Natural) is
      begin
         Parser.Reader.Read (Into, Last, Parser.Is_Last_Line);
         if Last < Into'Last then
            while Last > Into'First
              and then Into (Last) in Wiki.Helpers.CR | Wiki.Helpers.LF
              and then Into (Last - 1) in Wiki.Helpers.CR | Wiki.Helpers.LF
            loop
              Last := Last - 1;
            end loop;
         end if;
      end Read;

      procedure Fill is new Wiki.Buffers.Inline_Append (Read);
   begin
      Parser.Line_Buffer.Clear;
      Fill (Parser.Line_Buffer);
      if Parser.Is_Last_Line and Parser.Line_Buffer.First.Last = 0 then
         Buffer := null;
      else
         Buffer := Parser.Line_Buffer.First'Unchecked_Access;
      end if;
   end Read_Line;

   --  ------------------------------
   --  Flush the wiki text that was collected in the text buffer.
   --  ------------------------------
   procedure Flush_Text (P : in out Parser;
                         Trim : in Trim_End := None) is

      procedure Add_Text (Content : in Wiki.Strings.WString);

      procedure Add_Text (Content : in Wiki.Strings.WString) is
         First : Natural;
         Last  : Natural;
      begin
         if P.Pre_Tag_Counter = 0 then
            case Trim is
               when Left =>
                  First := Wiki.Helpers.Skip_Spaces (Content, Content'First);
                  Last  := Content'Last;

               when Right =>
                  First := Content'First;
                  Last  := Wiki.Helpers.Trim_Spaces (Content, Content'Last);

               when Both =>
                  First := Wiki.Helpers.Skip_Spaces (Content, Content'First);
                  Last  := Wiki.Helpers.Trim_Spaces (Content, Content'Last);

               when None =>
                  First := Content'First;
                  Last := Content'Last;
            end case;
         else
            First := Content'First;
            Last  := Content'Last;
         end if;
         if First <= Last then
            P.Context.Filters.Add_Text (P.Document, Content (First .. Last), P.Format);
            P.Is_Empty_Paragraph := False;
         end if;
      end Add_Text;
      pragma Inline (Add_Text);

      procedure Add_Text is
         new Wiki.Strings.Wide_Wide_Builders.Get (Add_Text);
      pragma Inline (Add_Text);

   begin
      if P.Pending_Paragraph then
         P.Context.Filters.Add_Node (P.Document, Wiki.Nodes.N_PARAGRAPH);
         P.Pending_Paragraph := False;
      end if;
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
      while P.Current_Node in Nodes.N_LIST_ITEM | Nodes.N_LIST_START | Nodes.N_NUM_LIST_START loop
         Pop_Block (P);
      end loop;
   end Flush_List;

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
   --  Parse the beginning or the end of a single character sequence.
   --  Example:
   --    _name_    *bold*   `code`
   --  ------------------------------
   procedure Parse_Format (P      : in out Parser;
                           Text   : in out Wiki.Buffers.Buffer_Access;
                           From   : in out Positive;
                           Expect : in Wiki.Strings.WChar;
                           Format : in Format_Type) is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
   begin
      if P.Format (Format) then
         Toggle_Format (P, Format);
         Next (Text, From);
         return;
      end if;

      Next (Block, Pos);
      while Block /= null loop
         declare
            Last : constant Natural := Block.Last;
         begin
            while Pos <= Last loop
               if Block.Content (Pos) = Expect then
                  Toggle_Format (P, Format);
                  Next (Text, From);
                  return;
               end if;
               Pos := Pos + 1;
            end loop;
         end;
         Next (Block, Pos);
      end loop;
      Append (P.Text, Text.Content (From));
      Next (Text, From);
   end Parse_Format;

   function Is_Single_Token (Text : in Wiki.Buffers.Buffer_Access;
                             From : in Positive;
                             Token : in Wiki.Strings.WChar) return Boolean is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
   begin
      if Block = null then
         return False;
      end if;
      if Block.Content (Pos) /= Token then
         return False;
      end if;
      Next (Block, Pos);
      return Block = null or else Block.Content (Pos) /= Token;
   end Is_Single_Token;

   --  ------------------------------
   --  Parse the beginning or the end of a single character sequence.
   --  Example:
   --    _name_    *bold*   `code`
   --  ------------------------------
   procedure Parse_Format_Double (P      : in out Parser;
                           Text   : in out Wiki.Buffers.Buffer_Access;
                           From   : in out Positive;
                           Expect : in Wiki.Strings.WChar;
                           Format : in Format_Type) is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
   begin
      Next (Block, Pos);
      if Is_Single_Token (Block, Pos, Expect) then
         if P.Format (Format) then
            Toggle_Format (P, Format);
            Next (Block, Pos);
            Text := Block;
            From := Pos;
            return;
         end if;

         while Block /= null loop
            declare
               Last : constant Natural := Block.Last;
            begin
               while Pos <= Last loop
                  if Block.Content (Pos) = Expect then
                     Pos := Pos + 1;
                     if Pos > Last then
                        Block := Block.Next_Block;
                        if Block = null then
                           Append (P.Text, Text.Content (From));
                           Next (Text, From);
                           return;
                        end if;
                        Pos := 1;
                     end if;
                     if Block.Content (Pos) = Expect then
                        Toggle_Format (P, Format);
                        Next (Text, From);
                        Next (Text, From);
                        return;
                     end if;
                  end if;
                  Pos := Pos + 1;
               end loop;
            end;
            Next (Block, Pos);
         end loop;
      end if;
      Append (P.Text, Text.Content (From));
      Next (Text, From);
   end Parse_Format_Double;

   procedure Process_Html (P          : in out Parser;
                           Kind       : in Wiki.Html_Parser.State_Type;
                           Name       : in Wiki.Strings.WString;
                           Attributes : in out Wiki.Attributes.Attribute_List) is
      use type Wiki.Html_Parser.State_Type;

      Tag  : constant Wiki.Html_Tag := Wiki.Find_Tag (Name);
   begin
      if Tag = Wiki.UNKNOWN_TAG then
         if Name = "noinclude" then
            Flush_Text (P, Trim => None);
            P.Context.Is_Hidden := Kind = Html_Parser.HTML_START and P.Context.Is_Included;
         elsif Name = "includeonly" then
            Flush_Text (P, Trim => None);
            P.Context.Is_Hidden := not (kind = Html_Parser.HTML_START and P.Context.Is_Included);
         end if;
      elsif Kind = Wiki.Html_Parser.HTML_START then
         Start_Element (P, Tag, Attributes);
      elsif Kind = Wiki.Html_Parser.HTML_END then
         End_Element (P, Tag);
      elsif Kind = Wiki.Html_Parser.HTML_START_END then
         Start_Element (P, Tag, Attributes);
         End_Element (P, Tag);
      end if;
   end Process_Html;

   procedure Start_Element (P          : in out Parser;
                            Tag        : in Wiki.Html_Tag;
                            Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      if Tag_Text (Tag) then
         if Tag_Text (P.Previous_Tag) then
            Flush_Text (P, Trim => None);
         else
            Flush_Text (P, Trim => Left);
         end if;
      elsif Tag_Text (P.Previous_Tag) then
         Flush_Text (P, Trim => Right);
      else
         Flush_Text (P, Trim => Both);
      end if;
      if P.Previous_Tag /= UNKNOWN_TAG and then No_End_Tag (P.Previous_Tag) then
         if not P.Context.Is_Hidden then
            P.Context.Filters.Pop_Node (P.Document, P.Previous_Tag);
         end if;
         P.Previous_Tag := UNKNOWN_TAG;
      end if;
      if not P.Context.Is_Hidden then
         P.Context.Filters.Push_Node (P.Document, Tag, Attributes);
      end if;

      --  When we are within a <pre> HTML element, switch to HTML to emit the text as is.
      if Tag = PRE_TAG and P.Context.Syntax /= SYNTAX_HTML then
         P.Previous_Syntax := P.Context.Syntax;
         P.Set_Syntax (SYNTAX_HTML);
      end if;
      P.Previous_Tag := Tag;
      if Tag = PRE_TAG then
         P.Pre_Tag_Counter := P.Pre_Tag_Counter + 1;
      end if;
   end Start_Element;

   procedure End_Element (P    : in out Parser;
                          Tag  : in Wiki.Html_Tag) is
      Previous_Tag : constant Wiki.Html_Tag := P.Previous_Tag;
   begin
      P.Pending_Paragraph := False;
      P.Previous_Tag := UNKNOWN_TAG;
      if Previous_Tag /= UNKNOWN_TAG
        and then Previous_Tag /= Tag
        and then No_End_Tag (Previous_Tag)
      then
         if not P.Context.Is_Hidden then
            P.Context.Filters.Pop_Node (P.Document, Previous_Tag);
         end if;
      end if;

      if Tag_Text (Tag) then
         Flush_Text (P, Trim => None);
      else
         Flush_Text (P, Trim => Right);
      end if;
      if not P.Context.Is_Hidden then
         P.Context.Filters.Pop_Node (P.Document, Tag);
      end if;

      if Tag = PRE_TAG and P.Pre_Tag_Counter > 0 then
         P.Pre_Tag_Counter := P.Pre_Tag_Counter - 1;
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
                         Syntax : in Wiki_Syntax := SYNTAX_MARKDOWN) is
   begin
      Engine.Context.Syntax := Syntax;
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
      Engine.Reader      := Stream;
      Engine.Link_Double_Bracket := False;
      Engine.Escape_Char := '~';
      Engine.Param_Char := Wiki.Strings.WChar'Last;
      if Main then
         Push_Block (Engine, Wiki.Nodes.N_PARAGRAPH);
      else
         Engine.Current_Node := Wiki.Nodes.N_PARAGRAPH;
      end if;
      case Engine.Context.Syntax is
         when SYNTAX_DOTCLEAR =>
            Engine.Is_Dotclear := True;
            Engine.Escape_Char := '\';
            Engine.Header_Offset := -6;
            Engine.Link_Title_First := True;
            Engine.Parse_Block := Wiki.Parsers.Dotclear.Parse_Line'Access;

         when SYNTAX_CREOLE =>
            Engine.Link_Double_Bracket := True;
            Engine.Param_Char := '<';
            Engine.Parse_Block := Wiki.Parsers.Creole.Parse_Line'Access;

         when SYNTAX_MEDIA_WIKI =>
            Engine.Link_Double_Bracket := True;
            Engine.Check_Image_Link := True;
            Engine.Param_Char := '{';
            Engine.Parse_Block := Wiki.Parsers.MediaWiki.Parse_Line'Access;

         when SYNTAX_TEXTILE =>
            Engine.Link_Double_Bracket := True;
            Engine.Check_Image_Link := True;
            Engine.Param_Char := '{';
            Engine.Parse_Block := Wiki.Parsers.Textile.Parse_Line'Access;

         when SYNTAX_GOOGLE =>
            Engine.Link_No_Space := True;
            Engine.Parse_Block := Wiki.Parsers.Google.Parse_Line'Access;

         when SYNTAX_MARKDOWN =>
            Engine.Preformat_Column := 4;
            Engine.Escape_Char := '\';
            Engine.Parse_Block := Wiki.Parsers.Markdown.Parse_Line'Access;
            Engine.Parse_Inline := Wiki.Parsers.Markdown.Parse_Inline_Text'Access;

         when SYNTAX_HTML | SYNTAX_PHPBB =>
            Engine.Parse_Block := Wiki.Parsers.Html.Parse_Line'Access;

      end case;
      declare
         Buffer : Wiki.Buffers.Buffer_Access;
      begin
         loop
            Read_Line (Engine, Buffer);
            exit when Buffer = null;
            Engine.Parse_Block (Engine, Buffer);
         end loop;
         while not Block_Stack.Is_Empty (Engine.Blocks) loop
            if Engine.Current_Node in Nodes.N_PARAGRAPH | Nodes.N_LIST_ITEM then
               Flush_Text (Engine, Trim => Right);
            end if;
            Pop_Block (Engine);
         end loop;
         if Engine.Parse_Inline /= null then
            Engine.Parse_Inline (Engine, Engine.Text_Buffer.First'Unchecked_Access);
         end if;
      end;

      if Main then
         Engine.Context.Filters.Finish (Engine.Document);
      end if;
      Doc := Engine.Document;
   end Parse;

end Wiki.Parsers;
