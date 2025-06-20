-----------------------------------------------------------------------
--  wiki-parsers-common -- Common operations with several wiki parsers
--  Copyright (C) 2011 - 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Nodes;
with Wiki.Helpers;
package body Wiki.Parsers.Common is

   use Wiki.Helpers;
   use Wiki.Nodes;
   use Wiki.Buffers;

   Attr_Names_Title_First : constant String_Array (1 .. 4)
     := (NAME_ATTR'Access, HREF_ATTR'Access, LANG_ATTR'Access, TITLE_ATTR'Access);

   Attr_Names_Link_First  : constant String_Array (1 .. 4)
     := (HREF_ATTR'Access, NAME_ATTR'Access, LANG_ATTR'Access, TITLE_ATTR'Access);

   Attr_Name              : constant String_Array (1 .. 1)
     := (1 => NAME_ATTR'Access);

   procedure Expand_Parameter (Parser  : in out Parser_Type;
                               Text    : in out Wiki.Buffers.Cursor;
                               Into    : in out Wiki.Strings.BString);

   procedure Parse_Parameters (Parser     : in out Parser_Type;
                               Text       : in out Wiki.Buffers.Cursor;
                               Separator  : in Wiki.Strings.WChar;
                               Terminator : in Wiki.Strings.WChar;
                               Names      : in String_Array;
                               Max        : in Positive := 200);

   procedure Parse_Parameter (Parser  : in out Parser_Type;
                              Text    : in out Wiki.Buffers.Cursor;
                              Expect  : in Wiki.Strings.WChar);

   procedure Append (Into : in out Wiki.Strings.BString;
                     Text : in Wiki.Buffers.Cursor) is
      Pos   : Wiki.Buffers.Cursor := Text;
   begin
      while Buffers.Is_Valid (Pos) loop
         declare
            Last : constant Natural := Pos.Block.Last;
         begin
            Append (Into, Pos.Block.Content (Pos.Pos .. Last));
         end;
         Pos.Block := Pos.Block.Next_Block;
         Pos.Pos := 1;
      end loop;
   end Append;

   procedure Parse_Token (Text        : in out Wiki.Buffers.Buffer_Access;
                          From        : in out Positive;
                          Escape_Char : in Wiki.Strings.WChar;
                          Marker1     : in Wiki.Strings.WChar;
                          Marker2     : in Wiki.Strings.WChar;
                          Into        : in out Wiki.Strings.BString) is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
   begin
      while Block /= null loop
         declare
            Last : Natural := Block.Last;
            C    : Wiki.Strings.WChar;
         begin
            while Pos <= Block.Last loop
               C := Block.Content (Pos);
               if C = Escape_Char then
                  Pos := Pos + 1;
                  if Pos > Last then
                     Block := Block.Next_Block;
                     if Block = null or else Block.Last = 0 then
                        Text := null;
                        From := Pos;
                        return;
                     end if;
                     Last := Block.Last;
                     Pos := 1;
                  end if;
                  C := Block.Content (Pos);

               elsif C = LF or else C = CR or else C = Marker1 or else C = Marker2 then
                  Text := Block;
                  From := Pos;
                  return;
               end if;
               Next (Block, Pos);
               if Block = null then
                  Text := null;
                  From := Pos;
                  return;
               end if;
               Wiki.Strings.Wide_Wide_Builders.Append (Into, C);
            end loop;
         end;
         Block := Block.Next_Block;
         Pos := 1;
      end loop;
      Text := null;
      From := Pos;
   end Parse_Token;

   --  ------------------------------
   --  Parse a template parameter and expand it to the target buffer.
   --  Example:
   --    {{{1}}}                      MediaWiki
   --    <<<1>>>                      Creole extension
   --  ------------------------------
   procedure Expand_Parameter (Parser  : in out Parser_Type;
                               Text    : in out Wiki.Buffers.Cursor;
                               Into    : in out Wiki.Strings.BString) is
      procedure Expand (Content : in Wiki.Strings.WString);

      Pos    : Wiki.Buffers.Cursor := Text;
      C      : Wiki.Strings.WChar;
      Expect : Wiki.Strings.WChar;
      Param  : Wiki.Strings.BString (256);

      procedure Expand (Content : in Wiki.Strings.WString) is
         Name : constant String := Wiki.Strings.To_String (Content);
         Pos  : constant Attributes.Cursor
           := Wiki.Attributes.Find (Parser.Context.Variables, Name);
      begin
         if Wiki.Attributes.Has_Element (Pos) then
            Append (Into, Wiki.Attributes.Get_Wide_Value (Pos));
         else
            Append (Into, Parser.Param_Char);
            Append (Into, Parser.Param_Char);
            Append (Into, Parser.Param_Char);
            Append (Into, Content);
            Append (Into, Expect);
            Append (Into, Expect);
            Append (Into, Expect);
         end if;
      end Expand;

      procedure Expand is
         new Wiki.Strings.Wide_Wide_Builders.Get (Expand);

      Count : Natural;
   begin
      Buffers.Next (Pos);
      if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= Parser.Param_Char then
         Append (Into, Parser.Param_Char);
         Text := Pos;
         return;
      end if;

      Buffers.Next (Pos);
      if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= Parser.Param_Char then
         Append (Into, Parser.Param_Char);
         Append (Into, Parser.Param_Char);
         Text := Pos;
         return;
      end if;
      Buffers.Next (Pos);

      if Parser.Param_Char = '{' then
         Expect := '}';
      else
         Expect := '>';
      end if;

      --  Collect the parameter name or index until we find the end marker.
      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         if C = Expect then
            Count := Count_Occurence (Pos.Block, Pos.Pos, Expect);
            if Count >= 3 then
               --  Expand the result.
               Expand (Param);
               for I in 1 .. 3 loop
                  Next (Pos);
               end loop;
               Text := Pos;
               return;
            end if;
            Append (Param, C);
         else
            Append (Param, C);
         end if;
         Next (Pos);
      end loop;

      Append (Into, Parser.Param_Char);
      Append (Into, Parser.Param_Char);
      Text := Pos;
   end Expand_Parameter;

   --  ------------------------------
   --  Extract a list of parameters separated by the given separator (ex: '|').
   --  ------------------------------
   procedure Parse_Parameters (Parser     : in out Parser_Type;
                               Text       : in out Wiki.Buffers.Cursor;
                               Separator  : in Wiki.Strings.WChar;
                               Terminator : in Wiki.Strings.WChar;
                               Names      : in String_Array;
                               Max        : in Positive := 200) is
      procedure Add_Parameter (Content : in Wiki.Strings.WString);

      Index : Positive := 1;

      procedure Add_Parameter (Content : in Wiki.Strings.WString) is
         Last : Natural := Content'Last;
      begin
         while Last >= Content'First and then Wiki.Helpers.Is_Space (Content (Last)) loop
            Last := Last - 1;
         end loop;
         if Index <= Names'Last then
            Wiki.Attributes.Append (Parser.Attributes, Names (Index).all,
                                    Content (Content'First .. Last));
         else
            declare
               Name : constant String := Positive'Image (Index - Names'Length);
            begin
               Wiki.Attributes.Append (Parser.Attributes, Name (Name'First + 1 .. Name'Last),
                                       Content (Content'First .. Last));
            end;
         end if;
         Index := Index + 1;
      end Add_Parameter;

      procedure Add_Attribute is
         new Wiki.Strings.Wide_Wide_Builders.Get (Add_Parameter);

      C      : Wiki.Strings.WChar;
      Value  : Wiki.Strings.BString (256);
      Pos    : Wiki.Buffers.Cursor := Text;
   begin
      Wiki.Attributes.Clear (Parser.Attributes);
      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         if C = Parser.Escape_Char then
            Buffers.Next (Pos);
            if not Buffers.Is_Valid (Pos) then
               Wiki.Parsers.Read_Line (Parser, Pos.Block);
               Pos.Pos := 1;
               if not Buffers.Is_Valid (Pos) then
                  Text := (null, 1);
                  return;
               end if;
            end if;
            C := Buffers.Char_At (Pos);
            Append (Value, C);
            Buffers.Next (Pos);
         elsif C = Separator and then Index <= Max then
            Add_Attribute (Value);
            Clear (Value);
            Buffers.Next (Pos);
         elsif C = Parser.Param_Char then
            Expand_Parameter (Parser, Pos, Value);
            if not Buffers.Is_Valid (Pos) then
               Wiki.Parsers.Read_Line (Parser, Pos.Block);
               Pos.Pos := 1;
               if not Buffers.Is_Valid (Pos) then
                  Text := (null, 1);
                  return;
               end if;
            end if;
         elsif C = Terminator then
            Add_Attribute (Value);
            Text := Pos;
            return;
         elsif Length (Value) > 0 or else not Wiki.Helpers.Is_Space (C) then
            Append (Value, C);
            Buffers.Next (Pos);
         else
            Buffers.Next (Pos);
         end if;
         if not Buffers.Is_Valid (Pos) then
            Wiki.Parsers.Read_Line (Parser, Pos.Block);
            Pos.Pos := 1;
         end if;
      end loop;
      Text := (null, 1);
   end Parse_Parameters;

   --  ------------------------------
   --  Parse a quote.
   --  Example:
   --    {{name}}
   --    {{name|language}}
   --    {{name|language|url}}
   --    ??citation??
   --  ------------------------------
   procedure Parse_Quote (Parser  : in out Parser_Type;
                          Text    : in out Wiki.Buffers.Cursor;
                          Marker  : in Wiki.Strings.WChar) is
      Link     : Wiki.Strings.BString (128);
      Quote    : Wiki.Strings.BString (128);
      Language : Wiki.Strings.BString (128);
      Pos      : Wiki.Buffers.Cursor := Text;
      Expect   : Wiki.Strings.WChar;
   begin
      Next (Pos);
      if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= Marker then
         Common.Parse_Text (Parser, Text);
         return;
      end if;

      Next (Pos);
      if not Buffers.Is_Valid (Pos) then
         Common.Parse_Text (Parser, Text, Count => 2);
         return;
      end if;

      Expect := (if Marker = '{' then '}' else Marker);
      Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, '|', Expect, Quote);
      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '|' then
         Buffers.Next (Pos);
         Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, '|', Expect, Language);
         if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '|' then
            Buffers.Next (Pos);
            Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, Expect, Expect, Link);
         end if;
      end if;

      --  Check for a last '}' or '?'.
      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = Expect then
         Buffers.Next (Pos);
      end if;

      --  Check for the second '}', abort the quote and emit the '{{' if the '}}' is missing.
      if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= Expect then
         Common.Parse_Text (Parser, Text, Count => 2);
         return;
      end if;
      Buffers.Next (Pos);
      Text := Pos;

      Flush_Text (Parser);
      if not Parser.Context.Is_Hidden then
         Wiki.Attributes.Clear (Parser.Attributes);
         Wiki.Attributes.Append (Parser.Attributes, "cite", Link);
         Wiki.Attributes.Append (Parser.Attributes, "lang", Language);
         Parser.Context.Filters.Add_Quote (Parser.Document,
                                           Wiki.Strings.To_WString (Quote),
                                           Parser.Attributes);
      end if;
   end Parse_Quote;

   --  ------------------------------
   --  Parse a link.
   --  Example:
   --    [name]
   --    [url]
   --    [name|url]
   --    [name|url|language]
   --    [name|url|language|title]
   --  MediaWiki
   --    [[link]]
   --    [[link|name]]
   --    [[link|mode|size|center|alt]]
   --    [http://...]
   --    [http://... title]
   --  ------------------------------
   procedure Parse_Link (Parser  : in out Parser_Type;
                         Text    : in out Wiki.Buffers.Cursor) is

      procedure Add_Mediawiki_Image (Link : in Wiki.Strings.WString);

      --  ------------------------------
      --  Extract MediaWiki image attribute and normalize them for the renderer.
      --  Attributes:   alt,  src,   align,   frame
      --  border,frameless,thumb,thumbnail
      --  left,right,center,none
      --  baseline,sup,super,top,text-top,middle,bottom,text-bottom
      --  <width>px, x<height>px, <width>x<height>px
      --  ------------------------------
      procedure Add_Mediawiki_Image (Link : in Wiki.Strings.WString) is
         procedure Collect_Attribute (Name  : in String;
                                      Value : in Wiki.Strings.WString);

         Len  : constant Natural := Wiki.Attributes.Length (Parser.Attributes);
         Pos  : Natural := 0;
         Attr : Wiki.Attributes.Attribute_List;

         procedure Collect_Attribute (Name  : in String;
                                      Value : in Wiki.Strings.WString) is
            pragma Unreferenced (Name);
         begin
            Pos := Pos + 1;
            if Pos = 1 then
               return;
            end if;
            if Value in "border" | "frameless" | "thumb" | "thumbnail" then
               Wiki.Attributes.Append (Attr, String '("frame"), Value);

            elsif Value in "left" | "right" | "center" | "none" then
               Wiki.Attributes.Append (Attr, String '("align"), Value);

            elsif Value in "baseline" | "sup" | "super" | "top"
              | "text-top" | "middle" | "bottom" | "text-bottom"
            then
               Wiki.Attributes.Append (Attr, String '("valign"), Value);

            elsif Value'Length > 3 and then Value (Value'Last - 1 .. Value'Last) = "px" then
               Wiki.Attributes.Append (Attr, String '("size"), Value);

            else
               Wiki.Attributes.Append (Attr, String '("alt"), Value);
               if Pos = Len then
                  Parser.Context.Filters.Add_Image (Parser.Document, Value, Attr, False);
               end if;
               return;
            end if;
            if Pos = Len then
               Parser.Context.Filters.Add_Image (Parser.Document, Link, Attr, False);
            end if;
         end Collect_Attribute;

      begin
         Wiki.Attributes.Append (Attr, String '("src"), Link);
         Wiki.Attributes.Iterate (Parser.Attributes, Collect_Attribute'Access);
      end Add_Mediawiki_Image;

      C              : Wiki.Strings.WChar;
      Separator      : Wiki.Strings.WChar := '|';
      Double_Bracket : Boolean := Parser.Link_Double_Bracket;
      Max_Count      : Positive := 200;
      Pos            : Wiki.Buffers.Cursor := Text;
   begin
      --  If links have the form '[[link]]', check the second bracket.
      Next (Pos);
      if Double_Bracket then
         if not Buffers.Is_Valid (Pos) then
            Append (Parser.Text, '[');
            Text := Pos;
            return;
         end if;
         C := Buffers.Char_At (Pos);
         if C /= '[' then
            if Parser.Context.Syntax /= SYNTAX_MEDIA_WIKI and then C /= 'h' then
               Append (Parser.Text, '[');
               Next (Text);
               return;
            end if;
            Separator := ' ';
            Double_Bracket := False;
            Max_Count := 1;
         else
            Next (Pos);
         end if;
      end if;
      if Parser.Link_No_Space then
         if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) = ' ' then
            Append (Parser.Text, '[');
            Text := Pos;
            return;
         end if;
      end if;

      Wiki.Attributes.Clear (Parser.Attributes);
      if Parser.Link_Title_First then
         Parse_Parameters (Parser, Pos, Separator, ']', Attr_Names_Title_First, Max_Count);
      else
         Parse_Parameters (Parser, Pos, Separator, ']', Attr_Names_Link_First, Max_Count);
      end if;

      if Double_Bracket and then Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = ']' then
         Buffers.Next (Pos);
      end if;

      if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= ']' then
         Append (Parser.Text, '[');
         Text := Pos;
         return;
      end if;
      Next (Pos);

      Flush_Text (Parser);

      Parser.Empty_Line := False;
      if not Parser.Context.Is_Hidden then
         declare
            Link : constant Strings.WString
              := Attributes.Get_Attribute (Parser.Attributes, HREF_ATTR);
            Name : constant Strings.WString
              := Attributes.Get_Attribute (Parser.Attributes, NAME_ATTR);
            Pos  : constant Natural := Parser.Is_Image (Link);
         begin
            if Pos > 0 then
               Add_Mediawiki_Image (Link (Pos .. Link'Last));
            else
               if Parser.Link_Title_First and then Link'Length = 0 then
                  Wiki.Attributes.Append (Parser.Attributes, HREF_ATTR, Name);
               end if;
               if not Parser.Link_Title_First and then Name'Length = 0 then
                  Parser.Context.Filters.Add_Link (Parser.Document, Link,
                                                   Parser.Attributes, False);
               else
                  Parser.Context.Filters.Add_Link (Parser.Document, Name,
                                                   Parser.Attributes, False);
               end if;
            end if;
         end;
      end if;
      Text := Pos;
   end Parse_Link;

   procedure Parse_Html_Element (Parser  : in out Parser_Type;
                                 Text    : in out Wiki.Buffers.Cursor;
                                 Start   : in Boolean) is
      use type Wiki.Html_Parser.State_Type;
      Pos  : Wiki.Buffers.Cursor := Text;
      Kind : Wiki.Html_Parser.State_Type;
   begin
      --  Feed the HTML parser if there are some pending state.
      if (for some Mode of Parser.Format => Mode) then
         Parse_Text (Parser, Text);
         return;
      end if;
      if Start then
         Next (Pos);
      end if;

      --  Feed the HTML parser if there are some pending state.
      loop
         Wiki.Html_Parser.Parse_Element (Parser.Html, Pos.Block.Content (1 .. Pos.Block.Last),
                                         Pos.Pos, Kind, Pos.Pos);
         if Kind /= Wiki.Html_Parser.HTML_NONE then
            Process_Html (Parser, Kind, Wiki.Strings.To_WString (Parser.Html.Elt_Name),
                          Parser.Html.Attributes);
         end if;
         if Pos.Pos = Pos.Block.Last + 1 then
            Pos.Block := Pos.Block.Next_Block;
            exit when Pos.Block = null;
            Pos.Pos := 1;
         end if;
         exit when Wiki.Html_Parser.Is_Empty (Parser.Html);
      end loop;
      if Parser.Pre_Tag_Counter > 0 then
         Common.Parse_Html_Preformatted (Parser, Pos);
      end if;
      Text := Pos;
   end Parse_Html_Element;

   procedure Parse_Html_Preformatted (Parser  : in out Parser_Type;
                                      Text    : in out Wiki.Buffers.Cursor) is
      use type Wiki.Html_Parser.State_Type;
      Pos    : Wiki.Buffers.Cursor := Text;
      C      : Wiki.Strings.WChar;
      Kind   : Wiki.Html_Parser.State_Type;
   begin
      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         if C = '<' then
            Wiki.Html_Parser.Parse_Element (Parser.Html, Pos.Block.Content (1 .. Pos.Block.Last),
                                            Pos.Pos + 1, Kind, Pos.Pos);
            if Kind /= Wiki.Html_Parser.HTML_NONE then
               Process_Html (Parser, Kind, Wiki.Strings.To_WString (Parser.Html.Elt_Name),
                             Parser.Html.Attributes);
            end if;
         else
            Append (Parser.Text, C);
            Buffers.Next (Pos);
         end if;
      end loop;
      Text := Pos;
   end Parse_Html_Preformatted;

   --  ------------------------------
   --  Check if this is a list item composed of '*' and '#'
   --  and terminated by a space.
   --  ------------------------------
   function Is_List (Text : in Wiki.Buffers.Cursor) return Boolean is
      Pos   : Wiki.Buffers.Cursor := Text;
      Count : Natural := 0;
   begin
      while Buffers.Is_Valid (Pos) loop
         if Buffers.Char_At (Pos) in '*' | '#' then
            Count := Count + 1;
         elsif Wiki.Helpers.Is_Space (Buffers.Char_At (Pos)) then
            return Count > 0;
         else
            return False;
         end if;
         Next (Pos);
      end loop;
      return False;
   end Is_List;

   procedure Parse_List (Parser  : in out Parser_Type;
                         Text    : in out Wiki.Buffers.Cursor) is
      function Is_List (Kind : in Wiki.Nodes.Node_Kind;
                        C    : in Wiki.Strings.WChar) return Boolean;
      procedure Check_Stack (Stack : in Block_Stack.Element_Type_Array);

      Pos    : Wiki.Buffers.Cursor := Text;
      C      : Wiki.Strings.WChar;
      Level  : Natural := 0;

      function Is_List (Kind : in Wiki.Nodes.Node_Kind;
                        C    : in Wiki.Strings.WChar) return Boolean is
      begin
         return (C = '#' and then Kind = N_NUM_LIST_START)
           or else (C = '*' and then Kind = N_LIST_START);
      end Is_List;

      procedure Check_Stack (Stack : in Block_Stack.Element_Type_Array) is
         I : Positive := Stack'Last;
      begin
         while I >= Stack'First
           and then Stack (I).Kind in N_NUM_LIST_START | N_LIST_START | N_LIST_ITEM
         loop
            I := I - 1;
         end loop;
         I := I + 1;
         while I <= Stack'Last loop
            if Stack (I).Kind /= N_LIST_ITEM then
               C := Buffers.Char_At (Pos);
               if not Is_List (Stack (I).Kind, C) then
                  if not (C in '#' | '*') and then Pos.Pos > 1 then
                     Buffers.Next (Pos);
                  end if;
                  exit when I = Stack'First;
                  if Stack (I - 1).Kind = N_LIST_ITEM then
                     I := I - 2;
                  else
                     I := I - 1;
                  end if;
                  exit;
               end if;
               Buffers.Next (Pos);
            end if;
            I := I + 1;
         end loop;
         while I < Stack'Last loop
            Pop_Block (Parser);
            I := I + 1;
         end loop;
      end Check_Stack;

   begin
      Flush_Text (Parser, Trim => Right);
      Block_Stack.Read (Parser.Blocks, Check_Stack'Access);

      if Parser.Current_Node = N_LIST_ITEM
        and then not (Buffers.Char_At (Pos) in '#' | '*')
      then
         Pop_Block (Parser);
      elsif Parser.Current_Node = N_PARAGRAPH then
         Pop_Block (Parser);
      end if;

      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         exit when C not in '*' | '#';

         if Parser.Current_Node in N_LIST_START | N_NUM_LIST_START then
            Push_Block (Parser, Nodes.N_LIST_ITEM, Level, C);
         elsif C = '#' then
            Push_Block (Parser, Nodes.N_NUM_LIST_START, Level, C, 1);
            Push_Block (Parser, Nodes.N_LIST_ITEM, Level, C);
         else
            Push_Block (Parser, Nodes.N_LIST_START, Level, C);
            Push_Block (Parser, Nodes.N_LIST_ITEM, Level, C);
         end if;
         Buffers.Next (Pos);
         Level := Level + 1;
      end loop;

      if Parser.Current_Node /= Nodes.N_LIST_ITEM then
         Push_Block (Parser, Nodes.N_LIST_ITEM, Level, C);
      end if;

      if Wiki.Helpers.Is_Space (Buffers.Char_At (Pos)) then
         Buffers.Next (Pos);
      end if;
      Text := Pos;
   end Parse_List;

   --  ------------------------------
   --  Parse a list definition that starts with ';':
   --    ;item 1
   --    : definition 1
   --  ------------------------------
   procedure Parse_Definition (Parser  : in out Parser_Type;
                               Text    : in out Wiki.Buffers.Cursor;
                               Is_Term : in Boolean) is
      Pos    : Wiki.Buffers.Cursor := Text;
      C      : constant Wiki.Strings.WChar := Next (Pos);
      Count  : Natural;
   begin
      if C = Helpers.NUL or else Wiki.Helpers.Is_Newline (C) then
         Common.Parse_Text (Parser, Text);
         return;
      end if;

      Flush_Text (Parser, Trim => Right);

      --  Leave current definition or term.
      if Parser.Current_Node in Nodes.N_DEFINITION | Nodes.N_DEFINITION_TERM then
         Pop_Block (Parser);
         Parser.Context.Filters.End_Block (Parser.Document, Parser.Current_Node);
      end if;

      --  Start the new term or definition.
      if Is_Term then
         Push_Block (Parser, Nodes.N_DEFINITION_TERM);
         Parser.Context.Filters.Start_Block (Parser.Document, Nodes.N_DEFINITION_TERM, 0);
      else
         Push_Block (Parser, Nodes.N_DEFINITION);
         Parser.Context.Filters.Start_Block (Parser.Document, Nodes.N_DEFINITION, 0);
      end if;
      Buffers.Skip_Spaces (Pos, Count);
      Text := Pos;
   end Parse_Definition;

   --  ------------------------------
   --  Parse a template parameter and expand it.
   --  Example:
   --    {{{1}}}                      MediaWiki
   --    <<<2>>>>                     Creole
   --  ------------------------------
   procedure Parse_Parameter (Parser  : in out Parser_Type;
                              Text    : in out Wiki.Buffers.Cursor;
                              Expect  : in Wiki.Strings.WChar) is
      Pos   : Wiki.Buffers.Cursor := Text;
   begin
      Common.Parse_Parameters (Parser, Pos, '|', Expect, Attr_Name);
      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = Expect then
         Buffers.Next (Pos);
      end if;
      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = Expect then
         Buffers.Next (Pos);
      end if;
      if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= Expect then
         Text := (null, 1);
         return;
      end if;
      Buffers.Next (Pos);
      Text := Pos;

      if not Parser.Context.Is_Hidden then
         declare
            Name : constant String
              := Attributes.Get_Value (Wiki.Attributes.First (Parser.Attributes));
            Pos  : constant Wiki.Attributes.Cursor
              := Wiki.Attributes.Find (Parser.Context.Variables, Name);
         begin
            if Wiki.Attributes.Has_Element (Pos) then
               Append (Parser.Text, Wiki.Attributes.Get_Wide_Value (Pos));
            end if;
         end;
      end if;
   end Parse_Parameter;

   --  ------------------------------
   --  Parse a single text character and add it to the text buffer.
   --  ------------------------------
   procedure Parse_Text (Parser : in out Parser_Type;
                         Text   : in out Wiki.Buffers.Cursor;
                         Count  : in Positive := 1) is
   begin
      for I in 1 .. Count loop
         exit when not Buffers.Is_Valid (Text);
         Append (Parser.Text, Buffers.Char_At (Text));
         Buffers.Next (Text);
      end loop;
   end Parse_Text;

   procedure Parse_Paragraph (Parser : in out Parser_Type;
                              Text   : in out Wiki.Buffers.Cursor) is
      pragma Unreferenced (Text);
   begin
      if Parser.Current_Node /= Nodes.N_PARAGRAPH
        or else not Parser.Is_Empty_Paragraph
        or else Wiki.Strings.Length (Parser.Text) > 0
      then
         Flush_Text (Parser, Trim => Right);
         Pop_List (Parser);
         Pop_Block (Parser);
         Push_Block (Parser, Nodes.N_PARAGRAPH);
      end if;
   end Parse_Paragraph;

   procedure Parse_Horizontal_Rule (Parser : in out Parser_Type;
                                    Text   : in out Wiki.Buffers.Cursor;
                                    Marker : in Wiki.Strings.WChar) is
      Count : constant Natural := Count_Occurence (Text.Block, Text.Pos, Marker);
      Pos   : Wiki.Buffers.Cursor := Text;
   begin
      if Count /= 4 then
         return;
      end if;
      Buffers.Next (Pos, Count);
      if not Wiki.Helpers.Is_Newline (Buffers.Char_At (Pos)) then
         return;
      end if;
      Text := (null, 1);

      Add_Horizontal_Rule (Parser);
   end Parse_Horizontal_Rule;

   --  Parse a preformatted header block.
   --  Example:
   --    ///
   --    ///html
   --    ///[Ada]
   --    {{{
   --    ```
   procedure Parse_Preformatted (Parser : in out Parser_Type;
                                 Text   : in out Wiki.Buffers.Cursor;
                                 Marker : in Wiki.Strings.WChar;
                                 Keep_Block : in Boolean := False) is
      Count : constant Natural := Count_Occurence (Text.Block, Text.Pos, Marker);
   begin
      if Count < 3 then
         return;
      end if;
      if Parser.Context.Syntax /= SYNTAX_MARKDOWN and then Count /= 3 then
         return;
      end if;

      --  Extract the format either 'Ada' or '[Ada]'
      declare
         Pos   : Wiki.Buffers.Cursor := Text;
         Space_Count : Natural;
      begin
         Buffers.Next (Pos, Count);
         Wiki.Strings.Clear (Parser.Preformat_Format);
         Buffers.Skip_Spaces (Pos, Space_Count);
         if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '[' then
            Next (Pos);
            Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, ']', ']',
                                Parser.Preformat_Format);
            Next (Pos);
         else
            Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, Marker, LF,
                                Parser.Preformat_Format);
         end if;
         Buffers.Skip_Spaces (Pos, Space_Count);
         Text := Pos;
      end;

      Parser.Preformat_Indent := 0;
      Parser.Preformat_Fence := Marker;
      Parser.Preformat_Fcount := Count;
      Flush_Text (Parser, Trim => Right);
      if not Keep_Block then
         Pop_Block (Parser);
      end if;
      Push_Block (Parser, N_PREFORMAT);
   end Parse_Preformatted;

   --  ------------------------------
   --  Parse a wiki heading.  The heading could start with '=' or '!'.
   --  The trailing equals are ignored.
   --  Example:
   --    ==== Level 4       Creole
   --    == Level 2 ==      MediaWiki
   --    !!! Level 3        Dotclear
   --  ------------------------------
   procedure Parse_Header (Parser : in out Parser_Type;
                           Text   : in out Wiki.Buffers.Cursor;
                           Marker : in Wiki.Strings.WChar) is
      Count : constant Natural := Count_Occurence (Text.Block, Text.Pos, Marker);
      Level : Integer;
   begin
      if Count = 0 or else Count >= 6 then
         return;
      end if;

      Level := Count + Parser.Header_Offset;
      if Level <= 0 then
         Level := -Level;
      end if;

      Flush_Text (Parser, Trim => Right);
      --  Pop_Block (Parser);
      Parser.Header_Level := Level;
      Push_Block (Parser, Nodes.N_HEADER, Level);
      declare
         Pos    : Wiki.Buffers.Cursor := Text;
      begin
         Buffers.Next (Pos, Count);
         while Buffers.Is_Valid (Pos) loop
            if not Wiki.Helpers.Is_Space_Or_Newline (Buffers.Char_At (Pos)) then
               Common.Append (Parser.Text, Pos);
               Text := (null, 1);
               return;
            end if;
            Buffers.Next (Pos);
         end loop;
      end;
   end Parse_Header;

   --  ------------------------------
   --  Parse a template with parameters.
   --  Example:
   --    {{Name|param|...}}           MediaWiki
   --    {{Name|param=value|...}}     MediaWiki
   --    <<Name param=value ...>>     Creole
   --    [{Name param=value ...}]     JSPWiki
   --  ------------------------------
   procedure Parse_Template (Parser  : in out Parser_Type;
                             Text    : in out Wiki.Buffers.Cursor;
                             Token   : in Wiki.Strings.WChar) is
      use type Wiki.Plugins.Wiki_Plugin_Access;

      Pos    : Wiki.Buffers.Cursor := Text;
      Expect : Wiki.Strings.WChar;
   begin
      Buffers.Next (Pos);
      if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= Token then
         Parse_Text (Parser, Text);
         return;
      end if;
      Buffers.Next (Pos);
      if not Buffers.Is_Valid (Pos) then
         Parse_Text (Parser, Text);
         return;
      end if;

      Expect := (if Token = '{' then '}' else '>');

      if Buffers.Char_At (Pos) = Token then
         Buffers.Next (Pos);
         Parse_Parameter (Parser, Pos, Expect);
         if Buffers.Is_Valid (Pos) then
            Text := Pos;
         else
            Parse_Text (Parser, Text);
         end if;
         return;
      end if;

      Wiki.Attributes.Clear (Parser.Attributes);
      if Parser.Context.Syntax = SYNTAX_MEDIA_WIKI then
         Common.Parse_Parameters (Parser, Pos, '|', Expect, Attr_Name);
      else
         Common.Parse_Parameters (Parser, Pos, ' ', Expect, Attr_Name);
      end if;

      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = Expect then
         Buffers.Next (Pos);
      end if;

      if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= Expect then
         Parse_Text (Parser, Text);
         return;
      end if;
      Buffers.Next (Pos);
      Text := Pos;

      Flush_Text (Parser);

      Parser.Empty_Line := False;
      declare
         use type Wiki.Strings.UString;
         Name    : constant Strings.WString
           := Attributes.Get_Attribute (Parser.Attributes, NAME_ATTR);
         Plugin  : constant Wiki.Plugins.Wiki_Plugin_Access := Parser.Find (Name);
         Context : Wiki.Plugins.Plugin_Context;
         Ctx     : access Wiki.Plugins.Plugin_Context;
      begin
         if Plugin /= null then
            --  Check that we are not including the template recursively.
            Ctx := Parser.Context'Access;
            while Ctx /= null loop
               if Ctx.Ident = Name then
                  Append (Parser.Text, "Recursive call to ");
                  Append (Parser.Text, Name);
                  return;
               end if;
               Ctx := Ctx.Previous;
            end loop;
            Context.Previous := Parser.Context'Unchecked_Access;
            Context.Factory := Parser.Context.Factory;
            Context.Syntax  := Parser.Context.Syntax;
            Context.Variables := Parser.Attributes;
            Context.Is_Included := True;
            Context.Ident := Wiki.Strings.To_UString (Name);
            Context.Filters.Set_Chain (Parser.Context.Filters);
            Plugin.Expand (Parser.Document, Parser.Attributes, Context);
         end if;
      end;
   end Parse_Template;

   procedure Parse_Entity (Parser : in out Parser_Type;
                           Text   : in out Wiki.Buffers.Cursor;
                           Status : out Wiki.Html_Parser.Entity_State_Type;
                           Entity : out Wiki.Strings.WChar) is
      Pos  : Wiki.Buffers.Cursor := Text;
   begin
      Status := Wiki.Html_Parser.ENTITY_NONE;
      Entity := Wiki.Html_Parser.NUL;
      Buffers.Next (Pos);
      if not Buffers.Is_Valid (Pos) then
         return;
      end if;
      loop
         Wiki.Html_Parser.Parse_Entity (Parser.Html, Pos.Block.Content (1 .. Pos.Block.Last),
                                        Pos.Pos, Status, Entity, Pos.Pos);
         case Status is
            when Wiki.Html_Parser.ENTITY_VALID =>
               if Pos.Pos > Pos.Block.Last then
                  Text := (Pos.Block.Next_Block, 1);
               else
                  Text := Pos;
               end if;
               return;

            when Wiki.Html_Parser.ENTITY_NONE =>
               return;

            when Wiki.Html_Parser.ENTITY_MIDDLE =>
               Pos.Block := Pos.Block.Next_Block;
               if Pos.Block = null then
                  Status := Wiki.Html_Parser.ENTITY_NONE;
                  return;
               end if;
               Pos.Pos := 1;

         end case;
      end loop;
   end Parse_Entity;

   procedure Parse_Entity (Parser : in out Parser_Type;
                           Text   : in out Wiki.Buffers.Cursor) is
      Pos    : Wiki.Buffers.Cursor := Text;
      Status : Wiki.Html_Parser.Entity_State_Type := Wiki.Html_Parser.ENTITY_NONE;
      C      : Wiki.Strings.WChar;
   begin
      Buffers.Next (Pos);
      if not Buffers.Is_Valid (Pos) then
         Parse_Text (Parser, Text);
         return;
      end if;
      loop
         Wiki.Html_Parser.Parse_Entity (Parser.Html, Pos.Block.Content (1 .. Pos.Block.Last),
                                        Pos.Pos, Status, C, Pos.Pos);
         case Status is
            when Wiki.Html_Parser.ENTITY_VALID =>
               Append (Parser.Text, C);
               Text := Pos;
               return;

            when Wiki.Html_Parser.ENTITY_NONE =>
               Parse_Text (Parser, Text);
               return;

            when Wiki.Html_Parser.ENTITY_MIDDLE =>
               Pos.Block := Pos.Block.Next_Block;
               if Pos.Block = null then
                  Parse_Text (Parser, Text);
                  return;
               end if;
               Pos.Pos := 1;

         end case;
      end loop;
   end Parse_Entity;

end Wiki.Parsers.Common;
