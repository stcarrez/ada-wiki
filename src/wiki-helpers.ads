-----------------------------------------------------------------------
--  wiki-helpers -- Helper operations for wiki parsers and renderer
--  Copyright (C) 2016, 2020, 2022, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Strings;
package Wiki.Helpers is

   pragma Preelaborate;

   LF   : constant Wiki.Strings.WChar := Wiki.Strings.WChar'Val (16#0A#);
   CR   : constant Wiki.Strings.WChar := Wiki.Strings.WChar'Val (16#0D#);
   HT   : constant Wiki.Strings.WChar := Wiki.Strings.WChar'Val (16#09#);
   NBSP : constant Wiki.Strings.WChar := Wiki.Strings.WChar'Val (16#A0#);

   --  Returns True if the character is a space or tab.
   function Is_Space (C : in Wiki.Strings.WChar) return Boolean;

   --  Returns True if the character is a space, tab or a newline.
   function Is_Space_Or_Newline (C : in Wiki.Strings.WChar) return Boolean;

   --  Returns True if the character is a punctuation character.
   function Is_Punctuation (C : in Wiki.Strings.WChar) return Boolean;

   --  Returns True if the character is a punctuation or symbol character
   --  (class P or S from Unicode).
   function Is_Symbol_Or_Punctuation (C : in Wiki.Strings.WChar) return Boolean;

   --  Returns True if the character is a line terminator.
   function Is_Newline (C : in Wiki.Strings.WChar) return Boolean;

   --  Returns True if the text is a valid URL
   function Is_Url (Text : in Wiki.Strings.WString) return Boolean;

   --  Returns True if the extension part correspond to an image.
   --  Recognized extension are: .png, .gif, .jpg, .jpeg.
   --  The extension case is ignored.
   function Is_Image_Extension (Ext : in Wiki.Strings.WString) return Boolean;

   --  Given the current tag on the top of the stack and the new tag that will be pushed,
   --  decide whether the current tag must be closed or not.
   --  Returns True if the current tag must be closed.
   function Need_Close (Tag         : in Html_Tag;
                        Current_Tag : in Html_Tag) return Boolean;

   --  Get the dimension represented by the string.  The string has one of the following
   --  formats:
   --    original           -> Width, Height := Natural'Last
   --    default            -> Width := 800, Height := 0
   --    upright            -> Width := 800, Height := 0
   --    <width>px          -> Width := <width>, Height := 0
   --    x<height>px        -> Width := 0, Height := <height>
   --    <width>x<height>px -> Width := <width>, Height := <height>
   procedure Get_Sizes (Dimension : in Wiki.Strings.WString;
                        Width     : out Natural;
                        Height    : out Natural);

   --  Find the position of the first non space character in the text starting at the
   --  given position.  Returns Text'Last + 1 if the text only contains spaces.
   function Skip_Spaces (Text : in Wiki.Strings.WString;
                         From : in Positive) return Positive;

   --  Find the position of the last non space character scanning the text backward
   --  from the given position.  Returns Text'First - 1 if the text only contains spaces.
   function Trim_Spaces (Text : in Wiki.Strings.WString;
                         From : in Positive) return Natural;

   --  Find the position of the given character in the string starting at the given position.
   function Index (Text : in Wiki.Strings.WString;
                   Item : in Wiki.Strings.WChar;
                   From : in Positive) return Natural;

end Wiki.Helpers;
