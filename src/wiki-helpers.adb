-----------------------------------------------------------------------
--  wiki-helpers -- Helper operations for wiki parsers and renderer
--  Copyright (C) 2016, 2020, 2022 Stephane Carrez
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
with Ada.Wide_Wide_Characters.Handling;
with Ada.Wide_Wide_Characters.Unicode;
package body Wiki.Helpers is

   --  ------------------------------
   --  Returns True if the character is a space or tab.
   --  ------------------------------
   function Is_Space (C : in Wiki.Strings.WChar) return Boolean is
   begin
      return Ada.Wide_Wide_Characters.Handling.Is_Space (C) or else C = HT or else C = NBSP;
   end Is_Space;

   --  ------------------------------
   --  Returns True if the character is a space, tab or a newline.
   --  ------------------------------
   function Is_Space_Or_Newline (C : in Wiki.Strings.WChar) return Boolean is
   begin
      return Is_Space (C) or else Ada.Wide_Wide_Characters.Handling.Is_Line_Terminator (C);
   end Is_Space_Or_Newline;

   --  ------------------------------
   --  Returns True if the character is a punctuation character.
   --  ------------------------------
   function Is_Punctuation (C : in Wiki.Strings.WChar) return Boolean is
   begin
      return Ada.Wide_Wide_Characters.Unicode.Is_Punctuation (C);
   end Is_Punctuation;

   --  ------------------------------
   --  Returns True if the character is a line terminator.
   --  ------------------------------
   function Is_Newline (C : in Wiki.Strings.WChar) return Boolean is
   begin
      return Ada.Wide_Wide_Characters.Handling.Is_Line_Terminator (C);
   end Is_Newline;

   --  ------------------------------
   --  Returns True if the text is a valid URL
   --  ------------------------------
   function Is_Url (Text : in Wiki.Strings.WString) return Boolean is
   begin
      if Text'Length <= 9 then
         return False;
      else
         return Text (Text'First .. Text'First + 6) = "http://"
           or else Text (Text'First .. Text'First + 7) = "https://";
      end if;
   end Is_Url;

   --  ------------------------------
   --  Returns True if the extension part correspond to an image.
   --  Recognized extension are: .png, .gif, .jpg, .jpeg.
   --  The extension case is ignored.
   --  ------------------------------
   function Is_Image_Extension (Ext : in Wiki.Strings.WString) return Boolean is
      S : constant Wiki.Strings.WString := Ada.Wide_Wide_Characters.Handling.To_Lower (Ext);
   begin
      return S in ".png" | ".jpg" | ".gif" | ".jpeg";
   end Is_Image_Extension;

   --  ------------------------------
   --  Given the current tag on the top of the stack and the new tag that will be pushed,
   --  decide whether the current tag must be closed or not.
   --  Returns True if the current tag must be closed.
   --  ------------------------------
   function Need_Close (Tag         : in Html_Tag;
                        Current_Tag : in Html_Tag) return Boolean is
   begin
      if No_End_Tag (Current_Tag) then
         return True;
      elsif Current_Tag = Tag and then Tag_Omission (Current_Tag) then
         return True;
      else
         case Current_Tag is
            when DT_TAG | DD_TAG =>
               return Tag in DD_TAG | DL_TAG | DT_TAG;

            when TD_TAG =>
               return Tag in TD_TAG | TR_TAG | TH_TAG;

            when TR_TAG =>
               return False;

            when others =>
               return False;

         end case;
      end if;
   end Need_Close;

   --  ------------------------------
   --  Get the dimension represented by the string.  The string has one of the following
   --  formats:
   --    original           -> Width, Height := Natural'Last
   --    default            -> Width := 800, Height := 0
   --    upright            -> Width := 800, Height := 0
   --    <width>px          -> Width := <width>, Height := 0
   --    x<height>px        -> Width := 0, Height := <height>
   --    <width>x<height>px -> Width := <width>, Height := <height>
   --  ------------------------------
   procedure Get_Sizes (Dimension : in Wiki.Strings.WString;
                        Width     : out Natural;
                        Height    : out Natural) is
      Pos  : Natural;
      Last : Natural;
   begin
      if Dimension = "original" then
         Width  := Natural'Last;
         Height := Natural'Last;
      elsif Dimension in "default" | "upright" then
         Width  := 800;
         Height := 0;
      else
         Pos  := Wiki.Strings.Index (Dimension, "x");
         Last := Wiki.Strings.Index (Dimension, "px");
         if Pos > Dimension'First and then Last + 1 /= Pos then
            Width := Natural'Wide_Wide_Value (Dimension (Dimension'First .. Pos - 1));
         elsif Last > 0 then
            Width := Natural'Wide_Wide_Value (Dimension (Dimension'First .. Last - 1));
         else
            Width := 0;
         end if;
         if Pos < Dimension'Last then
            Height := Natural'Wide_Wide_Value (Dimension (Pos + 1 .. Last - 1));
         else
            Height := 0;
         end if;
      end if;

   exception
      when Constraint_Error =>
         Width  := 0;
         Height := 0;
   end Get_Sizes;

   --  ------------------------------
   --  Find the position of the first non space character in the text starting at the
   --  given position.  Returns Text'Last + 1 if the text only contains spaces.
   --  ------------------------------
   function Skip_Spaces (Text : in Wiki.Strings.WString;
                         From : in Positive) return Positive is
      Pos : Positive := From;
   begin
      while Pos <= Text'Last and then Is_Space (Text (Pos)) loop
         Pos := Pos + 1;
      end loop;
      return Pos;
   end Skip_Spaces;

   --  ------------------------------
   --  Find the position of the last non space character scanning the text backward
   --  from the given position.  Returns Text'First - 1 if the text only contains spaces.
   --  ------------------------------
   function Trim_Spaces (Text : in Wiki.Strings.WString;
                         From : in Positive) return Natural is
      Pos : Natural := From;
   begin
      while Pos >= Text'First and then Is_Space_Or_Newline (Text (Pos)) loop
         Pos := Pos - 1;
      end loop;
      return Pos;
   end Trim_Spaces;

   --  ------------------------------
   --  Find the position of the given character in the string starting at the given position.
   --  ------------------------------
   function Index (Text : in Wiki.Strings.WString;
                   Item : in Wiki.Strings.WChar;
                   From : in Positive) return Natural is
      Pos : Natural := From;
   begin
      while Pos <= Text'Last loop
         if Text (Pos) = Item then
            return Pos;
         end if;
         Pos := Pos + 1;
      end loop;
      return 0;
   end Index;

end Wiki.Helpers;
