-----------------------------------------------------------------------
--  wiki-helpers -- Helper operations for wiki parsers and renderer
--  Copyright (C) 2016 Stephane Carrez
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

package body Wiki.Helpers is

   --  ------------------------------
   --  Returns True if the character is a space or tab.
   --  ------------------------------
   function Is_Space (C : in Wiki.Strings.WChar) return Boolean is
   begin
      return Ada.Wide_Wide_Characters.Handling.Is_Space (C) or C = HT;
   end Is_Space;

   --  ------------------------------
   --  Returns True if the character is a space, tab or a newline.
   --  ------------------------------
   function Is_Space_Or_Newline (C : in Wiki.Strings.WChar) return Boolean is
   begin
      return Is_Space (C) or Ada.Wide_Wide_Characters.Handling.Is_Line_Terminator (C);
   end Is_Space_Or_Newline;

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
           or Text (Text'First .. Text'First + 7) = "https://";
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
      return S = ".png" or S = ".jpg" or S = ".gif" or S = ".jpeg";
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
      elsif Current_Tag = Tag and Tag_Omission (Current_Tag) then
         return True;
      else
         case Current_Tag is
            when DT_TAG | DD_TAG =>
               return Tag = DD_TAG or Tag = DL_TAG or Tag = DT_TAG;

            when TD_TAG =>
               return Tag = TD_TAG or Tag = TR_TAG or Tag = TH_TAG;

            when TR_TAG =>
               return False;

            when others =>
               return False;

         end case;
      end if;
   end Need_Close;

end Wiki.Helpers;
