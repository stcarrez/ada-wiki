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

package Wiki.Helpers is

   pragma Preelaborate;

   LF : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#0A#);
   CR : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#0D#);
   HT : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#09#);

   --  Returns True if the character is a space or tab.
   function Is_Space (C : in Wide_Wide_Character) return Boolean;

   --  Returns True if the character is a space, tab or a newline.
   function Is_Space_Or_Newline (C : in Wide_Wide_Character) return Boolean;

   --  Returns True if the text is a valid URL
   function Is_Url (Text : in Wide_Wide_String) return Boolean;

   --  Returns True if the extension part correspond to an image.
   --  Recognized extension are: .png, .gif, .jpg, .jpeg.
   --  The extension case is ignored.
   function Is_Image_Extension (Ext : in Wide_Wide_String) return Boolean;

end Wiki.Helpers;
