-----------------------------------------------------------------------
--  wiki-utils -- Wiki utility operations
--  Copyright (C) 2015, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Strings;
package Wiki.Utils is

   --  Render the wiki text according to the wiki syntax in HTML into a string.
   function To_Html (Text   : in Wiki.Strings.WString;
                     Syntax : in Wiki.Wiki_Syntax) return String;

   --  Render the wiki text according to the wiki syntax in text into a string.
   --  Wiki formatting and decoration are removed.
   function To_Text (Text   : in Wiki.Strings.WString;
                     Syntax : in Wiki.Wiki_Syntax) return String;

end Wiki.Utils;
