-----------------------------------------------------------------------
--  wiki-utils -- Wiki utility operations
--  Copyright (C) 2015 Stephane Carrez
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
with Wiki.Render.Text;
with Wiki.Render.Html;
with Wiki.Filters.Html;
with Wiki.Writers.Builders;
package body Wiki.Utils is

   --  ------------------------------
   --  Render the wiki text according to the wiki syntax in HTML into a string.
   --  ------------------------------
   function To_Html (Text   : in Wide_Wide_String;
                     Syntax : in Wiki.Parsers.Wiki_Syntax_Type) return String is
      Writer   : aliased Wiki.Writers.Builders.Html_Writer_Type;
      Renderer : aliased Wiki.Render.Html.Html_Renderer;
      Filter   : aliased Wiki.Filters.Html.Html_Filter_Type;
   begin
      Renderer.Set_Writer (Writer'Unchecked_Access);
      Filter.Set_Document (Renderer'Unchecked_Access);
      Wiki.Parsers.Parse (Filter'Unchecked_Access, Text, Syntax);
      return Writer.To_String;
   end To_Html;

   --  ------------------------------
   --  Render the wiki text according to the wiki syntax in text into a string.
   --  Wiki formatting and decoration are removed.
   --  ------------------------------
   function To_Text (Text   : in Wide_Wide_String;
                     Syntax : in Wiki.Parsers.Wiki_Syntax_Type) return String is
      Writer   : aliased Wiki.Writers.Builders.Writer_Builder_Type;
      Renderer : aliased Wiki.Render.Text.Text_Renderer;
   begin
      Renderer.Set_Writer (Writer'Unchecked_Access);
      Wiki.Parsers.Parse (Renderer'Unchecked_Access, Text, Syntax);
      return Writer.To_String;
   end To_Text;

end Wiki.Utils;
