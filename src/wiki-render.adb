-----------------------------------------------------------------------
--  wiki-render -- Wiki renderer
--  Copyright (C) 2015, 2016, 2019, 2024, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Nodes.Lists;
package body Wiki.Render is

   --  ------------------------------
   --  Render the list of nodes from the document.
   --  ------------------------------
   procedure Render (Engine : in out Renderer'Class;
                     Doc    : in Wiki.Documents.Document;
                     List   : in Wiki.Nodes.Node_List_Access) is
      use type Wiki.Nodes.Node_List_Access;

      procedure Process (Node : in Wiki.Nodes.Node_Type);

      procedure Process (Node : in Wiki.Nodes.Node_Type) is
      begin
         Engine.Render (Doc, Node);
      end Process;

   begin
      if List /= null then
         Wiki.Nodes.Lists.Iterate (List, Process'Access);
      end if;
   end Render;

   --  ------------------------------
   --  Render the document.
   --  ------------------------------
   procedure Render (Engine : in out Renderer'Class;
                     Doc    : in Wiki.Documents.Document) is
      procedure Process (Node : in Wiki.Nodes.Node_Type);

      procedure Process (Node : in Wiki.Nodes.Node_Type) is
      begin
         Engine.Render (Doc, Node);
      end Process;
   begin
      Doc.Iterate (Process'Access);
      Engine.Finish (Doc);
   end Render;

   --  ------------------------------
   --  Format the section or list number with the optional prefix and separator.
   --  ------------------------------
   function Format_Section_Number (List      : in List_Level_Array;
                                   Prefix    : in Wiki.Strings.WString;
                                   Separator : in Wiki.Strings.WChar)
                                   return Wiki.Strings.WString is
      Result : Wiki.Strings.UString;
      Empty  : Boolean := True;
   begin
      if List'Length = 0 then
         return "";
      end if;
      Wiki.Strings.Append (Result, Prefix);
      for Value of List loop
         if Value > 0 or else not Empty then
            declare
               N : constant Strings.WString := Positive'Wide_Wide_Image (Value);
            begin
               if not Empty then
                  Wiki.Strings.Append (Result, Separator);
               end if;
               Empty := False;
               Wiki.Strings.Append (Result, N (N'First + 1 .. N'Last));
            end;
         end if;
      end loop;
      return Wiki.Strings.To_WString (Result);
   end Format_Section_Number;

end Wiki.Render;
