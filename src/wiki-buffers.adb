-----------------------------------------------------------------------
--  util-texts-builders -- Text builder
--  Copyright (C) 2013, 2016, 2017, 2021, 2022, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

with Wiki.Helpers;
package body Wiki.Buffers is

   subtype Input is Wiki.Strings.WString;
   subtype Block_Access is Buffer_Access;

   --  ------------------------------
   --  Move forward to skip a number of items.
   --  ------------------------------
   procedure Next (Content : in out Buffer_Access;
                   Pos     : in out Positive) is
   begin
      if Pos + 1 > Content.Last then
         Content := Content.Next_Block;
         Pos := 1;
      else
         Pos := Pos + 1;
      end if;
   end Next;

   procedure Next (Pos : in out Cursor) is
   begin
      if Pos.Block /= null then
         if Pos.Pos >= Pos.Block.Last then
            Pos.Block := Pos.Block.Next_Block;
            Pos.Pos := 1;
         else
            Pos.Pos := Pos.Pos + 1;
         end if;
      end if;
   end Next;

   procedure Next (Content : in out Buffer_Access;
                   Pos     : in out Positive;
                   Count   : in Natural) is
   begin
      for I in 1 .. Count loop
         if Pos + 1 > Content.Last then
            Content := Content.Next_Block;
            Pos := 1;
            exit when Content = null;
         else
            Pos := Pos + 1;
         end if;
      end loop;
   end Next;

   procedure Next (Pos   : in out Cursor;
                   Count : in Natural) is
   begin
      for I in 1 .. Count loop
         Next (Pos);
      end loop;
   end Next;

   --  ------------------------------
   --  Move forward and return the next character or NUL.
   --  ------------------------------
   function Next (Text : in out Cursor) return Strings.WChar is
   begin
      if Text.Block = null then
         return Helpers.NUL;
      elsif Text.Pos >= Text.Block.Last then
         Text.Block := Text.Block.Next_Block;
         Text.Pos := 1;
         return (if Text.Block /= null then Text.Block.Content (1) else Helpers.NUL);
      else
         Text.Pos := Text.Pos + 1;
         return Text.Block.Content (Text.Pos);
      end if;
   end Next;

   --  ------------------------------
   --  Get the length of the item builder.
   --  ------------------------------
   function Length (Source : in Builder) return Natural is
   begin
      return Source.Length;
   end Length;

   --  ------------------------------
   --  Get the capacity of the builder.
   --  ------------------------------
   function Capacity (Source : in Builder) return Natural is
      B : constant Block_Access := Source.Current;
   begin
      return Source.Length + B.Len - B.Last;
   end Capacity;

   --  ------------------------------
   --  Get the builder block size.
   --  ------------------------------
   function Block_Size (Source : in Builder) return Positive is
   begin
      return Source.Block_Size;
   end Block_Size;

   --  ------------------------------
   --  Set the block size for the allocation of next chunks.
   --  ------------------------------
   procedure Set_Block_Size (Source : in out Builder;
                             Size   : in Positive) is
   begin
      Source.Block_Size := Size;
   end Set_Block_Size;

   --  ------------------------------
   --  Append the <tt>New_Item</tt> at the end of the source growing the buffer if necessary.
   --  ------------------------------
   procedure Append (Source   : in out Builder;
                     New_Item : in Input) is
      B     : Block_Access := Source.Current;
      Start : Natural := New_Item'First;
      Last  : constant Natural := New_Item'Last;
   begin
      while Start <= Last loop
         declare
            Space : Natural := B.Len - B.Last;
            Size  : constant Natural := Last - Start + 1;
         begin
            if Space > Size then
               Space := Size;
            elsif Space = 0 then
               if Size > Source.Block_Size then
                  B.Next_Block := new Buffer (Size);
               else
                  B.Next_Block := new Buffer (Source.Block_Size);
               end if;
               B.Next_Block.Offset := B.Offset + B.Len;
               B := B.Next_Block;
               Source.Current := B;
               if B.Len > Size then
                  Space := Size;
               else
                  Space  := B.Len;
               end if;
            end if;
            B.Content (B.Last + 1 .. B.Last + Space) := New_Item (Start .. Start + Space - 1);
            Source.Length := Source.Length + Space;
            B.Last := B.Last + Space;
            Start  := Start + Space;
         end;
      end loop;
   end Append;

   --  ------------------------------
   --  Append the <tt>New_Item</tt> at the end of the source growing the buffer if necessary.
   --  ------------------------------
   procedure Append (Source   : in out Builder;
                     New_Item : in Wiki.Strings.WChar) is
      B     : Block_Access := Source.Current;
   begin
      if B.Len = B.Last then
         B.Next_Block := new Buffer (Source.Block_Size);
         B.Next_Block.Offset := B.Offset + B.Len;
         B := B.Next_Block;
         Source.Current := B;
      end if;
      Source.Length := Source.Length + 1;
      B.Last := B.Last + 1;
      B.Content (B.Last) := New_Item;
   end Append;

   procedure Inline_Append (Source   : in out Builder) is
      B     : Block_Access := Source.Current;
      Last  : Natural;
      Done  : Boolean;
   begin
      loop
         if B.Len = B.Last then
            B.Next_Block := new Buffer (Source.Block_Size);
            B.Next_Block.Offset := B.Offset + B.Len;
            B := B.Next_Block;
            Source.Current := B;
         end if;
         Process (B.Content (B.Last + 1 .. B.Len), Last, Done);
         exit when Last > B.Len or else Last <= B.Last;
         Source.Length := Source.Length + Last - B.Last;
         B.Last := Last;
         exit when Last < B.Len or else Done;
      end loop;
   end Inline_Append;

   procedure Append (Into     : in out Builder;
                     Buffer   : in Buffer_Access;
                     From     : in Positive) is
      Block : Buffer_Access := Buffer;
      First : Positive := From;
   begin
      while Block /= null loop
         Append (Into, Block.Content (First .. Block.Last));
         Block := Block.Next_Block;
         First := 1;
      end loop;
   end Append;

   procedure Append_Until (Into     : in out Builder;
                           Buffer   : in Buffer_Access;
                           From     : in Positive) is
      Block : Buffer_Access := Buffer;
      Pos   : Positive := From;
   begin
      while Block /= null and then not Condition (Block, Pos) loop
         Append (Into, Block.Content (Pos));
         Next (Block, Pos);
      end loop;
   end Append_Until;

   procedure Trim (Source : in out Builder) is

      procedure Trim_Buffer (Buffer : in Buffer_Access;
                             Done   : out Boolean) is
      begin
         if Buffer.Next_Block /= null then
            Trim_Buffer (Buffer.Next_Block, Done);
            if Done then
               return;
            end if;
         end if;
         Done := False;
         while Buffer.Last > 0 loop
            if Trim_Character (Buffer.Content (Buffer.Last)) then
               Buffer.Last := Buffer.Last - 1;
               Source.Length := Source.Length - 1;
            else
               Done := True;
               return;
            end if;
         end loop;
      end Trim_Buffer;

      Done : Boolean := False;
   begin
      Trim_Buffer (Source.First'Unchecked_Access, Done);
   end Trim;

   --  ------------------------------
   --  Clear the source freeing any storage allocated for the buffer.
   --  ------------------------------
   procedure Clear (Source : in out Builder) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Buffer, Name => Buffer_Access);
      Current, Next : Block_Access;
   begin
      Next := Source.First.Next_Block;
      while Next /= null loop
         Current := Next;
         Next    := Current.Next_Block;
         Free (Current);
      end loop;
      Source.First.Next_Block := null;
      Source.First.Last       := 0;
      Source.Current          := Source.First'Unchecked_Access;
      Source.Length           := 0;
   end Clear;

   --  ------------------------------
   --  Truncate the current buffer at the given block and position.
   --  ------------------------------
   procedure Truncate (Source : in out Builder;
                       Block  : in Buffer_Access;
                       Pos    : in Positive) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Buffer, Name => Buffer_Access);
      Current, Next : Block_Access;
      Length : Natural := 0;
   begin
      Next := Source.First'Unchecked_Access;
      while Next /= null and then Next /= Block loop
         Length := Length + Next.Last;
         Next := Next.Next_Block;
      end loop;
      if Next = Block then
         Length := Length + Pos - 1;
         Next.Last := Pos - 1;
         Source.Length := Length;
         Source.Current := Next;
         Current := Next.Next_Block;
         Next.Next_Block := null;
         while Current /= null loop
            Next := Current.Next_Block;
            Free (Current);
            Current := Next;
         end loop;
      end if;
   end Truncate;

   procedure Inline_Iterate (Source  : in Builder) is
   begin
      if Source.First.Last > 0 then
         Process (Source.First.Content (1 .. Source.First.Last));
         declare
            B : Block_Access := Source.First.Next_Block;
         begin
            while B /= null loop
               Process (B.Content (1 .. B.Last));
               B := B.Next_Block;
            end loop;
         end;
      end if;
   end Inline_Iterate;

   procedure Inline_Update (Source  : in out Builder) is
   begin
      if Source.First.Last > 0 then
         Process (Source.First.Content (1 .. Source.First.Last));
         declare
            B : Block_Access := Source.First.Next_Block;
         begin
            while B /= null loop
               Process (B.Content (1 .. B.Last));
               B := B.Next_Block;
            end loop;
         end;
      end if;
   end Inline_Update;

   --  ------------------------------
   --  Get the buffer content as an array.
   --  ------------------------------
   function To_Array (Source : in Builder) return Input is
      Result : Input (1 .. Source.Length);
   begin
      if Source.First.Last > 0 then
         declare
            Pos : Positive := Source.First.Last;
            B   : Block_Access := Source.First.Next_Block;
         begin
            Result (1 .. Pos) := Source.First.Content (1 .. Pos);
            while B /= null loop
               Result (Pos + 1 .. Pos + B.Last) := B.Content (1 .. B.Last);
               Pos := Pos + B.Last;
               B   := B.Next_Block;
            end loop;
         end;
      end if;
      return Result;
   end To_Array;

   --  ------------------------------
   --  Call the <tt>Process</tt> procedure with the full buffer content, trying to avoid
   --  secondary stack copies as much as possible.
   --  ------------------------------
   procedure Get (Source : in Builder) is
   begin
      if Source.Length < Source.First.Len then
         Process (Source.First.Content (1 .. Source.Length));
      else
         declare
            Content : constant Input := To_Array (Source);
         begin
            Process (Content);
         end;
      end if;
   end Get;

   --  ------------------------------
   --  Setup the builder.
   --  ------------------------------
   overriding
   procedure Initialize (Source : in out Builder) is
   begin
      Source.Current := Source.First'Unchecked_Access;
   end Initialize;

   --  ------------------------------
   --  Finalize the builder releasing the storage.
   --  ------------------------------
   overriding
   procedure Finalize (Source : in out Builder) is
   begin
      Clear (Source);
   end Finalize;

   function Count_Occurence (Buffer : in Buffer_Access;
                             From   : in Positive;
                             Item   : in Wiki.Strings.WChar) return Natural is
      Count   : Natural := 0;
      Current : Buffer_Access := Buffer;
      Pos     : Positive := From;
   begin
      while Current /= null loop
         declare
            First : constant Positive := Pos;
            Last  : constant Natural := Current.Last;
         begin
            while Pos <= Last and then Current.Content (Pos) = Item loop
               Pos := Pos + 1;
            end loop;
            if Pos > First then
               Count := Count + Pos - First;
            end if;
            exit when Pos <= Last;
         end;
         Current := Current.Next_Block;
         Pos := 1;
      end loop;
      return Count;
   end Count_Occurence;

   procedure Count_Occurence (From   : in out Cursor;
                              Item   : in Wiki.Strings.WChar;
                              Count  : out Natural) is
   begin
      Count := 0;
      while Is_Valid (From) and then Char_At (From) = Item loop
         Count := Count + 1;
         Next (From);
      end loop;
   end Count_Occurence;

   --  ------------------------------
   --  Skip spaces and tabs starting at the given position in the buffer
   --  and return the number of spaces skipped.
   --  ------------------------------
   procedure Skip_Spaces (Buffer : in out Buffer_Access;
                          From   : in out Positive;
                          Count  : out Natural) is
      Block : Wiki.Buffers.Buffer_Access := Buffer;
      Pos   : Positive := From;
   begin
      Count := 0;
      Main_Loop :
      while Block /= null loop
         declare
            Last : constant Natural := Block.Last;
         begin
            while Pos <= Last and then Helpers.Is_Space_Or_Newline (Block.Content (Pos)) loop
               Pos := Pos + 1;
               Count := Count + 1;
            end loop;
            exit Main_Loop when Pos <= Last;
         end;
         Block := Block.Next_Block;
         Pos := 1;
      end loop Main_Loop;
      Buffer := Block;
      From := Pos;
   end Skip_Spaces;

   procedure Skip_Spaces (From   : in out Cursor;
                          Count  : out Natural) is
   begin
      Count := 0;
      Main_Loop :
      while From.Block /= null loop
         declare
            Last : constant Natural := From.Block.Last;
         begin
            while From.Pos <= Last
              and then Helpers.Is_Space_Or_Newline (Char_At (From))
            loop
               From.Pos := From.Pos + 1;
               Count := Count + 1;
            end loop;
            exit Main_Loop when From.Pos <= Last;
         end;
         From.Block := From.Block.Next_Block;
         From.Pos := 1;
      end loop Main_Loop;
   end Skip_Spaces;

   procedure Skip_Ascii_Spaces (From   : in out Cursor;
                                Count  : out Natural) is
   begin
      Count := 0;
      while Is_Valid (From) and then Is_Space_Or_Tab (From) loop
         Count := Count + 1;
         Next (From);
      end loop;
   end Skip_Ascii_Spaces;

   procedure Skip_Spaces (Buffer      : in out Buffer_Access;
                          From        : in out Positive;
                          Space_Count : out Natural;
                          Line_Count  : out Natural) is
      Block : Wiki.Buffers.Buffer_Access := Buffer;
      Pos   : Positive := From;
   begin
      Space_Count := 0;
      Line_Count := 0;
      Main_Loop :
      while Block /= null loop
         declare
            Last : constant Natural := Block.Last;
            C    : Wiki.Strings.WChar;
         begin
            while Pos <= Last loop
               C := Block.Content (Pos);
               if C in Helpers.LF | Helpers.CR then
                  Line_Count := Line_Count + 1;
               else
                  exit Main_Loop when not Helpers.Is_Space (C);
               end if;
               Pos := Pos + 1;
               Space_Count := Space_Count + 1;
            end loop;
         end;
         Block := Block.Next_Block;
         Pos := 1;
      end loop Main_Loop;
      Buffer := Block;
      From := Pos;
   end Skip_Spaces;

   procedure Skip_Spaces (From        : in out Cursor;
                          Space_Count : out Natural;
                          Line_Count  : out Natural) is
   begin
      Space_Count := 0;
      Line_Count := 0;
      while Is_Valid (From) loop
         declare
            C : constant Wiki.Strings.WChar := Char_At (From);
         begin
            if C in Helpers.LF | Helpers.CR then
               Line_Count := Line_Count + 1;
            else
               exit when not Helpers.Is_Space (C);
            end if;
            Space_Count := Space_Count + 1;
            Next (From);
         end;
      end loop;
      Main_Loop :
      while From.Block /= null loop
         declare
            Last : constant Natural := From.Block.Last;
            C    : Wiki.Strings.WChar;
         begin
            while From.Pos <= Last loop
               C := Char_At (From);
               if C in Helpers.LF | Helpers.CR then
                  Line_Count := Line_Count + 1;
               else
                  exit Main_Loop when not Helpers.Is_Space (C);
               end if;
               From.Pos := From.Pos + 1;
               Space_Count := Space_Count + 1;
            end loop;
         end;
         From.Block := From.Block.Next_Block;
         From.Pos := 1;
      end loop Main_Loop;
   end Skip_Spaces;

   --  ------------------------------
   --  Skip one optional space or tab.
   --  ------------------------------
   procedure Skip_Optional_Space (Buffer : in out Buffer_Access;
                                  From   : in out Positive) is
   begin
      if Buffer /= null and then From <= Buffer.Last
        and then Wiki.Helpers.Is_Space (Buffer.Content (From))
      then
         Next (Buffer, From);
      end if;
   end Skip_Optional_Space;

   procedure Find (Buffer : in out Buffer_Access;
                   From   : in out Positive;
                   Item   : in Wiki.Strings.WChar) is
      Pos : Positive := From;
   begin
      while Buffer /= null loop
         declare
            Last : constant Natural := Buffer.Last;
         begin
            while Pos <= Last loop
               if Buffer.Content (Pos) = Item then
                  From := Pos;
                  return;
               end if;
               Pos := Pos + 1;
            end loop;
         end;
         Buffer := Buffer.Next_Block;
         Pos := 1;
      end loop;
   end Find;

end Wiki.Buffers;
