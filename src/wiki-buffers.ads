-----------------------------------------------------------------------
--  util-texts-builders -- Text builder
--  Copyright (C) 2013, 2017, 2021, 2022, 2024, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Finalization;
with Wiki.Strings;
with Wiki.Helpers;

--  == Text Builders ==
--  The `Util.Texts.Builders` generic package was designed to provide string builders.
--  The interface was designed to reduce memory copies as much as possible.
--
--    * The `Builder` type holds a list of chunks into which texts are appended.
--    * The builder type holds an initial chunk whose capacity is defined when the builder
--      instance is declared.
--    * There is only an `Append` procedure which allows to append text to the builder.
--      This is the only time when a copy is made.
--    * The package defines the `Iterate` operation that allows to get the content
--      collected by the builder.  When using the `Iterate` operation, no copy is
--      performed since chunks data are passed passed by reference.
--    * The type is limited to forbid copies of the builder instance.
--
--  First, instantiate the package for the element type (eg, String):
--
--    package String_Builder is new Util.Texts.Builders (Character, String);
--
--  Declare the string builder instance with its initial capacity:
--
--    Builder : String_Builder.Builder (256);
--
--  And append to it:
--
--    String_Builder.Append (Builder, "Hello");
--
--  To get the content collected in the builder instance, write a procedure that receives
--  the chunk data as parameter:
--
--    procedure Collect (Item : in String) is ...
--
--  And use the `Iterate` operation:
--
--    String_Builder.Iterate (Builder, Collect'Access);
--
private package Wiki.Buffers with Preelaborate is

   Chunk_Size : constant := 256;

   type Buffer;
   type Buffer_Access is access all Buffer;

   type Buffer (Len : Positive) is limited record
      Next_Block : Buffer_Access;
      Last       : Natural := 0;
      Offset     : Natural := 0;
      Content    : Wiki.Strings.WString (1 .. Len);
   end record;

   type Builder (Len : Positive) is new Ada.Finalization.Limited_Controlled with record
      Current    : Buffer_Access;
      Block_Size : Positive := Chunk_Size;
      Length     : Natural  := 0;
      First      : aliased Buffer (Len);
   end record;
   pragma Finalize_Storage_Only (Builder);

   type Cursor is record
      Block : Buffer_Access;
      Pos   : Natural;
   end record;

   function Is_Valid (C : in Cursor) return Boolean is
     (C.Block /= null and then C.Pos <= C.Block.Last);

   function Has_Next (C : in Cursor) return Boolean is
     (C.Block /= null
      and then (C.Pos < C.Block.Last or else C.Block.Next_Block /= null));

   function Char_At (C : in Cursor) return Wiki.Strings.WChar is
      (C.Block.Content (C.Pos));

   function Next_At (C : in Cursor) return Wiki.Strings.WChar is
     (if C.Pos < C.Block.Last then C.Block.Content (C.Pos + 1)
      else C.Block.Next_Block.Content (1));

   function Is_Space_Or_Tab (C : in Cursor) return Boolean is
      (Helpers.Is_Space_Or_Tab (C.Block.Content (C.Pos)));

   --  Setup the builder.
   overriding
   procedure Initialize (Source : in out Builder);

   --  Finalize the builder releasing the storage.
   overriding
   procedure Finalize (Source : in out Builder);

   --  Move forward to skip a number of items.
   procedure Next (Content : in out Buffer_Access;
                   Pos     : in out Positive) with Inline_Always;
   procedure Next (Pos : in out Cursor) with Inline_Always;

   procedure Next (Content : in out Buffer_Access;
                   Pos     : in out Positive;
                   Count   : in Natural);

   --  Move forward and return the next character or NUL.
   function Next (Text  : in out Cursor) return Strings.WChar with Inline_Always;

   --  Get the length of the item builder.
   function Length (Source : in Builder) return Natural;

   --  Get the capacity of the builder.
   function Capacity (Source : in Builder) return Natural;

   --  Get the builder block size.
   function Block_Size (Source : in Builder) return Positive;

   --  Set the block size for the allocation of next chunks.
   procedure Set_Block_Size (Source : in out Builder;
                             Size   : in Positive);

   --  Append the <tt>New_Item</tt> at the end of the source growing the buffer if necessary.
   procedure Append (Source   : in out Builder;
                     New_Item : in Wiki.Strings.WString);

   --  Append the <tt>New_Item</tt> at the end of the source growing the buffer if necessary.
   procedure Append (Source   : in out Builder;
                     New_Item : in Wiki.Strings.WChar);

   --  Append in `Into` builder the `Content` builder starting at `From` position
   --  and the up to and including the `To` position.
   procedure Append (Into     : in out Builder;
                     Buffer   : in Buffer_Access;
                     From     : in Positive);

   --  Append in `Into` builder the `Content` builder starting at `From` position
   --  until the condition is met or the end of buffer is reached.
   generic
      with function Condition (Buffer : in Buffer_Access;
                               Pos    : in Positive) return Boolean;
   procedure Append_Until (Into   : in out Builder;
                           Buffer : in Buffer_Access;
                           From   : in Positive);

   generic
      with procedure Process (Content : in out Wiki.Strings.WString;
                              Last    : out Natural;
                              Done    : out Boolean);
   procedure Inline_Append (Source  : in out Builder);

   --  Clear the source freeing any storage allocated for the buffer.
   procedure Clear (Source : in out Builder);

   --  Truncate the current buffer at the given block and position.
   procedure Truncate (Source : in out Builder;
                       Block  : in Buffer_Access;
                       Pos    : in Positive);

   --  Iterate over the buffer content calling the <tt>Process</tt> procedure with each
   --  chunk.
   generic
      with procedure Process (Content : in Wiki.Strings.WString);
   procedure Inline_Iterate (Source  : in Builder);

   generic
      with procedure Process (Content : in out Wiki.Strings.WString);
   procedure Inline_Update (Source : in out Builder);

   --  Get the buffer content as an array.
   function To_Array (Source : in Builder) return Wiki.Strings.WString;

   --  Call the <tt>Process</tt> procedure with the full buffer content, trying to avoid
   --  secondary stack copies as much as possible.
   generic
      with procedure Process (Content : in Wiki.Strings.WString);
   procedure Get (Source : in Builder);

   function Count_Occurence (Buffer : in Buffer_Access;
                             From   : in Positive;
                             Item   : in Wiki.Strings.WChar) return Natural;

   procedure Count_Occurence (From   : in out Cursor;
                              Item   : in Wiki.Strings.WChar;
                              Count  : out Natural);

   --  Skip spaces and tabs starting at the given position in the buffer
   --  and return the number of spaces skipped.
   procedure Skip_Spaces (Buffer       : in out Buffer_Access;
                          From         : in out Positive;
                          Count        : out Natural);
   procedure Skip_Spaces (From  : in out Cursor;
                          Count : out Natural);
   procedure Skip_Spaces (Buffer       : in out Buffer_Access;
                          From         : in out Positive;
                          Space_Count  : out Natural;
                          Line_Count   : out Natural);
   procedure Skip_Spaces (From         : in out Cursor;
                          Space_Count  : out Natural;
                          Line_Count   : out Natural);

   --  Skip only ascii space and tab.
   procedure Skip_Ascii_Spaces (From         : in out Cursor;
                                Count        : out Natural);

   --  Skip one optional space or tab.
   procedure Skip_Optional_Space (Buffer : in out Buffer_Access;
                                  From   : in out Positive);

   procedure Find (Buffer : in out Buffer_Access;
                   From   : in out Positive;
                   Item   : in Wiki.Strings.WChar);

   generic
      with function Trim_Character (C : in Wiki.Strings.WChar) return Boolean;
   procedure Trim (Source : in out Builder);

end Wiki.Buffers;
