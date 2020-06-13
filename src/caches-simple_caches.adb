--------------------------------------------------------------------------------
-- Copyright (C) 2012-2020 by Heisenbug Ltd.
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Numerics.Float_Random;

package body Caches.Simple_Caches is

   Debug : constant Boolean := False;

   Random : Ada.Numerics.Float_Random.Generator;

   -----------------------------------------------------------------------------
   --  Connect
   -----------------------------------------------------------------------------
   procedure Connect (This         : in out Simple_Cache;
                      Next         : access Cache'Class;
                      Speed_Factor : in     Long_Float := 8.0) is
   begin
      This.Next_Level          := Cache_Ptr (Next);
      This.Config.Speed_Factor := Speed_Factor;
   end Connect;

   -----------------------------------------------------------------------------
   --  Fetch_Line
   -----------------------------------------------------------------------------
   procedure Fetch_Line (This  : in out Simple_Cache;
                         Block : in     Unsigned;
                         Tag   : in     Address;
                         State : in     Line_State)
   is
      Slot : Unsigned;
   begin
      if Debug then
         Ada.Text_IO.Put_Line
           ("LEVEL"            & Positive'Image (This.Level_Id) &
            ": Fetching block" & Unsigned'Image (Block) &
            ", tag"            & Address'Image (Tag) &
            ".");
      end if;

      --  If a line needs to be fetched, we had a cache miss.
      This.Events.Misses        := This.Events.Misses + 1;
      This.Events.Lines_Fetched := This.Events.Lines_Fetched + 1;

      --  Find a free slot in the block and fetch the line.
      for i in This.State'Range (2) loop
         if This.State (Block, i).State = Invalid then
            Slot := i;
            goto FETCH;
         end if;
      end loop;

      --  No free slot has been found, select a random slot.
      --  (TODO: LRU scheme)
      Slot :=
        Unsigned
          (Ada.Numerics.Float_Random.Random (Gen => Random) *
             Float (This.Association - 1)) + 1;

      if Debug then
         Ada.Text_IO.Put_Line
           ("LEVEL"           & Positive'Image (This.Level_Id) &
            ": Selected slot" & Unsigned'Image (Slot) &
            ".");
      end if;

      --  If selected slot contains a dirty cache line, it must be written out
      --  first.
      if This.State (Block, Slot).State = Dirty then
         --  Flush cache line.
         This.Events.Lines_Flushed := This.Events.Lines_Flushed + 1;

         --  Propagate the flush to the next level if there is one.
         if This.Next_Level /= null then
            Write (This  => This.Next_Level.all,
                   Where => To_Address (This  => Simple_Cache'Class (This),
                                        Tag   => This.State (Block, Slot).Tag,
                                        Block => Block));
         end if;
      end if;

      <<FETCH>>
      --  Simulate getting the line from the next level if there is one.
      if This.Next_Level /= null then
         Read (This  => This.Next_Level.all,
               Where => To_Address (This  => Simple_Cache'Class (This),
                                    Tag   => Tag,
                                    Block => Block));
      end if;

      This.State (Block, Slot) := Cache_Line_Info'(State => State,
                                                   Tag   => Tag);
   end Fetch_Line;

   -----------------------------------------------------------------------------
   --  Flush
   -----------------------------------------------------------------------------
   procedure Flush (This      : in out Simple_Cache;
                    Recursive : in     Boolean := True) is
   begin
      This.State := (others => (others => Cache_Line_Info'(State => Invalid,
                                                           Tag   => 0)));

      if Recursive and then This.Next_Level /= null then
         Flush (This => This.Next_Level.all);
      end if;
   end Flush;

   -----------------------------------------------------------------------------
   --  Get_Block
   -----------------------------------------------------------------------------
   procedure Get_Block (This  : in     Simple_Cache;
                        Where : in     Address;
                        Block :    out Unsigned;
                        Tag   :    out Address)
   is
      Mem_Block : Address;
   begin
      --  Check if address is cached already. Scan the appropriate block.
      --
      --   _ _ _ _ _ _ _ _ _
      --  |_|_|_|_|_|_|_|_|_|
      --   |___| |_____| |_|
      --     |      |     |
      --     |      |     +------ cache line length
      --     |      +------------ cache block
      --     +------------------- address tag
      --
      --  Calculation:
      --    Address blocks divided by the cache-line length results in the
      --    memory block.
      Mem_Block := Where / Address (This.Cache_Line);

      --  The address tag to be stored
      Tag := Mem_Block / Address (This.Num_Blocks);

      --  The responsible cache block is the memory block modulo block size.
      Block := (Unsigned (Mem_Block) mod This.Num_Blocks) + 1;

      if Debug then
         Ada.Text_IO.Put
           ("LEVEL"               & Positive'Image (This.Level_Id) &
            ": Accessing address" & Address'Image (Where)          &
            " (block"             & Unsigned'Image (Block)         &
            ", tag"               & Address'Image (Tag)            &
            ")...");
      end if;
   end Get_Block;

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize (This : in out Simple_Cache) is
   begin
      This.Config :=
        Configuration'
          (Cache_Line   => This.Cache_Line,
           Association  => This.Association,
           Num_Blocks   => This.Num_Blocks,
           Cache_Size   => This.Cache_Line * This.Association * This.Num_Blocks,
           Speed_Factor => 1.0);

      Reset (This => Simple_Cache'Class (This));
      Flush (This => Simple_Cache'Class (This));

      This.Next_Level := null;
   end Initialize;

   -----------------------------------------------------------------------------
   --  Perf_Index
   -----------------------------------------------------------------------------
   function Perf_Index (This      : in Simple_Cache;
                        Recursive : in Boolean := True) return Long_Float
   is
      Result : Long_Float;
   begin
      Result :=
        1.0 * Long_Float (This.Events.Hits)   +
        1.0 * Long_Float (This.Events.Misses) +
        Long_Float (This.Cache_Line) * Long_Float (This.Events.Lines_Fetched) +
        Long_Float (This.Cache_Line) * Long_Float (This.Events.Lines_Flushed);

      --  Simpler calculation, probably "good enough".
      --  # hits + # misses * Some_Miss_Factor.
--        Result := (  1.0 * Long_Float (This.Events.Hits)
--                   + 8.0 * Long_Float (This.Events.Misses));

      if Recursive and then This.Next_Level /= null then
         Result :=
           (Result +
              This.Config.Speed_Factor * Perf_Index (This.Next_Level.all));
      end if;

      return Result;
   end Perf_Index;

   -----------------------------------------------------------------------------
   --  Read
   -----------------------------------------------------------------------------
   procedure Read (This  : in out Simple_Cache;
                   Where : in     Address)
   is
      Tag   : Address;
      Block : Unsigned;
   begin
      if Debug then
         Ada.Text_IO.Put_Line
           ("LEVEL"               & Positive'Image (This.Level_Id) &
            ": Read from address" & Address'Image (Where) & ".");
      end if;

      This.Events.Reads := This.Events.Reads + 1;

      --  Get block in cache where to look up for given address.
      Get_Block (This  => Simple_Cache'Class (This),
                 Where => Where,
                 Block => Block,
                 Tag   => Tag);

      --  Check if memory is loaded in one of the available slots.
      for i in This.State'Range (2) loop
         if
                    This.State (Block, i).State in Full .. Dirty
           and then This.State (Block, i).Tag = Tag
         then
            if Debug then
               Ada.Text_IO.Put_Line
                 ("HIT (slot" & Unsigned'Image (i) & ")!");
            end if;

            This.Events.Hits  := This.Events.Hits + 1;
            return;
         end if;
      end loop;

      if Debug then
         Ada.Text_IO.Put_Line ("MISS!");
      end if;

      --  Cache miss. Fetch the required cache line.
      Fetch_Line (This  => Simple_Cache'Class (This),
                  Block => Block,
                  Tag   => Tag,
                  State => Full);
   end Read;

   -----------------------------------------------------------------------------
   --  Reset
   -----------------------------------------------------------------------------
   procedure Reset (This      : in out Simple_Cache;
                    Recursive : in     Boolean := True) is
   begin
      This.Events := Event_Info'(Reads         => 0,
                                 Writes        => 0,
                                 Lines_Fetched => 0,
                                 Lines_Flushed => 0,
                                 Hits          => 0,
                                 Misses        => 0);

      if Recursive and then This.Next_Level /= null then
         Reset (This.Next_Level.all);
      end if;
   end Reset;

   -----------------------------------------------------------------------------
   --  To_Address
   -----------------------------------------------------------------------------
   function To_Address (This  : in Simple_Cache;
                        Tag   : in Address;
                        Block : in Unsigned) return Address is
   begin
      return
        Tag * Address (This.Num_Blocks) * Address (This.Cache_Line) +
        Address (Block - 1) * Address (This.Cache_Line);
   end To_Address;

   -----------------------------------------------------------------------------
   --  Write
   -----------------------------------------------------------------------------
   procedure Write (This  : in out Simple_Cache;
                    Where : in     Address)
   is
      Block : Unsigned;
      Tag   : Address;
   begin
      if Debug then
         Ada.Text_IO.Put_Line
           ("LEVEL"              & Positive'Image (This.Level_Id) &
            ": Write to address" & Address'Image (Where)          &
            ".");
      end if;

      This.Events.Writes := This.Events.Writes + 1;

      --  Get block in cache where to look up for given address.
      Get_Block (This  => Simple_Cache'Class (This),
                 Where => Where,
                 Block => Block,
                 Tag   => Tag);

      --  Check if memory is loaded in one of the available slots.
      for i in This.State'Range (2) loop
         if
                    This.State (Block, i).State in Full .. Dirty
           and then This.State (Block, i).Tag = Tag
         then
            if Debug then
               Ada.Text_IO.Put_Line
                 ("HIT (slot" & Unsigned'Image (i) & ")!");
            end if;

            --  Mark line as dirty, as we've written to it.
            This.State (Block, i).State := Dirty;
            This.Events.Hits  := This.Events.Hits + 1;
            return;
         end if;
      end loop;

      if Debug then
         Ada.Text_IO.Put_Line ("MISS!");
      end if;

      --  Cache miss. Whole line needs to be fetched.
      Fetch_Line (This  => Simple_Cache'Class (This),
                  Block => Block,
                  Tag   => Tag,
                  State => Dirty);
   end Write;

end Caches.Simple_Caches;
