------------------------------------------------------------------------------
--  Copyright (C) 2012-2020 by Heisenbug Ltd.
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--  Provides the implementation of a simple cache.
--
--  All abstract methods from the Cache class are provided, plus a Connect
--  method to connect a cache to a secondary cache, so that a cache-hierarchy
--  can be built and properly simulated.
--
--  Fixed cache parameters like associativity, length of a cache line and the
--  number of available blocks are provided as discriminants.
--  The private methods Fetch_Line as well as address translation services
--  (Get_Block, To_Address) are declared and defined.
--
--  Assumptions made in the implementation:
--    Cache-line lengths are not expected to decrease with several levels
--    (generally spoken, a block of a lower-order cache should always be
--    contained within a block of the higher-order cache).
--    For instance, if the first level has a 64-byte granularity, and the second
--    level has a 32-byte cache-line, it would always require two (much slower)
--    accesses to load the whole cache-line of the first level. Likewise,
--    flushing a cache-line would require two distinct write operations to the
--    higher-order cache.
--    Doing such stupid thing does not make sense in the real world, so I see no
--    point in supporting it in this crappy simulation. The assumption is made
--    that a read or write access always operates on whole cache-lines
--    throughout the hierarchy.
--
--  Cache behaviour:
--    This simple cache simulates a cache similar to those found in any modern
--    micro-processor, yet there are some noteworthy differences:
--
--    1) The strategy to flush cache-lines is to select a random line from the
--       available slots, most real-world caches employ a least-recently-used
--       strategy. So major differences between the simulated and an actual
--       performance are expected if certain pathological access pattern (like
--       forward/backward runs) are expected.
--       If such pathological cases are to be simulated, it might be better to
--       derive from this class and do a proper implementation for the
--       (protected) method Fetch_Line.
--    2) The size of a memory access is not taken into account.
--       The cache behaves as if a single byte is requested each time.
--       In the real-word, a memory access could span across cache-lines, which
--       is not simulated here. However, it can be simulated by checking if an
--       unaligned (or large) access is attempted and then doing consecutive
--       reads accordingly.
--       This is how most processors handle unaligned accesses and as this is a
--       cache-, not a processor-simulation, you have to do it on your own if
--       you need it.
--
--  Performance Parameters:
--    The speed differences between multiple layers of caches can be influenced
--    by the Speed_Factor parameter in the Connect method.
--    The default is 8.0, as usual access time differences are in the ordner of
--    a magnitude.
--    To accomodate for the longer time when a cache line is loaded or flushed,
--    such an event is multiplied with the number of bytes in the cache-line
--    before adding it to the performance index. This might not be very precise,
--    because such loads are probably more direct (I'd expect that a cache-line
--    load is a single operation between caches), and writes are often buffered.
--    Yet, considering the much slower speed of the higher-order cache connected
--    it is probably still a good approximation. If all else fails, you can
--    still override the Perf_Index method to implement your own weighting of
--    the cache events.
--------------------------------------------------------------------------------

package Caches.Simple_Caches is

   --  The basic cache class.
   type Simple_Cache (Cache_Line  : Line_Length;
                      Association : Associativity;
                      Num_Blocks  : Full_Size;
                      Level_Id    : Positive) is new Cache with private;
   type Simple_Cache_Ptr is access all Simple_Cache'Class;

   --  Overridden primitive operations.

   -----------------------------------------------------------------------------
   --  Initialize
   --
   --  Initializes the given cache. Result is a cold, empty cache.
   -----------------------------------------------------------------------------
   procedure Initialize (This : in out Simple_Cache);

   -----------------------------------------------------------------------------
   --  Flush
   --
   --  Flushes the given cache. Collected performance data is not erased. If
   --  Recursive is True, any connected sub-level cache will be flushed, too.
   -----------------------------------------------------------------------------
   procedure Flush (This      : in out Simple_Cache;
                    Recursive : in     Boolean := True);

   -----------------------------------------------------------------------------
   --  Reset
   --
   --  Resets the performance data of the cache. It has no effect on the current
   --  cache's state. If Recursive is True, applies to any connected sub-level
   --  cache, too.
   -----------------------------------------------------------------------------
   procedure Reset (This      : in out Simple_Cache;
                    Recursive : in     Boolean := True);

   -----------------------------------------------------------------------------
   --  Read
   --
   --  Simulates a read access to the given address.
   -----------------------------------------------------------------------------
   procedure Read (This  : in out Simple_Cache;
                   Where : in     Address);

   -----------------------------------------------------------------------------
   --  Write
   --
   --  Simulates a write access to the given address.
   -----------------------------------------------------------------------------
   procedure Write (This  : in out Simple_Cache;
                    Where : in     Address);

   -----------------------------------------------------------------------------
   --  Perf_Index
   --
   --  Returns a float value representing the performance of the cache This. If
   --  Recursive is True, any connected cache's performance value is added to
   --  the performance value after correcting it via the appropriate speed
   --  factor.
   -----------------------------------------------------------------------------
   function Perf_Index (This      : in Simple_Cache;
                        Recursive : in Boolean := True) return Long_Float;

   --  New primitive (inheritable) operations.

   -----------------------------------------------------------------------------
   --  Connect
   --
   --  Connects the given cache Next as next stage to the This one.
   --  Speed_Factor is a multiplicator to be used when calculating cache
   --  hierarchy performance.
   -----------------------------------------------------------------------------
   procedure Connect (This         : in out Simple_Cache;
                      Next         : access Cache'Class;
                      Speed_Factor : in     Long_Float := 8.0);

private

   --  Cache lines can be invalid (unused, empty), full (i.e. loaded),
   --  and dirty (i.e. full, but written to it).
   type Line_State is (Invalid, Full, Dirty);

   --  Cache lines also store a tag indicating the upper bits of the cached
   --  address.
   type Cache_Line_Info is
      record
         State : Line_State;
         Tag   : Address;
      end record;

   --  Cache line state information for each line and # of associations.
   type Cache_State is array (Full_Size     range <>,
                              Associativity range <>) of Cache_Line_Info;

   --  The cache class.
   type Simple_Cache (Cache_Line  : Line_Length;
                      Association : Associativity;
                      Num_Blocks  : Full_Size;
                      Level_Id    : Positive) is new Cache with
      record
         Next_Level : Cache_Ptr;
         State      : Cache_State (1 .. Num_Blocks, 1 .. Association);
      end record;

   --  Primitive operations with "protected" visibility.

   -----------------------------------------------------------------------------
   --  Get_Block
   --
   --  Returns block an tag number for given address with the current cache's
   --  configuration.
   -----------------------------------------------------------------------------
   procedure Get_Block (This  : in     Simple_Cache;
                        Where : in     Address;
                        Block :    out Unsigned;
                        Tag   :    out Address);

   -----------------------------------------------------------------------------
   --  To_Address
   --
   --  Reverse function of Get_Block. Return the address associated with given
   --  tag and block.
   -----------------------------------------------------------------------------
   function To_Address (This  : in Simple_Cache;
                        Tag   : in Address;
                        Block : in Unsigned) return Address;

   -----------------------------------------------------------------------------
   --  Fetch_Line
   --
   --  Simulate fetching a new cache line in the block.
   --  State is the state of the cache line after being fetched.
   --  Block and Tag indicate the address for the line to be fetched.
   -----------------------------------------------------------------------------
   procedure Fetch_Line (This  : in out Simple_Cache;
                         Block : in     Unsigned;
                         Tag   : in     Address;
                         State : in     Line_State);

end Caches.Simple_Caches;
