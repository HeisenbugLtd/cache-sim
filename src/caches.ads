--------------------------------------------------------------------------------
--
--  Caches provides basic types for the implementation of simulating cache
--  behaviour in computer systems.
--
--  Here, the basic data types are declared.
--
--  An abstract class is introduced which provides the interface to the usual
--  operations expected for a cache (initialization, read, write, flush) plus
--  some house-keeping methods (performance counter reset, information
--  extraction and provision of an overall performance indicator).
--
--------------------------------------------------------------------------------
-- Copyright (C) 2012-2020 by Heisenbug Ltd.
--------------------------------------------------------------------------------

package Caches is

   pragma Preelaborate;

   --  Provide some basic constants.
   KI_BYTE : constant := 2 ** 10;
   MI_BYTE : constant := 2 ** 20;
   GI_BYTE : constant := 2 ** 30;

   --  Basic types representing addresses and indices.
   type Unsigned is range 0 .. 2 ** 32 - 1;
   type Address is new Unsigned;

   --  Counter for several purposes.
   type Count is new Unsigned;

   --  Representation of non-null bit lengths and their respective ranges.
   subtype Bit_Number is Natural  range 0 .. 32;
   subtype Bytes      is Unsigned range
     2 ** Bit_Number'First .. 2 ** Bit_Number'Last - 1;

   --  Subtypes for maximum supported cache line lengths, associativity, and
   --  cache sizes.
   subtype Line_Length   is Bytes range Bytes'First .. 1 * KI_BYTE;
   subtype Associativity is Bytes range Bytes'First .. 1 * KI_BYTE;
   subtype Full_Size     is Bytes range Bytes'First .. 2 * GI_BYTE;

   --  Info about the dynamic behaviour of a specific cache (cache event
   --  counter).
   type Event_Info is
      record
         Reads         : Count;
         Writes        : Count;
         Lines_Fetched : Count;
         Lines_Flushed : Count;
         Hits          : Count;
         Misses        : Count;
      end record;

   --  Info about the static properties (configuration) of a cache.
   type Configuration is
      record
         Cache_Line   : Line_Length;   --  Length of a single cache line.
         Association  : Associativity; --  Number of entries per slot.
         Num_Blocks   : Full_Size;     --  # of cache block.
         Cache_Size   : Full_Size;     --  Cache size.
         Speed_Factor : Long_Float;    --  Speed factor compared to next level.
      end record;

   type Cache is abstract tagged limited private;
   type Cache_Ptr is access all Cache'Class;

   --  Primitive operation of all sort of caches.

   -----------------------------------------------------------------------------
   --  Initialize
   --
   --  Initializes the given cache. Result is a cold, empty cache.
   -----------------------------------------------------------------------------
   procedure Initialize (This : in out Cache) is abstract;

   -----------------------------------------------------------------------------
   --  Flush
   --
   --  Flushes the given cache. Collected performance data is not erased. If
   --  Recursive is True, any connected sub-level cache will be flushed, too.
   -----------------------------------------------------------------------------
   procedure Flush (This      : in out Cache;
                    Recursive : in     Boolean := True) is abstract;

   -----------------------------------------------------------------------------
   --  Reset
   --
   --  Resets the performance data of the cache. It has no effect on the current
   --  cache's state. If Recursive is True, applies to any connected sub-level
   --  cache, too.
   -----------------------------------------------------------------------------
   procedure Reset (This      : in out Cache;
                    Recursive : in     Boolean := True) is abstract;

   -----------------------------------------------------------------------------
   --  Read
   --
   --  Simulates a read access to the given address.
   -----------------------------------------------------------------------------
   procedure Read (This  : in out Cache;
                   Where : in     Address) is abstract;

   -----------------------------------------------------------------------------
   --  Write
   --
   --  Simulates a write access to the given address.
   -----------------------------------------------------------------------------
   procedure Write (This  : in out Cache;
                    Where : in     Address) is abstract;

   -----------------------------------------------------------------------------
   --  Perf_Index
   --
   --  Returns a float value representing the performance of the cache This. If
   --  Recursive is True, any connected cache's performance value is added to
   --  the performance value after correcting it via the appropriate speed
   --  factor.
   -----------------------------------------------------------------------------
   function Perf_Index
     (This      : in Cache;
      Recursive : in Boolean := True) return Long_Float is abstract;

   --  Class wide operations

   -----------------------------------------------------------------------------
   --  Info
   --
   --  Returns the dynamic performance info record of the given cache.
   -----------------------------------------------------------------------------
   function Info (This : in Cache'Class) return Event_Info;

   -----------------------------------------------------------------------------
   --  Info
   --
   --  Returns the static configuration info record of the given cache.
   -----------------------------------------------------------------------------
   function Info (This : in Cache'Class) return Configuration;

private

   type Cache is abstract tagged limited
      record
         Config : Configuration;
         Events : Event_Info;
      end record;

end Caches;
