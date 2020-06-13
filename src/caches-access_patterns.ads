--------------------------------------------------------------------------------
--  Abstract class providing an iterator-like interface to implement different
--  access patterns.
--------------------------------------------------------------------------------
-- Copyright (C) 2012-2020 by Heisenbug Ltd.
--------------------------------------------------------------------------------

package Caches.Access_Patterns is

   pragma Preelaborate;

   --  The pattern class.
   type Pattern is abstract tagged private;

   -----------------------------------------------------------------------------
   --  End_Of_Pattern
   --
   --  Returns True if the end of the pattern is reached. Can be overridden by
   --  derived types if more complex pattern require a more sophisticated
   --  end-of-pattern detection than a simple comparison with the count.
   -----------------------------------------------------------------------------
   function End_Of_Pattern (This : in Pattern) return Boolean;

   -----------------------------------------------------------------------------
   --  Next
   --
   --  Provides the next address of the access pattern.
   -----------------------------------------------------------------------------
   function Next (This_Ptr : access Pattern) return Address is abstract;

private

   --  The pattern class.
   type Pattern is abstract tagged
      record
         Length        : Bytes;
         Count         : Unsigned;
         Start_Address : Address;
      end record;

end Caches.Access_Patterns;
