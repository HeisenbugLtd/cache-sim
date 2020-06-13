------------------------------------------------------------------------------
--  Copyright (C) 2012-2020 by Heisenbug Ltd.
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--  Abstract class providing an iterator-like interface to implement different
--  access patterns.
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
