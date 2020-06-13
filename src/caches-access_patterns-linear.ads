------------------------------------------------------------------------------
--  Copyright (C) 2012-2020 by Heisenbug Ltd.
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--  Implements a linear (ascending order) access pattern.
--------------------------------------------------------------------------------

package Caches.Access_Patterns.Linear is

   pragma Preelaborate;

   type Linear_Pattern is new Pattern with private;

   -----------------------------------------------------------------------------
   --  Create
   --
   --  Creates a new linear access pattern with given step width.
   -----------------------------------------------------------------------------
   function Create (Length : in Bytes;
                    Step   : in Bytes := 1) return Linear_Pattern;

   -----------------------------------------------------------------------------
   --  Next
   --
   --  Returns the next address for this access pattern.
   -----------------------------------------------------------------------------
   function Next (This_Ptr : access Linear_Pattern) return Address;

private

   type Linear_Pattern is new Pattern with
      record
         Step : Address;
      end record;

end Caches.Access_Patterns.Linear;
