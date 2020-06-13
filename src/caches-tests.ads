------------------------------------------------------------------------------
--  Copyright (C) 2012-2020 by Heisenbug Ltd.
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
--  Cache testing subroutines.
--------------------------------------------------------------------------------

package Caches.Tests is

   pragma Preelaborate;

   type Result_Array is array (Bytes range <>) of Float;

   -----------------------------------------------------------------------------
   --  Linear
   --
   --  Performs a test with a linear access pattern. Starting address is zero,
   --  Length is the number of accesses to be performed, Iterations is the
   --  number of iterations of the inner access loop.
   --  The step width range of the accesses can be controlled by the indices of
   --  the Result array.
   --  If Warmup is True, a first dry run will be performed before actually
   --  collecting performance data. This nulls out the effect of the first run
   --  with a cold cache and may greatly enhance test performance by reducing
   --  the number of necessary rounds required to collect meaningful average
   --  case performance data.
   --  The collected performance data for each step-width is returned in the
   --  Result array.
   -----------------------------------------------------------------------------
   procedure Linear (The_Cache  : in out Cache'Class;
                     Length     : in     Bytes;
                     Iterations : in     Positive;
                     Warmup     : in     Boolean;
                     Result     :    out Result_Array);

end Caches.Tests;
