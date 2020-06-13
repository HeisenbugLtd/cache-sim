------------------------------------------------------------------------------
--  Copyright (C) 2012-2020 by Heisenbug Ltd.
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

package body Caches.Access_Patterns is

   -----------------------------------------------------------------------------
   --  End_Of_Pattern
   -----------------------------------------------------------------------------
   function End_Of_Pattern (This : in Pattern) return Boolean is
   begin
      return This.Length = This.Count;
   end End_Of_Pattern;

end Caches.Access_Patterns;
