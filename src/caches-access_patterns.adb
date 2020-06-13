--------------------------------------------------------------------------------
-- Copyright (C) 2012-2020 by Heisenbug Ltd.
--------------------------------------------------------------------------------

package body Caches.Access_Patterns is

   -----------------------------------------------------------------------------
   --  End_Of_Pattern
   -----------------------------------------------------------------------------
   function End_Of_Pattern (This : in Pattern) return Boolean is
   begin
      return This.Length = This.Count;
   end End_Of_Pattern;

end Caches.Access_Patterns;
