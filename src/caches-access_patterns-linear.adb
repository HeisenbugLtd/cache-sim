--------------------------------------------------------------------------------
-- Copyright (C) 2012-2020 by Heisenbug Ltd.
--------------------------------------------------------------------------------

package body Caches.Access_Patterns.Linear is

   -----------------------------------------------------------------------------
   --  Create
   -----------------------------------------------------------------------------
   function Create (Length : in Bytes;
                    Step   : in Bytes := 1) return Linear_Pattern is
   begin
      return Linear_Pattern'(Length        => Length,
                             Count         => 0,
                             Start_Address => 0,
                             Step          => Address (Step));
   end Create;

   -----------------------------------------------------------------------------
   --  Next
   -----------------------------------------------------------------------------
   function Next (This_Ptr : access Linear_Pattern) return Address is
      This : Linear_Pattern renames This_Ptr.all;
   begin
      This.Count := This.Count + 1;

      return This.Start_Address + Address (This.Count - 1) * This.Step;
   end Next;

end Caches.Access_Patterns.Linear;
