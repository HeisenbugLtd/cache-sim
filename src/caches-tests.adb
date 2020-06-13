------------------------------------------------------------------------------
--  Copyright (C) 2012-2020 by Heisenbug Ltd.
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

with Caches.Access_Patterns.Linear;

package body Caches.Tests is

   -----------------------------------------------------------------------------
   --  Linear
   -----------------------------------------------------------------------------
   procedure Linear (The_Cache  : in out Cache'Class;
                     Length     : in     Bytes;
                     Iterations : in     Positive;
                     Warmup     : in     Boolean;
                     Result     :    out Result_Array) is
      Start : constant Natural := 1 - Boolean'Pos (Warmup);
   begin
      Flush (This      => The_Cache,
             Recursive => True);

      for Step in Result'Range loop
         for i in Start .. Iterations loop
            --  Throw away results of the zeroth warmup run.
            if i = 1 then
               Reset (This      => The_Cache,
                      Recursive => True);
            end if;

            declare
               A : Address;
               P : aliased Access_Patterns.Pattern'Class :=
                     Access_Patterns.Linear.Create (Length => Length,
                                                    Step   => Step * 4);
            begin
               while not Access_Patterns.End_Of_Pattern (This => P) loop
                  A := Access_Patterns.Next (This_Ptr => P'Access);

                  Read (This  => The_Cache,
                        Where => A);
                  Write (This  => The_Cache,
                         Where => A);
               end loop;
            end;
         end loop;

         Result (Step) :=
           Float (Perf_Index (This      => The_Cache,
                              Recursive => True) /
               (Long_Float (Iterations) * Long_Float (Length)));
      end loop;
   end Linear;

end Caches.Tests;
