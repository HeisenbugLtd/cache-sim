------------------------------------------------------------------------------
--  Copyright (C) 2012-2020 by Heisenbug Ltd.
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

--------------------------------------------------------------------------------
-- Simple test program.
--------------------------------------------------------------------------------

with Ada.Characters.Latin_1,
     Ada.Float_Text_IO,
     Ada.Text_IO;

with Caches.Simple_Caches,
     Caches.Tests;

use type Caches.Count;
use type Caches.Unsigned;

procedure Main is

   package Bytes_IO is new Ada.Text_IO.Integer_IO (Num => Caches.Bytes);

   TAB : Character renames Ada.Characters.Latin_1.HT;

   -----------------------------------------------------------------------------
   --  Put_Dynamic_Info
   -----------------------------------------------------------------------------
   procedure Put_Dynamic_Info (Info : Caches.Event_Info);

   -----------------------------------------------------------------------------
   --  Put_Dynamic_Info
   -----------------------------------------------------------------------------
   procedure Put_Dynamic_Info (Info : Caches.Event_Info) is
   begin
      Ada.Text_IO.Put
        (Caches.Count'Image (Info.Reads)         & TAB &
         Caches.Count'Image (Info.Writes)        & TAB &
         Caches.Count'Image (Info.Lines_Fetched) & TAB &
         Caches.Count'Image (Info.Lines_Flushed) & TAB &
         Caches.Count'Image (Info.Hits)          & TAB &
         Caches.Count'Image (Info.Misses)        & TAB);

      if Info.Hits + Info.Misses /= 0 then
         Ada.Float_Text_IO.Put
           (Item => 100.0 * Float (Info.Hits) / Float (Info.Hits + Info.Misses),
            Fore => 3,
            Aft  => 2,
            Exp  => 0);
         Ada.Text_IO.Put ("%");
      else
         Ada.Text_IO.Put ("N/A");
      end if;

      Ada.Text_IO.New_Line;
   end Put_Dynamic_Info;

   -----------------------------------------------------------------------------
   --  Put_Static_Info
   -----------------------------------------------------------------------------
   procedure Put_Static_Info (Info : Caches.Configuration);

   -----------------------------------------------------------------------------
   --  Put_Static_Info
   -----------------------------------------------------------------------------
   procedure Put_Static_Info (Info : Caches.Configuration) is
   begin
      Ada.Text_IO.Put_Line
        ("cache line   :" & Caches.Unsigned'Image (Info.Cache_Line));
      Ada.Text_IO.Put_Line
        ("associativity:" & Caches.Unsigned'Image (Info.Association));
      Ada.Text_IO.Put_Line
        ("blocks       :" & Caches.Unsigned'Image (Info.Num_Blocks));
      Ada.Text_IO.Put_Line
        ("size         :" & Caches.Unsigned'Image (Info.Cache_Size));
   end Put_Static_Info;

   First_Level  : aliased Caches.Simple_Caches.Simple_Cache
     (Cache_Line  =>  64,
      Association =>   8,
      Num_Blocks  =>  64,   --  32 KiBytes
      Level_Id    =>   1);

   Second_Level : aliased Caches.Simple_Caches.Simple_Cache
     (Cache_Line  =>  64,
      Association =>   8,
      Num_Blocks  => 256,  -- 128 KiBytes
      Level_Id    =>   2);

   Third_Level  : aliased Caches.Simple_Caches.Simple_Cache
     (Cache_Line  =>  64,
      Association =>  32,
      Num_Blocks  => 512,  --  2 MiBytes
      Level_Id    =>   3);

   Cache_Hierarchy : constant array (Positive range <>) of
     Caches.Simple_Caches.Simple_Cache_Ptr :=
       (1 => First_Level'Unchecked_Access,
        2 => Second_Level'Unchecked_Access,
        3 => Third_Level'Unchecked_Access);
begin
   for Cache in Cache_Hierarchy'Range loop
      Caches.Simple_Caches.Initialize
        (This => Cache_Hierarchy (Cache).all);
   end loop;

   for Cache in Cache_Hierarchy'First .. Cache_Hierarchy'Last - 1 loop
      Caches.Simple_Caches.Connect
        (This         => Cache_Hierarchy (Cache).all,
         Next         => Cache_Hierarchy (Cache + 1),
         Speed_Factor => 2.0);
   end loop;

   for Cache in Cache_Hierarchy'Range loop
      Ada.Text_IO.Put_Line ("LEVEL" & Integer'Image (Cache));
      Put_Static_Info (Caches.Info (This => Cache_Hierarchy (Cache).all));
      Ada.Text_IO.New_Line;
   end loop;

   declare
      First : Caches.Simple_Caches.Simple_Cache'Class renames
                Cache_Hierarchy (Cache_Hierarchy'First).all;

      subtype Result_Array is Caches.Tests.Result_Array (1 .. 256);

      type Test_Def is
         record
            Length : Caches.Bytes;
            Result : Result_Array;
         end record;

      Result : array (Caches.Bytes range <>) of Test_Def :=
                 (1 => Test_Def'(Length => 1 * Caches.KI_BYTE,
                                 Result => Result_Array'(others => 0.0)),
                  2 => Test_Def'(Length => 4 * Caches.KI_BYTE,
                                 Result => Result_Array'(others => 0.0)),
                  3 => Test_Def'(Length => 32 * Caches.KI_BYTE,
                                 Result => Result_Array'(others => 0.0)),
                  4 => Test_Def'(Length => 128 * Caches.KI_BYTE,
                                 Result => Result_Array'(others => 0.0)));
   begin
      for i in Result'Range loop
         Bytes_IO.Put (Item  => Result (i).Length,
                       Width => 0);
         Ada.Text_IO.Put (" running...");
         Caches.Tests.Linear (The_Cache   => First,
                              Length      => Result (i).Length,
                              Iterations  => 1,
                              Warmup      => True,
                              Result      => Result (i).Result);
         Ada.Text_IO.Put_Line ("finished.");

         for Cache in Cache_Hierarchy'Range loop
            Ada.Text_IO.Put ("LEVEL" & Integer'Image (Cache));
            Put_Dynamic_Info (Caches.Info (Cache_Hierarchy (Cache).all));
         end loop;
      end loop;

      Ada.Text_IO.Put_Line ("Results:");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Steps" & TAB);

      for i in Result'Range loop
         Ada.Text_IO.Put ("Test #");
         Bytes_IO.Put (Item  => i,
                       Width => 3);

         if i /= Result'Last then
            Ada.Text_IO.Put (TAB & "");
         end if;
      end loop;

      Ada.Text_IO.New_Line;

      for Row in Result_Array'Range loop
         Bytes_IO.Put (Item => Row,
                       Width => 3);
         Ada.Text_IO.Put (TAB & "");

         for Col in Result'Range loop
            Ada.Float_Text_IO.Put (Item => Result (Col).Result (Row),
                                   Aft  => 0,
                                   Exp  => 0);

            if Col /= Result'Last then
               Ada.Text_IO.Put (TAB & "");
            end if;
         end loop;

         Ada.Text_IO.New_Line;
      end loop;
   end;
end Main;
