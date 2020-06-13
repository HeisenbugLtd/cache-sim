--------------------------------------------------------------------------------
-- Copyright (C) 2012-2020 by Heisenbug Ltd.
--------------------------------------------------------------------------------

package body Caches is

   -----------------------------------------------------------------------------
   --  Info
   -----------------------------------------------------------------------------
   function Info (This : in Cache'Class) return Event_Info is
   begin
      return This.Events;
   end Info;

   -----------------------------------------------------------------------------
   --  Info
   -----------------------------------------------------------------------------
   function Info (This : in Cache'Class) return Configuration is
   begin
      return This.Config;
   end Info;

end Caches;
