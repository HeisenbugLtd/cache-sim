------------------------------------------------------------------------------
--  Copyright (C) 2012-2020 by Heisenbug Ltd.
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
--------------------------------------------------------------------------------
pragma License (Unrestricted);

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
