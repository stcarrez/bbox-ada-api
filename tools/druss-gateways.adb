-----------------------------------------------------------------------
--  druss-gateways -- Gateway management
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Util.Properties.Basic;
with Util.Strings;
with Bbox.API;
package body Druss.Gateways is

   use Ada.Strings.Unbounded;

   protected body Gateway_State is
      function Get_State return State_Type is
      begin
         return State;
      end Get_State;

   end Gateway_State;

   function "=" (Left, Right : in Gateway_Ref) return Boolean is
   begin
      return False;
   end "=";

   --  ------------------------------
   --  Initalize the list of gateways from the property list.
   --  ------------------------------
   procedure Initialize (Config : in Util.Properties.Manager;
                         List   : in out Gateway_Vector) is
      Count : constant Natural := Util.Properties.Basic.Integer_Property.Get (Config, "druss.bbox.count", 0);
   begin
      for I in 1 .. Count loop
         declare
            Gw   : Gateway_Ref := Gateway_Refs.Create;
            Base : constant String := "druss.bbox." & Util.Strings.Image (I);
         begin
            Gw.Value.Ip := Config.Get (Base & ".ip");
            Gw.Value.Images := Config.Get (Base & ".images");
            Gw.Value.Passwd := Config.Get (Base & ".password");
            List.Append (Gw);

         exception
            when Util.Properties.NO_PROPERTY =>
               null;
         end;
      end loop;
   end Initialize;

   --  ------------------------------
   --  Refresh the information by using the Bbox API.
   --  ------------------------------
   procedure Refresh (Gateway : in out Gateway_Type) is
      Box     : Bbox.API.Client_Type;
   begin
      if Gateway.State.Get_State = BUSY then
         return;
      end if;
      Box.Set_Server (To_String (Gateway.IP));
      Box.Login (To_String (Gateway.Passwd));
      Box.Get ("wan/ip", Gateway.Wan);
      Box.Get ("lan/ip", Gateway.Lan);
      Box.Get ("device", Gateway.Device);
      Box.Get ("wireless", Gateway.Wifi);
   end Refresh;

   --  ------------------------------
   --  Refresh the information by using the Bbox API.
   --  ------------------------------
   procedure Refresh (Gateway : in Gateway_Ref) is
   begin
      Gateway.Value.Refresh;
   end Refresh;

   --  ------------------------------
   --  Iterate over the list of gateways and execute the <tt>Process</tt> procedure.
   --  ------------------------------
   procedure Iterate (List    : in Gateway_Vector;
                      Process : not null access procedure (G : in out Gateway_Type)) is
   begin
      for G of List loop
         Process (G.Value.all);
      end loop;
   end Iterate;

end Druss.Gateways;
