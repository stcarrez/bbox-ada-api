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
with Util.Log.Loggers;
with Bbox.API;
package body Druss.Gateways is

   use Ada.Strings.Unbounded;

   --  The logger
   Log   : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Druss.Gateways");

   protected body Gateway_State is
      function Get_State return State_Type is
      begin
         return State;
      end Get_State;

   end Gateway_State;

   function "=" (Left, Right : in Gateway_Ref) return Boolean is
   begin
      if Left.Value = Right.Value then
         return True;
      elsif Left.Is_Null or Right.Is_Null then
         return False;
      else
         return Left.Value.IP = Right.Value.IP;
      end if;
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
            Gw   : constant Gateway_Ref := Gateway_Refs.Create;
            Base : constant String := "druss.bbox." & Util.Strings.Image (I);
         begin
            Gw.Value.Ip := Config.Get (Base & ".ip");
            if Config.Exists (Base & ".images") then
               Gw.Value.Images := Config.Get (Base & ".images");
            end if;
            Gw.Value.Passwd := Config.Get (Base & ".password");
            Gw.Value.Serial := Config.Get (Base & ".serial");
            List.Append (Gw);

         exception
            when Util.Properties.NO_PROPERTY =>
               Log.Debug ("Ignoring gatway {0}", Base);
         end;
      end loop;
   end Initialize;

   --  ------------------------------
   --  Save the list of gateways.
   --  ------------------------------
   procedure Save_Gateways (Config : in out Util.Properties.Manager;
                            List   : in Druss.Gateways.Gateway_Vector) is
      Pos : Natural := 0;
   begin
      for Gw of List loop
         Pos := Pos + 1;
         declare
            Base : constant String := "druss.bbox." & Util.Strings.Image (Pos);
         begin
            Config.Set (Base & ".ip", Gw.Value.Ip);
            Config.Set (Base & ".password", Gw.Value.Passwd);
            Config.Set (Base & ".serial", Gw.Value.Serial);

         exception
            when Util.Properties.NO_PROPERTY =>
               null;
         end;
      end loop;
      Util.Properties.Basic.Integer_Property.Set (Config, "druss.bbox.count", Pos);
   end Save_Gateways;

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
      if Ada.Strings.Unbounded.Length (Gateway.Passwd) > 0 then
         Box.Login (To_String (Gateway.Passwd));
      end if;
      Box.Get ("wan/ip", Gateway.Wan);
      Box.Get ("lan/ip", Gateway.Lan);
      Box.Get ("device", Gateway.Device);
      Box.Get ("wireless", Gateway.Wifi);
      Box.Get ("voip", Gateway.Voip);
      if Gateway.Device.Exists ("device.serialnumber") then
         Gateway.Serial := Gateway.Device.Get ("device.serialnumber");
      end if;
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

   Null_Gateway : Gateway_Ref;

   --  ------------------------------
   --  Find the gateway with the given IP address.
   --  ------------------------------
   function Find_IP (List : in Gateway_Vector;
                     IP   : in String) return Gateway_Ref is
   begin
      for G of List loop
         if G.Value.IP = IP then
            return G;
         end if;
      end loop;
      --  raise Not_Found;
      return Null_Gateway;
   end Find_IP;

end Druss.Gateways;
