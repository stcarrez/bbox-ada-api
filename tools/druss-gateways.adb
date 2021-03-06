-----------------------------------------------------------------------
--  druss-gateways -- Gateway management
--  Copyright (C) 2017, 2021 Stephane Carrez
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
with Util.Http.Clients;
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
      if Left.Is_Null then
         return Right.Is_Null;
      elsif Right.Is_Null then
         return False;
      else
         declare
            Left_Rule  : constant Gateway_Refs.Element_Accessor := Left.Value;
            Right_Rule : constant Gateway_Refs.Element_Accessor := Right.Value;
         begin
            return Left_Rule.Element = Right_Rule.Element;
         end;
      end if;
   end "=";

   package Int_Property renames Util.Properties.Basic.Integer_Property;
   package Bool_Property renames Util.Properties.Basic.Boolean_Property;

   --  ------------------------------
   --  Initalize the list of gateways from the property list.
   --  ------------------------------
   procedure Initialize (Config : in Util.Properties.Manager;
                         List   : in out Gateway_Vector) is
      Count : constant Natural := Int_Property.Get (Config, "druss.bbox.count", 0);
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
            Gw.Value.Enable := Bool_Property.Get (Config, Base & ".enable", True);
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
            Bool_Property.Set (Config, Base & ".enable", Gw.Value.Enable);

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
      Box.Set_Server (To_String (Gateway.Ip));
      if Ada.Strings.Unbounded.Length (Gateway.Passwd) > 0 then
         Box.Login (To_String (Gateway.Passwd));
      end if;
      Box.Get ("wan/ip", Gateway.Wan);
      Box.Get ("lan/ip", Gateway.Lan);
      Box.Get ("device", Gateway.Device);
      Box.Get ("wireless", Gateway.Wifi);
      Box.Get ("voip", Gateway.Voip);
      Box.Get ("iptv", Gateway.IPtv);
      Box.Get ("hosts", Gateway.Hosts);
      if Gateway.Device.Exists ("device.serialnumber") then
         Gateway.Serial := Gateway.Device.Get ("device.serialnumber");
      end if;

   exception
      when Util.Http.Clients.Connection_Error =>
         Log.Error ("Cannot connect to {0}", To_String (Gateway.Ip));

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
                      Mode    : in Iterate_Type := ITER_ALL;
                      Process : not null access procedure (G : in out Gateway_Type)) is
      Expect : constant Boolean := Mode = ITER_ENABLE;
   begin
      for G of List loop
         if Mode = ITER_ALL or else G.Value.Enable = Expect then
            Process (G.Value);
         end if;
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
         if G.Value.Ip = IP then
            return G;
         end if;
      end loop;
      Log.Debug ("No gateway with IP {0}", IP);
      return Null_Gateway;
   end Find_IP;

end Druss.Gateways;
