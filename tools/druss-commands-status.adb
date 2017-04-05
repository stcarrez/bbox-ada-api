-----------------------------------------------------------------------
--  druss-commands-status -- Druss status commands
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
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Util.Properties;
with Druss.Gateways;
package body Druss.Commands.Status is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   procedure Box_Status (Gateway : in out Druss.Gateways.Gateway_Type) is
   begin
      Gateway.Refresh;
      Put (To_String (Gateway.Ip));
      Set_Col (30);
      Put (Gateway.Wan.Get ("wan.internet.state", "?"));
      Set_Col (38);
      Put (Gateway.Wan.Get ("wan.ip.address", "?"));
      Set_Col (60);
      Put (Gateway.Device.Get ("device.numberofboots", "-"));
      Set_Col (68);
      Put (Gateway.Device.Get ("device.uptime", "-"));
      New_Line;
   end Box_Status;

   --  ------------------------------
   --  Report wan status.
   --  ------------------------------
   procedure Wan_Status (Name    : in String;
                         Args    : in Argument_List'Class;
                         Context : in out Context_Type) is
   begin
      Druss.Gateways.Iterate (Context.Gateways, Box_Status'Access);
   end Wan_Status;

end Druss.Commands.Status;
