-----------------------------------------------------------------------
--  druss-commands -- Commands available for Druss
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
package body Druss.Commands.Bboxes is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

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

   --  Execute the command with the arguments.  The command name is passed with the command
   --  arguments.
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Util.Commands.Argument_List'Class;
                      Context   : in out Context_Type) is
   begin
      Druss.Gateways.Iterate (Context.Gateways, Box_Status'Access);
   end Execute;

   --  Write the help associated with the command.
   overriding
   procedure Help (Command   : in Command_Type;
                   Context   : in out Context_Type) is
   begin
      Put_Line ("bbox: Operations to control the bbox");
      Put_Line ("Usage: druss bbox list");
      Put_Line ("       druss bbox add IP");
      Put_Line ("       druss bbox del IP");
   end Help;

end Druss.Commands.Bboxes;
