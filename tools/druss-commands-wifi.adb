-----------------------------------------------------------------------
--  druss-commands-wifi -- Wifi related commands
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
package body Druss.Commands.Wifi is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   procedure Wifi_Status (Gateway : in out Druss.Gateways.Gateway_Type) is
   begin
      Gateway.Refresh;
      Put (To_String (Gateway.Ip));
      Set_Col (30);
      Put (Gateway.Wifi.Get ("wireless.radio.24.enable", " "));
      Set_Col (33);
      Put (Gateway.Wifi.Get ("wireless.radio.24.current_channel", " "));
      Set_Col (37);
      Put (Gateway.Wifi.Get ("wireless.ssid.24.id", " "));
      Set_Col (60);
      Put (Gateway.Wifi.Get ("wireless.ssid.24.security.protocol", " "));
      Set_Col (70);
      Put (Gateway.Wifi.Get ("wireless.ssid.24.security.encryption", " "));
      New_Line;
      --  Set_Col (70);
      if Gateway.Wifi.Exists ("wireless.radio.5.enable") then
         Put (To_String (Gateway.Ip));
         Set_Col (30);
         Put (Gateway.Wifi.Get ("wireless.radio.5.enable", " "));
         Set_Col (33);
         Put (Gateway.Wifi.Get ("wireless.radio.5.current_channel", " "));
         Set_Col (37);
         Put (Gateway.Wifi.Get ("wireless.ssid.5.id", " "));
         Set_Col (60);
         Put (Gateway.Wifi.Get ("wireless.ssid.24.security.protocol", " "));
         Set_Col (70);
         Put (Gateway.Wifi.Get ("wireless.ssid.24.security.encryption", " "));
         New_Line;
      end if;
   end Wifi_Status;

   --  ------------------------------
   --  Execute a command to control or get status about the Wifi.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
   begin
      Druss.Gateways.Iterate (Context.Gateways, Wifi_Status'Access);
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Command   : in Command_Type;
                   Context   : in out Context_Type) is
   begin
      Put_Line ("wifi: Control and get status about the Bbox Wifi");
      Put_Line ("Usage: wifi {<action>} [<parameters>]");
      New_Line;
      Put_Line ("  wifi on                 Turn ON the wifi on the Bbox.");
      Put_Line ("  wifi off                Turn OFF the wifi on the Bbox.");
      Put_Line ("  wifi show               Show information about the wifi on the Bbox.");
      Put_Line ("  wifi devices            Show the wifi devices which are connected.");
   end Help;

end Druss.Commands.Wifi;
