-----------------------------------------------------------------------
--  druss-commands-status -- Druss status commands
--  Copyright (C) 2017, 2018, 2019 Stephane Carrez
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
with Util.Properties;
with Util.Strings;
with Druss.Gateways;
package body Druss.Commands.Status is

   --  ------------------------------
   --  Execute the wifi 'status' command to print the Wifi current status.
   --  ------------------------------
   procedure Do_Status (Command   : in Command_Type;
                        Args      : in Argument_List'Class;
                        Context   : in out Context_Type) is
      pragma Unreferenced (Command, Args);
      procedure Box_Status (Gateway : in out Druss.Gateways.Gateway_Type);

      Console : constant Druss.Commands.Consoles.Console_Access := Context.Console;
      N : Natural := 0;

      procedure Box_Status (Gateway : in out Druss.Gateways.Gateway_Type) is
      begin
         Gateway.Refresh;
         Console.Start_Row;
         Console.Print_Field (F_IP_ADDR, Gateway.Ip);
         Console.Print_Field (F_WAN_IP, Gateway.Wan.Get ("wan.ip.address", " "));
         Print_Status (Console, F_INTERNET, Gateway.Wan.Get ("wan.internet.state", " "));
         Console.Print_Field (F_VOIP, Gateway.Voip.Get ("voip.0.status", " "));
         Print_On_Off (Console, F_WIFI, Gateway.Wifi.Get ("wireless.radio.24.enable", " "));
         if not Gateway.Wifi.Exists ("wireless.radio.5.enable") then
            Console.Print_Field (F_WIFI5, "");
         else
            Print_On_Off (Console, F_WIFI5, Gateway.Wifi.Get ("wireless.radio.5.enable", " "));
         end if;
         Console.Print_Field (F_ACCESS_CONTROL, Gateway.IPtv.Get ("iptv.length", "x"));
         Console.Print_Field (F_DEVICES, Gateway.Hosts.Get ("hosts.list.length", ""));
         Print_Uptime (Console, F_UPTIME, Gateway.Device.Get ("device.uptime", ""));
         Console.End_Row;
         N := N + 1;
         Gateway.Hosts.Save_Properties ("sum-" & Util.Strings.Image (N) & ".properties");
      end Box_Status;

   begin
      Console.Start_Title;
      Console.Print_Title (F_IP_ADDR, "LAN IP", 16);
      Console.Print_Title (F_WAN_IP, "WAN IP", 16);
      Console.Print_Title (F_INTERNET, "Internet", 9);
      Console.Print_Title (F_VOIP, "VoIP", 6);
      Console.Print_Title (F_WIFI, "Wifi 2.4G", 10);
      Console.Print_Title (F_WIFI5, "Wifi 5G", 10);
      Console.Print_Title (F_ACCESS_CONTROL, "Parental", 10);
      Console.Print_Title (F_DYNDNS, "DynDNS", 10);
      Console.Print_Title (F_DEVICES, "Devices", 12);
      Console.Print_Title (F_UPTIME, "Uptime", 12);
      Console.End_Title;
      Druss.Gateways.Iterate (Context.Gateways, Gateways.ITER_ENABLE, Box_Status'Access);
   end Do_Status;

   --  ------------------------------
   --  Execute a status command to report information about the Bbox.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in out Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      pragma Unreferenced (Name);
   begin
      Command.Do_Status (Args, Context);
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Command   : in out Command_Type;
                   Context   : in out Context_Type) is
      pragma Unreferenced (Command);

      Console : constant Druss.Commands.Consoles.Console_Access := Context.Console;
   begin
      Console.Notice (N_HELP, "status: give information about the Bbox status");
      Console.Notice (N_HELP, "Usage: wifi {<action>} [<parameters>]");
      Console.Notice (N_HELP, "");
      Console.Notice (N_HELP, "  status [IP]...      Give information about the Bbox status");
   end Help;

end Druss.Commands.Status;
