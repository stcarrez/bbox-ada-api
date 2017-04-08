-----------------------------------------------------------------------
--  druss-commands-devices -- Print information about the devices
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
with Util.Properties;
with Bbox.API;
with Druss.Gateways;
package body Druss.Commands.Devices is

   --  ------------------------------
   --  Execute the wifi 'status' command to print the Wifi current status.
   --  ------------------------------
   procedure Do_List (Command   : in Command_Type;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      pragma Unreferenced (Command, Args);
      procedure Box_Status (Gateway : in out Druss.Gateways.Gateway_Type);

      Console : constant Druss.Commands.Consoles.Console_Access := Context.Console;

      procedure Box_Status (Gateway : in out Druss.Gateways.Gateway_Type) is
         procedure Print_Device (Manager : in Util.Properties.Manager;
                                 Name    : in String);

         procedure Print_Device (Manager : in Util.Properties.Manager;
                                 Name    : in String) is
            Link : constant String := Manager.Get (Name & ".link", "");
            Kind : constant String := Manager.Get (Name & ".devicetype", "");
         begin
            if Manager.Get (Name & ".active", "") = "0" then
               return;
            end if;
            Console.Start_Row;
            Console.Print_Field (F_BBOX_IP_ADDR, Gateway.Ip);
            Console.Print_Field (F_IP_ADDR, Manager.Get (Name & ".ipaddress", ""));
            Console.Print_Field (F_ETHERNET, Manager.Get (Name & ".macaddress", ""));
            Console.Print_Field (F_HOSTNAME, Manager.Get (Name & ".hostname", ""));
            Console.Print_Field (F_ACTIVE, Manager.Get (Name & ".active", ""));
            Console.Print_Field (F_DEVTYPE, (if Kind = "STB" then "STB" else ""));
            if Link = "Ethernet" then
               Console.Print_Field (F_LINK, Link & " port "
                                    & Manager.Get (Name & ".ethernet.logicalport", ""));
            else
               Console.Print_Field (F_LINK, Link & " RSSI "
                                    & Manager.Get (Name & ".wireless.rssi0", ""));
            end if;
            Console.End_Row;
         end Print_Device;

      begin
         Gateway.Refresh;

         Bbox.API.Iterate (Gateway.Hosts, "hosts.list", Print_Device'Access);
      end Box_Status;

   begin
      Console.Start_Title;
      Console.Print_Title (F_BBOX_IP_ADDR, "Bbox IP", 16);
      Console.Print_Title (F_IP_ADDR, "Device IP", 16);
      Console.Print_Title (F_ETHERNET, "Ethernet", 20);
      Console.Print_Title (F_HOSTNAME, "Hostname", 28);
      Console.Print_Title (F_DEVTYPE, "Type", 6);
      --  Console.Print_Title (F_ACTIVE, "Active", 8);
      Console.Print_Title (F_LINK, "Link", 18);
      Console.End_Title;
      Druss.Gateways.Iterate (Context.Gateways, Gateways.ITER_ENABLE, Box_Status'Access);
   end Do_List;

   --  ------------------------------
   --  Execute a status command to report information about the Bbox.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      pragma Unreferenced (Name);
   begin
      Command.Do_List (Args, Context);
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Command   : in Command_Type;
                   Context   : in out Context_Type) is
      pragma Unreferenced (Command);
      Console : constant Druss.Commands.Consoles.Console_Access := Context.Console;
   begin
      Console.Notice (N_HELP, "devices: Print information about the devices");
      Console.Notice (N_HELP, "Usage: devices [options]");
      Console.Notice (N_HELP, "");
   end Help;

end Druss.Commands.Devices;
