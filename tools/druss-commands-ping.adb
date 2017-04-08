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
with Util.Log.Loggers;
with Bbox.API;
with Druss.Gateways;
with Ada.Strings.Unbounded;
package body Druss.Commands.Ping is

   use Ada.Strings.Unbounded;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Druss.Commands.Ping");

   --  ------------------------------
   --  Execute the wifi 'status' command to print the Wifi current status.
   --  ------------------------------
   procedure Do_Ping (Command   : in Command_Type;
                      Args      : in Argument_List'Class;
                      Selector  : in Device_Selector_Type;
                      Context   : in out Context_Type) is
      pragma Unreferenced (Command, Args);
      procedure Do_Ping (Gateway : in out Druss.Gateways.Gateway_Type);
      procedure Box_Status (Gateway : in out Druss.Gateways.Gateway_Type);

      Console : constant Druss.Commands.Consoles.Console_Access := Context.Console;

      procedure Do_Ping (Gateway : in out Druss.Gateways.Gateway_Type) is
         procedure Ping_Device (Manager : in Util.Properties.Manager;
                                Name    : in String);

         Box  : Bbox.API.Client_Type;

         procedure Ping_Device (Manager : in Util.Properties.Manager;
                                Name    : in String) is
            Id   : constant String := Manager.Get (Name & ".id", "");
         begin
            case Selector is
               when DEVICE_ALL =>
                  null;

               when DEVICE_ACTIVE =>
                  if Manager.Get (Name & ".active", "") = "0" then
                     return;
                  end if;

               when DEVICE_INACTIVE =>
                  if Manager.Get (Name & ".active", "") = "1" then
                     return;
                  end if;

            end case;
            Log.Info ("Ping command on {0}", Manager.Get (Name & ".ipaddress", ""));
            Box.Post ("hosts/" & Id, "action=ping");
         end Ping_Device;

      begin
         if Ada.Strings.Unbounded.Length (Gateway.Passwd) = 0 then
            return;
         end if;
         Gateway.Refresh;
         Box.Set_Server (To_String (Gateway.Ip));
         Box.Login (To_String (Gateway.Passwd));

         Bbox.API.Iterate (Gateway.Hosts, "hosts.list", Ping_Device'Access);
      end Do_Ping;

      procedure Box_Status (Gateway : in out Druss.Gateways.Gateway_Type) is
         procedure Print_Device (Manager : in Util.Properties.Manager;
                                 Name    : in String);

         procedure Print_Device (Manager : in Util.Properties.Manager;
                                 Name    : in String) is
            Link : constant String := Manager.Get (Name & ".link", "");
         begin
            if Manager.Get (Name & ".active", "") = "0" then
               return;
            end if;
            Console.Start_Row;
            Console.Print_Field (F_BBOX_IP_ADDR, Gateway.Ip);
            Console.Print_Field (F_IP_ADDR, Manager.Get (Name & ".ipaddress", ""));
            Console.Print_Field (F_HOSTNAME, Manager.Get (Name & ".hostname", ""));
            Print_Perf (Console, F_ACTIVE, Manager.Get (Name & ".ping.average", ""));
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
      Druss.Gateways.Iterate (Context.Gateways, Gateways.ITER_ENABLE, Do_Ping'Access);
      delay 5.0;
      Console.Start_Title;
      Console.Print_Title (F_BBOX_IP_ADDR, "Bbox IP", 16);
      Console.Print_Title (F_IP_ADDR, "Device IP", 16);
      Console.Print_Title (F_HOSTNAME, "Hostname", 28);
      Console.Print_Title (F_ACTIVE, "Ping", 15);
      Console.Print_Title (F_LINK, "Link", 18);
      Console.End_Title;
      Druss.Gateways.Iterate (Context.Gateways, Gateways.ITER_ENABLE, Box_Status'Access);
   end Do_Ping;

   --  ------------------------------
   --  Execute a ping from the gateway to each device.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      pragma Unreferenced (Name);
   begin
      if Args.Get_Count > 1 then
         Context.Console.Notice (N_USAGE, "Too many arguments to the command");
         Druss.Commands.Driver.Usage (Args);
      elsif Args.Get_Count = 0 then
         Command.Do_Ping (Args, DEVICE_ALL, Context);
      elsif Args.Get_Argument (1) = "all" then
         Command.Do_Ping (Args, DEVICE_ALL, Context);
      elsif Args.Get_Argument (1) = "active" then
         Command.Do_Ping (Args, DEVICE_ACTIVE, Context);
      elsif Args.Get_Argument (1) = "inactive" then
         Command.Do_Ping (Args, DEVICE_INACTIVE, Context);
      else
         Context.Console.Notice (N_USAGE, "Invalid argument: " & Args.Get_Argument (1));
         Druss.Commands.Driver.Usage (Args);
      end if;
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
      Console.Notice (N_HELP, "ping: Ask the Bbox to ping the devices");
      Console.Notice (N_HELP, "Usage: ping [all | active | inactive]");
      Console.Notice (N_HELP, "");
      Console.Notice (N_HELP, "  Ask the Bbox to ping the devices.  By default it will ping");
      Console.Notice (N_HELP, "  all the devices that have been discovered by the Bbox.");
      Console.Notice (N_HELP, "  The command will wait 5 seconds and it will list the active");
      Console.Notice (N_HELP, "  devices with their ping performance.");
      Console.Notice (N_HELP, "");
      Console.Notice (N_HELP, "    all        Ping all the devices");
      Console.Notice (N_HELP, "    active     Ping the active devices only");
      Console.Notice (N_HELP, "    inative    Ping the inactive devices only");
   end Help;

end Druss.Commands.Ping;
