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
with Bbox.API;
package body Druss.Commands.Wifi is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   --  ------------------------------
   --  Enable or disable with wifi radio.
   --  ------------------------------
   procedure Set_Enable (Command   : in Command_Type;
                         Args      : in Argument_List'Class;
                         Value     : in String;
                         Context   : in out Context_Type) is

      procedure Radio (Gateway : in out Druss.Gateways.Gateway_Type;
                       State   : in String) is
         Box     : Bbox.API.Client_Type;
      begin
         Box.Set_Server (To_String (Gateway.Ip));
         if Ada.Strings.Unbounded.Length (Gateway.Passwd) > 0 then
            Box.Login (To_String (Gateway.Passwd));
         end if;
         Box.Put ("wireless", (if State = "on" then "radio.enable=1" else "radio.enable=0"));
      end Radio;

   begin
      Druss.Commands.Gateway_Command (Command, Args, 1, Radio'Access, Context);
   end Set_Enable;

   --  ------------------------------
   --  Execute the wifi 'status' command to print the Wifi current status.
   --  ------------------------------
   procedure Do_Status (Command   : in Command_Type;
                        Args      : in Argument_List'Class;
                        Context   : in out Context_Type) is
      Console : constant Druss.Commands.Consoles.Console_Access := Context.Console;

      procedure Wifi_Status (Gateway : in out Druss.Gateways.Gateway_Type) is
      begin
         Gateway.Refresh;
         Console.Start_Row;
         Console.Print_Field (F_IP_ADDR, Gateway.Ip);
         Console.Print_Field (F_BOOL, Gateway.Wifi.Get ("wireless.radio.24.enable", " "));
         Console.Print_Field (F_CHANNEL, Gateway.Wifi.Get ("wireless.radio.24.current_channel", " "));
         Console.Print_Field (F_SSID, Gateway.Wifi.Get ("wireless.ssid.24.id", " "));
         Console.Print_Field (F_PROTOCOL, Gateway.Wifi.Get ("wireless.ssid.24.security.protocol", " "));
         Console.Print_Field (F_ENCRYPTION, Gateway.Wifi.Get ("wireless.ssid.24.security.encryption", " "));
         Console.End_Row;

         if not Gateway.Wifi.Exists ("wireless.radio.5.enable") then
            return;
         end if;
         Console.Start_Row;
         Console.Print_Field (F_IP_ADDR, To_String (Gateway.Ip));
         Console.Print_Field (F_BOOL, Gateway.Wifi.Get ("wireless.radio.5.enable", " "));
         Console.Print_Field (F_CHANNEL, Gateway.Wifi.Get ("wireless.radio.5.current_channel", " "));
         Console.Print_Field (F_SSID, Gateway.Wifi.Get ("wireless.ssid.5.id", " "));
         Console.Print_Field (F_PROTOCOL, Gateway.Wifi.Get ("wireless.ssid.5.security.protocol", " "));
         Console.Print_Field (F_ENCRYPTION, Gateway.Wifi.Get ("wireless.ssid.5.security.encryption", " "));
         Console.End_Row;
      end Wifi_Status;

   begin
      Console.Start_Title;
      Console.Print_Title (F_IP_ADDR, "Bbox IP", 15);
      Console.Print_Title (F_BOOL, "Enable", 8);
      Console.Print_Title (F_CHANNEL, "Channel", 8);
      Console.Print_Title (F_SSID, "SSID", 20);
      Console.Print_Title (F_PROTOCOL, "Protocol", 12);
      Console.Print_Title (F_ENCRYPTION, "Encryption", 12);
      Console.End_Title;
      Druss.Gateways.Iterate (Context.Gateways, Wifi_Status'Access);
   end Do_Status;

   --  ------------------------------
   --  Execute a command to control or get status about the Wifi.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is

   begin
      if Args.Get_Count = 0 then
         Command.Do_Status (Args, Context);
      elsif Args.Get_Argument (1) = "on" then
         Command.Set_Enable (Args, "on", Context);
      elsif Args.Get_Argument (1) = "off" then
         Command.Set_Enable (Args, "on", Context);
      elsif Args.Get_Argument (1) = "status" then
         Command.Do_Status (Args, Context);
      else
         Put_Line ("Invalid sub-command: " & Args.Get_Argument (1));
         Druss.Commands.Driver.Usage (Args);
      end if;
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
      Put_Line ("  wifi on [IP]...         Turn ON the wifi on the Bbox.");
      Put_Line ("  wifi off [IP]...        Turn OFF the wifi on the Bbox.");
      Put_Line ("  wifi show               Show information about the wifi on the Bbox.");
      Put_Line ("  wifi devices            Show the wifi devices which are connected.");
   end Help;

end Druss.Commands.Wifi;
