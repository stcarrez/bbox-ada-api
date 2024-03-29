-----------------------------------------------------------------------
--  druss-commands -- Commands available for Druss
--  Copyright (C) 2017, 2018, 2023 Stephane Carrez
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

with Util.Strings;
with Util.Commands.Drivers;
with Util.Commands.Parsers;
with Util.Commands.Consoles;
with Util.Commands.Consoles.Text;
with Util.Commands.Text_IO;
with Druss.Gateways;
package Druss.Commands is

   --  Exception raised to stop the interactive loop.
   Stop_Interactive : exception;

   --  Device selector to select all, active or inactive devices.
   type Device_Selector_Type is (DEVICE_ALL, DEVICE_ACTIVE, DEVICE_INACTIVE);

   --  The list of fields that are printed on the console.
   type Field_Type is (F_IP_ADDR,
                       F_BBOX_IP_ADDR,
                       F_WAN_IP,
                       F_ETHERNET,
                       F_INTERNET,
                       F_VOIP,
                       F_HOSTNAME,
                       F_CONNECTION,
                       F_DEVTYPE,
                       F_ACTIVE,
                       F_LINK,
                       F_WIFI,
                       F_WIFI5,
                       F_ACCESS_CONTROL,
                       F_DYNDNS,
                       F_DEVICES,
                       F_UPTIME,
                       F_COUNT,
                       F_BOOL,
                       F_CHANNEL,
                       F_PROTOCOL,
                       F_ENCRYPTION,
                       F_SSID);

   --  The type of notice that are reported.
   type Notice_Type is (N_HELP,
                        N_USAGE,
                        N_INFO);

   --  Make the generic abstract console interface.
   package Consoles is
     new Util.Commands.Consoles (Field_Type   => Field_Type,
                                 Notice_Type  => Notice_Type,
                                 Element_Type => Character,
                                 Input_Type   => String,
                                 To_Input     => Util.Strings.Image);

   function To_String (S : in String) return String is (S);

   --  And the text console to write on stdout (a Gtk console could be done someday).
   package Text_Consoles is
     new Consoles.Text (IO        => Util.Commands.Text_IO,
                        To_String => To_String);

   type Context_Type is limited record
      Gateways : Druss.Gateways.Gateway_Vector;
      Console  : Consoles.Console_Access;
   end record;

   package Drivers is
     new Util.Commands.Drivers (Context_Type  => Context_Type,
                                Config_Parser => Util.Commands.Parsers.No_Parser,
                                IO            => Util.Commands.Text_IO,
                                Driver_Name   => "druss-drivers");

   subtype Argument_List is Util.Commands.Argument_List;

   Driver : Drivers.Driver_Type;

   procedure Gateway_Command (Command   : in Drivers.Command_Type'Class;
                              Args      : in Util.Commands.Argument_List'Class;
                              Arg_Pos   : in Positive;
                              Process   : access procedure (Gateway : in out Gateways.Gateway_Type;
                                                            Param   : in String);
                              Context   : in out Context_Type);

   procedure Initialize;

   --  Enter in the interactive main loop waiting for user commands and executing them.
   procedure Interactive (Context : in out Context_Type);

   --  Print the bbox API status.
   procedure Print_Status (Console : in Consoles.Console_Access;
                           Field   : in Field_Type;
                           Value   : in String);

   --  Print a ON/OFF status.
   procedure Print_On_Off (Console : in Consoles.Console_Access;
                           Field   : in Field_Type;
                           Value   : in String);

   --  Print a uptime.
   procedure Print_Uptime (Console : in Consoles.Console_Access;
                           Field   : in Field_Type;
                           Value   : in String);

   --  Print a performance measure in us or ms.
   procedure Print_Perf (Console : in Consoles.Console_Access;
                         Field   : in Field_Type;
                         Value   : in String);

end Druss.Commands;
