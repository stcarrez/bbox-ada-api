-----------------------------------------------------------------------
--  druss-commands-bboxes -- Commands to manage the bboxes
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
with Ada.Streams;
with Ada.Strings.Unbounded;
with Util.Log.Loggers;
with Util.Properties;
with Util.Strings;
with Util.Strings.Sets;
with Bbox.API;
with Druss.Gateways;
with Druss.Config;
with UPnP.SSDP;
package body Druss.Commands.Bboxes is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use type Ada.Streams.Stream_Element_Offset;
   use type Ada.Streams.Stream_Element;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Druss.Commands.Bboxes");

   procedure Discover (IP : in String) is
      Box  : Bbox.API.Client_Type;
      Info : Util.Properties.Manager;
   begin
      Box.Set_Server (IP);
      Box.Get ("device", Info);
      if Info.Get ("device.modelname", "") /= "" then
         Log.Info ("Found a bbox at {0}", IP);
      end if;

   exception
      when E : others =>
         null;
   end Discover;

   --  Add the bbox with the given IP address.
   procedure Add_Bbox (Command : in Command_Type;
                       IP      : in String;
                       Context : in out Context_Type) is
      Box  : Bbox.API.Client_Type;
      Info : Util.Properties.Manager;
      Gw   : Druss.Gateways.Gateway_Ref := Druss.Gateways.Find_IP (Context.Gateways, IP);
   begin
      if not Gw.Is_Null then
         Log.Debug ("Bbox {0} is already registered", IP);
         return;
      end if;
      Box.Set_Server (IP);
      Box.Get ("device", Info);
      if Info.Get ("device.modelname", "") /= "" then
         Log.Info ("Found a new bbox at {0}", IP);
         Gw := Druss.Gateways.Gateway_Refs.Create;
         Gw.Value.IP := Ada.Strings.Unbounded.To_Unbounded_String (IP);
         Context.Gateways.Append (Gw);
      end if;

   exception
      when E : others =>
         null;
   end Add_Bbox;

   procedure Discover (Command   : in Command_Type;
                       Context   : in out Context_Type) is
      Retry        : Natural := 0;
      Scanner      : UPnP.SSDP.Scanner_Type;
      Itf_IPs      : Util.Strings.Sets.Set;

      procedure Check_Bbox (IP : in String) is
         Box  : Bbox.API.Client_Type;
         Info : Util.Properties.Manager;
      begin
         Box.Set_Server (IP);
         Box.Get ("device", Info);
         if Info.Get ("device.modelname", "") /= "" then
            Log.Info ("Found a bbox at {0}", IP);
         end if;

      exception
         when E : others =>
            null;
      end Check_Bbox;

      procedure Process (URI : in String) is
         Pos : Natural;
      begin
         if URI'Length <= 7 or else URI (URI'First .. URI'First + 6) /= "http://" then
            return;
         end if;
         Pos := Util.Strings.Index (URI, ':', 6);
         if Pos > 0 then
            Command.Add_Bbox (URI (URI'First + 7 .. Pos - 1), Context);
            --  Check_Bbox ();
         end if;
      end Process;

   begin
      Log.Info ("Discovering gateways on the network");

      Scanner.Initialize;
      Scanner.Find_IPv4_Addresses (Itf_IPs);
      while Retry < 5 loop
         Scanner.Send_Discovery ("urn:schemas-upnp-org:device:InternetGatewayDevice:1", Itf_IPs);
         Scanner.Discover ("urn:schemas-upnp-org:device:InternetGatewayDevice:1",
                           Process'Access, 1.0);
         Retry := Retry + 1;
      end loop;

      Druss.Config.Save_Gateways (Context.Gateways);
   end Discover;

   --  ------------------------------
   --  Set the password to be used by the Bbox API to connect to the box.
   --  ------------------------------
   procedure Password (Command   : in Command_Type;
                       Args      : in Util.Commands.Argument_List'Class;
                       Context   : in out Context_Type) is

      procedure Change_Password (Gateway : in out Druss.Gateways.Gateway_Type;
                                 Passwd  : in String) is
      begin
         Gateway.Passwd := Ada.Strings.Unbounded.To_Unbounded_String (Passwd);
      end Change_Password;

   begin
      Druss.Commands.Gateway_Command (Command, Args, 2, Change_Password'Access, Context);
      Druss.Config.Save_Gateways (Context.Gateways);
   end Password;

   --  ------------------------------
   --  Execute the command with the arguments.  The command name is passed with the command
   --  arguments.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Util.Commands.Argument_List'Class;
                      Context   : in out Context_Type) is
   begin
      if Args.Get_Count = 0 then
         Druss.Commands.Driver.Usage (Args);
      elsif Args.Get_Argument (1) = "discover" then
         Command.Discover (Context);
      elsif Args.Get_Argument (1) = "password" then
         Command.Password (Args, Context);
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
      Put_Line ("bbox: Manage and define the configuration to connect to the Bbox");
      Put_Line ("Usage: bbox <operation>...");
      New_Line;
      Put_Line ("  Druss needs to know the list of Bboxes which are available on the network.");
      Put_Line ("  It also need some credentials to connect to the Bbox using the Bbox API.");
      Put_Line ("  The 'bbox' command allows to manage that list and configuration.");
      Put_Line ("  Examples:");
      Put_Line ("    bbox discover               Discover the bbox(es) connected to the LAN");
      Put_Line ("    bbox add IP                 Add a bbox knowing its IP address");
      Put_Line ("    bbox del IP                 Delete a bbox from the list");
      Put_Line ("    bbox password <pass> [IP]   Set the bbox API connection password");
   end Help;

end Druss.Commands.Bboxes;
