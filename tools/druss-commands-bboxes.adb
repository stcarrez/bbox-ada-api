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
with Util.Log.Loggers;
with Util.Properties;
with Util.Strings;
with Util.Strings.Sets;
with Bbox.API;
with Druss.Gateways;
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

   procedure Discover (Command   : in Command_Type) is
      IPs          : Util.Strings.Sets.Set;
      Retry        : Natural := 0;
      Scanner      : UPnP.SSDP.Scanner_Type;
      Itf_IPs      : Util.Strings.Sets.Set;

      procedure Process (URI : in String) is
         Pos2 : Natural;
      begin
         --  http://
         Pos2 := Util.Strings.Index (URI, ':', 7);
         if Pos2 > 0 and then not IPs.Contains (URI (URI'First + 7 .. Pos2 - 1)) then
            Log.Info ("Detected an IGD device at {0}", URI (URI'First + 7 .. Pos2 - 1));
            IPs.Include (URI (URI'First + 7 .. Pos2 - 1));
         end if;
         Log.Warn ("Found: {0}", URI);
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

      for IP of IPs loop
         Discover (IP);
      end loop;
   end Discover;

   --  Execute the command with the arguments.  The command name is passed with the command
   --  arguments.
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Util.Commands.Argument_List'Class;
                      Context   : in out Context_Type) is
   begin
      if Args.Get_Count = 0 then
         Druss.Commands.Driver.Usage (Args);
      elsif Args.Get_Argument (1) = "discover" then
         Command.Discover;
      end if;
   end Execute;

   --  Write the help associated with the command.
   overriding
   procedure Help (Command   : in Command_Type;
                   Context   : in out Context_Type) is
   begin
      Put_Line ("bbox: Manage and define the configuration to connect to the Bbox");
      Put_Line ("Usage: bbox <operation>...");
      New_Line;
      Put_Line ("  The Bbox API operation are called and the raw JSON result is printed.");
      Put_Line ("  When several operations are called, a JSON array is formed to insert");
      Put_Line ("  their result in the final JSON content so that it is valid.");
      Put_Line ("  Examples:");
      Put_Line ("    bbox discover          Discover the bbox(es) connected to the LAN");
      Put_Line ("    bbox add IP            Add a bbox Get information about the Bbox");
      Put_Line ("    bbox del IP            Get the list of hosts detected by the Bbox");
   end Help;

end Druss.Commands.Bboxes;
