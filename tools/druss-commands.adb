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
with Ada.IO_Exceptions;
with Util.Strings;
with Util.Log.Loggers;
with Readline;
with Druss.Commands.Bboxes;
with Druss.Commands.Get;
with Druss.Commands.Status;
with Druss.Commands.Wifi;
with Druss.Commands.Devices;
with Druss.Commands.Ping;
package body Druss.Commands is

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Druss.Commands");

   function Uptime_Image (Value : in Natural) return String;
   procedure Quit (Name    : in String;
                   Args    : in Argument_List'Class;
                   Context : in out Context_Type);

   Help_Command    : aliased Druss.Commands.Drivers.Help_Command_Type;
   Bbox_Commands   : aliased Druss.Commands.Bboxes.Command_Type;
   Get_Commands    : aliased Druss.Commands.Get.Command_Type;
   Wifi_Commands   : aliased Druss.Commands.Wifi.Command_Type;
   Status_Commands : aliased Druss.Commands.Status.Command_Type;
   Device_Commands : aliased Druss.Commands.Devices.Command_Type;
   Ping_Commands   : aliased Druss.Commands.Ping.Command_Type;

   procedure Gateway_Command (Command   : in Drivers.Command_Type'Class;
                              Args      : in Util.Commands.Argument_List'Class;
                              Arg_Pos   : in Positive;
                              Process   : access procedure (Gateway : in out Gateways.Gateway_Type;
                                                            Param   : in String);
                              Context   : in out Context_Type) is
      pragma Unreferenced (Command);
   begin
      if Args.Get_Count < Arg_Pos then
         Context.Console.Notice (N_USAGE, "Missing argument for command");
         Druss.Commands.Driver.Usage (Args);
      end if;
      declare
         procedure Operation (Gateway : in out Druss.Gateways.Gateway_Type);

         Param  : constant String := Args.Get_Argument (Arg_Pos);
         Gw     : Druss.Gateways.Gateway_Ref;

         procedure Operation (Gateway : in out Druss.Gateways.Gateway_Type) is
         begin
            Process (Gateway, Param);
         end Operation;

      begin
         if Args.Get_Count = Arg_Pos then
            Druss.Gateways.Iterate (Context.Gateways, Gateways.ITER_ENABLE, Operation'Access);
         else
            for I in Arg_Pos + 1 .. Args.Get_Count loop
               Gw := Druss.Gateways.Find_IP (Context.Gateways, Args.Get_Argument (I));
               if not Gw.Is_Null then
                  Operation (Gw.Value.all);
               end if;
            end loop;
         end if;
      end;
   end Gateway_Command;

   procedure Initialize is
   begin
      Driver.Set_Description ("Druss - The Bbox master controller");
      Driver.Set_Usage ("[-v] [-d] [-i] [-o file] [-c config] <command> [<args>]" & ASCII.LF &
                          "where:" & ASCII.LF &
                          "  -v         Verbose execution mode" & ASCII.LF &
                          "  -d         Debug execution mode" & ASCII.LF &
                          "  -i         Enter the interactive mode" & ASCII.LF &
                          "  -c config  Use the configuration file" &
                          " (instead of $HOME/.config/druss/druss.properties)" & ASCII.LF &
                          "  -o file    The output file to use");
      Driver.Add_Command ("help", Help_Command'Access);
      Driver.Add_Command ("bbox", Bbox_Commands'Access);
      Driver.Add_Command ("get", Get_Commands'Access);
      Driver.Add_Command ("wifi", Wifi_Commands'Access);
      Driver.Add_Command ("status", Status_Commands'Access);
      Driver.Add_Command ("devices", Device_Commands'Access);
      Driver.Add_Command ("ping", Ping_Commands'Access);
   end Initialize;

   --  ------------------------------
   --  Quit the interactive loop.
   --  ------------------------------
   procedure Quit (Name    : in String;
                   Args    : in Argument_List'Class;
                   Context : in out Context_Type) is
      pragma Unreferenced (Name, Args, Context);
   begin
      raise Stop_Interactive;
   end Quit;

   --  ------------------------------
   --  Enter in the interactive main loop waiting for user commands and executing them.
   --  ------------------------------
   procedure Interactive (Context : in out Context_Type) is
      Args : Util.Commands.String_Argument_List (Max_Length => 1000,
                                                 Max_Args   => 100);
   begin
      Log.Debug ("Entering in interactive mode");
      Driver.Add_Command (Name => "quit", Handler => Quit'Access);
      loop
         declare
            Line : constant String := Readline.Get_Line ("druss>");
         begin
            Log.Debug ("Execute: {0}", Line);
            Args.Initialize (Line);
            Driver.Execute (Args.Get_Command_Name, Args, Context);

         exception
            when Stop_Interactive =>
               Log.Debug ("Leaving interactive mode");
               exit;

            when others =>
               Context.Console.Notice (N_INFO, "Command failed");
         end;
      end loop;

   exception
      when Ada.IO_Exceptions.End_Error =>
         Log.Debug ("End_Error exception received");

   end Interactive;

   --  ------------------------------
   --  Print the bbox API status.
   --  ------------------------------
   procedure Print_Status (Console : in Consoles.Console_Access;
                           Field   : in Field_Type;
                           Value   : in String) is
   begin
      if Value = "2" then
         Console.Print_Field (Field, "OK");
      elsif Value = "-1" then
         Console.Print_Field (Field, "KO");
      elsif Value = "1" then
         Console.Print_Field (Field, "Starting");
      elsif Value = "0" then
         Console.Print_Field (Field, "Stopped");
      else
         Console.Print_Field (Field, "?");
      end if;
   end Print_Status;

   --  ------------------------------
   --  Print a ON/OFF status.
   --  ------------------------------
   procedure Print_On_Off (Console : in Consoles.Console_Access;
                           Field   : in Field_Type;
                           Value   : in String) is
   begin
      if Value = "1" then
         Console.Print_Field (Field, "ON");
      elsif Value = "0" then
         Console.Print_Field (Field, "OFF");
      else
         Console.Print_Field (Field, "");
      end if;
   end Print_On_Off;

   function Uptime_Image (Value : in Natural) return String is
      D : constant Natural := Value / 86400;
      R : constant Natural := Value mod 86400;
      H : constant Natural := R / 3600;
      M : constant Natural := (R mod 3600) / 60;
      S : constant Natural := (R mod 3600) mod 60;
   begin
      if D > 0 then
         return Util.Strings.Image (D) & "d"
           & (if H > 0 then Natural'Image (H) & "h" else "")
           & (if M > 0 then Natural'Image (M) & "m" else "");
      elsif H > 0 then
         return Util.Strings.Image (H) & "h"
           & (if M > 0 then Natural'Image (M) & "m" else "");
      else
         return Util.Strings.Image (M) & "m"
           & Natural'Image (S) & "s";
      end if;
   end Uptime_Image;

   --  ------------------------------
   --  Print a uptime.
   --  ------------------------------
   procedure Print_Uptime (Console : in Consoles.Console_Access;
                           Field   : in Field_Type;
                           Value   : in String) is
   begin
      if Value = "" then
         Console.Print_Field (Field, Value);
      else
         Console.Print_Field (Field, Uptime_Image (Natural'Value (Value)));
      end if;

   exception
      when others =>
            Console.Print_Field (Field, Value);
   end Print_Uptime;

   --  ------------------------------
   --  Print a performance measure in us or ms.
   --  ------------------------------
   procedure Print_Perf (Console : in Consoles.Console_Access;
                         Field   : in Field_Type;
                         Value   : in String) is
      use Commands.Consoles;
   begin
      if Value = "" then
         Console.Print_Field (Field, Value);
      elsif Value'Length <= 3 then
         Console.Print_Field (Field, Value & " us", J_RIGHT);
      elsif Value'Length <= 6 then
         Console.Print_Field (Field, Value (Value'First .. Value'Last - 3) & "."
                              & Value (Value'Last - 2 .. Value'Last) & " ms", J_RIGHT);
      else
         Console.Print_Field (Field, Value (Value'First .. Value'Last - 6) & "."
                              & Value (Value'Last - 5 .. Value'Last - 3) & " s", J_RIGHT);
      end if;
   end Print_Perf;

end Druss.Commands;
