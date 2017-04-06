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

with Druss.Commands.Bboxes;
with Druss.Commands.Get;
with Druss.Commands.Status;
with Druss.Commands.Wifi;
package body Druss.Commands is

   Help_Command    : aliased Druss.Commands.Drivers.Help_Command_Type;
   Bbox_Commands   : aliased Druss.Commands.Bboxes.Command_Type;
   Get_Commands    : aliased Druss.Commands.Get.Command_Type;
   Wifi_Commands   : aliased Druss.Commands.Wifi.Command_Type;
   Status_Commands : aliased Druss.Commands.Status.Command_Type;

   procedure Gateway_Command (Command   : in Drivers.Command_Type'Class;
                              Args      : in Util.Commands.Argument_List'Class;
                              Arg_Pos   : in Positive;
                              Process   : access procedure (Gateway : in out Gateways.Gateway_Type;
                                                            Param   : in String);
                              Context   : in out Context_Type) is

   begin
      if Args.Get_Count < Arg_Pos + 1 then
         Druss.Commands.Driver.Usage (Args);
      end if;
      declare
         Param  : constant String := Args.Get_Argument (Arg_Pos);
         Gw     : Druss.Gateways.Gateway_Ref;

         procedure Operation (Gateway : in out Druss.Gateways.Gateway_Type) is
         begin
            Process (Gateway, Param);
         end Operation;

      begin
         if Args.Get_Count = Arg_Pos then
            Druss.Gateways.Iterate (Context.Gateways, Operation'Access);
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
      Driver.Set_Usage ("[-v] [-o file] [-c config] <command> [<args>]" & ASCII.LF &
                          "where:" & ASCII.LF &
                          "  -v         Verbose execution mode" & ASCII.LF &
                          "  -c config  Use the configuration file" &
                          " (instead of $HOME/.config/druss/druss.properties)" & ASCII.LF &
                          "  -o file    The output file to use");
      Driver.Add_Command ("help", Help_Command'Access);
      Driver.Add_Command ("bbox", Bbox_Commands'Access);
      Driver.Add_Command ("get", Get_Commands'Access);
      Driver.Add_Command ("wifi", Wifi_Commands'Access);
      Driver.Add_Command ("status", Status_Commands'Access);
   end Initialize;

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

end Druss.Commands;
