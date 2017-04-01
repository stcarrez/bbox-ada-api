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

with Util.Commands.Drivers;
with Util.Properties;
with Druss.Commands.Bboxes;
with Druss.Commands.Status;
package body Druss.Commands is

   Help_Command  : aliased Druss.Commands.Drivers.Help_Command_Type;
   Bbox_Commands : aliased Druss.Commands.Bboxes.Command_Type;

   procedure Initialize is
   begin
      Driver.Set_Description ("Druss - The Bbox master controller");
      Driver.Set_Usage ("[-o file] <command> [<args>]" & ASCII.LF &
                        "  -o file    The output file to use");
      Driver.Add_Command ("help", Help_Command'Access);
      Driver.Add_Command ("bbox", Bbox_Commands'Access);
      Driver.Add_Command ("status", Druss.Commands.Status.Wan_Status'Access);
   end Initialize;

end Druss.Commands;
