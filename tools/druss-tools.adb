-----------------------------------------------------------------------
--  druss-tools -- Druss main tool
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
with GNAT.Command_Line;  use GNAT.Command_Line;
with Ada.Command_Line;
with Bbox.API;
with Util.Properties;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Util.Commands;
with Util.Http.Clients.Curl;
with Druss.Gateways;
with Druss.Config;
with Druss.Commands;
procedure Druss.Tools is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   Debug   : Boolean := False;
   Verbose : Boolean := False;
   List    : Druss.Gateways.Gateway_Vector;
   First   : Natural := 0;
begin
   Druss.Commands.Initialize;
   Initialize_Option_Scan (Stop_At_First_Non_Switch => True, Section_Delimiters => "targs");
   --  Parse the command line
   loop
      case Getopt ("* v d e E o: t: c:") is
         when ASCII.NUL => exit;

         when 'c' =>
            null;--  Set_Config_Directory (Parameter);

         when 'd' =>
            Debug := True;

         when 'v' =>
            Verbose := True;

         when '*' =>
            exit;

         when others =>
            null;
      end case;
      First := First + 1;
   end loop;
   Druss.Config.Initialize;
   Util.Http.Clients.Curl.Register;
   declare
      Cmd_Name : constant String := Full_Switch;
      Args     : Util.Commands.Default_Argument_List (First + 1);
      Ctx      : Druss.Commands.Context_Type;
   begin
      if Cmd_Name = "" then
         Druss.Commands.Driver.Usage (Args);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;
      Druss.Config.Get_Gateways (Ctx.Gateways);
      Druss.Commands.Driver.Execute (Cmd_Name, Args, Ctx);
   end;

exception
   when E : Invalid_Switch =>
      Ada.Text_IO.Put_Line ("Invalid option: " & Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (2);

end Druss.Tools;
