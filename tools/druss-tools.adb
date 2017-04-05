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
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Util.Commands;
with Util.Http.Clients.Curl;
with Util.Log.Loggers;
with Util.Properties;
with Druss.Config;
with Druss.Commands;
procedure Druss.Tools is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   Log_Config : Util.Properties.Manager;
   Config     : Ada.Strings.Unbounded.Unbounded_String;
   Output     : Ada.Strings.Unbounded.Unbounded_String;
   Debug      : Boolean := False;
   Verbose    : Boolean := False;
   First      : Natural := 0;
   All_Args   : Util.Commands.Default_Argument_List (0);
   Console    : aliased Druss.Commands.Text_Consoles.Console_Type;
begin
   Log_Config.Set ("log4j.rootCategory", "DEBUG,console");
   Log_Config.Set ("log4j.appender.console", "Console");
   Log_Config.Set ("log4j.appender.console.level", "ERROR");
   Log_Config.Set ("log4j.appender.console.layout", "level-message");
   Log_Config.Set ("log4j.appender.stdout", "Console");
   Log_Config.Set ("log4j.appender.stdout.level", "INFO");
   Log_Config.Set ("log4j.appender.stdout.layout", "message");
   Log_Config.Set ("log4j.logger.Util", "WARN");

   Util.Log.Loggers.Initialize (Log_Config);
   Druss.Commands.Initialize;
   Initialize_Option_Scan (Stop_At_First_Non_Switch => True, Section_Delimiters => "targs");
   --  Parse the command line
   loop
      case Getopt ("* v d o: c:") is
         when ASCII.NUL => exit;

         when 'c' =>
            Config := Ada.Strings.Unbounded.To_Unbounded_String (Parameter);

         when 'o' =>
            Output := Ada.Strings.Unbounded.To_Unbounded_String (Parameter);

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
   if Verbose then
      Log_Config.Set ("log4j.appender.console.level", "INFO");
   end if;
   if Debug then
      Log_Config.Set ("log4j.appender.console.level", "DEBUG");
   end if;
   Util.Log.Loggers.Initialize (Log_Config);
   Druss.Config.Initialize (To_String (Config));
   Util.Http.Clients.Curl.Register;
   if First >= Ada.Command_Line.Argument_Count then
      Ada.Text_IO.Put_Line ("Missing command name to execute.");
      Druss.Commands.Driver.Usage (All_Args);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   declare
      Cmd_Name : constant String := Full_Switch;
      Args     : Util.Commands.Default_Argument_List (First + 1);
      Ctx      : Druss.Commands.Context_Type;
   begin
      Ctx.Console := Console'Unchecked_Access;
      Druss.Config.Get_Gateways (Ctx.Gateways);
      Druss.Commands.Driver.Execute (Cmd_Name, Args, Ctx);
   end;

exception
   when E : Invalid_Switch =>
      Ada.Text_IO.Put_Line ("Invalid option: " & Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (2);

end Druss.Tools;
