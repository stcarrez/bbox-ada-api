-----------------------------------------------------------------------
--  druss-commands-get -- Raw JSON API Get command
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
with Ada.Containers;
with Bbox.API;
with Druss.Gateways;
package body Druss.Commands.Get is

   use type Ada.Containers.Count_Type;
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Execute a GET operation on the Bbox API and return the raw JSON result.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      pragma Unreferenced (Command, Name);
      procedure Execute_One (Gateway : in out Druss.Gateways.Gateway_Type);

      Need_Colon : Boolean := False;

      --  Execute the GET API operation and print the raw JSON result.
      procedure Execute_One (Gateway : in out Druss.Gateways.Gateway_Type) is
         Box     : Bbox.API.Client_Type;
      begin
         Box.Set_Server (To_String (Gateway.Ip));
         if Ada.Strings.Unbounded.Length (Gateway.Passwd) > 0 then
            Box.Login (To_String (Gateway.Passwd));
         end if;
         for I in 1 .. Args.Get_Count loop
            declare
               Operation : constant String := Args.Get_Argument (I);
               Content   : constant String := Box.Get (Operation);
               Last      : Natural := Content'Last;
            begin
               if Need_Colon then
                  Ada.Text_IO.Put (",");
               end if;

               while Last > Content'First and Content (Last) = ASCII.LF loop
                  Last := Last - 1;
               end loop;

               --  We did a mistake when we designed the Bbox API and used '[' ... ']' arrays
               --  for most of the JSON result.  Strip that unecessary array.
               if Content (Content'First) = '[' and Content (Last) = ']' then
                  Ada.Text_IO.Put_Line (Content (Content'First + 1 .. Last - 1));
               else
                  Ada.Text_IO.Put_Line (Box.Get (Operation));
               end if;
               Need_Colon := True;
            end;
         end loop;
      end Execute_One;

   begin
      if Args.Get_Count = 0 then
         Druss.Commands.Driver.Usage (Args);
      else
         if Args.Get_Count > 1 or else Context.Gateways.Length > 1 then
            Ada.Text_IO.Put_Line ("[");
         end if;
         Druss.Gateways.Iterate (Context.Gateways, Gateways.ITER_ENABLE, Execute_One'Access);
         if Args.Get_Count > 1 or else Context.Gateways.Length > 1 then
            Ada.Text_IO.Put_Line ("]");
         end if;
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
      Console.Notice (N_HELP, "get: Execute one or several GET operation on the Bbox API" &
                        " and print the raw JSON result");
      Console.Notice (N_HELP,
                      "Usage: get <operation>...");
      Console.Notice (N_HELP,
                      "");
      Console.Notice (N_HELP,
                      "  The Bbox API operation are called and the raw JSON result is printed.");
      Console.Notice (N_HELP,
                      "  When several operations are called, a JSON array is formed to insert");
      Console.Notice (N_HELP,
                      "  their result in the final JSON content so that it is valid.");
      Console.Notice (N_HELP,
                      "  Examples:");
      Console.Notice (N_HELP,
                      "    get device             Get information about the Bbox");
      Console.Notice (N_HELP,
                      "    get hosts              Get the list of hosts detected by the Bbox");
      Console.Notice (N_HELP,
                      "    get wan/ip             Get information about the WAN connection");
      Console.Notice (N_HELP,
                      "    get wan/ip wan/xdsl    Get the WAN and xDSL line information");
   end Help;

end Druss.Commands.Get;
