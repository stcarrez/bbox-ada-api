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
with Bbox.API;
with Druss.Gateways;
package body Druss.Commands.Get is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Execute the GET API operation and print the raw JSON result.
   --  ------------------------------
   procedure Execute_Operation (Command   : in Command_Type;
                                Operation : in String;
                                Context   : in out Context_Type) is
      pragma Unreferenced (Command);
      procedure Execute_One (Gateway : in out Druss.Gateways.Gateway_Type);

      procedure Execute_One (Gateway : in out Druss.Gateways.Gateway_Type) is
         Box     : Bbox.API.Client_Type;
      begin
         Box.Set_Server (To_String (Gateway.Ip));
         Box.Login (To_String (Gateway.Passwd));
         Ada.Text_IO.Put_Line (Box.Get (Operation));
      end Execute_One;

   begin
      Druss.Gateways.Iterate (Context.Gateways, Execute_One'Access);
   end Execute_Operation;

   --  ------------------------------
   --  Execute a GET operation on the Bbox API and return the raw JSON result.
   --  ------------------------------
   overriding
   procedure Execute (Command   : in Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type) is
      pragma Unreferenced (Name);
   begin
      if Args.Get_Count = 0 then
         Druss.Commands.Driver.Usage (Args);
      end if;
      for I in 1 .. Args.Get_Count loop
         declare
            Operation : constant String := Args.Get_Argument (I);
         begin
            Execute_Operation (Command, Operation, Context);
         end;
      end loop;
   end Execute;

   --  ------------------------------
   --  Write the help associated with the command.
   --  ------------------------------
   overriding
   procedure Help (Command   : in Command_Type;
                   Context   : in out Context_Type) is
      pragma Unreferenced (Command, Context);
   begin
      Put_Line ("get: Execute a GET operation on the Bbox API and print the raw JSON result");
      Put_Line ("Usage: get <operation>");
      New_Line;
      Put_Line ("  The Bbox API operation is called and the raw JSON result is printed.");
      Put_Line ("  Examples:");
      Put_Line ("    get device         Get information about the Bbox");
      Put_Line ("    get hosts          Get the list of hosts detected by the Bbox");
      Put_Line ("    get wan/ip         Get information about the WAN connection");
   end Help;

end Druss.Commands.Get;
