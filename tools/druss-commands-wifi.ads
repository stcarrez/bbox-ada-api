-----------------------------------------------------------------------
--  druss-commands-wifi -- Wifi related commands
--  Copyright (C) 2017, 2018 Stephane Carrez
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

package Druss.Commands.Wifi is

   type Command_Type is new Druss.Commands.Drivers.Command_Type with null record;

   --  Enable or disable with wifi radio.
   procedure Do_Enable (Command   : in Command_Type;
                        Args      : in Argument_List'Class;
                        Context   : in out Context_Type);

   --  Execute the wifi 'status' command to print the Wifi current status.
   procedure Do_Status (Command   : in Command_Type;
                        Args      : in Argument_List'Class;
                        Context   : in out Context_Type);

   --  Execute a command to control or get status about the Wifi.
   overriding
   procedure Execute (Command   : in out Command_Type;
                      Name      : in String;
                      Args      : in Argument_List'Class;
                      Context   : in out Context_Type);

   --  Write the help associated with the command.
   overriding
   procedure Help (Command   : in Command_Type;
                   Context   : in out Context_Type);

end Druss.Commands.Wifi;
