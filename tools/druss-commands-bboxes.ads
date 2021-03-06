-----------------------------------------------------------------------
--  druss-commands-bboxes -- Commands to manage the bboxes
--  Copyright (C) 2017, 2018, 2019, 2021 Stephane Carrez
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

package Druss.Commands.Bboxes is

   type Command_Type is new Druss.Commands.Drivers.Command_Type with null record;

   procedure Discover (Command   : in Command_Type;
                       Context   : in out Context_Type);

   --  Add the bbox with the given IP address.
   procedure Add_Bbox (Command : in Command_Type;
                       IP      : in String;
                       Context : in out Context_Type);

   --  Enable or disable the bbox management by Druss.
   procedure Do_Enable (Command   : in Command_Type;
                        Args      : in Util.Commands.Argument_List'Class;
                        Context   : in out Context_Type);

   --  Set the password to be used by the Bbox API to connect to the box.
   procedure Do_Password (Command   : in Command_Type;
                          Args      : in Util.Commands.Argument_List'Class;
                          Context   : in out Context_Type);

   --  Execute the command with the arguments.  The command name is passed with the command
   --  arguments.
   overriding
   procedure Execute (Command   : in out Command_Type;
                      Name      : in String;
                      Args      : in Druss.Commands.Argument_List'Class;
                      Context   : in out Context_Type);

   --  Write the help associated with the command.
   overriding
   procedure Help (Command   : in out Command_Type;
                   Name      : in String;
                   Context   : in out Context_Type);

end Druss.Commands.Bboxes;
