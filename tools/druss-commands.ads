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
with Druss.Gateways;
package Druss.Commands is

   type Context_Type is limited record
      Gateways : Druss.Gateways.Gateway_Vector;
   end record;

   package Drivers is
     new Util.Commands.Drivers (Context_Type => Context_Type,
                                Driver_Name  => "druss-drivers");

   subtype Argument_List is Util.Commands.Argument_List;

   Driver : Drivers.Driver_Type;

   procedure Initialize;

end Druss.Commands;
