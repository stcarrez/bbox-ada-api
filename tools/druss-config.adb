-----------------------------------------------------------------------
--  druss-config -- Configuration management for Druss
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
with Ada.Environment_Variables;
with Ada.Directories;
with Util.Files;
package body Druss.Config is

   Cfg : Util.Properties.Manager;

   --  ------------------------------
   --  Initialize the configuration.
   --  ------------------------------
   procedure Initialize is
      Home : constant String := Ada.Environment_Variables.Value ("HOME");
      Path : constant String := Util.Files.Compose (Home, ".config/druss/druss.properties");
   begin
      if Ada.Directories.Exists (Path) then
         Cfg.Load_Properties (Path);
      end if;
   end Initialize;

   --  ------------------------------
   --  Get the configuration parameter.
   --  ------------------------------
   function Get (Name : in String) return String is
   begin
      return Cfg.Get (Name);
   end Get;

   --  ------------------------------
   --  Initalize the list of gateways from the configuration file.
   --  ------------------------------
   procedure Get_Gateways (List   : in out Druss.Gateways.Gateway_Vector) is
   begin
      Druss.Gateways.Initialize (Cfg, List);
   end Get_Gateways;

end Druss.Config;