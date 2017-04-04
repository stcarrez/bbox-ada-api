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
with Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with Util.Files;
with Util.Log.Loggers;
with Util.Systems.Os;
with Util.Properties;
package body Druss.Config is

   use Ada.Strings.Unbounded;

   --  The logger
   Log   : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Druss.Config");

   function Get_Default_Path return String;
   procedure Save;

   Cfg      : Util.Properties.Manager;
   Cfg_Path : Ada.Strings.Unbounded.Unbounded_String;

   --  ------------------------------
   --  Get the default configuration path.
   --  ------------------------------
   function Get_Default_Path return String is
      Home     : constant String := Ada.Environment_Variables.Value ("HOME");
      Def_Path : constant String := Util.Files.Compose (Home, ".config/druss/druss.properties");
   begin
      return Def_Path;
   end Get_Default_Path;

   --  ------------------------------
   --  Initialize the configuration.
   --  ------------------------------
   procedure Initialize (Path : in String) is
      Def_Path : constant String := Get_Default_Path;
   begin
      if Path'Length > 0 and then Ada.Directories.Exists (Path) then
         Cfg.Load_Properties (Path);
         Cfg_Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
      elsif Ada.Directories.Exists (Def_Path) then
         Cfg.Load_Properties (Def_Path);
         Cfg_Path := Ada.Strings.Unbounded.To_Unbounded_String (Def_Path);
      end if;
   end Initialize;

   procedure Save is
      Path : constant String
        := (if Length (Cfg_Path) = 0 then Get_Default_Path else To_String (Cfg_Path));
      Dir  : constant String := Ada.Directories.Containing_Directory (Path);
      P    : Interfaces.C.Strings.chars_ptr;
   begin
      if not Ada.Directories.Exists (Path) then
         Ada.Directories.Create_Path (Dir);
         P := Interfaces.C.Strings.New_String (Dir);
         if Util.Systems.Os.Sys_Chmod (P, 8#0700#) /= 0 then
            Log.Error ("Cannot set the permission of {0}", Dir);
         end if;
         Interfaces.C.Strings.Free (P);
      end if;
      Cfg.Save_Properties (Path);

      --  Set the permission on the file to allow only the user to read/write that file.
      P := Interfaces.C.Strings.New_String (Path);
      if Util.Systems.Os.Sys_Chmod (P, 8#0600#) /= 0 then
         Log.Error ("Cannot set the permission of {0}", Path);
      end if;
      Interfaces.C.Strings.Free (P);
   end Save;

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

   --  ------------------------------
   --  Save the list of gateways.
   --  ------------------------------
   procedure Save_Gateways (List  : in Druss.Gateways.Gateway_Vector) is
   begin
      Druss.Gateways.Save_Gateways (Cfg, List);
      Save;
   end Save_Gateways;

end Druss.Config;
