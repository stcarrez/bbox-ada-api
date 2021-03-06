-----------------------------------------------------------------------
--  bbox -- Bbox API
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
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Util.Properties;
with Util.Http.Clients;
package Bbox.API is

   type Client_Type is tagged limited private;

   --  Set the server IP address.
   procedure Set_Server (Client : in out Client_Type;
                         Server : in String);

   --  Login to the server Bbox API with the password.
   procedure Login (Client   : in out Client_Type;
                    Password : in String);

   --  Execute a GET operation on the Bbox API to retrieve the result into the property list.
   procedure Get (Client    : in out Client_Type;
                  Operation : in String;
                  Result    : in out Util.Properties.Manager);

   --  Execute a PUT operation on the Bbox API to change some parameter.
   procedure Put (Client    : in out Client_Type;
                  Operation : in String;
                  Params    : in String);

   --  Execute a POST operation on the Bbox API to change some parameter.
   procedure Post (Client    : in out Client_Type;
                   Operation : in String;
                   Params    : in String);

   --  Execute a GET operation on the Bbox API to retrieve the JSON result and return it.
   function Get (Client    : in out Client_Type;
                 Operation : in String) return String;

   --  Iterate over a JSON array flattened in the properties.
   procedure Iterate (Props : in Util.Properties.Manager;
                      Name  : in String;
                      Process : access procedure (P : in Util.Properties.Manager;
                                                  Base : in String));

private

   --  Internal operation to get the URI based on the operation being called.
   function Get_URI (Client    : in Client_Type;
                     Operation : in String) return String;

   type Client_Type is tagged limited record
      Password  : Ada.Strings.Unbounded.Unbounded_String;
      Server    : Ada.Strings.Unbounded.Unbounded_String;
      Auth      : Ada.Strings.Unbounded.Unbounded_String;
      Is_Logged : Boolean := False;
      Http      : Util.Http.Clients.Client;
      Token     : Ada.Strings.Unbounded.Unbounded_String;
      Expires   : Ada.Calendar.Time;
   end record;

   procedure Refresh_Token (Client : in out Client_Type);

end Bbox.API;
