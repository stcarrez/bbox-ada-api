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
with Util.Properties.JSON;
with Util.Log.Loggers;
with Util.Strings;
package body Bbox.API is

   use Ada.Strings.Unbounded;

   --  The logger
   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Bbox.API");

   function Strip_Unecessary_Array (Content : in String) return String;

   --  ------------------------------
   --  Set the server IP address.
   --  ------------------------------
   procedure Set_Server (Client : in out Client_Type;
                         Server : in String) is
   begin
      Log.Debug ("Using bbox server {0}", Server);
      Client.Server := Ada.Strings.Unbounded.To_Unbounded_String (Server);
   end Set_Server;

   --  ------------------------------
   --  Internal operation to get the URI based on the operation being called.
   --  ------------------------------
   function Get_URI (Client    : in Client_Type;
                     Operation : in String) return String is
   begin
      return "http://" & To_String (Client.Server) & "/api/v1/" & Operation;
   end Get_URI;

   --  ------------------------------
   --  Login to the server Bbox API with the password.
   --  ------------------------------
   procedure Login (Client   : in out Client_Type;
                    Password : in String) is
      procedure Process (Name, Value : in String);

      URI      : constant String := Client.Get_URI ("login");
      Response : Util.Http.Clients.Response;

      procedure Process (Name, Value : in String) is
         Pos, Last : Natural;
      begin
         if Name = "Set-Cookie" then
            Pos := Util.Strings.Index (Value, '=');
            if Pos = 0 then
               return;
            end if;
            Last := Util.Strings.Index (Value, ';');
            if Last = 0 or else Last < Pos then
               return;
            end if;
            Client.Http.Set_Header ("Cookie", Value (Value'First .. Last - 1));
         end if;
      end Process;

   begin
      Log.Debug ("Login to {0}", URI);
      Client.Http.Add_Header ("X-Requested-By", "Bbox Ada Api");
      Client.Http.Post (URI, "password=" & Password, Response);
      if Response.Get_Status = Util.Http.SC_OK then
         Response.Iterate_Headers (Process'Access);
      else
         Log.Error ("Connection and login to {0} failed", URI);
      end if;
   end Login;

   --  ------------------------------
   --  Strip [] for some Json content.
   --  We did a mistake when we designed the Bbox API and used '[' ... ']' arrays
   --  for most of the JSON result.  Strip that unecessary array.
   --  ------------------------------
   function Strip_Unecessary_Array (Content : in String) return String is
      Last      : Natural := Content'Last;
   begin
      while Last > Content'First and Content (Last) = ASCII.LF loop
         Last := Last - 1;
      end loop;
      if Content (Content'First) = '[' and Content (Last) = ']' then
         return Content (Content'First + 1 .. Last - 1);
      else
         return Content;
      end if;
   end Strip_Unecessary_Array;

   --  ------------------------------
   --  Execute a GET operation on the Bbox API to retrieve the result into the property list.
   --  ------------------------------
   procedure Get (Client    : in out Client_Type;
                  Operation : in String;
                  Result    : in out Util.Properties.Manager) is
      URI      : constant String := Client.Get_URI (Operation);
      Response : Util.Http.Clients.Response;
   begin
      Log.Debug ("Get {0}", URI);
      Client.Http.Get (URI, Response);
      Util.Properties.JSON.Parse_JSON (Result, Strip_Unecessary_Array (Response.Get_Body));
   end Get;

   --  ------------------------------
   --  Execute a GET operation on the Bbox API to retrieve the JSON result and return it.
   --  ------------------------------
   function Get (Client    : in out Client_Type;
                 Operation : in String) return String is
      URI      : constant String := Client.Get_URI (Operation);
      Response : Util.Http.Clients.Response;
   begin
      Log.Debug ("Get {0}", URI);
      Client.Http.Get (URI, Response);
      return Response.Get_Body;
   end Get;

   --  ------------------------------
   --  Execute a PUT operation on the Bbox API to change some parameter.
   --  ------------------------------
   procedure Put (Client    : in out Client_Type;
                  Operation : in String;
                  Params    : in String) is
      URI      : constant String := Client.Get_URI (Operation);
      Response : Util.Http.Clients.Response;
   begin
      Log.Debug ("Put {0}", URI);
      Client.Http.Put (URI, Params, Response);
   end Put;

end Bbox.API;
