-----------------------------------------------------------------------
--  bbox -- Bbox API
--  Copyright (C) 2017, 2019, 2023 Stephane Carrez
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
with Util.Properties.Basic;
package body Bbox.API is

   use Ada.Strings.Unbounded;

   package Int_Property renames Util.Properties.Basic.Integer_Property;
   package Bool_Property renames Util.Properties.Basic.Boolean_Property;

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
      return "https://" & To_String (Client.Server) & "/api/v1/" & Operation;
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
            Client.Is_Logged := True;
         end if;
      end Process;

   begin
      Log.Debug ("Login to {0}", URI);
      Client.Is_Logged := False;
      Client.Http.Set_Header ("Cookie", "");
      Client.Http.Add_Header ("X-Requested-By", "Bbox Ada Api");
      Client.Http.Set_Timeout (10.0);
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
      if Content'Length = 0 then
         return Content;
      end if;
      while Last > Content'First and then Content (Last) = ASCII.LF loop
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
      Client.Http.Set_Timeout (10.0);
      Client.Http.Get (URI, Response);
      if Response.Get_Status = 200 then
         Util.Properties.JSON.Parse_JSON (Result, Strip_Unecessary_Array (Response.Get_Body));
      end if;
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
      Client.Http.Set_Timeout (10.0);
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
      Client.Http.Set_Timeout (10.0);
      Client.Http.Put (URI, Params, Response);
   end Put;

   procedure Refresh_Token (Client : in out Client_Type) is
      use type Ada.Calendar.Time;

      Now      : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Response : Util.Http.Clients.Response;
      Tokens   : Util.Properties.Manager;
   begin
      if Length (Client.Token) /= 0 and then Client.Expires > Now then
         return;
      end if;
      Log.Debug ("Get bbox token");
      Client.Http.Set_Timeout (10.0);
      Client.Http.Get (Client.Get_URI ("device/token"), Response);
      Util.Properties.JSON.Parse_JSON (Tokens, Strip_Unecessary_Array (Response.Get_Body));
      Client.Token := To_Unbounded_String (Tokens.Get ("device.token", ""));
      Client.Expires := Ada.Calendar.Clock + 60.0;
   end Refresh_Token;

   --  Execute a POST operation on the Bbox API to change some parameter.
   procedure Post (Client    : in out Client_Type;
                   Operation : in String;
                   Params    : in String) is
      URI      : constant String := Client.Get_URI (Operation);
      Response : Util.Http.Clients.Response;
   begin
      Log.Debug ("Post {0}", URI);
      Client.Refresh_Token;
      Client.Http.Set_Timeout (10.0);
      Client.Http.Post (URI & "?btoken=" & To_String (Client.Token), Params, Response);
   end Post;

   --  Iterate over a JSON array flattened in the properties.
   procedure Iterate (Props : in Util.Properties.Manager;
                      Name  : in String;
                      Process : access procedure (P : in Util.Properties.Manager;
                                                  Base : in String)) is
      Count : constant Integer := Int_Property.Get (Props, Name & ".length", 0);
   begin
      for I in 0 .. Count loop
         declare
            Base : constant String := Name & "." & Util.Strings.Image (I);
         begin
            Process (Props, Base);
         end;
      end loop;
   end Iterate;

end Bbox.API;
