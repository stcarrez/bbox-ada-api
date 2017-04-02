-----------------------------------------------------------------------
--  druss-gateways -- Gateway management
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
with Ada.Containers.Vectors;
with Util.Properties;
with Util.Refs;
with Bbox.API;
package Druss.Gateways is

   type State_Type is (IDLE, READY, BUSY);

   protected type Gateway_State is
      function Get_State return State_Type;

   private
      State : State_Type := IDLE;
   end Gateway_State;

   type Gateway_Type is limited new Util.Refs.Ref_Entity with record
      --  Gateway IP address.
      Ip     : Ada.Strings.Unbounded.Unbounded_String;

      --  API password.
      Passwd : Ada.Strings.Unbounded.Unbounded_String;

      --  Directory that contains the images.
      Images : Ada.Strings.Unbounded.Unbounded_String;

      --  The gateway state.
      State  : Gateway_State;

      --  Current WAN information (from api/v1/wan).
      Wan    : Util.Properties.Manager;

      --  Current LAN information (from api/v1/lan).
      Lan    : Util.Properties.Manager;

      --  Wireless information (From api/v1/wireless).
      Wifi   : Util.Properties.Manager;

      --  Current Device information (from api/v1/device).
      Device : Util.Properties.Manager;

      --  The Bbox API client.
      Client : Bbox.API.Client_Type;
   end record;
   type Gateway_Type_Access is access all Gateway_Type;

   --  Refresh the information by using the Bbox API.
   procedure Refresh (Gateway : in out Gateway_Type);

   package Gateway_Refs is
     new Util.Refs.References (Element_Type   => Gateway_Type,
                               Element_Access => Gateway_Type_Access);

   subtype Gateway_Ref is Gateway_Refs.Ref;

   function "=" (Left, Right : in Gateway_Ref) return Boolean;

   package Gateway_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Gateway_Ref,
                                 "="          => "=");

   subtype Gateway_Vector is Gateway_Vectors.Vector;
   subtype Gateway_Cursor is Gateway_Vectors.Cursor;

   --  Initalize the list of gateways from the property list.
   procedure Initialize (Config : in Util.Properties.Manager;
                         List   : in out Gateway_Vector);

   --  Refresh the information by using the Bbox API.
   procedure Refresh (Gateway : in Gateway_Ref)
     with pre => not Gateway.Is_Null;

   --  Iterate over the list of gateways and execute the <tt>Process</tt> procedure.
   procedure Iterate (List    : in Gateway_Vector;
                      Process : not null access procedure (G : in out Gateway_Type));

end Druss.Gateways;
