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
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
with Util.Properties;
with Util.Refs;
with Bbox.API;
package Druss.Gateways is

   Not_Found : exception;

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

      --  The Bbox serial number.
      Serial : Ada.Strings.Unbounded.Unbounded_String;

      --  Directory that contains the images.
      Images : Ada.Strings.Unbounded.Unbounded_String;

      --  Whether the gateway entry is enabled or not.
      Enable : Boolean := True;

      --  The gateway state.
      State  : Gateway_State;

      --  Current WAN information (from api/v1/wan).
      Wan    : Util.Properties.Manager;

      --  Current LAN information (from api/v1/lan).
      Lan    : Util.Properties.Manager;

      --  Wireless information (From api/v1/wireless).
      Wifi   : Util.Properties.Manager;

      --  Voip information (From api/v1/voip).
      Voip   : Util.Properties.Manager;

      --  IPtv information (From api/v1/iptv).
      IPtv   : Util.Properties.Manager;

      --  Hosts information (From api/v1/hosts).
      Hosts  : Util.Properties.Manager;

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

   package Gateway_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => Gateway_Ref,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=",
                                                "="             => "=");

   --  Initalize the list of gateways from the property list.
   procedure Initialize (Config : in Util.Properties.Manager;
                         List   : in out Gateway_Vector);

   --  Save the list of gateways.
   procedure Save_Gateways (Config : in out Util.Properties.Manager;
                            List   : in Druss.Gateways.Gateway_Vector);

   --  Refresh the information by using the Bbox API.
   procedure Refresh (Gateway : in Gateway_Ref)
     with pre => not Gateway.Is_Null;

   type Iterate_Type is (ITER_ALL, ITER_ENABLE, ITER_DISABLE);

   --  Iterate over the list of gateways and execute the <tt>Process</tt> procedure.
   procedure Iterate (List    : in Gateway_Vector;
                      Mode    : in Iterate_Type := ITER_ALL;
                      Process : not null access procedure (G : in out Gateway_Type));

   --  Find the gateway with the given IP address.
   function Find_IP (List : in Gateway_Vector;
                     IP   : in String) return Gateway_Ref;

end Druss.Gateways;
