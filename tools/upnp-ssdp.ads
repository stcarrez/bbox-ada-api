-----------------------------------------------------------------------
--  upnp-ssdp -- UPnP SSDP operations
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
with Ada.Finalization;
with Util.Strings.Sets;
private with GNAT.Sockets;
package UPnP.SSDP is

   type Scanner_Type is limited new Ada.Finalization.Limited_Controlled with private;

   --  Find the IPv4 addresses of the network interfaces.
   procedure Find_IPv4_Addresses (Scanner : in out Scanner_Type;
                                  IPs     : out Util.Strings.Sets.Set);

   --  Initialize the SSDP scanner by opening the UDP socket.
   procedure Initialize (Scanner : in out Scanner_Type);

   --  Send the SSDP discovery UDP packet on the UPnP multicast group 239.255.255.250.
   --  Set the "ST" header to the given target.  The UDP packet is sent on each interface
   --  whose IP address is defined in the set <tt>IPs</tt>.
   procedure Send_Discovery (Scanner : in out Scanner_Type;
                             Target  : in String;
                             IPs     : in Util.Strings.Sets.Set);

   --  Receive the UPnP SSDP discovery messages for the given target.
   --  Call the <tt>Process</tt> procedure for each of them.  The <tt>Desc</tt> parameter
   --  represents the UPnP location header which gives the UPnP XML root descriptor.
   --  Wait at most the given time.
   procedure Discover (Scanner : in out Scanner_Type;
                       Target  : in String;
                       Process : not null access procedure (Desc : in String);
                       Wait    : in Duration);

   --  Release the socket.
   overriding
   procedure Finalize (Scanner : in out Scanner_Type);

private

   type Scanner_Type is limited new Ada.Finalization.Limited_Controlled with record
      Socket : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
   end record;

end UPnP.SSDP;
