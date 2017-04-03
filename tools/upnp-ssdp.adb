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
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Util.Log.Loggers;

package body UPnP.SSDP is
   use type Ada.Streams.Stream_Element_Offset;
   use type Ada.Streams.Stream_Element;

   --  The logger
   Log   : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("UPnP.SSDP");

   Group : constant String := "239.255.255.250";

   procedure Send (Socket  : in GNAT.Sockets.Socket_Type;
                   Content : in String;
                   To      : access GNAT.Sockets.Sock_Addr_Type);

   --  ------------------------------
   --  Initialize the SSDP scanner by opening the UDP socket.
   --  ------------------------------
   procedure Initialize (Scanner : in out Scanner_Type) is
      Address      : aliased GNAT.Sockets.Sock_Addr_Type;
   begin
      Log.Info ("Discovering gateways on the network");

      Address.Addr := GNAT.Sockets.Any_Inet_Addr;
      GNAT.Sockets.Create_Socket (Scanner.Socket, GNAT.Sockets.Family_Inet,
                                  GNAT.Sockets.Socket_Datagram);
      GNAT.Sockets.Bind_Socket (Scanner.Socket, Address);
      GNAT.Sockets.Set_Socket_Option (Scanner.Socket, GNAT.Sockets.IP_Protocol_For_IP_Level,
                                      (GNAT.Sockets.Add_Membership, GNAT.Sockets.Inet_Addr (Group),
                                       GNAT.Sockets.Any_Inet_Addr));
   end Initialize;

   procedure Send (Socket  : in GNAT.Sockets.Socket_Type;
                   Content : in String;
                   To      : access GNAT.Sockets.Sock_Addr_Type) is
      Last : Ada.Streams.Stream_Element_Offset;
      Buf  : Ada.Streams.Stream_Element_Array (1 .. Content'Length);
      for Buf'Address use Content'Address;
      pragma Import (Ada, Buf);
      pragma Unreferenced (Last);
   begin
      GNAT.Sockets.Send_Socket (Socket, Buf, Last, To);
   end Send;


   --  ------------------------------
   --  Send the SSDP discovery UDP packet on the UPnP multicast group 239.255.255.250.
   --  Set the "ST" header to the given target.  The UDP packet is sent on each interface
   --  whose IP address is defined in the set <tt>IPs</tt>.
   --  ------------------------------
   procedure Send_Discovery (Scanner : in out Scanner_Type;
                             Target  : in String;
                             IPs     : in Util.Strings.Sets.Set) is
      Address : aliased GNAT.Sockets.Sock_Addr_Type;
   begin
      Address.Addr := GNAT.Sockets.Inet_Addr (Group);
      Address.Port := 1900;
      for IP of IPs loop
         GNAT.Sockets.Set_Socket_Option (Scanner.Socket, GNAT.Sockets.IP_Protocol_For_IP_Level,
                                         (GNAT.Sockets.Multicast_If,
                                          GNAT.Sockets.Inet_Addr (IP)));

         Log.Debug ("Sending SSDP search for IGD device from {0}", IP);
         Send (Scanner.Socket, "M-SEARCH * HTTP/1.1" & ASCII.CR & ASCII.LF &
                 "HOST: 239.255.255.250:1900" & ASCII.CR & ASCII.LF &
                 "ST: " & Target & ASCII.CR & ASCII.LF &
                 "MAN: ""ssdp:discover""" & ASCII.CR & ASCII.LF &
                 "MX: 2" & ASCII.CR & ASCII.LF, Address'Access);
      end loop;
   end Send_Discovery;

   procedure Receive (Socket : in GNAT.Sockets.Socket_Type;
                      Target : in String;
                      Desc   : out Ada.Strings.Unbounded.Unbounded_String) is
      function Get_Line return String;

      Data         : Ada.Streams.Stream_Element_Array (0 .. 1500);
      Last         : Ada.Streams.Stream_Element_Offset;
      Pos   : Ada.Streams.Stream_Element_Offset := Data'First;

      function Get_Line return String is
         First : constant Ada.Streams.Stream_Element_Offset := Pos;
      begin
         while Pos <= Last loop
            if Data (Pos) = 16#0D# and then Pos + 1 <= Last and then Data (Pos + 1) = 16#0A# then
               declare
                  Result : String (1 .. Natural (Pos - First));
                  P      : Natural := 1;
               begin
                  for I in First .. Pos - 1 loop
                     Result (P) := Character'Val (Data (I));
                     P := P + 1;
                  end loop;
                  Log.Debug ("L: {0}", Result);
                  Pos := Pos + 2;
                  return Result;
               end;
            end if;
            Pos := Pos + 1;
         end loop;
         return "";
      end Get_Line;

   begin
      Desc := Ada.Strings.Unbounded.To_Unbounded_String ("");
      GNAT.Sockets.Receive_Socket (Socket, Data, Last);
      if Get_Line /= "HTTP/1.1 200 OK" then
         Log.Debug ("Receive a non HTTP/1.1 response");
         return;
      end if;
      loop
         declare
            Line : constant String := Get_Line;
            Pos  : constant Natural := Util.Strings.Index (Line, ':');
            Pos2 : Natural := Pos + 1;
         begin
            exit when Pos = 0;
            while Pos2 < Line'Last and then Line (Pos2) = ' ' loop
               Pos2 := Pos2 + 1;
            end loop;
            if Line (1 .. Pos) = "ST:" then
               if Line (Pos2 .. Line'Last) /= Target then
                  return;
               end if;
            elsif Line (1 .. Pos) = "LOCATION:" then
               Desc := Ada.Strings.Unbounded.To_Unbounded_String (Line (Pos2 .. Line'Last));
               Log.Debug ("Found a IGD device: {0}", Ada.Strings.Unbounded.To_String (Desc));
               return;
            end if;
         end;
      end loop;

   exception
      when E : others =>
         Log.Error ("Exception received", E);
   end Receive;

   --  ------------------------------
   --  Receive the UPnP SSDP discovery messages for the given target.
   --  Call the <tt>Process</tt> procedure for each of them.  The <tt>Desc</tt> parameter
   --  represents the UPnP location header which gives the UPnP XML root descriptor.
   --  Wait at most the given time.
   --  ------------------------------
   procedure Discover (Scanner : in out Scanner_Type;
                       Target  : in String;
                       Process : not null access procedure (Desc : in String);
                       Wait    : in Duration) is
      use type Ada.Calendar.Time;
      use type GNAT.Sockets.Selector_Status;

      Sel      : GNAT.Sockets.Selector_Type;
      Rset     : GNAT.Sockets.Socket_Set_Type;
      Wset     : GNAT.Sockets.Socket_Set_Type;
      Status   : GNAT.Sockets.Selector_Status;
      Desc     : Ada.Strings.Unbounded.Unbounded_String;
      Deadline : constant Ada.Calendar.Time := Ada.Calendar.Clock + Wait;
      Remain   : Duration;
   begin
      GNAT.Sockets.Create_Selector (Sel);
      loop
         GNAT.Sockets.Empty (Rset);
         GNAT.Sockets.Empty (Wset);
         GNAT.Sockets.Set (Rset, Scanner.Socket);

         Remain := Deadline - Ada.Calendar.Clock;
         exit when Remain < 0.0;
         GNAT.Sockets.Check_Selector (Selector     => Sel,
                                      R_Socket_Set => Rset,
                                      W_Socket_Set => Wset,
                                      Status       => Status,
                                      Timeout      => Remain);
         exit when Status = GNAT.Sockets.Expired;
         Receive (Scanner.Socket, Target, Desc);
         declare
            URI  : constant String := Ada.Strings.Unbounded.To_String (Desc);
            Pos  : constant Natural := Util.Strings.Index (URI, ':');
         begin
            if Pos > 0 and URI (URI'First .. Pos + 2) = "http://" then
               Process (URI);
            end if;
         end;
      end loop;
   end Discover;

   --  ------------------------------
   --  Release the socket.
   --  ------------------------------
   overriding
   procedure Finalize (Scanner : in out Scanner_Type) is
      use type GNAT.Sockets.Socket_Type;
   begin
      if Scanner.Socket /= GNAT.Sockets.No_Socket then
         GNAT.Sockets.Close_Socket (Scanner.Socket);
      end if;
   end Finalize;

end UPnP.SSDP;
