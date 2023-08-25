%%% Copyright (c) 2011-2023 Michael Santos <michael.santos@gmail.com>. All
%%% rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice,
%%% this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc tunctl is an Erlang API for creating and using TUN/TAP interfaces.
%%
%% tunctl does the actual tun/tap device manipulation. Some functions take
%% a device name, others a file descriptor. It is up to the caller to make
%% sure the file descriptors are closed (the device will disappear after
%% the fd is closed if the device is not persistent).
-module(tunctl).

-include("tuntap.hrl").

-include_lib("procket/include/ioctl.hrl").
-include_lib("procket/include/procket.hrl").

-callback create(binary(), proplists:proplist()) -> {ok, fd(), binary()} | {error, file:posix()}.
-callback persist(fd(), boolean()) -> ok | {error, file:posix()}.
-callback owner(fd(), integer()) -> ok | {error, file:posix()}.
-callback group(fd(), integer()) -> ok | {error, file:posix()}.

-export([
    create/0, create/1, create/2,
    persist/2,
    owner/2,
    group/2,
    up/2, up/3,
    dstaddr/2,
    broadcast/2,
    down/1,

    mtu/3,

    header/1
]).

-export([
    ioctl/3,
    cmd/1
]).

-type fd() :: integer().
-type uint16_t() :: 0..16#ffff.

-export_type([
    fd/0,
    uint16_t/0
]).

%%--------------------------------------------------------------------
%%% Exports
%%--------------------------------------------------------------------

%% @doc Create the tap0 device.
%%
%% == Examples ==
%%
%% ```
%% 1> tunctl:create().
%% {ok,22,<<"tap0">>}
%% '''
%%
%% ```
%% $ ip link show tap0
%% 17: tap0: <BROADCAST,MULTICAST> mtu 1500 qdisc noop state DOWN mode DEFAULT group default qlen 1000
%%     link/ether 16:ae:f3:36:be:3f brd ff:ff:ff:ff:ff:ff
%% '''
-spec create() -> {ok, fd(), binary()} | {error, file:posix()}.
create() ->
    create(<<>>).

%% @doc Create a named tap device.
%%
%% == Examples ==
%%
%% ```
%% 1> tunctl:create(<<"tuncer">>).
%% {ok,22,<<"tuncer">>}
%% '''
%%
%% ```
%% $ ip link show tuncer
%% 18: tuncer: <BROADCAST,MULTICAST> mtu 1500 qdisc noop state DOWN mode DEFAULT group default qlen 1000
%%     link/ether 9e:bf:d9:4a:06:72 brd ff:ff:ff:ff:ff:ff
%% '''
-spec create(binary()) -> {ok, fd(), binary()} | {error, file:posix()}.
create(Ifname) ->
    create(Ifname, [tap, no_pi]).

%% @doc Create a tuntap device.
%%
%%  Device is the TUN/TAP device name. If a device name is empty,
%%  the TUN/TAP driver will choose one (for tap devices,
%%  starting from `tap0'; for tun devices, beginning from `tun0').
%%
%%  Options contains a list of flags.
%%
%%      tun: create a tun interface
%%
%%      tap: create a tap interface
%%
%%      no_pi: do not prepend the data with a 4 byte header describing
%%             the physical interface
%%
%%  The options default to `[tap, no_pi, {active, false}]'.
%%
%% == Examples ==
%%
%% ```
%% 1> tunctl:create(<<"tuncer">>, [tun, no_pi]).
%% {ok,22,<<"tuncer">>}
%% '''
%%
%% ```
%% $ ip link show tun0
%% 19: tuncer: <POINTOPOINT,MULTICAST,NOARP> mtu 1500 qdisc noop state DOWN mode DEFAULT group default qlen 500
%%     link/none
%% '''
-spec create(binary(), proplists:proplist()) -> {ok, fd(), binary()} | {error, file:posix()}.
create(Ifname, Opt) when byte_size(Ifname) < ?IFNAMSIZ, is_list(Opt) ->
    Module = os(),
    Module:create(Ifname, Opt).

%% @doc Set the interface to exist after the Erlang process exits.
%%
%% == Support ==
%%
%% * Linux
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, FD, Dev} = tunctl:create(<<"tuncer">>, [tun, no_pi]).
%% {ok,22,<<"tuncer">>}
%% 3> tunctl:persist(FD, true).
%% ok
%% '''
-spec persist(fd(), boolean()) -> ok | {error, file:posix()}.
persist(FD, Status) ->
    Module = os(),
    Module:persist(FD, bool(Status)).

%% @doc Set the UID owning the interface.
%%
%% == Support ==
%%
%% * Linux
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, FD, Dev} = tunctl:create(<<"tuncer">>).
%% {ok,22,<<"tuncer">>}
%% 2> tunctl:owner(FD, 1000).
%% ok
%% '''
-spec owner(fd(), integer()) -> ok | {error, file:posix()}.
owner(FD, Owner) when is_integer(FD), is_integer(Owner) ->
    Module = os(),
    Module:owner(FD, Owner).

%% @doc Set the GID owning the interface.
%%
%% == Support ==
%%
%% * Linux
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, FD, Dev} = tunctl:create(<<"tuncer">>).
%% {ok,22,<<"tuncer">>}
%% 2> tunctl:group(FD, 1000).
%% ok
%% '''
-spec group(fd(), integer()) -> ok | {error, file:posix()}.
group(FD, Group) when is_integer(FD), is_integer(Group) ->
    Module = os(),
    Module:group(FD, Group).

%% @doc Configure a TUN/TAP device using the default netmask and broadcast for the network.
%%
%% Configure the interface just like ifconfig except with fewer features
%% and no error checking.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, FD, Dev} = tunctl:create(<<"tuncer">>).
%% {ok,22,<<"tuncer">>}
%% 2> tunctl:up(Dev, {127,8,8,8}).
%% ok
%% '''
%%
%% ```
%% $ ip a show dev tuncer
%% 24: tuncer: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UNKNOWN group default qlen 1000
%%     link/ether 52:0a:b2:71:f2:cd brd ff:ff:ff:ff:ff:ff
%%     inet 127.8.8.8/32 brd 127.255.255.255 scope host tuncer
%%        valid_lft forever preferred_lft forever
%%     inet6 fe80::500a:b2ff:fe71:f2cd/64 scope link
%%        valid_lft forever preferred_lft forever
%% '''
-spec up(binary(), inet:socket_address()) -> ok | {error, file:posix()}.
up(Dev, {A, B, C, D}) ->
    up(Dev, {A, B, C, D}, 32);
up(Dev, {A, B, C, D, E, F, G, H}) ->
    up(Dev, {A, B, C, D, E, F, G, H}, 64).

%% @doc Configure a TUN/TAP device with a netmask.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, FD, Dev} = tunctl:create(<<"tuncer">>).
%% {ok,22,<<"tuncer">>}
%% 2> tunctl:up(Dev, {127,8,8,8}, 24).
%% ok
%% '''
%%
%% ```
%% $ ip a show dev tuncer
%% 25: tuncer: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UNKNOWN group default qlen 1000
%%     link/ether 5e:6a:6a:a2:67:0f brd ff:ff:ff:ff:ff:ff
%%     inet 127.8.8.8/24 brd 127.8.8.255 scope host tuncer
%%        valid_lft forever preferred_lft forever
%%     inet6 fe80::5c6a:6aff:fea2:670f/64 scope link
%%        valid_lft forever preferred_lft forever
%% '''
-spec up(binary(), inet:socket_address(), 0..32) -> ok | {error, file:posix()}.
up(Dev, Addr, Mask) when byte_size(Dev) < ?IFNAMSIZ, is_integer(Mask) ->
    Module = os(),
    case Module of
        tunctl_linux -> tunctl_linux:up(Dev, Addr, Mask);
        _ -> os_up(Dev, Addr, Mask)
    end.

%% @doc Configure the remote address for a TUN/TAP device in point-to-point mode.
%%
%% == Support ==
%%
%% * Linux (IPv4 addresses only)
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, FD, Dev} = tunctl:create(<<"tuncer">>).
%% {ok,22,<<"tuncer">>}
%% 2> tunctl:up(Dev, {127,8,8,1}).
%% ok
%% 3> tunctl:dstaddr(Dev, {127,8,8,2}).
%% ok
%% '''
%%
%% ```
%% $ ip a show tun0
%% 26: tuncer: <BROADCAST,MULTICAST,NOTRAILERS,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UNKNOWN group default qlen 1000
%%     link/ether 8a:0f:74:4e:5e:1c brd ff:ff:ff:ff:ff:ff
%%     inet 127.8.8.1 peer 127.8.8.2/32 brd 127.255.255.255 scope host tuncer
%%        valid_lft forever preferred_lft forever
%%     inet6 fe80::880f:74ff:fe4e:5e1c/64 scope link
%%        valid_lft forever preferred_lft forever
%% '''
-spec dstaddr(binary(), inet:socket_address()) -> ok | {error, file:posix()}.
dstaddr(Dev, Addr) when byte_size(Dev) < ?IFNAMSIZ ->
    Module = os(),
    case Module of
        tunctl_linux ->
            tunctl_linux:dstaddr(Dev, Addr);
        _ ->
            % TODO: add support
            {error, enoent}
    end.

%% @doc Set the MTU (maximum transmission unit) for the TUN/TAP device.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, FD, Dev} = tunctl:create(<<"tuncer">>).
%% {ok,22,<<"tuncer">>}
%% 2> tunctl:up(Dev, {127,8,8,1}).
%% ok
%% 3> tunctl:mtu(FD, Dev, 1400).
%% {ok,22,<<"tuncer">>}
%% '''
-spec mtu(fd(), binary(), integer()) -> {ok, tunctl:fd(), binary()} | {error, file:posix()}.
mtu(FD, Ifname, MTU) ->
    Module = os(),
    case Module of
        tunctl_linux ->
            tunctl_linux:mtu(FD, Ifname, MTU);
        _ ->
            % TODO: add support
            {error, enoent}
    end.

%% @doc Configure the broadcast address for a TUN/TAP device.
%%
%% == Support ==
%%
%% * Linux (IPv4 addresses only)
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, FD, Dev} = tunctl:create(<<"tuncer">>).
%% {ok,22,<<"tuncer">>}
%% 2> tunctl:up(Dev, {127,8,8,1}).
%% ok
%% 3> tunctl:broadcast(Dev, {127,8,8,3}).
%% ok
%% '''
%%
%% ```
%% 28: tuncer: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UNKNOWN group default qlen 1000
%%     link/ether 6e:f2:e9:31:d4:3a brd ff:ff:ff:ff:ff:ff
%%     inet 127.8.8.1/32 brd 127.8.8.3 scope host tuncer
%%        valid_lft forever preferred_lft forever
%%     inet6 fe80::6cf2:e9ff:fe31:d43a/64 scope link
%%        valid_lft forever preferred_lft forever
%% '''
-spec broadcast(binary(), inet:socket_address()) -> ok | {error, file:posix()}.
broadcast(Dev, Addr) when byte_size(Dev) < ?IFNAMSIZ ->
    Module = os(),
    case Module of
        tunctl_linux ->
            tunctl_linux:broadcast(Dev, Addr);
        _ ->
            % TODO: add support
            {error, enoent}
    end.

%% @doc Unconfigure a TUN/TAP device.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, FD, Dev} = tunctl:create(<<"tuncer">>).
%% {ok,22,<<"tuncer">>}
%% 2> tunctl:up(Dev, {127,8,8,1}).
%% ok
%% 4> tunctl:down(Dev).
%% ok
%% '''
-spec down(binary()) -> ok | {error, file:posix()}.
down(Dev) when byte_size(Dev) < ?IFNAMSIZ ->
    Module = os(),
    case Module of
        tunctl_linux -> tunctl_linux:down(Dev);
        _ -> os_down(Dev)
    end.

%% @doc Parse TUN/TAP packet header.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, FD, Dev} = tunctl:create(<<"tuncer">>).
%% {ok,22,<<"tuncer">>}
%% 2> tunctl:up(Dev, {127,8,8,1}).
%% ok
%% 3> {ok, Bin} = procket:read(FD, 1024).
%% {ok,<<51,51,0,0,0,22,254,45,152,87,204,21,134,221,96,0,0,
%%       0,0,36,0,1,0,0,0,0,0,...>>}
%% 4> tunctl:header(Bin).
%% {tun_pi,13107,0,
%%         <<0,22,254,45,152,87,204,21,134,221,96,0,0,0,0,36,0,1,0,
%%           0,0,0,0,0,0,...>>}
%% '''
-spec header(binary()) -> {tun_pi, uint16_t(), uint16_t()} | {error, file:posix()}.
header(Packet) ->
    Module = os(),
    Module:header(Packet).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @private
ioctl(FD, Request, Opt) ->
    case procket:ioctl(FD, Request, Opt) of
        {ok, _} -> ok;
        Error -> Error
    end.

%% @private
cmd(Cmd) ->
    case os:cmd(Cmd) of
        [] -> ok;
        Error -> {error, Error}
    end.

bool(true) -> 1;
bool(false) -> 0.

os() ->
    case os:type() of
        {unix, linux} -> tunctl_linux;
        {unix, darwin} -> tunctl_darwin;
        {unix, freebsd} -> tunctl_freebsd;
        {unix, netbsd} -> tunctl_netbsd
    end.

%% Shell out to ifconfig on systems where ioctl requires
%% root privs (or native code hasn't been written yet).
os_up(Dev, {A, B, C, D}, Mask) ->
    Cmd =
        "sudo ifconfig " ++
            binary_to_list(Dev) ++
            " " ++
            inet_parse:ntoa({A, B, C, D}) ++
            "/" ++ integer_to_list(Mask) ++ " up",
    cmd(Cmd);
os_up(Dev, {A, B, C, D, E, F, G, H}, Mask) ->
    Cmd =
        "sudo ifconfig " ++
            binary_to_list(Dev) ++
            " inet6 add " ++
            inet_parse:ntoa({A, B, C, D, E, F, G, H}) ++
            "/" ++ integer_to_list(Mask) ++ " up",
    cmd(Cmd).

os_down(Dev) ->
    % BSD systems don't destroy the interface so clean up
    % any IPv6 addresses
    _ = os_ipv6_down(Dev),

    Cmd = "sudo ifconfig " ++ binary_to_list(Dev) ++ " down",
    cmd(Cmd).

os_ipv6_down(Dev) ->
    case os:type() of
        {unix, linux} ->
            ok;
        {unix, BSD} when BSD == freebsd; BSD == darwin ->
            os_ipv6_down_1(Dev)
    end.

os_ipv6_down_1(Ifname) ->
    Dev = binary_to_list(Ifname),
    {ok, Devs} = inet:getifaddrs(),
    Attr = proplists:get_value(Dev, Devs),

    [
        os_ipv6_down_2(Dev, Addr)
     || Addr <-
            proplists:get_all_values(addr, Attr),
        tuple_size(Addr) == 8
    ].

os_ipv6_down_2(Dev, Addr) ->
    Cmd =
        "sudo ifconfig " ++
            Dev ++
            " inet6 " ++
            inet_parse:ntoa(Addr) ++ " -alias",
    cmd(Cmd).
