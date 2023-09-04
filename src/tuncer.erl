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

%% @doc tuncer manages tuntap devices.
-module(tuncer).

-behaviour(gen_server).

-export([
    create/0, create/1, create/2,
    devname/1,
    flags/1,
    getfd/1,
    destroy/1,

    persist/2,
    owner/2,
    group/2,

    read/1, read/2,
    write/2,

    send/2,
    recv/1, recv/2,

    header/1,

    up/2, up/3,
    dstaddr/2,
    broadcast/2,
    down/1,
    mtu/1, mtu/2,

    controlling_process/2,
    setopt/2
]).

-export([start_link/2]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type dev() :: pid().
-type portopt() ::
    {parallelism, boolean()}
    | {busy_limits_port, {non_neg_integer(), non_neg_integer()} | disabled}
    | {busy_limits_msgq, {non_neg_integer(), non_neg_integer()} | disabled}.

-export_type([
    dev/0,
    portopt/0
]).

-record(state, {
    % false, port
    port,
    % port options
    port_opt :: [portopt()],
    % PID of controlling process
    pid :: pid(),
    % TUN/TAP file descriptor
    fd,
    % device name
    dev,
    % TUNSETIFF ifr flags
    flag,
    persist = false :: boolean()
}).

-define(IFNAMSIZ, 16).

%%--------------------------------------------------------------------
%%% Exports
%%--------------------------------------------------------------------

%% @doc Create the tap0 device.
%%
%% == Examples ==
%%
%% ```
%% 1> tuncer:create().
%% {ok,<0.175.0>}
%% '''
%%
%% ```
%% $ ip link show tap0
%% 2: tap0: <BROADCAST,MULTICAST> mtu 1500 qdisc noop state DOWN mode DEFAULT group default qlen 1000
%%     link/ether 32:a9:6a:5b:48:c3 brd ff:ff:ff:ff:ff:ff
%% '''
-spec create() -> gen_server:start_ret().
create() ->
    create(<<>>).

%% @doc Create a named tap device.
%%
%% == Examples ==
%%
%% ```
%% 1> tuncer:create("foo").
%% {ok,<0.175.0>}
%% '''
%%
%% ```
%% $ ip link show foo
%% 3: foo: <BROADCAST,MULTICAST> mtu 1500 qdisc noop state DOWN mode DEFAULT group default qlen 1000
%%     link/ether 9e:f1:90:e2:2c:66 brd ff:ff:ff:ff:ff:ff
%% '''
-spec create(Ifname :: binary() | string()) -> gen_server:start_ret().
create(Ifname) ->
    create(Ifname, [tap, no_pi]).

%% @doc Create a tuntap device.
%%
%%  Device is the TUN/TAP device name. If a device name is empty,
%%  the TUN/TAP driver will choose one (for tap devices,
%%  starting from `tap0'; for tun devices, beginning from `tun0').
%%
%%  When the device is in `{active, true}' mode, data is sent as
%%  messages:
%%
%%      `{tuntap, PID, binary()}'
%%
%%  If an error is encountered:
%%
%%      `{tuntap_error, PID, posix()}'
%%
%%  Retrieving data from devices in `{active, false}' mode can be done
%%  using recv/1,2 or read/1,2.
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
%% 1> tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.181.0>}
%% '''
%%
%% ```
%% $ ip link show tun0
%% 4: tun0: <POINTOPOINT,MULTICAST,NOARP> mtu 1500 qdisc noop state DOWN mode DEFAULT group default qlen 500
%%     link/none
%% '''
-spec create(Ifname :: binary() | string(), Opt :: proplists:proplist()) -> gen_server:start_ret().
create(Ifname, Opt) when is_list(Ifname) ->
    create(list_to_binary(Ifname), Opt);
create(Ifname, Opt) when byte_size(Ifname) < ?IFNAMSIZ, is_list(Opt) ->
    start_link(Ifname, Opt);
create(_, _) ->
    {error, badargs}.

%% @doc Parse TUN/TAP packet header.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.205.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> {ok, Bin} = tuncer:recv(Dev).
%% {ok,<<96,0,0,0,0,8,58,255,254,128,0,0,0,0,0,0,247,188,77,
%%       238,78,171,184,107,255,2,0,...>>}
%% 4> tuncer:header(Bin).
%% {tun_pi,96,0,
%%         <<0,8,58,255,254,128,0,0,0,0,0,0,247,188,77,238,78,171,
%%           184,107,255,2,0,0,0,...>>}
%% '''
-spec header(binary()) -> {tun_pi, tunctl:uint16_t(), tunctl:uint16_t()} | {error, file:posix()}.
header(Buf) when byte_size(Buf) > 4 ->
    tunctl:header(Buf).

%% @doc Get the TUN/TAP device name.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create().
%% {ok,<0.175.0>}
%% 2> tuncer:devname(Dev).
%% <<"tap0">>
%% '''
-spec devname(dev()) -> binary().
devname(Ref) when is_pid(Ref) ->
    getstate(Ref, dev).

%% @doc Returns an integer holding the interface creation flags.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create().
%% {ok,<0.175.0>}
%% 2> tuncer:flags(Dev).
%% [tap,no_pi]
%% '''
-spec flags(dev()) -> integer().
flags(Ref) when is_pid(Ref) ->
    getstate(Ref, flag).

%% @doc Get TUN/TAP device file descriptor.
%%
%% Get the file descriptor associated with the process. Use getfd/1
%% with read/1,2 and write/2 to interact directly with the tuntap device
%% (bypassing the gen_server).
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create().
%% {ok,<0.175.0>}
%% 2> tuncer:getfd(Dev).
%% 22
%% '''
-spec getfd(dev()) -> integer().
getfd(Ref) when is_pid(Ref) ->
    getstate(Ref, fd).

%% @doc Remove the TUN/TAP device.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create().
%% {ok,<0.175.0>}
%% 2> tuncer:destroy(Dev).
%% ok
%% '''
-spec destroy(dev()) -> ok.
destroy(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, destroy, infinity).

%% @doc Set the interface to exist after the Erlang process exits.
%%
%% == Support ==
%%
%% * Linux
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create().
%% {ok,<0.201.0>}
%% 2> tuncer:persist(Dev, true).
%% ok
%% '''
-spec persist(dev(), boolean()) -> ok | {error, file:posix()}.
persist(Ref, Bool) when is_pid(Ref), is_boolean(Bool) ->
    gen_server:call(Ref, {persist, Bool}, infinity).

%% @doc Set the UID owning the interface.
%%
%% == Support ==
%%
%% * Linux
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create().
%% {ok,<0.201.0>}
%% 2> tuncer:owner(Dev, 1000).
%% ok
%% '''
-spec owner(dev(), integer()) -> ok | {error, file:posix()}.
owner(Ref, Owner) when is_pid(Ref), is_integer(Owner) ->
    gen_server:call(Ref, {owner, Owner}, infinity).

%% @doc Set the GID owning the interface.
%%
%% == Support ==
%%
%% * Linux
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create().
%% {ok,<0.201.0>}
%% 2> tuncer:group(Dev, 1000).
%% ok
%% '''
-spec group(dev(), integer()) -> ok | {error, file:posix()}.
group(Ref, Group) when is_pid(Ref), is_integer(Group) ->
    gen_server:call(Ref, {group, Group}, infinity).

%% @doc Configure a TUN/TAP device using the default netmask and broadcast for the network.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create().
%% {ok,<0.201.0>}
%% 2> tuncer:up(Dev, "127.8.8.8").
%% ok
%% '''
%%
%% ```
%% # ip a show dev tap0
%% 7: tap0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UNKNOWN group default qlen 1000
%%    link/ether 6e:fb:29:46:df:00 brd ff:ff:ff:ff:ff:ff
%%    inet 127.8.8.8/32 brd 127.255.255.255 scope host tap0
%%    valid_lft forever preferred_lft forever
%%    inet6 fe80::6cfb:29ff:fe46:df00/64 scope link
%%    valid_lft forever preferred_lft forever
%% '''
-spec up(dev(), inet:socket_address() | inet:hostname()) -> ok | {error, file:posix()}.
up(Ref, Addr) when is_pid(Ref), is_list(Addr) ->
    case inet_parse:address(Addr) of
        {ok, IPv4} -> up(Ref, IPv4);
        {error, _} = Error -> Error
    end;
up(Ref, Addr) when is_pid(Ref), is_tuple(Addr) ->
    gen_server:call(Ref, {up, Addr}, infinity).

%% @doc Configure a TUN/TAP device.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.197.0>}
%% 2> tuncer:up(Dev, "10.1.1.1", 24).
%% ok
%% '''
%%
%% ```
%% 16: tun0: <POINTOPOINT,MULTICAST,NOARP,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UNKNOWN group default qlen 500
%%     link/none
%%     inet 10.1.1.1/24 scope global tun0
%%        valid_lft forever preferred_lft forever
%%     inet6 fe80::5472:aa66:7afa:64b9/64 scope link stable-privacy
%%        valid_lft forever preferred_lft forever
%% '''
-spec up(dev(), inet:socket_address() | inet:hostname(), 0..32) -> ok | {error, file:posix()}.
up(Ref, Addr, Mask) when is_pid(Ref), is_list(Addr), Mask >= 0, Mask =< 32 ->
    case inet_parse:address(Addr) of
        {ok, IPv4} -> up(Ref, IPv4, Mask);
        {error, _} = Error -> Error
    end;
up(Ref, Addr, Mask) when is_pid(Ref), is_tuple(Addr), Mask >= 0, Mask =< 32 ->
    gen_server:call(Ref, {up, Addr, Mask}, infinity).

%% @doc Configure the remote address for a TUN/TAP device in point-to-point mode.
%%
%% == Support ==
%%
%% * Linux (IPv4 addresses only)
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.197.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> tuncer:dstaddr(Dev, "10.1.1.2").
%% ok
%% '''
%%
%% ```
%% $ ip a show tun0
%% 4: tun0: <POINTOPOINT,MULTICAST,NOARP,NOTRAILERS,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UNKNOWN group default qlen 500
%%     link/none
%%     inet 10.1.1.1 peer 10.1.1.2/32 scope global tun0
%%        valid_lft forever preferred_lft forever
%%     inet6 fe80::e0f7:c90f:e2d8:91e8/64 scope link stable-privacy
%%        valid_lft forever preferred_lft forever
%% '''
-spec dstaddr(dev(), inet:socket_address() | inet:hostname()) -> ok | {error, file:posix()}.
dstaddr(Ref, Addr) when is_pid(Ref), is_list(Addr) ->
    case inet_parse:address(Addr) of
        {ok, IP} ->
            dstaddr(Ref, IP);
        {error, _} = Error ->
            Error
    end;
dstaddr(Ref, Addr) when is_pid(Ref), is_tuple(Addr) ->
    gen_server:call(Ref, {dstaddr, Addr}, infinity).

%% @doc Configure the broadcast address for a TUN/TAP device.
%%
%% == Support ==
%%
%% * Linux (IPv4 addresses only)
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.205.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> tuncer:broadcast(Dev, "10.1.1.3").
%% ok
%% '''
%%
%% ```
%% 6: tun0: <POINTOPOINT,MULTICAST,NOARP,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UNKNOWN group default qlen 500
%%     link/none
%%     inet 10.1.1.1/32 brd 10.1.1.3 scope global tun0
%%        valid_lft forever preferred_lft forever
%%     inet6 fe80::9d37:9d1:efba:75a1/64 scope link stable-privacy
%%        valid_lft forever preferred_lft forever
%% '''
-spec broadcast(dev(), inet:socket_address() | inet:hostname()) -> ok | {error, file:posix()}.
broadcast(Ref, Addr) when is_pid(Ref), is_list(Addr) ->
    case inet_parse:address(Addr) of
        {ok, IP} ->
            broadcast(Ref, IP);
        {error, _} = Error ->
            Error
    end;
broadcast(Ref, Addr) when is_pid(Ref), is_tuple(Addr) ->
    gen_server:call(Ref, {broadcast, Addr}, infinity).

%% @doc Unconfigure a TUN/TAP device.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.205.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> tuncer:down(Dev).
%% ok
%% '''
-spec down(dev()) -> ok | {error, file:posix()}.
down(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, down, infinity).

%% @doc Get the MTU (maximum transmission unit) for the TUN/TAP device.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.205.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> tuncer:mtu(Dev).
%% 1500
%% '''
-spec mtu(dev()) -> integer().
mtu(Ref) when is_pid(Ref) ->
    Dev = binary_to_list(devname(Ref)),
    {ok, MTU} = inet:ifget(Dev, [mtu]),
    proplists:get_value(mtu, MTU).

%r @doc Set the MTU (maximum transmission unit) for the TUN/TAP device.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.205.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> tuncer:mtu(Dev).
%% 1500
%% 4> tuncer:mtu(Dev, 1400).
%% {ok,22,<<"tun0">>}
%% 5> tuncer:mtu(Dev).
%% 1400
%% '''
-spec mtu(dev(), integer()) -> {ok, tunctl:fd(), binary()} | {error, file:posix()}.
mtu(Ref, MTU) when is_pid(Ref), is_integer(MTU) ->
    gen_server:call(Ref, {mtu, MTU}, infinity).

%% @doc Read data from the tuntap device file descriptor.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.197.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> FD = tuncer:getfd(Dev).
%% 22
%% 4> tuncer:read(FD).
%% {ok,<<96,0,0,0,0,8,58,255,254,128,0,0,0,0,0,0,36,14,228,
%%       30,140,106,15,112,255,2,0,...>>}
%% '''
-spec read(tunctl:fd()) -> {ok, binary()} | {error, file:posix()}.
read(FD) ->
    read(FD, 16#FFFF).

%% @doc Read up to the specified number of bytes from the tuntap device file
%% descriptor.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.197.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> FD = tuncer:getfd(Dev).
%% 22
%% 4> tuncer:read(FD, 16).
%% {ok,<<96,0,0,0,0,8,58,255,254,128,0,0,0,0,0,0>>}
%% '''
-spec read(tunctl:fd(), integer()) -> {ok, binary()} | {error, file:posix()}.
read(FD, Len) when is_integer(FD), is_integer(Len) ->
    procket:read(FD, Len).

%% @doc Write data to the tuntap device file descriptor.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.205.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> FD = tuncer:getfd(Dev).
%% 22
%% 4> {ok, Data} = tuncer:read(FD, 16).
%% {ok,<<96,0,0,0,0,8,58,255,254,128,0,0,0,0,0,0>>}
%% 6> tuncer:write(FD, Data).
%% ok
%% '''
-spec write(tunctl:fd(), binary()) -> ok | {error, file:posix()}.
write(FD, Data) when is_integer(FD), is_binary(Data) ->
    procket:write(FD, Data).

%% @doc Write data to the tuntap device.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.205.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> {ok, Data} = tuncer:recv(Dev).
%% {ok,<<96,0,0,0,0,8,58,255,254,128,0,0,0,0,0,0,89,201,218,
%%       161,25,116,199,243,255,2,0,...>>}
%% 4> tuncer:send(Dev, Data).
%% ok
%% '''
send(Ref, Data) when is_pid(Ref), is_binary(Data) ->
    gen_server:call(Ref, {send, Data}, infinity).

%% @doc Read data to the tuntap device.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.205.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> {ok, D} = tuncer:recv(Dev).
%% {ok,<<96,0,0,0,0,8,58,255,254,128,0,0,0,0,0,0,89,201,218,
%%       161,25,116,199,243,255,2,0,...>>}
%% '''
recv(Ref) ->
    recv(Ref, 16#FFFF).

%% @doc Read up to the specified number of bytes from the tuntap device.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.205.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> tuncer:recv(Dev, 16).
%% {ok,<<96,0,0,0,0,8,58,255,254,128,0,0,0,0,0,0>>}
%% '''
recv(Ref, Len) when is_pid(Ref), is_integer(Len) ->
    gen_server:call(Ref, {recv, Len}, infinity).

%% @doc Change the controlling process of the TUN/TAP device.
%%
%% Change the process owning the socket. Allows another process to
%% send and receive packets from the TUN/TAP device.
controlling_process(Ref, Pid) when is_pid(Ref), is_pid(Pid) ->
    Owner = self(),
    case gen_server:call(Ref, {state, [pid, port]}, infinity) of
        [{pid, Owner}, {port, false}] ->
            controlling_process_1(Ref, Pid, false, ok);
        [{pid, Owner}, {port, _}] ->
            controlling_process_1(Ref, Pid, true, setopt(Ref, {active, false}));
        _ ->
            {error, not_owner}
    end.

controlling_process_1(Ref, Pid, Mode, ok) ->
    case gen_server:call(Ref, {controlling_process, Pid}, infinity) of
        ok ->
            flush_events(Ref, Pid),
            setopt(Ref, {active, Mode});
        Error ->
            Error
    end;
controlling_process_1(_Ref, _Pid, _Mode, Error) ->
    Error.

%% @doc Set TUN/TAP device option.
%%
%% setopt/2 currently supports toggling `active' mode for performing
%% flow control.
%%
%% == Examples ==
%%
%% ```
%% 1> {ok, Dev} = tuncer:create("tun0", [tun, no_pi, {active, false}]).
%% {ok,<0.205.0>}
%% 2> tuncer:up(Dev, "10.1.1.1").
%% ok
%% 3> tuncer:setopt(Dev, {active, true}).
%% ok
%% 4> tuncer:setopt(Dev, {active, false}).
%% ok
%% 5> flush().
%% Shell got {tuntap,<0.205.0>,
%%                   <<96,0,0,0,0,8,58,255,254,128,0,0,0,0,0,0,44,191,1,21,92,
%%                     149,243,132,255,2,0,0,0,0,0,0,0,0,0,0,0,0,0,2,133,0,255,
%%                     72,0,0,0,0>>}
%% Shell got {tuntap,<0.205.0>,
%%                   <<96,0,0,0,0,8,58,255,254,128,0,0,0,0,0,0,44,191,1,21,92,
%%                     149,243,132,255,2,0,0,0,0,0,0,0,0,0,0,0,0,0,2,133,0,255,
%%                     72,0,0,0,0>>}
%% Shell got {tuntap,<0.205.0>,
%%                   <<96,0,0,0,0,8,58,255,254,128,0,0,0,0,0,0,44,191,1,21,92,
%%                     149,243,132,255,2,0,0,0,0,0,0,0,0,0,0,0,0,0,2,133,0,255,
%%                     72,0,0,0,0>>}
%% ok
%% '''
setopt(Ref, Option) when is_tuple(Option) ->
    gen_server:call(Ref, {setopt, Option}, infinity).

%% @private
start_link(Ifname, Opt) when is_binary(Ifname), is_list(Opt) ->
    Pid = self(),
    gen_server:start_link(?MODULE, [Pid, Ifname, Opt], []).

%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------

%% @private
init([Pid, Ifname, Flag]) ->
    process_flag(trap_exit, true),

    % if Dev is NULL, the tuntap driver will choose an
    % interface name
    case tunctl:create(Ifname, Flag) of
        {ok, FD, Dev} ->
            Active = proplists:get_value(active, Flag, false),
            Port_options = proplists:get_value(port_options, Flag, [
                {busy_limits_port, disabled}, {busy_limits_msgq, disabled}
            ]),

            Port =
                case Active of
                    true -> set_mode(active, FD, Port_options);
                    false -> false
                end,

            {ok, #state{
                port = Port,
                port_opt = Port_options,
                pid = Pid,
                fd = FD,
                dev = Dev,
                flag = Flag
            }};
        Else ->
            Else
    end.

%%
%% retrieve/modify gen_server state
%%
%% @private
handle_call({state, Field}, _From, State) ->
    {reply, state(Field, State), State};
handle_call({controlling_process, Owner}, {Owner, _}, #state{pid = Owner} = State) ->
    {reply, ok, State};
handle_call({controlling_process, Pid}, {Owner, _}, #state{pid = Owner} = State) ->
    link(Pid),
    unlink(Owner),
    {reply, ok, State#state{pid = Pid}};
handle_call({controlling_process, _}, _, State) ->
    {reply, {error, not_owner}, State};
handle_call(
    {setopt, {active, true}}, _From, #state{port = false, fd = FD, port_opt = Port_opt} = State
) ->
    try set_mode(active, FD, Port_opt) of
        Port ->
            {reply, ok, State#state{port = Port}}
    catch
        error:Error ->
            {reply, {error, Error}, State}
    end;
handle_call({setopt, {active, true}}, _From, State) ->
    {reply, ok, State};
handle_call({setopt, {active, false}}, _From, #state{port = false} = State) ->
    {reply, ok, State};
handle_call({setopt, {active, false}}, _From, #state{port = Port} = State) ->
    Reply = set_mode(passive, Port),
    {reply, Reply, State#state{port = false}};
handle_call({setopt, _}, _From, State) ->
    {reply, {error, badarg}, State};
%%
%% manipulate the tun/tap device
%%
handle_call({send, Data}, _From, #state{port = false, fd = FD} = State) ->
    Reply = procket:write(FD, Data),
    {reply, Reply, State};
handle_call({send, Data}, _From, #state{port = Port} = State) ->
    Reply =
        try erlang:port_command(Port, Data) of
            true ->
                ok
        catch
            error:Error ->
                {error, Error}
        end,
    {reply, Reply, State};
handle_call({recv, Len}, _From, #state{port = false, fd = FD} = State) ->
    Reply = procket:read(FD, Len),
    {reply, Reply, State};
handle_call({recv, _Len}, _From, State) ->
    {reply, {error, einval}, State};
handle_call({persist, Status}, _From, #state{fd = FD} = State) ->
    Reply = tunctl:persist(FD, Status),
    {reply, Reply, State#state{persist = Status}};
handle_call({owner, Owner}, _From, #state{fd = FD} = State) ->
    Reply = tunctl:owner(FD, Owner),
    {reply, Reply, State};
handle_call({group, Group}, _From, #state{fd = FD} = State) ->
    Reply = tunctl:group(FD, Group),
    {reply, Reply, State};
handle_call({up, IP}, _From, #state{dev = Dev} = State) ->
    Reply = tunctl:up(Dev, IP),
    {reply, Reply, State};
handle_call({up, IP, Mask}, _From, #state{dev = Dev} = State) ->
    Reply = tunctl:up(Dev, IP, Mask),
    {reply, Reply, State};
handle_call({dstaddr, IP}, _From, #state{dev = Dev} = State) ->
    Reply = tunctl:dstaddr(Dev, IP),
    {reply, Reply, State};
handle_call({broadcast, IP}, _From, #state{dev = Dev} = State) ->
    Reply = tunctl:broadcast(Dev, IP),
    {reply, Reply, State};
handle_call(down, _From, #state{dev = Dev} = State) ->
    Reply = tunctl:down(Dev),
    {reply, Reply, State};
handle_call({mtu, MTU}, _From, #state{dev = Dev, fd = FD} = State) ->
    Reply = tunctl:mtu(FD, Dev, MTU),
    {reply, Reply, State};
handle_call(destroy, _From, State) ->
    {stop, normal, ok, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% {active, true} mode
%%
%% @private
handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _, _}, #state{port = false} = State) ->
    {noreply, State};
handle_info(
    {'EXIT', Port, Error},
    #state{port = Port, pid = Pid, fd = FD, dev = Dev, persist = Persist} = State
) ->
    Pid ! {tuntap_error, self(), Error},
    _ =
        case Persist of
            true ->
                ok;
            false ->
                _ = tunctl:down(Dev),
                tunctl:persist(FD, false)
        end,
    procket:close(FD),
    {stop, normal, State};
handle_info({Port, {data, Data}}, #state{port = Port, pid = Pid} = State) ->
    Pid ! {tuntap, self(), Data},
    {noreply, State};
% WTF?
handle_info(Info, State) ->
    error_logger:error_report([wtf, Info]),
    {noreply, State}.

%% @private
terminate(_Reason, #state{fd = FD, dev = Dev, port = Port, persist = Persist}) ->
    terminate_port(Port),
    ok = tun_down(Dev, Persist),
    procket:close(FD).

terminate_port(Port) when is_port(Port) ->
    catch erlang:port_close(Port);
terminate_port(_Port) ->
    ok.

tun_down(_Dev, true = _Persist) ->
    ok;
tun_down(Dev, false = _Persist) ->
    tunctl:down(Dev).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
set_mode(active, FD, Opt) ->
    open_port({fd, FD, FD}, [stream, binary] ++ Opt).
set_mode(passive, Port) ->
    try erlang:port_close(Port) of
        true ->
            ok
    catch
        error:Error ->
            {error, Error}
    end.

flush_events(Ref, Pid) ->
    receive
        {tuntap, Ref, _} = Event ->
            Pid ! Event,
            flush_events(Ref, Pid)
    after 0 -> ok
    end.

getstate(Ref, Key) when is_atom(Key) ->
    [{Key, Value}] = gen_server:call(Ref, {state, [Key]}, infinity),
    Value.

state(Fields, State) ->
    state(Fields, State, []).

state([], _State, Acc) ->
    lists:reverse(Acc);
state([Field | Fields], State, Acc) ->
    state(Fields, State, [{Field, field(Field, State)} | Acc]).

field(port, #state{port = Port}) -> Port;
field(pid, #state{pid = Pid}) -> Pid;
field(fd, #state{fd = FD}) -> FD;
field(dev, #state{dev = Dev}) -> Dev;
field(flag, #state{flag = Flag}) -> Flag;
field(_, _) -> unsupported.
