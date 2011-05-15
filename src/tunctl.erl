%% Copyright (c) 2011, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(tunctl).
-behaviour(gen_server).

-include("tuntap.hrl").
-include("ioctl.hrl").
-include("procket.hrl").

-export([
        create/0, create/1, create/2,
        devname/1, flags/1, fd/1,
        destroy/1,

        persist/2,
        owner/2, group/2,

        read/2, write/2,

        header/1,

        up/2, down/1,
        mtu/1, mtu/2
    ]).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
        fd,         % TUN/TAP file descriptor
        dev,        % device name
        flag        % TUNSETIFF ifr flags
    }).

-define(SIOCGIFFLAGS, 16#8913).
-define(SIOCSIFFLAGS, 16#8914).
-define(SIOCSIFADDR, 16#8916).

-define(IFF_RUNNING, 16#40).
-define(IFF_UP, 16#01).

-define(UINT16, 2/native-unsigned-integer-unit:8).

%%--------------------------------------------------------------------
%%% Exports
%%--------------------------------------------------------------------
create() ->
    create(<<0:(15*8)>>).
create(Ifname) ->
    create(Ifname, [tap, no_pi]).

create(Ifname, Opt) when is_list(Ifname) ->
    create(list_to_binary(Ifname), Opt);
create(Ifname, Opt) when byte_size(Ifname) < ?IFNAMSIZ, is_list(Opt) ->
    start_link(Ifname, Opt).

header(<<Flags:?UINT16, Proto:?UINT16, Buf/binary>>) ->
    {tun_pi, Flags, Proto, Buf}.

devname(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, devname).

flags(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, flags).

fd(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, fd).

destroy(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, destroy).

persist(Ref, Bool) when is_pid(Ref), is_atom(Bool) ->
    gen_server:call(Ref, {persist, bool(Bool)}).

owner(Ref, Owner) when is_pid(Ref), is_integer(Owner) ->
    gen_server:call(Ref, {owner, int_to_bin(Owner)}).

group(Ref, Group) when is_pid(Ref), is_integer(Group) ->
    gen_server:call(Ref, {group, int_to_bin(Group)}).

up(Ref, Addr) when is_pid(Ref), is_list(Addr) ->
    {ok, IPv4} = inet_parse:address(Addr),
    up(Ref, IPv4);
up(Ref, Addr) when is_pid(Ref), is_tuple(Addr) ->
    gen_server:call(Ref, {up, Addr}).

down(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, down).

mtu(Ref) when is_pid(Ref) ->
    Dev = binary_to_list(devname(Ref)),
    {ok, MTU} = inet:ifget(Dev, [mtu]),
    proplists:get_value(mtu, MTU).

mtu(Ref, MTU) when is_pid(Ref), is_integer(MTU) ->
    gen_server:call(Ref, {mtu, MTU}).

read(Ref, Len) when is_pid(Ref), is_integer(Len) ->
    gen_server:call(Ref, {read, Len}).

write(Ref, Data) when is_pid(Ref), is_binary(Data) ->
    gen_server:call(Ref, {write, Data}).

start_link(Ifname, Opt) when is_binary(Ifname), is_list(Opt) ->
    gen_server:start_link(?MODULE, [Ifname, Opt], []).


%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
init([Dev, Opt]) ->
    process_flag(trap_exit, true),
    Flag = lists:foldl(fun(N, F) -> F bor flag(N) end, 0, Opt),
    % if Dev is NULL, the tuntap driver will choose an
    % interface name
    {ok, FD, Dev1} = mkdev(Dev, Flag),
    {ok, #state{
            fd = FD,
            dev = Dev1,
            flag = Flag
    }}.


handle_call(devname, _From, #state{dev = Dev} = State) ->
    {reply, Dev, State};

handle_call(flags, _From, #state{flag = Flag} = State) ->
    {reply, Flag, State};

handle_call(fd, _From, #state{fd = FD} = State) ->
    {reply, FD, State};

handle_call({read, Len}, _From, #state{fd = FD} = State) ->
    Reply = procket:read(FD, Len),
    {reply, Reply, State};

handle_call({write, Data}, _From, #state{fd = FD} = State) ->
    Reply = procket:write(FD, Data),
    {reply, Reply, State};

handle_call({persist, Status}, _From, #state{fd = FD} = State) ->
    Reply = set_persist(FD, Status),
    {reply, Reply, State};

handle_call({owner, Owner}, _From, #state{fd = FD} = State) ->
    Reply = ioctl(FD, ?TUNSETOWNER, Owner),
    {reply, Reply, State};

handle_call({group, Group}, _From, #state{fd = FD} = State) ->
    Reply = ioctl(FD, ?TUNSETGROUP, Group),
    {reply, Reply, State};

handle_call({up, IP}, _From, #state{dev = Dev} = State) ->
    Reply = ifup(Dev, IP),
    {reply, Reply, State};

handle_call(down, _From, #state{dev = Dev} = State) ->
    Reply = ifdown(Dev),
    {reply, Reply, State};

handle_call({mtu, _MTU}, _From, #state{dev = _Dev} = State) ->
    {reply, {error, unsupported}, State};

handle_call(destroy, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([wtf, Info]),
    {noreply, State}.

terminate(_Reason, #state{fd = FD, dev = Dev}) ->
    ok = ifdown(Dev),
    ok = set_persist(FD, bool(false)),
    ok = procket:close(FD),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
ioctl(FD, Request, Opt) ->
    case procket:ioctl(FD, Request, Opt) of
        {ok, _} -> ok;
        Error -> Error
    end.

mkdev(Dev, Flag) ->
    {ok, FD} = procket:dev("net/tun"),
    Result = procket:ioctl(FD, ?TUNSETIFF,
        <<Dev/binary, 0:((15*8) - (byte_size(Dev)*8)), 0:8, % ifrn_name[IFNAMSIZ]: interface name
        Flag:2/native-signed-integer-unit:8,               % ifru_flags
        0:(14*8)>>),

    case Result of
        {ok, Dev1} ->
            {ok, FD, hd(binary:split(Dev1, <<0>>))};
        Error ->
            ok = procket:close(FD),
            Error
    end.

set_persist(FD, Status) ->
    ioctl(FD, ?TUNSETPERSIST, Status).

flag(tun) -> ?IFF_TUN;
flag(tap) -> ?IFF_TAP;
flag(no_pi) -> ?IFF_NO_PI;
flag(one_queue) -> ?IFF_ONE_QUEUE;
flag(vnet_hdr) -> ?IFF_VNET_HDR;
flag(tun_excl) -> ?IFF_TUN_EXCL.

int_to_bin(Int) ->
    <<Int:4/native-integer-unsigned-unit:8>>.

bool(true) -> <<1:4/native-integer-unsigned-unit:8>>;
bool(false) -> <<0:4/native-integer-unsigned-unit:8>>.

set_flag(Socket, Dev, Flag) ->
    {ok, _} = procket:ioctl(Socket, ?SIOCSIFFLAGS,
        <<Dev/bytes, 0:((15-byte_size(Dev))*8), 0:8,
        Flag:2/native-signed-integer-unit:8,
        0:(14*8)>>),
    ok.

get_flag(Socket, Dev) ->
    {ok, <<_:(16*8), Flag:2/native-signed-integer-unit:8, _/binary>>} = procket:ioctl(
        Socket, ?SIOCGIFFLAGS, <<Dev/bytes, 0:((15-byte_size(Dev))*8), 0:(16*8)>>
    ),
    {ok, Flag}.


%% Just like ifconfig but with fewer options
ifup(Dev, {A,B,C,D}) when byte_size(Dev) < ?IFNAMSIZ ->
    {ok, Socket} = procket:socket(inet, dgram,  0),

    % struct sockaddr_in
    % dev[IFNAMSIZ], family:2 bytes, port:2 bytes, ipaddr:4 bytes
    Ifr = <<Dev/bytes, 0:( (?IFNAMSIZ - byte_size(Dev) - 1)*8), 0:8,
        ?PF_INET:16/native, 0:16, A:8, B:8, C:8, D:8, 0:(8*8)>>,

    Res = try ok = ioctl(Socket, ?SIOCSIFADDR, Ifr),
        {ok, Flag} = get_flag(Socket, Dev),
        ok = set_flag(Socket, Dev, Flag bor ?IFF_RUNNING bor ?IFF_UP) of
        _ -> ok
    catch
        error:Error ->
            Error
    end,

    ok = procket:close(Socket),
    Res.

ifdown(Dev) when byte_size(Dev) < ?IFNAMSIZ ->
    {ok, Socket} = procket:socket(inet, dgram,  0),

    Res = try {ok, Flags} = get_flag(Socket, Dev),
        ok = set_flag(Socket, Dev, Flags band bnot(?IFF_UP)) of
        _ -> ok
    catch
        error:Error ->
            Error
    end,

    ok = procket:close(Socket),
    Res.
