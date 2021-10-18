%% Copyright (c) 2011-2021, Michael Santos <michael.santos@gmail.com>
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

-record(state, {
    % false, port
    port,
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
create() ->
    create(<<>>).

create(Ifname) ->
    create(Ifname, [tap, no_pi]).

create(Ifname, Opt) when is_list(Ifname) ->
    create(list_to_binary(Ifname), Opt);
create(Ifname, Opt) when byte_size(Ifname) < ?IFNAMSIZ, is_list(Opt) ->
    start_link(Ifname, Opt).

header(Buf) when byte_size(Buf) > 4 ->
    tunctl:header(Buf).

devname(Ref) when is_pid(Ref) ->
    getstate(Ref, dev).

flags(Ref) when is_pid(Ref) ->
    getstate(Ref, flag).

getfd(Ref) when is_pid(Ref) ->
    getstate(Ref, fd).

destroy(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, destroy, infinity).

persist(Ref, Bool) when is_pid(Ref), is_boolean(Bool) ->
    gen_server:call(Ref, {persist, Bool}, infinity).

owner(Ref, Owner) when is_pid(Ref), is_integer(Owner) ->
    gen_server:call(Ref, {owner, Owner}, infinity).

group(Ref, Group) when is_pid(Ref), is_integer(Group) ->
    gen_server:call(Ref, {group, Group}, infinity).

up(Ref, Addr) when is_pid(Ref), is_list(Addr) ->
    case inet_parse:address(Addr) of
        {ok, IPv4} -> up(Ref, IPv4);
        {error, _} = Error -> Error
    end;
up(Ref, Addr) when is_pid(Ref), is_tuple(Addr) ->
    gen_server:call(Ref, {up, Addr}, infinity).

up(Ref, Addr, Mask) when is_pid(Ref), is_list(Addr), Mask >= 0, Mask =< 32 ->
    case inet_parse:address(Addr) of
        {ok, IPv4} -> up(Ref, IPv4, Mask);
        {error, _} = Error -> Error
    end;
up(Ref, Addr, Mask) when is_pid(Ref), is_tuple(Addr), Mask >= 0, Mask =< 32 ->
    gen_server:call(Ref, {up, Addr, Mask}, infinity).

dstaddr(Ref, Addr) when is_pid(Ref), is_list(Addr) ->
    case inet_parse:address(Addr) of
        {ok, IP} ->
            dstaddr(Ref, IP);
        {error, _} = Error ->
            Error
    end;
dstaddr(Ref, Addr) when is_pid(Ref), is_tuple(Addr) ->
    gen_server:call(Ref, {dstaddr, Addr}, infinity).

down(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, down, infinity).

mtu(Ref) when is_pid(Ref) ->
    Dev = binary_to_list(devname(Ref)),
    {ok, MTU} = inet:ifget(Dev, [mtu]),
    proplists:get_value(mtu, MTU).

mtu(Ref, MTU) when is_pid(Ref), is_integer(MTU) ->
    gen_server:call(Ref, {mtu, MTU}, infinity).

read(FD) ->
    read(FD, 16#FFFF).

read(FD, Len) when is_integer(FD), is_integer(Len) ->
    procket:read(FD, Len).

write(FD, Data) when is_integer(FD), is_binary(Data) ->
    procket:write(FD, Data).

send(Ref, Data) when is_pid(Ref), is_binary(Data) ->
    gen_server:call(Ref, {send, Data}, infinity).

recv(Ref) ->
    recv(Ref, 16#FFFF).

recv(Ref, Len) when is_pid(Ref), is_integer(Len) ->
    gen_server:call(Ref, {recv, Len}, infinity).

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

setopt(Ref, Option) when is_tuple(Option) ->
    gen_server:call(Ref, {setopt, Option}, infinity).

start_link(Ifname, Opt) when is_binary(Ifname), is_list(Opt) ->
    Pid = self(),
    gen_server:start_link(?MODULE, [Pid, Ifname, Opt], []).

%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
init([Pid, Ifname, Flag]) ->
    process_flag(trap_exit, true),

    % if Dev is NULL, the tuntap driver will choose an
    % interface name
    {ok, FD, Dev} = tunctl:create(Ifname, Flag),

    Active = proplists:get_value(active, Flag, false),

    Port =
        case Active of
            true -> set_mode(active, FD);
            false -> false
        end,

    {ok, #state{
        port = Port,
        pid = Pid,
        fd = FD,
        dev = Dev,
        flag = Flag
    }}.

%%
%% retrieve/modify gen_server state
%%
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
handle_call({setopt, {active, true}}, _From, #state{port = false, fd = FD} = State) ->
    try set_mode(active, FD) of
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
handle_call({up, IP}, _From, #state{dev = Dev, flag = Flag} = State) ->
    Reply = up_tunnel(fun() -> tunctl:up(Dev, IP) end, Flag),
    {reply, Reply, State};
handle_call({up, IP, Mask}, _From, #state{dev = Dev, flag = Flag} = State) ->
    Reply = up_tunnel(fun() -> tunctl:up(Dev, IP, Mask) end, Flag),
    {reply, Reply, State};
handle_call({dstaddr, IP}, _From, #state{dev = Dev} = State) ->
    Reply = tunctl:dstaddr(Dev, IP),
    {reply, Reply, State};
handle_call(down, _From, #state{dev = Dev} = State) ->
    Reply = tunctl:down(Dev),
    {reply, Reply, State};
handle_call({mtu, _MTU}, _From, #state{dev = _Dev} = State) ->
    {reply, {error, unsupported}, State};
handle_call(destroy, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% {active, true} mode
%%
handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _, _}, #state{port = false} = State) ->
    {noreply, State};
handle_info(
    {'EXIT', Port, Error},
    #state{port = Port, pid = Pid, fd = FD, dev = Dev, persist = Persist} = State
) ->
    Pid ! {tuntap_error, self(), Error},
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

terminate(_Reason, #state{fd = FD, dev = Dev, port = Port, persist = Persist}) ->
    if
        is_port(Port) ->
            catch erlang:port_close(Port);
        true ->
            ok
    end,
    case Persist of
        true ->
            ok;
        false ->
            _ = tunctl:down(Dev)
    end,
    procket:close(FD),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
set_mode(active, FD) ->
    open_port({fd, FD, FD}, [stream, binary]);
set_mode(passive, Port) ->
    try erlang:port_close(Port) of
        true ->
            ok
    catch
        error:Error ->
            {error, Error}
    end.

-spec up_tunnel(fun(), list()) -> ok | {error, term()}.
up_tunnel(UpCallback, Flag) ->
    Extra = proplists:get_value(extra, Flag, []),
    case proplists:get_value(namespace, Extra) of
        undefined ->
            UpCallback();
        NS ->
            tunctl:up_into_namespace(UpCallback, NS)
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
