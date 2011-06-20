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
-module(tuncer).
-behaviour(gen_server).


-export([
        create/0, create/1, create/2,
        devname/1, flags/1, fd/1,
        destroy/1,

        persist/2,
        owner/2, group/2,

        read/2, write/2,

        header/1,

        up/2, down/1,
        mtu/1, mtu/2,

        controlling_process/2
    ]).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
        port,       % false, port
        pid,        % PID of controlling process
        fd,         % TUN/TAP file descriptor
        dev,        % device name
        flag        % TUNSETIFF ifr flags
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
    gen_server:call(Ref, devname).

flags(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, flags).

fd(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, fd).

destroy(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, destroy).

persist(Ref, Bool) when is_pid(Ref), ( Bool == true orelse Bool == false )  ->
    gen_server:call(Ref, {persist, Bool}).

owner(Ref, Owner) when is_pid(Ref), is_integer(Owner) ->
    gen_server:call(Ref, {owner, Owner}).

group(Ref, Group) when is_pid(Ref), is_integer(Group) ->
    gen_server:call(Ref, {group, Group}).

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
    Fd = fd(Ref),
    procket:read(Fd, Len).

write(Ref, Data) when is_pid(Ref), is_binary(Data) ->
    Fd = fd(Ref),
    procket:write(Fd, Data).

% FIXME: race condition: events can be delivered out of order
controlling_process(Ref, Pid) when is_pid(Ref), is_pid(Pid) ->
    flush_events(Ref, Pid),
    gen_server:call(Ref, {controlling_process, Pid}),
    flush_events(Ref, Pid).

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

    Port = case Active of
        true -> set_active(FD);
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
handle_call(devname, _From, #state{dev = Dev} = State) ->
    {reply, Dev, State};

handle_call(flags, _From, #state{flag = Flag} = State) ->
    {reply, Flag, State};

handle_call(fd, _From, #state{fd = FD} = State) ->
    {reply, FD, State};

handle_call({controlling_process, Pid}, {Owner,_}, #state{pid = Owner} = State) ->
    {reply, ok, State#state{pid = Pid}};


%%
%% manipulate the tun/tap device
%%
handle_call({persist, Status}, _From, #state{fd = FD} = State) ->
    Reply = tunctl:persist(FD, Status),
    {reply, Reply, State};

handle_call({owner, Owner}, _From, #state{fd = FD} = State) ->
    Reply = tunctl:owner(FD, Owner),
    {reply, Reply, State};

handle_call({group, Group}, _From, #state{fd = FD} = State) ->
    Reply = tunctl:group(FD, Group),
    {reply, Reply, State};

handle_call({up, IP}, _From, #state{dev = Dev} = State) ->
    Reply = tunctl:up(Dev, IP),
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
handle_info({Port, {data, Data}}, #state{port = Port, pid = Pid} = State) ->
    Pid ! {tuntap, self(), Data},
    {noreply, State};

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([wtf, Info]),
    {noreply, State}.

terminate(_Reason, #state{fd = FD, dev = Dev}) ->
    tunctl:down(Dev),
    tunctl:persist(FD, false),
    procket:close(FD),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
set_active(FD) ->
    open_port({fd, FD, FD}, [stream, binary]).

flush_events(Ref, Pid) ->
    receive
        {tuntap, Ref, _} = Event ->
            Pid ! Event,
            flush_events(Ref, Pid)
    after
        0 -> ok
    end.
