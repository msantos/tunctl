%%
%% VPN over Erlang distribution protocol
%%
%% Usage:
%%  node2: Start up Erlang on the destination node:
%%
%%      erl -pa deps/*/ebin ebin -setcookie OMNOMNOM -name node
%%
%%  node1: then on the source node:
%%
%%      erl -pa deps/*/ebin ebin -setcookie OMNOMNOM -name node
%%
%%      vpwn_active:start('node@vpn.example.com', "10.10.10.1", "10.10.10.2").
%%
-module(vpwn_active).
-export([start/3]).


start(Node, SrcIP, DstIP) ->
    Pid = peer(Node, addr(SrcIP), addr(DstIP)),

    {ok, Dev} = tuncer:create("vpwn", [tap, no_pi, {active, true}]),
    ok = tuncer:up(Dev, SrcIP),

    proxy(Dev, Pid).

% Parent
peer(N, SrcIP, DstIP) when is_atom(N) ->
    pong = net_adm:ping(N),
    Self = self(),
    spawn_link(N, vpwn_active, start, [Self, DstIP, SrcIP]);
% Child
peer(N, _, _) when is_pid(N) ->
    N.

proxy(Dev, Pid) ->
    receive
        {tuntap, _Pid, Data} ->
            Pid ! {vpwn, Data},
            proxy(Dev, Pid);
        {vpwn, Data} ->
            ok = tuncer:send(Dev, Data),
            proxy(Dev, Pid);
        Error ->
            Error
    end.

addr(Addr) when is_tuple(Addr) ->
    Addr;
addr(Str) when is_list(Str) ->
    {ok, Addr} = inet_parse:address(Str),
    Addr.
