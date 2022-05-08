%%
%% VPN over Erlang distribution protocol
%%
%% Usage:
%%  node2: Start up Erlang on the destination node:
%%
%%      ./start.sh -setcookie OMNOMNOM -name node
%%
%%  node1: then on the source node:
%%
%%      ./start.sh -setcookie OMNOMNOM -name node
%%
%%      vpwn:start('node@vpn.example.com', "10.10.10.1", "10.10.10.2").
%%
-module(vpwn).
-export([start/3]).

start(Node, SrcIP, DstIP) ->
    Pid = peer(Node, addr(SrcIP), addr(DstIP)),

    {ok, Dev} = tuncer:create(),
    ok = tuncer:up(Dev, SrcIP),
    FD = tuncer:getfd(Dev),

    spawn_link(fun() -> read(FD, Pid) end),
    write(FD).

% Parent
peer(N, SrcIP, DstIP) when is_atom(N) ->
    pong = net_adm:ping(N),
    Self = self(),
    spawn_link(N, vpwn, start, [Self, DstIP, SrcIP]);
% Child
peer(N, _, _) when is_pid(N) ->
    N.

read(FD, Pid) ->
    case tuncer:read(FD, 16#FFFF) of
        {error,eagain} ->
            timer:sleep(10),
            read(FD, Pid);
        {ok, Data} ->
            Pid ! {vpwn, Data},
            read(FD, Pid)
    end.

write(FD) ->
    receive
        {vpwn, Data} ->
            ok = tuncer:write(FD, Data),
            write(FD);
        Error ->
            error_logger:error_report([{write_error, Error}])
    end.

addr(Addr) when is_tuple(Addr) ->
    Addr;
addr(Str) when is_list(Str) ->
    {ok, Addr} = inet_parse:address(Str),
    Addr.
