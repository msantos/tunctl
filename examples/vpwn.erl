%% Copyright (c) 2011-2012, Michael Santos <michael.santos@gmail.com>
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

%%
%% VPN over Erlang distribution protocol
%%
%% Usage:
%%  node2: Start up Erlang on the destination node:
%%
%%      ./start.sh -setcookie OMNOMNOM -name v
%%
%%  node1: then on the source node:
%%
%%      ./start.sh -setcookie OMNOMNOM -name v
%%
%%      vpwn:start('node@vpn.example.com', "10.10.10.1", "10.10.10.2").
%%
-module(vpwn).
-export([start/3]).


start(Node, Src, Dst) when is_list(Src) ->
    start(Node, inet_parse:address(Src), Dst);
start(Node, Src, Dst) when is_list(Dst) ->
    start(Node, Src, inet_parse:address(Dst));

start(Node, SrcIP, DstIP) ->
    Pid = peer(Node, SrcIP, DstIP),

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
