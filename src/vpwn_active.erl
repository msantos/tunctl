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

%%
%% VPN over Erlang distribution protocol (active mode)
%%
%% Usage:
%%  vpwn:start('node@vpn.example.com', {10,10,10,1}, {10,10,10,2}).
%%
-module(vpwn_active).
-export([start/3]).


start(Node, SrcIP, DstIP) ->
    Pid = peer(Node, SrcIP, DstIP),

    {ok, Dev} = tuncer:create("vpwn", [tap, no_pi, {active, true}]),
    ok = tuncer:up(Dev, SrcIP),

    proxy(Dev, Pid).

% Parent
peer(N, SrcIP, DstIP) when is_atom(N) ->
    pong = net_adm:ping(N),
    Self = self(),
    spawn_link(N, vpwn, start, [Self, DstIP, SrcIP]);
% Child
peer(N, _, _) when is_pid(N) ->
    N.

proxy(Dev, Pid) ->
    receive
        {tuntap, _Pid, Data} ->
            Pid ! {vpwn, Data},
            read(Dev, Pid);
        {vpwn, Data} ->
            ok = tuncer:send(Dev, Data),
            read(Dev, Pid);
        Error ->
            Error
    end.
