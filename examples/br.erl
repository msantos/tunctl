%% Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
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
-module(br).
-export([start/0, start/2]).


start() ->
    start("erlbr0", ["erl0"]).

start(Uplink, Ifaces) ->
    % Switch uplink
    {ok, Br} = tuncer:create(Uplink, [tap, no_pi, {active, true}]),

    % Switch ports
    Dev = [ begin
        {ok, N} = tuncer:create(Iface, [tap, no_pi, {active, true}]),
        N
      end || Iface <- Ifaces ],

    switch(Br, Dev).

switch(Br, Dev) ->
    receive
        {tuntap, Br, Data} ->
            % Data received on uplink: flood to ports
            error_logger:info_report([{br, Br}, {data, Data}]),
            [ ok = tuncer:send(N, Data) || N <- Dev ],
            switch(Br, Dev);
        {tuntap, Port, Data} ->
            % Data received on port: flood to all other ports and uplink
            error_logger:info_report([{dev, Port}, {data, Data}]),
            [ ok = tuncer:send(N, Data) || N <- Dev ++ [Br], N =/= Port ],
            switch(Br, Dev);
        Error ->
            error_logger:error_report([{error, Error}])
    end.
