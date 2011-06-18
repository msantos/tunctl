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
-module(tunctl_darwin).

-include("tuntap.hrl").
-include("ioctl.hrl").
-include("procket.hrl").

-export([
        create/2,
        persist/2,
        owner/2, group/2,
        up/2, down/1
    ]).


-define(SIZEOF_STRUCT_IFALIASREQ, 64).
-define(SIOCAIFADDR, ?IOW($i, 26, ?SIZEOF_STRUCT_IFALIASREQ)).

-define(IFF_RUNNING, 16#40).
-define(IFF_UP, 16#01).


%%--------------------------------------------------------------------
%%% Exports
%%--------------------------------------------------------------------

%% Explicitly specify the device name: procket_cmd doesn't return
%% the name of the device and will try each numbered device until
%% it succeeds or no devices are left.
%%
%% As a consequence, if the caller specifies "tap1" and that is busy,
%% they may accidentally open "tap11" (TODO: procket_cmd should have an
%% option for exact name matches).
create(<<>>, Opt) ->
    create(<<"tap0">>, Opt);

%% Ignore the options for now
create(Ifname, _Opt) when byte_size(Ifname) < ?IFNAMSIZ ->
    {ok, FD} = procket:dev(binary_to_list(Ifname)),
    {ok, FD, Ifname}.


%% N/A
persist(_FD, _Status) ->
    ok.

owner(_FD, _Owner) ->
    ok.

group(__FD, _Group) ->
    ok.


%% Calling SIOCAIFADDR requires beam runs with root
%% privs. For now, shell out to ifconfig.
up(Dev, {A,B,C,D}) ->
    Cmd = "sudo ifconfig " ++ binary_to_list(Dev) ++ " " ++
        inet_parse:ntoa({A,B,C,D}) ++ " up",
    case os:cmd(Cmd) of
        [] -> ok;
        Error -> Error
    end.

down(Dev) when byte_size(Dev) < ?IFNAMSIZ ->
    Cmd = "sudo ifconfig " ++ binary_to_list(Dev) ++ " down",
    case os:cmd(Cmd) of
        [] -> ok;
        Error -> Error
    end.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
