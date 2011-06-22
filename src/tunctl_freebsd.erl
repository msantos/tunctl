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
-module(tunctl_freebsd).

%% Uses the legacy tun/tap interface (rather than interface cloning)
%% net.link.(tun|tap).devfs_cloning must be non-zero to use.

-include("tuntap.hrl").
-include("ioctl.hrl").
-include("procket.hrl").

-export([
        create/2,
        persist/2,
        owner/2, group/2,

        header/1
    ]).


-define(SIZEOF_STRUCT_IFALIASREQ, 64).
-define(SIOCAIFADDR, ?IOW($i, 26, ?SIZEOF_STRUCT_IFALIASREQ)).

-define(TAPGIFNAME, ?IOR($t, 93, ?SIZEOF_STRUCT_IFREQ)).
-define(TUNSIFHEAD, ?IOW($t, 96, ?SIZEOF_INT)).


%%--------------------------------------------------------------------
%%% Exports
%%--------------------------------------------------------------------
create(<<>>, Opt) ->
    create(<<"tap0">>, Opt);

%% Ignore the options for now
create(Ifname, Opt) when byte_size(Ifname) < ?IFNAMSIZ ->
    case procket:dev(binary_to_list(Ifname)) of
        {ok, FD} ->
            create_1(FD, Ifname, Opt);
        Error ->
            Error
    end.

create_1(FD, Ifname, Opt) ->
    case {Ifname, proplists:get_bool(no_pi, Opt)} of
        {<<"tun", _/binary>>, false} ->
            ok = tunctl:ioctl(FD, ?TUNSIFHEAD, 1);
        _ ->
            % TUNSIFHEAD isn't supported by tap devices
            ok
    end,
    {ok, FD, Ifname}.


%% N/A
persist(_FD, _Status) ->
    ok.

owner(_FD, _Owner) ->
    ok.

group(__FD, _Group) ->
    ok.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
