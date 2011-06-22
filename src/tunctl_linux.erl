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
-module(tunctl_linux).

-include("tuntap.hrl").
-include("ioctl.hrl").
-include("procket.hrl").

-export([
        create/2,
        persist/2,
        owner/2, group/2,
        up/3, down/1,

        header/1
    ]).


-define(SIOCGIFFLAGS, 16#8913).
-define(SIOCSIFFLAGS, 16#8914).
-define(SIOCSIFADDR, 16#8916).

-define(TUNDEV, "net/tun").

%%--------------------------------------------------------------------
%%% Exports
%%--------------------------------------------------------------------
create(<<>>, Opt) ->
    create(<<0:(15*8)>>, Opt);
create(Ifname, Opt) when byte_size(Ifname) < ?IFNAMSIZ, is_list(Opt) ->
    case procket:dev(?TUNDEV) of
        {ok, FD} ->
            create_1(FD, Ifname, Opt);
        Error ->
            Error
    end.

create_1(FD, Ifname, Opt) ->
    Flag = lists:foldl(fun(N, F) -> F bor flag(N) end, 0, Opt),
    Result = procket:ioctl(FD, ?TUNSETIFF, <<
        Ifname/binary,
        0:((15*8) - (byte_size(Ifname)*8)), 0:8,    % ifrn_name[IFNAMSIZ]: interface name
        Flag:2/native-signed-integer-unit:8,        % ifru_flags
        0:(14*8)
    >>),
    case Result of
        {ok, Dev} ->
            {ok, FD, hd(binary:split(Dev, <<0>>))};
        Error ->
            ok = procket:close(FD),
            Error
    end.


persist(FD, Status) ->
    tunctl:ioctl(FD, ?TUNSETPERSIST, Status).


%%
%% Change the owner/group of the tun device
%%
owner(FD, Owner) when is_integer(FD), is_integer(Owner) ->
    tunctl:ioctl(FD, ?TUNSETOWNER, int_to_bin(Owner)).

group(FD, Group) when is_integer(FD), is_integer(Group) ->
    tunctl:ioctl(FD, ?TUNSETGROUP, int_to_bin(Group)).


%%
%% Configure the interface just like ifconfig except
%% with fewer features and no error checking.
%%
%% Also, we ignore the mask.
%%
up(Dev, {A,B,C,D}, _Mask) when byte_size(Dev) < ?IFNAMSIZ ->
    {ok, Socket} = procket:socket(inet, dgram,  0),

    % struct sockaddr_in
    % dev[IFNAMSIZ], family:2 bytes, port:2 bytes, ipaddr:4 bytes
    Ifr = <<Dev/bytes, 0:( (?IFNAMSIZ - byte_size(Dev) - 1)*8), 0:8,
        ?PF_INET:16/native, 0:16, A:8, B:8, C:8, D:8, 0:(8*8)>>,

    Res = try ok = tunctl:ioctl(Socket, ?SIOCSIFADDR, Ifr),
        {ok, Flag} = get_flag(Socket, Dev),
        ok = set_flag(Socket, Dev, Flag bor ?IFF_RUNNING bor ?IFF_UP) of
        _ -> ok
    catch
        error:Error ->
            Error
    end,

    ok = procket:close(Socket),
    Res.

down(Dev) when byte_size(Dev) < ?IFNAMSIZ ->
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


header(<<Flags:?UINT16, Proto:?UINT16, Buf/binary>>) ->
    {tun_pi, Flags, Proto, Buf}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%
%% tun/tap options
%%
flag(tun) -> ?IFF_TUN;
flag(tap) -> ?IFF_TAP;
flag(no_pi) -> ?IFF_NO_PI;
flag(one_queue) -> ?IFF_ONE_QUEUE;
flag(vnet_hdr) -> ?IFF_VNET_HDR;
flag(tun_excl) -> ?IFF_TUN_EXCL;
flag(_) -> 0.

set_flag(FD, Dev, Flag) ->
    tunctl:ioctl(FD, ?SIOCSIFFLAGS,
        <<Dev/bytes, 0:((15-byte_size(Dev))*8), 0:8,
        Flag:2/native-signed-integer-unit:8,
        0:(14*8)>>).
get_flag(FD, Dev) ->
    {ok, <<_:(16*8), Flag:2/native-signed-integer-unit:8, _/binary>>} = procket:ioctl(
        FD, ?SIOCGIFFLAGS, <<Dev/bytes, 0:((15-byte_size(Dev))*8), 0:(16*8)>>
    ),
    {ok, Flag}.


int_to_bin(Int) ->
    <<Int:4/native-integer-unsigned-unit:8>>.
