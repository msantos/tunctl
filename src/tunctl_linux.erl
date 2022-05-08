%%% Copyright (c) 2011-2022 Michael Santos <michael.santos@gmail.com>. All
%%% rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice,
%%% this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(tunctl_linux).

-include("tuntap.hrl").

-include_lib("procket/include/ioctl.hrl").
-include_lib("procket/include/procket.hrl").

-export([
    create/2,
    persist/2,
    owner/2,
    group/2,
    up/3,
    dstaddr/2,
    broadcast/2,
    down/1,

    mtu/3,

    header/1
]).

-define(SIOCGIFFLAGS, 16#8913).
-define(SIOCSIFFLAGS, 16#8914).
-define(SIOCSIFADDR, 16#8916).

-define(SIOCSIFMTU, 16#8922).

-define(SIOCSIFNETMASK, 16#891c).
% set remote PA address
-define(SIOCSIFDSTADDR, 16#8918).
% set broadcast PA address
-define(SIOCSIFBRDADDR, 16#891a).

-define(SIOGIFINDEX, 16#8933).

-define(TUNDEV, "net/tun").

%%--------------------------------------------------------------------
%%% Exports
%%--------------------------------------------------------------------
create(<<>>, Opt) ->
    create(<<0:(15 * 8)>>, Opt);
create(Ifname, Opt) when byte_size(Ifname) < ?IFNAMSIZ, is_list(Opt) ->
    % support namespace
    DevOpt =
        case proplists:get_value(namespace, Opt) of
            undefined ->
                [];
            Val ->
                [{namespace, Val}]
        end,
    case procket:dev(?TUNDEV, DevOpt) of
        {ok, FD} ->
            create_1(FD, Ifname, Opt);
        Error ->
            Error
    end.

create_1(FD, Ifname, Opt) ->
    Flag = lists:foldl(fun(N, F) -> F bor flag(N) end, 0, Opt),
    Result = procket:ioctl(FD, ?TUNSETIFF, <<
        Ifname/binary,
        % ifrn_name[IFNAMSIZ]: interface name
        0:((15 * 8) - (byte_size(Ifname) * 8)),
        0:8,
        % ifru_flags
        Flag:2/native-signed-integer-unit:8,
        0:(14 * 8)
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
up(Dev, {A, B, C, D}, Mask) when byte_size(Dev) < ?IFNAMSIZ, Mask >= 0, Mask =< 32 ->
    % struct sockaddr_in
    % dev[IFNAMSIZ], family:2 bytes, port:2 bytes, ipaddr:4 bytes
    case procket:socket(inet, dgram, 0) of
        {ok, Sock} ->
            Ifr =
                <<Dev/bytes, 0:((?IFNAMSIZ - byte_size(Dev) - 1) * 8), 0:8, ?PF_INET:16/native,
                    0:16, A:8, B:8, C:8, D:8, 0:(8 * 8)>>,
            case ifreq(Dev, Sock, Ifr, ?SIOCSIFADDR, ?IFF_RUNNING bor ?IFF_UP) of
                ok ->
                    ifreq(
                        Dev,
                        Sock,
                        <<Dev/bytes, 0:((?IFNAMSIZ - byte_size(Dev) - 1) * 8), 0:8,
                            ?PF_INET:16/native, 0:16,
                            ((16#ffffffff bsl (32 - Mask)) band 16#ffffffff):4/big-unsigned-integer-unit:8>>,
                        ?SIOCSIFNETMASK,
                        0
                    );
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
up(Dev, {A, B, C, D, E, F, G, H}, Mask) when byte_size(Dev) < ?IFNAMSIZ ->
    case procket:socket(inet6, dgram, 0) of
        {ok, Sock} ->
            {ok, IfIdx} = get_ifindex(Sock, Dev),
            BinAddr = <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>,
            Ifr = <<BinAddr/binary, Mask:32/little, IfIdx:4/native-signed-integer-unit:8>>,
            Res = ifreq(Dev, Sock, Ifr, ?SIOCSIFADDR, ?IFF_RUNNING bor ?IFF_UP),
            ok = procket:close(Sock),
            Res;
        {error, _} = Error ->
            Error
    end.

mtu(FD, Ifname, MTU) when byte_size(Ifname) < ?IFNAMSIZ ->
    case procket:socket(inet, dgram, 0) of
        {ok, Sock} ->
            Result = procket:ioctl(Sock, ?SIOCSIFMTU, <<
                Ifname/binary,
                % ifrn_name[IFNAMSIZ]: interface name
                0:((15 * 8) - (byte_size(Ifname) * 8)),
                0:8,
                % ifr_mtu
                MTU:4/native-signed-integer-unit:8,
                0:(12 * 8)
            >>),
            ok = procket:close(Sock),
            case Result of
                {ok, Dev} ->
                    {ok, FD, hd(binary:split(Dev, <<0>>))};
                Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

dstaddr(Dev, {A, B, C, D}) when byte_size(Dev) < ?IFNAMSIZ ->
    % struct sockaddr_in
    % dev[IFNAMSIZ], family:2 bytes, port:2 bytes, ipaddr:4 bytes
    case procket:socket(inet, dgram, 0) of
        {ok, Sock} ->
            Ifr =
                <<Dev/bytes, 0:((?IFNAMSIZ - byte_size(Dev) - 1) * 8), 0:8, ?PF_INET:16/native,
                    0:16, A:8, B:8, C:8, D:8, 0:(8 * 8)>>,
            Res = ifreq(Dev, Sock, Ifr, ?SIOCSIFDSTADDR, ?IFF_POINTOPOINT),
            ok = procket:close(Sock),
            Res;
        {error, _} = Error ->
            Error
    end;
dstaddr(Dev, {_A, _B, _C, _D, _E, _F, _G, _H}) when byte_size(Dev) < ?IFNAMSIZ ->
    % See netdevice(7):
    % Adding a new IPv6 address and deleting an existing IPv6 address
    % can be done via SIOCSIFADDR and SIOCDIFADDR or via rtnetlink(7).
    % Retrieving or changing destination IPv6 addresses of a point-to-point
    % interface is possible only via rtnetlink(7).
    {error, enodev}.

broadcast(Dev, {A, B, C, D}) when byte_size(Dev) < ?IFNAMSIZ ->
    % struct sockaddr_in
    % dev[IFNAMSIZ], family:2 bytes, port:2 bytes, ipaddr:4 bytes
    case procket:socket(inet, dgram, 0) of
        {ok, Sock} ->
            Ifr =
                <<Dev/bytes, 0:((?IFNAMSIZ - byte_size(Dev) - 1) * 8), 0:8, ?PF_INET:16/native,
                    0:16, A:8, B:8, C:8, D:8, 0:(8 * 8)>>,
            Res = ifreq(Dev, Sock, Ifr, ?SIOCSIFBRDADDR, 0),
            ok = procket:close(Sock),
            Res;
        {error, _} = Error ->
            Error
    end;
broadcast(Dev, {_A, _B, _C, _D, _E, _F, _G, _H}) when byte_size(Dev) < ?IFNAMSIZ ->
    {error, enodev}.

ifreq(Dev, Sock, Ifr, Op, Flags) ->
    try tunctl:ioctl(Sock, Op, Ifr) of
        ok ->
            {ok, Flag} = get_flag(Sock, Dev),
            ok = set_flag(Sock, Dev, Flag bor Flags);
        {error, eexist} ->
            ok;
        {error, _} = Error ->
            Error
    catch
        error:Error ->
            {error, Error}
    end.

down(Dev) when byte_size(Dev) < ?IFNAMSIZ ->
    {ok, Socket} = procket:socket(inet, dgram, 0),

    Res =
        try
            {ok, Flags} = get_flag(Socket, Dev),
            ok = set_flag(Socket, Dev, Flags band bnot (?IFF_UP))
        of
            _ -> ok
        catch
            error:Error ->
                {error, Error}
        end,

    ok = procket:close(Socket),
    Res.

header(<<?UINT16(Flags), ?UINT16(Proto), Buf/binary>>) ->
    {tun_pi, Flags, Proto, Buf}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%
%% tun/tap options
%%
flag(tun) -> ?IFF_TUN;
flag(tap) -> ?IFF_TAP;
flag(multi_queue) -> ?IFF_MULTI_QUEUE;
flag(no_pi) -> ?IFF_NO_PI;
flag(one_queue) -> ?IFF_ONE_QUEUE;
flag(vnet_hdr) -> ?IFF_VNET_HDR;
flag(tun_excl) -> ?IFF_TUN_EXCL;
flag(_) -> 0.

set_flag(FD, Dev, Flag) ->
    tunctl:ioctl(
        FD,
        ?SIOCSIFFLAGS,
        <<Dev/bytes, 0:((15 - byte_size(Dev)) * 8), 0:8, Flag:2/native-signed-integer-unit:8,
            0:(14 * 8)>>
    ).

get_flag(FD, Dev) ->
    {ok, <<_:(16 * 8), Flag:2/native-signed-integer-unit:8, _/binary>>} = procket:ioctl(
        FD,
        ?SIOCGIFFLAGS,
        <<Dev/bytes, 0:((15 - byte_size(Dev)) * 8), 0:(16 * 8)>>
    ),
    {ok, Flag}.

get_ifindex(FD, Dev) ->
    {ok, <<_:(16 * 8), Index:4/native-signed-integer-unit:8, _/binary>>} = procket:ioctl(
        FD,
        ?SIOGIFINDEX,
        <<Dev/bytes, 0:((15 - byte_size(Dev)) * 8), 0:(16 * 8)>>
    ),
    {ok, Index}.

int_to_bin(Int) ->
    <<Int:4/native-integer-unsigned-unit:8>>.
