-module(tuncer_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    create/1,
    up/1,
    recvsend/1,
    readwrite/1,

    dstaddr/1,
    broadcast/1,

    no_os_specific_tests/1
]).

suite() ->
    Timeout = list_to_integer(os:getenv("TUNCER_TEST_TIMEOUT", "60")),
    [{timetrap, {seconds, Timeout}}].

all() ->
    {unix, OS} = os:type(),
    [
        {group, OS},
        create,
        up,
        recvsend,
        readwrite
    ].

groups() ->
    [
        {linux, [], [dstaddr, broadcast]},
        {freebsd, [], [no_os_specific_tests]},
        {darwin, [], [no_os_specific_tests]},
        {netbsd, [], [no_os_specific_tests]},
        {openbsd, [], [no_os_specific_tests]},
        {solaris, [], [no_os_specific_tests]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

ifname() ->
    lists:concat(["tuncer", erlang:unique_integer([positive, monotonic])]).

ipaddr() ->
    N = rand:uniform(16#00FFFFFF),
    {10, N band 16#FF0000 bsr 16, N band 16#FF00 bsr 8, N band 16#FF}.

init_per_testcase(Test, Config) ->
    {ok, Dev} = tuncer:create(ifname()),
    {ok, DevActive} = tuncer:create(ifname(), [tun, no_pi, {active, true}]),
    {ok, DevBusy} = tuncer:create(ifname(), [
        tun,
        no_pi,
        {active, true},
        {port_options, []}
    ]),
    [{Test, [Dev, DevActive, DevBusy]} | Config].

end_per_testcase(Test, Config) ->
    Devs = ?config(Test, Config),
    [tuncer:destroy(Dev) || Dev <- Devs],
    Config.

create(_Config) ->
    {ok, Dev} = tuncer:create("tuncer", [tun, no_pi, {active, false}]),
    ok = tuncer:destroy(Dev).

foreach(_, []) ->
    ok;
foreach(F, [Dev | Devs]) ->
    case F(Dev) of
        ok -> foreach(F, Devs);
        {ok, _} -> foreach(F, Devs);
        {error, _} = Error -> {failed, Error}
    end.

up(Config) ->
    Devs = ?config(up, Config),
    foreach(fun(Dev) -> tuncer:up(Dev, ipaddr()) end, Devs).

recv(Dev) ->
    case tuncer:recv(Dev) of
        {ok, _} = X ->
            X;
        {error, eagain} ->
            recv(Dev);
        {error, einval} ->
            receive
                {tuntap, Dev, Bin} -> {ok, Bin};
                {tuntap_error, Dev, Error} -> {error, Error}
            end;
        {error, _} = Error ->
            Error
    end.

recvsend(Config) ->
    Devs = ?config(recvsend, Config),
    foreach(
        fun(Dev) ->
            ok = tuncer:up(Dev, ipaddr()),
            {ok, Bin} = recv(Dev),
            tuncer:send(Dev, Bin)
        end,
        Devs
    ).

read(FD) ->
    case tuncer:read(FD) of
        {ok, _} = X -> X;
        {error, eagain} -> read(FD);
        {error, _} = Error -> Error
    end.

readwrite(Config) ->
    Devs = ?config(readwrite, Config),
    foreach(
        fun(Dev) ->
            ok = tuncer:up(Dev, ipaddr()),
            FD = tuncer:getfd(Dev),
            {ok, Bin} = read(FD),
            tuncer:write(FD, Bin)
        end,
        Devs
    ).

dstaddr(Config) ->
    Devs = ?config(dstaddr, Config),
    foreach(
        fun(Dev) ->
            ok = tuncer:up(Dev, "127.241.173.1"),
            tuncer:dstaddr(Dev, "127.241.173.2")
        end,
        Devs
    ).

broadcast(Config) ->
    Devs = ?config(broadcast, Config),
    foreach(
        fun(Dev) ->
            ok = tuncer:up(Dev, "127.241.174.1"),
            tuncer:broadcast(Dev, "127.241.174.3")
        end,
        Devs
    ).

no_os_specific_tests(_Config) ->
    {skip, "No OS specific tests defined"}.
