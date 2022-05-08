-module(br).
-export([start/0, start/1]).


start() ->
    start(["erlbr0", "erl0"]).

start(Ifaces) ->
    % Switch ports
    Dev = [ begin
        {ok, N} = tuncer:create(Iface, [tap, no_pi, {active, true}]),
        N
      end || Iface <- Ifaces ],

    switch(Dev).

switch(Dev) ->
    receive
        {tuntap, Port, Data} ->
            % Data received on port: flood to all other ports and uplink
            error_logger:info_report([{dev, Port}, {data, Data}]),
            [ ok = tuncer:send(N, Data) || N <- Dev, N =/= Port ],
            switch(Dev);
        Error ->
            error_logger:error_report([{error, Error}])
    end.
