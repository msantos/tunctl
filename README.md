
tunctl is a pure Erlang API for creating and using TUN/TAP interfaces.


## EXPORTS

    create() -> {ok, PID}
    create(Device) -> {ok, PID}
    create(Device, Options) -> {ok, PID}
    
        Types   Device = [ string() | binary() ]
                Options = [ Flag ]
                Flag = [ tun | tap | no_pi | one_queue | vnet_hdr | tun_excl ]
    
        Device is the TUN/TAP interface name. If an interface name is not
        specified, the TUN/TAP driver will choose one (for tap devices,
        starting from "tap0"; for tun devices, beginning from "tun0").
    
        Options contains a list of flags.
    
            tun: create a tun interface
    
            tap: create a tap interface
    
            no_pi: do not prepend the data with a 4 byte header describing
                   the physical interface
    
        The options default to [tap, no_pi].
    
    destroy(Ref) -> ok
    
        Types   Ref = pid()
    
        Remove the TUN/TAP interface.
    
    up(Ref, IPv4) -> ok | {error, posix()}
    
        Types   Ref = pid()
                IPv4 = list() | tuple()
    
        Configure a TUN/TAP interface using the default netmask and broadcast
        for the network.
    
    down(Ref) -> ok | {error, posix()}
    
        Types   Ref = pid()
    
        Unconfigure a TUN/TAP interface.
    
    persist(Ref, Boolean) -> ok | {error, posix()}
    
        Types   Ref = pid()
                Boolean = [ true | false ]
    
        Enable/disable interface persistence.
    
    owner(Ref, Owner) -> ok | {error, posix()}
    
        Types   Ref = pid()
                Owner = integer()
    
        Set the uid owning the interface.
    
    group(Ref, Group) -> ok | {error, posix()}
    
        Types   Ref = pid()
                Group = integer()
    
        Set the gid owning the interface.
    
    read(Ref, Size) -> {ok, Buf} | {error, posix()}
    
        Types   Ref = pid()
                Size = integer()
                Buf = binary()
    
        Read _Size_ bytes from the interface.
    
    write(Ref, Buf) -> ok | {error, posix()}
    
        Types   Ref = pid()
                Buf = binary()
    
        Write _Buf_ to the interface.
    
    devname(Ref) -> Devname
    
        Types   Devname = binary()
    
        Returns the TUN/TAP device name.
    
    flags(Ref) -> integer()
    
        Returns an integer holding the interface creation flags.



## EXAMPLES

    1> {ok, Ref} = tunctl:create()
    {ok,<0.34.0>}

    2> tunctl:devname(Ref).
    <<"tap0">>

    3> tunctl:up(Ref, "192.168.123.4").
    ok

    4> tunctl:read(Ref, 1024).
    {ok,<<1,0,94,0,0,22,190,138,20,22,76,120,8,0,70,192,0,40,
          0,0,64,0,1,2,200,76,192,...>>}

    5> tunctl:destroy(Ref).

## TODO

* on Linux, the TUNSETIFF ioctl request to create the interface requires
  CAP_NET_ADMIN privileges. Look at moving the interface creation into
  the procket setuid binary for OSes that use the multiplexing dev.

  WORK AROUND:
  setcap cap_net_admin=ep /path/to/bin/beam

* compat for Mac OS X: uses /dev/tunN and /dev/tapN devices

* compat for other BSDs: /dev/tun multiplex dev, probably the same issue
  with interface creation

* make sure tunctl can never leak file descriptors

* the tun device is not removed even after the fd is closed

* add {active,true} mode using open_port/2

* add support for the tun packet header

* add support for tun filtering

* simple vpn client/server example
