
tunctl is an Erlang API for creating and using TUN/TAP interfaces.


## PRIVILEGES

### Linux

For IPv4 addresses, beam needs to have privileges to configure interfaces.

To add cap\_net\_admin capabilities:

     sudo setcap cap_net_admin=ep /path/to/bin/beam # or beam.smp

To check the privileges:

     getcap cap_net_admin=ep /path/to/bin/beam # or beam.smp

To remove the privileges

     sudo setcap -r cap_net_admin=ep /path/to/bin/beam # or beam.smp

Currently, IPv6 addresses are configured by calling ifconfig using sudo
(see below).

### Mac OS X

Requires the tun/tap driver from:

<http://tuntaposx.sourceforge.net/>

Allow the user running tunctl to call ifconfig using sudo:

    sudo visudo
    youruser ALL=NOPASSWD: /sbin/ifconfig tap*
    youruser ALL=NOPASSWD: /sbin/ifconfig tun*

### FreeBSD

tunctl uses the FreeBSD tuntap legacy interface.

1. Ensure the tap device kernel module is loaded:

	    $ kldstat
	    $ kldload if_tap

    If you want the tap driver loaded on boot, add to /boot/loader.conf:

        if_tap_load="YES"

2. Check cloning is enabled:

        $ sysctl net.link.tun.devfs_cloning
        net.link.tun.devfs_cloning: 1

        $ sysctl net.link.tap.devfs_cloning
        net.link.tap.devfs_cloning: 1

3. Allow the user running tunctl to call ifconfig using sudo:

        sudo visudo
        youruser ALL=NOPASSWD: /sbin/ifconfig tap*
        youruser ALL=NOPASSWD: /sbin/ifconfig tun*


## EXPORTS

### tuncer

Tuncer is a stand up guy and just like him, tuncer has your back.

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
    
    up(Ref, IP) -> ok | {error, posix()}
    
        Types   Ref = pid()
                IP = IPv4 | IPv6
                IPv4 = list() | tuple()
                IPv6 = list() | tuple()
    
        Configure a TUN/TAP interface using the default netmask and broadcast
        for the network.
    
    down(Ref) -> ok | {error, posix()}
    
        Types   Ref = pid()
    
        Unconfigure a TUN/TAP interface.
    
    persist(Ref, Boolean) -> ok | {error, posix()}
    
        Types   Ref = pid()
                Boolean = [ true | false ]
    
        (Linux only)

        Set the interface to exist after the Erlang process exits.

    owner(Ref, Owner) -> ok | {error, posix()}

        Types   Ref = pid()
                Owner = integer()

        (Linux only)

        Set the uid owning the interface.

    group(Ref, Group) -> ok | {error, posix()}

        Types   Ref = pid()
                Group = integer()

        (Linux only)

        Set the gid owning the interface.

    getfd(Ref) -> integer()

        Types   Ref = pid()

    Get the file descriptor associated with the process. Use getfd/1
    with read/1,2 and write/2 to interact directly with the tuntap device
    (bypassing the gen_server).

    read(Fd) -> {ok, Buf} | {error, posix()}
    read(Fd, Size) -> {ok, Buf} | {error, posix()}

        Types   Fd = integer()
                Size = integer() Buf = binary()

        Read _Size_ bytes from the interface.

    write(Fd, Buf) -> ok | {error, posix()}

        Types   Ref = integer()
                Buf = binary()

        Write _Buf_ to the interface.

    devname(Ref) -> Devname

        Types   Devname = binary()

        Returns the TUN/TAP device name.

    flags(Ref) -> integer()

        Returns an integer holding the interface creation flags.

### tunctl

tunctl does the actual tun/tap device manipulation. Some functions take
a device name, others a file descriptor. It is up to the caller to make
sure the file descriptors are closed (the device will disappear after
the fd is closed if the device is not persistent).

    create() -> {ok, FD, Device}
    create(Ifname) -> {ok, FD, Device}
    create(Ifname, Flags) -> {ok, FD, Device}

        Types   FD = integer()
                Device = binary()
                Flags = list()

    persist(FD, Bool) -> ok | {error, posix()}

        Types   FD = integer()
                Bool = true | false

    owner(FD, UID) -> ok | {error, posix()}

        Types   FD = integer()
                UID = integer()

    group(FD, GID) -> ok | {error, posix()}

        Types   FD = integer()
                UID = integer()

    up(Device, IPv4Address) -> ok

        Types   Device = binary()
                IPv4Address = tuple()

    down(Device) -> ok

        Types   Device = binary()


## EXAMPLES

    1> {ok, Ref} = tuncer:create().
    {ok,<0.34.0>}

    2> tuncer:devname(Ref).
    <<"tap0">>

    3> tuncer:up(Ref, "192.168.123.4").
    ok

    4> {ok, Buf} = tuncer:read(Ref, 1500).
    {ok,<<1,0,94,0,0,22,190,138,20,22,76,120,8,0,70,192,0,40,
          0,0,64,0,1,2,200,76,192,...>>}

    5> tuncer:destroy(Ref).
    ok

## TODO

* Linux:
    * the TUNSETIFF ioctl request to create the interface requires
      CAP\_NET\_ADMIN privileges. Look at moving the interface creation
      into the procket setuid binary for OSes that use the multiplexing
      dev.

    * support setting IPv6 address using ioctl

    * support setting netmask using ioctl

    * add support for tun filtering

* make sure tuncer can never leak file descriptors

* support for setting pointopoint

* add support for {active, mode}
