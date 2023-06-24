tunctl is an Erlang API for creating and using TUN/TAP interfaces.

## PRIVILEGES

### Linux

For IPv4 addresses, beam needs to have privileges to configure interfaces.

To add cap_net_admin capabilities:

```
 sudo setcap cap_net_admin=ep /path/to/bin/beam # or beam.smp
```

To check the privileges:

```
 getcap /path/to/bin/beam # or beam.smp
```

To remove the privileges:

```
 sudo setcap -r cap_net_admin=ep /path/to/bin/beam # or beam.smp
```

Currently, IPv6 addresses are configured by calling ifconfig using sudo
(see below).

### Mac OS X

Requires the tun/tap driver from:

http://tuntaposx.sourceforge.net/

Allow the user running tunctl to call ifconfig using sudo:

```
sudo visudo
youruser ALL=NOPASSWD: /sbin/ifconfig tap*
youruser ALL=NOPASSWD: /sbin/ifconfig tun*
```

### FreeBSD

tunctl uses the FreeBSD tuntap legacy interface.

1. Ensure the tap device kernel module is loaded:

   ```
    $ kldstat
    $ kldload if_tap
   ```

   If you want the tap driver loaded on boot, add to /boot/loader.conf:

   ```
    if_tap_load="YES"
   ```

2. Check cloning is enabled:

   ```
    $ sysctl net.link.tun.devfs_cloning
    net.link.tun.devfs_cloning: 1

    $ sysctl net.link.tap.devfs_cloning
    net.link.tap.devfs_cloning: 1
   ```

3. Allow the user running tunctl to call ifconfig using sudo:

   ```
    sudo visudo
    youruser ALL=NOPASSWD: /sbin/ifconfig tap*
    youruser ALL=NOPASSWD: /sbin/ifconfig tun*
   ```

## EXPORTS

### tuncer

Tuncer is a stand up guy and just like him, tuncer has your back.

```
create() -> {ok, PID}
create(Device) -> {ok, PID}
create(Device, Options) -> {ok, PID}

    Types   Device = [ string() | binary() ]
            Options = [ Flag ]
            Flag = [ tun | tap | no_pi | one_queue | multi_queue | vnet_hdr | tun_excl
                    | {active, false} | {active, true} | {namespace, NameSpace} ]

    Device is the TUN/TAP interface name. If an interface name is not
    specified, the TUN/TAP driver will choose one (for tap devices,
    starting from "tap0"; for tun devices, beginning from "tun0").

    When the device is in {active, true} mode, data is sent as
    messages:

        {tuntap, PID, binary()}

    If an error is encountered:

        {tuntap_error, PID, posix()}

    Retrieving data from devices in {active, false} mode can be done
    using recv/1,2 or read/1,2.

    Options contains a list of flags.

        tun: create a tun interface

        tap: create a tap interface

        no_pi: do not prepend the data with a 4 byte header describing
               the physical interface

    The options default to [tap, no_pi, {active, false}].

destroy(Ref) -> ok

    Types   Ref = pid()

    Remove the TUN/TAP interface.

send(Ref, Data) -> ok | {error, posix()}

    Types   Ref = pid()
            Data = binary()

    Write data to the tun/tap device.

recv(Ref) -> {ok, Buf} | {error, posix()}
recv(Ref, Size) -> {ok, Buf} | {error, posix()}

    Types   Ref = pid()
            Size = integer()
            Buf = binary()

    Read data from the tuntap interface.

    If the device is in {active, true} mode, recv/1,2 will return
    {error, einval}.

setopt(Ref, Option) -> ok | {error, posix()}

    Types   Ref = pid()
            Option = {active, true} | {active, false}

    Set an option. setopt/2 can be used for performing flow control
    when active mode is enabled.

controlling_process(Ref, NewOwner) -> ok | {error, posix()}

    Types   Ref = NewOwner = pid()

    Transfer ownership of the tuntap device to another process.

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

dstaddr(Ref, IP) -> ok | {error, posix()}

    Types   Ref = pid()
            IP = IPv4
            IPv4 = list() | tuple()

    Configure the remote address for a TUN/TAP interface in
    point-to-point mode.

    Currently only IPv4 addresses on Linux are supported.

broadcast(Ref, IP) -> ok | {error, posix()}

    Types   Ref = pid()
            IP = IPv4
            IPv4 = list() | tuple()

    Configure the broadcast address for a TUN/TAP interface.

    Currently only IPv4 addresses on Linux are supported.

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
            Size = integer()
            Buf = binary()

    Read data from the tuntap interface.

write(Fd, Buf) -> ok | {error, posix()}

    Types   Ref = integer()
            Buf = binary()

    Write _Buf_ to the interface.

devname(Ref) -> Devname

    Types   Devname = binary()

    Returns the TUN/TAP device name.

flags(Ref) -> integer()

    Returns an integer holding the interface creation flags.
```

### tunctl

tunctl does the actual tun/tap device manipulation. Some functions take
a device name, others a file descriptor. It is up to the caller to make
sure the file descriptors are closed (the device will disappear after
the fd is closed if the device is not persistent).

```
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
```

## EXAMPLES

* "Passive" mode

  ```
    1> {ok, Ref} = tuncer:create().
    {ok,<0.34.0>}

    2> tuncer:devname(Ref).
    <<"tap0">>

    3> tuncer:up(Ref, "192.168.123.4").
    ok

    4> FD = tuncer:getfd(Ref).
    9

    5> {ok, Buf} = tuncer:read(FD, 1500).
    {ok,<<1,0,94,0,0,22,190,138,20,22,76,120,8,0,70,192,0,40,
          0,0,64,0,1,2,200,76,192,...>>}

    6> tuncer:destroy(Ref).
    ok
  ```

* Active mode

  ```
    1> {ok, Ref} = tuncer:create(<<>>, [tap, no_pi, {active, true}]).
    {ok,<0.34.0>}

    2> tuncer:devname(Ref).
    <<"tap0">>

    3> tuncer:up(Ref, "192.168.123.4").
    ok

    4> flush().
    Shell got {tuntap,<0.47.0>,
                      <<51,51,0,0,0,22,250,6,27,10,131,177,134,221,96,0,0,0,0,36,
                        0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,2,0,0,0,0,0,0,0,0,
                        0,0,0,0,0,22,58,0,5,2,0,0,1,0,143,0,235,206,0,0,0,1,4,0,0,
                        0,255,2,0,0,0,0,0,0,0,0,0,1,255,10,131,177>>}
    Shell got {tuntap,<0.47.0>,
                      <<51,51,255,10,131,177,250,6,27,10,131,177,134,221,96,0,0,0,
                        0,24,58,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,2,0,0,0,0,
                        0,0,0,0,0,1,255,10,131,177,135,0,98,169,0,0,0,0,254,128,0,
                        0,0,0,0,0,248,6,27,255,254,10,131,177>>}

    5> tuncer:destroy(Ref).
    ok
  ```

### vpwn

vpwn will set up a point to point tunnel over the Erlang distribution
protocol.

Compile vpwn on the source and destination nodes:

```
make examples
```

Run Erlang on the destination node:

```
./start.sh -setcookie OMNOMNOM -name node
```

And on the source node:

```
./start.sh -setcookie OMNOMNOM -name node
```

Then start up the tunnel (replace the host name):

```
vpwn:start('node@vpn.example.com', "10.10.10.1", "10.10.10.2").
```

Then connect over the tunnel to the second node:

```
ping 10.10.10.2
ssh 10.10.10.2
```

### Bridging

`br` is an example of a simple bridge that floods frames to all the switch
ports. `br` uses a tap device plugged into a Linux bridge as an
uplink port and 1 or more tap devices as the switch ports.

This example uses the tap devices as interfaces for Linux containers
(LXC).

* Create a bridge and attach the physical ethernet interface

```
# /etc/network/interfaces
iface br0 inet dhcp
    bridge_ports eth0
    bridge_stp off
    bridge_fd 0
    bridge_maxwait 0
```

* Start the bridge:

  * `erlbr0` is the name of the tap device connected to the bridge
  * `erl0, erl1, erl2` are the tap devices used by the containers

```
br:start(["erlbr0", "erl0", "erl1", "erl2"]).
```

* In another shell, as root, bring up the uplink and attach it to the bridge:

```
# ifconfig erlbr0 up
# brctl addif br0 erlbr0
# brctl show br0
bridge name     bridge id               STP enabled     interfaces
br0             8000.4aec6d3a44d1       no              erlbr0
```

* Move the switch port interface into the container. The interface name inside the container will be known as "erl0".

```
lxc.network.type=phys
lxc.network.link=erl0
lxc.network.flags=up
```

## TODO

* Linux:
  * the TUNSETIFF ioctl request to create the interface requires
    CAP_NET_ADMIN privileges. Look at moving the interface creation
    into the procket setuid binary for OSes that use the multiplexing
    dev.

  * add support for tun filtering

* make sure tuncer can never leak file descriptors
