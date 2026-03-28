[![Package Version](https://img.shields.io/hexpm/v/epcap)](https://hex.pm/packages/tunctl)
[![Hex Docs](https://img.shields.io/badge/hex-docs)](https://hexdocs.pm/tunctl/)

tunctl is an Erlang API for creating and using TUN/TAP interfaces.

## PRIVILEGES

### Linux

For IPv4 addresses, beam needs to have privileges to configure interfaces.

To add cap_net_admin capabilities:

```bash
 sudo setcap cap_net_admin=ep /path/to/bin/beam.smp
```

To check the privileges:

```bash
 getcap /path/to/bin/beam.smp
```

To remove the privileges:

```bash
 sudo setcap -r cap_net_admin=ep /path/to/bin/beam.smp
```

Currently, IPv6 addresses are configured by calling ifconfig using sudo
(see below).

### Mac OS X

Requires the tun/tap driver from:

http://tuntaposx.sourceforge.net/

Allow the user running tunctl to call ifconfig using sudo:

```bash
sudo visudo
youruser ALL=NOPASSWD: /sbin/ifconfig tap*
youruser ALL=NOPASSWD: /sbin/ifconfig tun*
```

### FreeBSD

tunctl uses the FreeBSD tuntap legacy interface.

1. Ensure the tap device kernel module is loaded:

   ```bash
    $ kldstat
    $ kldload if_tap
   ```

   If you want the tap driver loaded on boot, add to /boot/loader.conf:

   ```bash
    if_tap_load="YES"
   ```

2. Check cloning is enabled:

   ```bash
    $ sysctl net.link.tun.devfs_cloning
    net.link.tun.devfs_cloning: 1

    $ sysctl net.link.tap.devfs_cloning
    net.link.tap.devfs_cloning: 1
   ```

3. Allow the user running tunctl to call ifconfig using sudo:

   ```bash
    sudo visudo
    youruser ALL=NOPASSWD: /sbin/ifconfig tap*
    youruser ALL=NOPASSWD: /sbin/ifconfig tun*
   ```

## EXAMPLES

* "Passive" mode

  ```erlang
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

  ```erlang
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

```bash
erlc -I deps -o ebin examples/*.erl
```

Run Erlang on the destination node:

```bash
erl -pa deps/*/ebin ebin -setcookie OMNOMNOM -name node
```

And on the source node:

```bash
erl -pa deps/*/ebin ebin -setcookie OMNOMNOM -name node
```

Then start up the tunnel (replace the host name):

```erlang
vpwn:start('node@vpn.example.com', "10.10.10.1", "10.10.10.2").
```

Then connect over the tunnel to the second node:

```bash
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

```erlang
br:start(["erlbr0", "erl0", "erl1", "erl2"]).
```

* In another shell, as root, bring up the uplink and attach it to the bridge:

```bash
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
