spi
===========

spi is a device driver application for SPI (Synchronous Peripheral Interface) written in erlang and C.

### Dependencies

To build spi you will need a working installation of Erlang R15B (or
later).<br/>
Information on building and installing [Erlang/OTP](http://www.erlang.org)
can be found [here](https://github.com/erlang/otp/wiki/Installation)
([more info](https://github.com/erlang/otp/blob/master/INSTALL.md)).

spi is built using rebar that can be found [here](https://github.com/rebar/rebar), with building instructions [here](https://github.com/rebar/rebar/wiki/Building-rebar). rebar's dynamic configuration mechanism, described [here](https://github.com/rebar/rebar/wiki/Dynamic-configuration), is used so the environment variable `REBAR_DEPS` should be set to the directory where your erlang applications are located.

### Download

Clone the repository in a suitable location:

```
$ git clone git://github.com/tonyrog/spi.git
```
### Build

Rebar will compile all needed dependencies.<br/>
Compile:

```sh
$ cd spi
$ rebar compile
...
==> spi (compile)
```

### Run

spi is started in a standard erlang fashion:

```
$ erl
(node@host) 1> application:start(spi).
```

### API

The following interface functions exist:
<ul>
<li>open</li>
<li>close</li>
<li>transfer</li>
<li>get_mode</li>
<li>get_bits_per_word/li>
<li>get_speed</li>
</ul>

For details see the source code.
