%%% @author tony <tony@rogvall.se>
%%% @copyright (C) 2013, tony
%%% @doc
%%%    Piface interface
%%% @end
%%% Created :  5 Apr 2013 by tony <tony@rogvall.se>

-module(piface).

-export([init/0]).
-export([gpio_get/1, gpio_set/1, gpio_clr/1]).

-include("../include/spi.hrl").

-define(SPI_BUS, 0).
-define(SPI_DEVICE, 0).

-define(TRANSFER_LEN, 3).
-define(TRANSFER_DELAY, 5).
-define(TRANSFER_SPEED, 1000000).
-define(TRANSFER_BPW, 8).

-define(SPI_WRITE_CMD, 16#40).
-define(SPI_READ_CMD,  16#41).

%% Port configuration
-define(IODIRA, 16#00).    %% I/O direction A
-define(IODIRB, 16#01).    %% I/O direction B
-define(IOCON,  16#0A).     %% I/O config
-define(GPIOA,  16#12).     %% port A
-define(GPIOB,  16#13).     %% port B
-define(GPPUA,  16#0C).     %% port A pullups
-define(GPPUB,  16#0D).     %% port B pullups
-define(OUTPUT_PORT, ?GPIOA).
-define(INPUT_PORT,  ?GPIOB).
-define(GPINTENA, 16#04).
-define(GPINTENB, 16#05).
-define(DEFVALA,  16#06).
-define(DEFVALB,  16#07).
-define(INTCONA,  16#08).
-define(INTCONB,  16#09).

-define(IOCON_HAEN,  2#00001000).
-define(IOCON_MIRROR, 2#01000000).

%% set up some ports
%% enable hardware addressing + mirror interrupts
%% I am not sure if MIRROR is needed, because I have not got a up-to-date
%% schematic of the piface card.

init() ->
    ok = spi:open(?SPI_BUS, ?SPI_DEVICE),
    spi_write(?IOCON,  ?IOCON_HAEN bor ?IOCON_MIRROR),
    spi_write(?IODIRA, 0),    %% set port A as outputs
    spi_write(?IODIRB, 16#FF), %% set port B as inputs
    spi_write(?GPIOA,  16#FF), %% set port A on
    %% spi:write(ID, ?GPIOB,  0xFF), %% set port B on
    spi_write(?GPPUA,  16#FF), %% set port A pullups on
    spi_write(?GPPUB,  16#FF), %% set port B pullups on
    spi_write(?INTCONB,  16#00), %% interrupt on any change
    spi_write(?GPINTENB, 16#FF), %% enable interrupts on B
    ok.

-spec gpio_get(Pin::uint8()) -> boolean().

gpio_get(Pin) when ?is_uint8(Pin) ->
    Bits = read_input(),
    Bits band (Pin bsl 1) =/= 0.
 
-spec gpio_set(Pin::uint8()) -> ok | {error,posix()}.
gpio_set(Pin) when ?is_uint8(Pin) ->
    Bits = read_output(),
    write_output(Bits bor (1 bsl Pin)).

-spec gpio_clr(Pin::uint8()) -> ok | {error,posix()}.
gpio_clr(Pin) when ?is_uint8(Pin) ->
    Bits = read_output(),
    write_output(Bits band (bnot (1 bsl Pin))).

read_input() ->
    spi_read(?INPUT_PORT).

read_output() ->
    spi_read(?OUTPUT_PORT).

write_output(Value) ->
    spi_write(?OUTPUT_PORT, Value).

spi_write(Port, Value) ->
    case spi:transfer(?SPI_BUS, ?SPI_DEVICE,
		      <<?SPI_WRITE_CMD, Port, Value>>,
		      ?TRANSFER_LEN,
		      ?TRANSFER_DELAY,
		      ?TRANSFER_SPEED,
		      ?TRANSFER_BPW, 0) of
	{ok,_Data} -> ok;
	Error -> Error
    end.

spi_read(Port) ->
    case spi:transfer(?SPI_BUS, ?SPI_DEVICE,
		      <<?SPI_READ_CMD, Port, 16#ff>>,
		      ?TRANSFER_LEN,
		      ?TRANSFER_DELAY,
		      ?TRANSFER_SPEED,
		      ?TRANSFER_BPW, 0) of
	{ok, <<_,_,Bits>>} -> Bits;
	{ok, _} -> {error,badbits};
	Error -> Error
    end.
