%%% @author  <pi@raspberrypi>
%%% @copyright (C) 2014, 
%%% @doc
%%%    Demo of ADC-DAC for raspberry pi
%%% @end
%%% Created : 12 Feb 2014 by  <pi@raspberrypi>

-module(spi_adcdac).
-compile(export_all).


start() ->
    application:start(spi),
    spi:open(0,0).

stop() ->
    spi:close(0,0).

raw_read(Channel) when is_integer(Channel), Channel >= 0, Channel =< 1 ->
    spi:transfer(0, 0, <<1,((2+Channel) bsl 6), 0>>, 3,
		 0, 2500000, 8, 0).

read(Channel) when is_integer(Channel), Channel >= 0, Channel =< 1 ->
    {ok,<<_,R1,R2>>} = raw_read(Channel),
    ((R1 band 16#f) bsl 8) + R2.
