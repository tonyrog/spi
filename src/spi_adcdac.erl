%%% @author  <pi@raspberrypi>
%%% @copyright (C) 2014, 
%%% @doc
%%%    Demo of ADC-DAC for raspberry pi
%%% @end
%%% Created : 12 Feb 2014 by  <pi@raspberrypi>

-module(spi_adcdac).
-compile(export_all).

-include("../include/spi.hrl").

start() ->
    application:start(spi),
    open_adc().

stop() ->
    close_adc().

open_adc() ->
    spi:open(0,0).

close_adc() ->
    spi:close(0,0).

%% read one 12 bit sample from channel Chan
read(Chan) when is_integer(Chan), Chan >= 0, Chan =< 1 ->
    {ok,<<_,_:4,Sample:12>>} = raw_one(Chan),
    Sample.

%% read one raw 12 bit sample from channel Chan
raw_one(Chan) when is_integer(Chan), Chan band (bnot 1) =:= 0 ->
    TxBuf = <<1,((2+Chan) bsl 6), 0>>,
    RxLen = 3,
    spi:transfer(0, 0, TxBuf, RxLen, 0, 2500000, 8, 0).

%% read N 12 bit samples from channel Chan
read(Chan, N) when 
      Chan band (bnot 1) =:= 0, is_integer(N), N>=0 ->
    {ok,Data} = raw_multiple(Chan, N),
    [ X || <<_,_:4,X:12>> <= Data ].

%% read N (total) 12 bit samples interleaved from channel 0 and channel 1
read(Chan0,Chan1,N) when 
      Chan0 band (bnot 1) =:= 0,
      Chan1 band (bnot 1) =:= 0,
      is_integer(N), N>=0 ->
    {ok,Data} = raw_multiple(Chan0, Chan1, N),
    if N band 1 =:= 0 ->
	    [ {X1,X2} || <<_,_:4,X1:12,_,_:4,X2:12>> <= Data ];
       true ->
	    SzE = (N-1)*3,
	    <<Data1:SzE/binary,_,_:4,Xn:12>> = Data,
	    [ {X1,X2} || <<_,_:4,X1:12,_,_:4,X2:12>> <= Data1 ] ++ [Xn]
    end.
	    
%% read N raw samples
raw_multiple(_Chan,0) -> {ok, <<>>};
raw_multiple(Chan,N) when is_integer(N), N > 0, N =< 256 ->
    raw_multiple_(Chan, N, 100).

%% read N interleaved raw samples
raw_multiple(_Chan0,_Chan1, 0) -> {ok, <<>>};
raw_multiple(Chan0,Chan1,N) when is_integer(N), N > 0, N =< 256 ->
    raw_multiple_(Chan0,Chan1, N, 100).

raw_multiple_(Chan0, N, Delay) ->
    raw_multiple_(Chan0, Chan0, N, Delay).

raw_multiple_(Chan0, Chan1, N, Delay) ->
    TxBuf1 = <<1,((2+Chan0) bsl 6), 0>>,
    TxBuf2 = <<1,((2+Chan1) bsl 6), 0>>,
    %% note that cs = 1 between all commands!
    T1 = #spi_transfer { tx_buf        = TxBuf1,
			 len           = 3,
			 speed_hz      = 2500000,
			 bits_per_word = 8,
			 delay_usecs   = Delay,
			 cs = 1
		       },
    T2 = #spi_transfer { tx_buf        = TxBuf2,
			 len           = 3,
			 speed_hz      = 2500000,
			 bits_per_word = 8,
			 delay_usecs   = Delay,
			 cs = 1
		       },
    Txs = interleave(N,T1,T2),
    spi:transfer(0, 0, Txs).

interleave(0, _T1, _T2) ->
    [];
interleave(I, T1, T2) when I band 1 =:= 1 ->
    interleave_(I-1, T1, T2, [T1]);
interleave(I, T1, T2) ->
    interleave_(I, T1, T2, []).

interleave_(0, _T1, _T2, Acc) -> 
    Acc;
interleave_(I, T1, T2, Acc) when I >= 2 ->
    interleave_(I-2, T1, T2, [T1,T2|Acc]).

open_dac() ->
    spi:open(0,1).

close_dac() ->
    spi:close(0,1).
