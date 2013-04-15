%%% @author tony <tony@rogvall.se>
%%% @copyright (C) 2013, tony
%%% @doc
%%%
%%% @end
%%% Created :  5 Apr 2013 by tony <tony@rogvall.se>

-module(spi).

-export([open/2]).
-export([close/2]).
-export([transfer/3]).
-export([transfer/8]).

-include("../include/spi.hrl").

-define(SPI_PORT, spi_port).

-define(CMD_OPEN,      1).
-define(CMD_CLOSE,     2).
-define(CMD_TRANSFER,  3).

-define(ENCODE(BufLen,Speed,Delay,BitsPerWord,Cs,TxData),
	(byte_size(TxData)):32,BufLen:32,Speed:32,Delay:16,
	BitsPerWord:8,Cs:8,TxData/binary).

open(Bus, Chip) when ?is_uint16(Bus), ?is_uint8(Chip) ->
    call(?SPI_PORT, ?CMD_OPEN, <<Bus:16, Chip:8>>).

close(Bus, Chip) when ?is_uint16(Bus), ?is_uint8(Chip) ->
    call(?SPI_PORT, ?CMD_CLOSE, <<Bus:16, Chip:8>>).

transfer(Bus,Chip,L=[#spi_transfer{}|_])
  when ?is_uint16(Bus), ?is_uint8(Chip) ->
    {N,Data} = encode_spi(L, [], 0),
    call(?SPI_PORT, ?CMD_TRANSFER, <<Bus:16, Chip:8, N:32, Data>>).
    
transfer(Bus,Chip,TxData,BufLen,Delay,Speed,BitsPerWord,Cs)
  when ?is_uint16(Bus), ?is_uint8(Chip),
       is_binary(TxData), ?is_uint32(BufLen),
       ?is_uint16(Delay), ?is_uint32(Speed),
       ?is_uint8(BitsPerWord), ?is_uint8(Cs) ->
    call(?SPI_PORT, ?CMD_TRANSFER,
	 [<<Bus:16, Chip:8, 1:32,
	    ?ENCODE(BufLen,Speed,Delay,BitsPerWord,Cs,TxData) >>]).

encode_spi([#spi_transfer {
	       tx_buf=TxData, 
	       len = BufLen,
	       speed_hz = Speed,
	       delay_usecs = Delay,
	       bits_per_word = BitsPerWord,
	       cs = Cs } | Rest ], Acc, I) when
      is_binary(TxData), ?is_uint32(BufLen),
      ?is_uint16(Delay), ?is_uint32(Speed),
      ?is_uint8(BitsPerWord), ?is_uint8(Cs) ->
    encode_spi(Rest, 
	       [<<?ENCODE(BufLen,Speed,Delay,BitsPerWord,Cs,TxData)>> |
		Acc], I+1);
encode_spi([],Acc,0) ->  {0, Acc};
encode_spi([],Acc,1) ->  {1, Acc};
encode_spi([],Acc,N) ->
    {N, lists:reverse(Acc)}.

call(Port, Cmd, Data) ->
    erlang:port_control(Port, Cmd, Data).
