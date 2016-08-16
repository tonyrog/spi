%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    SPI arduino programmer 
%%%    Commands from: http://www.atmel.com/images/doc0943.pdf
%%% @end
%%% Created : 30 Jun 2016 by Tony Rogvall <tony@rogvall.se>

-module(spi_arduino).

-export([start/0, start/1, start/3]).
-export([init/3]).
-export([loop/1]).

-compile(export_all).

-include_lib("spi/include/spi.hrl").

-define(SPI_SPEED, 500000).

-define(RESET,     25). %% SS

-define(LED_HB,    27).
-define(LED_ERR,   24).
-define(LED_PMODE, 27).

-define(HWVER, 2).
-define(SWMAJ, 1).
-define(SWMIN, 18).

-define(PTIME, 30).

%% STK Definitions
-define(STK_OK,      16#10).
-define(STK_FAILED,  16#11).
-define(STK_UNKNOWN, 16#12).
-define(STK_INSYNC,  16#14).
-define(STK_NOSYNC,  16#15).
-define(CRC_EOP,     16#20). %% ok it is a space...

-define(STK_PROG_FLASH, 16#60).
-define(STK_PROG_DATA,  16#61).
-define(STK_PROG_PAGE,  16#64).
-define(STK_READ_PAGE,  16#74).
-define(STK_READ_SIGN,  16#75).

-define(EECHUNK, 32).

-define(AVR_PROGRAMING_ENABLE,   <<16#AC, 16#53, 16#00, 16#00>>).
-define(AVR_DEVICE_CODE_VENDOR,  <<16#30, 16#00, 16#00, 16#00>>).
-define(AVR_DEVICE_CODE_FAMILY,  <<16#30, 16#00, 16#01, 16#00>>).
-define(AVR_DEVICE_CODE_PART,    <<16#30, 16#00, 16#02, 16#00>>).

-define(DEVICE_AT90S1200, 16#90).
-define(DEVICE_AT90S2313, 16#91).
-define(DEVICE_AT90S4414, 16#92).
-define(DEVICE_AT90S8515, 16#93).
-define(DEVICE_ERASED,  16#FF).
-define(DEVICE_LOCKED,  16#02).

-define(AVR_READ_FLASH_LOW(A), <<16#20, (A):16, 16#00>>).
-define(AVR_READ_FLASH_HIGH(A), <<16#28, (A):16, 16#00>>).

-define(AVR_WRITE_FLASH_LOW(A,D), <<16#60, (A):16, (D)>>).
-define(AVR_WRITE_FLASH_HIGH(A,D), <<16#68, (A):16, (D)>>).

-define(AVR_READ_EEPROM(A), <<16#A0, (A):16, 16#FF>>).
-define(AVR_WRITE_EEPROM(A,D), <<16#C0, (A):16, (D)>>).

-define(AVR_SET_LOCK(D), <<16#AC, (D), 16#00, 16#00>>).
-define(AVR_ERASE_CHIP,  <<16#AC, 16#80, 16#00, 16#00>>).





-record(param,
	{
	  devicecode = 0,
	  revision = 0,
	  progtype = 0,
	  parmode = 0,
	  polling = 0,
	  selftimed = 0,
	  lockbytes = 0,
	  fusebytes = 0,
	  flashpoll = 0,
	  eeprompoll = 0,
	  pagesize = 0,
	  eepromsize = 0,
	  flashsize = 0
	}).

-record(ctx,
	{
	  uart = undefined,
	  param = #param {},
	  error = 0,
	  pmode = false,
	  prog_flicker = true,
	  timer = undefined,
	  last_beat = 0,
	  beat = false,
	  here = 0,
	  low = 0,
	  high = 0,
	  data = 0,
	  bus = 0,
	  chip = 0
	}).

start() ->
    start("/dev/ttyAMA0").

start(Device) ->
    start(Device, 0, 0).

start(Device, Bus, Chip) ->
    spawn_link(
      fun() ->
	      Ctx = init(Device, Bus, Chip),
	      loop(Ctx)
      end).

init(Device, Bus, Chip) ->
    application:start(gpio),
    application:start(spi),
    application:start(uart),
    {ok,U} = uart:open(Device, [{baud,19200},
				{active,false},{mode,binary},
				{packet,0}]),
    gpio:init(?RESET),
    gpio:init(?LED_HB),
    gpio:init(?LED_ERR),
    gpio:init(?LED_PMODE),
    
    gpio:output(?LED_PMODE),
    pulse(?LED_PMODE, 2),
    gpio:output(?LED_ERR),
    pulse(?LED_ERR, 2),

    gpio:output(?LED_HB),
    pulse(?LED_HB, 2),

    T = erlang:start_timer(16#ffffffff, undefined, undefined),
    #ctx { uart = U, timer = T, bus = Bus, chip = Chip }.

loop(Ctx) ->
    Tick = 16#ffffffff - erlang:read_timer(Ctx#ctx.timer), %% program time tick
    if Ctx#ctx.pmode -> gpio:set(?LED_PMODE);
       true ->  gpio:clr(?LED_PMODE)
    end,
    if Ctx#ctx.error =:= 0 -> gpio:set(?LED_ERR);
       true ->  gpio:clr(?LED_ERR)
    end,
    Ctx1 = 
	if Tick - Ctx#ctx.last_beat >= 1000 ->
		if Ctx#ctx.beat -> gpio:set(?LED_HB);
		   true -> gpio:clr(?LED_HB)
		end,
		Ctx#ctx { last_beat = Tick, beat = not Ctx#ctx.beat };
	   true ->
		Ctx
	end,
    case uart:recv(Ctx1#ctx.uart, 1, 1000) of
	{ok, <<Cmd>>} ->
	    Ctx2 = avrisp(Cmd, Ctx1),
	    loop(Ctx2);
	{error, timeout} ->
	    loop(Ctx1)
    end.
    

pulse(_Pin, 0) ->
    ok;
pulse(Pin, Times) ->
    gpio:set(Pin),
    timer:sleep(?PTIME),
    gpio:clr(Pin),
    timer:sleep(?PTIME),
    pulse(Pin, Times-1).


empty_reply(Ctx) ->
    case uart:recv(Ctx#ctx.uart, 1) of
	{ok,<<?CRC_EOP>>} ->
	    uart:send_char(Ctx#ctx.uart, ?STK_INSYNC),
	    uart:send_char(Ctx#ctx.uart, ?STK_OK),
	    Ctx;
	_ ->
	    uart:send_char(Ctx#ctx.uart, ?STK_NOSYNC),
	    Ctx#ctx { error = Ctx#ctx.error + 1 }
    end.

breply(Ctx, B) ->
    case uart:recv(Ctx#ctx.uart, 1) of
	{ok,<<?CRC_EOP>>} ->
	    uart:send_char(Ctx#ctx.uart, ?STK_INSYNC),
	    uart:send_char(Ctx#ctx.uart, B),
	    uart:send_char(Ctx#ctx.uart, ?STK_OK),
	    Ctx;
	_ ->
	    uart:send_char(Ctx#ctx.uart, ?STK_NOSYNC),
	    Ctx#ctx { error = Ctx#ctx.error + 1 }
    end.

spi_send(Ctx, TxBuf) ->
    T = #spi_transfer { tx_buf = TxBuf,
			len    = byte_size(TxBuf),
			speed_hz = ?SPI_SPEED,
			bits_per_word = 8,
			delay_usecs   = 0,
			cs = 0 },
    {ok,<<_,_,_,R>>} = spi:transfer(Ctx#ctx.bus,Ctx#ctx.chip,[T]),
    R.

start_pmode(Ctx) ->
    spi:open(Ctx#ctx.bus,Ctx#ctx.chip),
    %% following delays may not work on all targets...
    gpio:output(?RESET),
    gpio:set(?RESET),
    %% pinMode(SCK, OUTPUT);
    %% digitalWrite(SCK, LOW);
    timer:sleep(50),
    gpio:clr(?RESET),
    timer:sleep(50),
    %% pinMode(MISO, INPUT);
    %% pinMode(MOSI, OUTPUT);
    spi_send(Ctx,?AVR_PROGRAMING_ENABLE),
    Ctx#ctx { pmode = true }.

end_pmode(Ctx) ->
    gpio:input(?RESET),
    spi:close(Ctx#ctx.bus,Ctx#ctx.chip),
    Ctx.

universal(Ctx) ->
    case uart:recv(Ctx#ctx.uart, 4) of
	{ok, Buf} ->
	    R = spi_send(Ctx, Buf),
	    breply(Ctx, R);
	_ ->
	    breply(Ctx, 0)
    end.

read_signature(Ctx) ->
    case uart:recv(Ctx#ctx.uart, 1) of
	{ok,<<?CRC_EOP>>} ->
	    uart:send_char(Ctx#ctx.uart, ?STK_INSYNC),
	    H = spi_send(Ctx, ?AVR_DEVICE_CODE_VENDOR),
	    uart:send_char(Ctx#ctx.uart, H),
	    M = spi_send(Ctx, ?AVR_DEVICE_CODE_FAMILY),
	    uart:send_char(Ctx#ctx.uart, M),
	    L = spi_send(Ctx, ?AVR_DEVICE_CODE_PART),
	    uart:send_char(Ctx#ctx.uart, L),
	    uart:send_char(Ctx#ctx.uart, ?STK_OK),
	    Ctx;
	_ ->
	    uart:send_char(Ctx#ctx.uart, ?STK_NOSYNC),
	    Ctx#ctx { error = Ctx#ctx.error + 1 }
    end.

current_page(Ctx, Addr) ->
    P = Ctx#ctx.param,
    if P#param.pagesize =:= 32 -> Addr  band 16#fffffff0;
       P#param.pagesize =:= 64 -> Addr  band 16#ffffffe0;
       P#param.pagesize =:= 128 -> Addr band 16#ffffffc0;
       P#param.pagesize =:= 256 -> Addr band 16#ffffff80;
       true -> Addr
    end.

prog_lamp(Ctx, Level) ->
    if Ctx#ctx.prog_flicker ->
	    if Level =:= 0 ->
		    gpio:clr(?LED_PMODE);
	       true ->
		    gpio:set(?LED_PMODE)
	    end;
       true ->
	    ok
    end.
	    
commit(Ctx, Addr) ->
    prog_lamp(Ctx, 0),
    spi_send(Ctx, <<16#4C, Addr:16, 0>>),
    if Ctx#ctx.prog_flicker ->
	    timer:delay(?PTIME),
	    prog_lamp(Ctx, 1);
       true ->
	    ok
    end.

write_data(Ctx, Addr, Page, <<>>) ->
    Page = current_page(Ctx, Addr),
    commit(Ctx, Page),
    Ctx#ctx { here = Addr};
write_data(Ctx, Addr, Page0, <<LB,HB,Buf>>) ->
    Page = 
	case current_page(Ctx, Addr) of
	    Page0 -> Page0;
	    Page1 ->
		commit(Ctx, Page0),
		Page1
	end,
    spi_send(Ctx, ?AVR_WRITE_FLASH_LOW(Addr,LB)),
    spi_send(Ctx, ?AVR_WRITE_FLASH_HIGH(Addr,HB)), 
    write_data(Ctx, Addr+1, Page, Buf).

write_flash_pages(Ctx, Buf) ->
    Page = current_page(Ctx, Ctx#ctx.here),
    Ctx1 = write_data(Ctx, Ctx#ctx.here, Page, Buf),
    {?STK_OK, Ctx1}.

write_flash(Ctx, Length) ->
    case uart:recv(Ctx#ctx.uart, Length) of
	{ok, Buf} ->
	    case uart:recv(Ctx#ctx.uart, 1) of
		{ok, <<?CRC_EOP>>} ->
		    uart:send_char(Ctx#ctx.uart, ?STK_INSYNC),
		    {R,Ctx1} = write_flash_pages(Ctx, Buf),
		    uart:send_char(Ctx#ctx.uart, R),
		    Ctx1;
		_ ->
		    uart:send_char(Ctx#ctx.uart, ?STK_NOSYNC),
		    Ctx#ctx { error = Ctx#ctx.error + 1 }
	    end;
	_ ->
	    uart:send_char(Ctx#ctx.uart, ?STK_NOSYNC),
	    Ctx#ctx { error = Ctx#ctx.error + 1 }
    end.

write_eeprom_buf(Ctx, Addr, <<B,Buf>>) ->
    spi_send(Ctx, ?AVR_WRITE_EEPROM(Addr,B)),
    timer:sleep(45),
    write_eeprom_buf(Ctx, Addr+1, Buf);
write_eeprom_buf(Ctx, _Addr, <<>>) ->
    Ctx.

%% write (length) bytes, (start) is a byte address
write_eeprom_chunk(Ctx, Start, Length) ->
    %% this writes byte-by-byte,
    %% page writing may be faster (4 bytes at a time)
    {ok, Buf} = uart:recv(Ctx#ctx.uart, Length),
    prog_lamp(Ctx, 0),
    Ctx1 = write_eeprom_buf(Ctx, Start, Buf),
    prog_lamp(Ctx, 1),
    {?STK_OK, Ctx1}.


write_eeprom_chunks(Ctx, Addr, Remain) when Remain > ?EECHUNK ->
    write_eeprom_chunk(Ctx, Addr, ?EECHUNK),
    write_eeprom_chunks(Ctx, Addr+?EECHUNK, Remain-?EECHUNK);
write_eeprom_chunks(Ctx, Addr, Remain) ->
    write_eeprom_chunk(Ctx, Addr, Remain).

write_eeprom(Ctx, Length) ->
    %%  here is a word address, get the byte address
    Start = Ctx#ctx.here*2,
    Remaining = Length,
    P = Ctx#ctx.param,
    if Length > P#param.eepromsize ->
	    { ? STK_FAILED, Ctx#ctx { error = Ctx#ctx.error + 1 }};
       true ->
	    Ctx1 = write_eeprom_chunks(Ctx, Start, Remaining),
	    {?STK_OK, Ctx1 }
    end.

program_page(Ctx) ->
    {ok, <<Length:16,MemType>>} = uart:recv(Ctx#ctx.uart, 3),
    %% flash memory @here, (length) bytes
    case MemType of 
	$F -> 
	    write_flash(Ctx, Length);
	$E -> 
	    R = write_eeprom(Ctx, Length),
	    case uart:recv(Ctx#ctx.uart, 1) of
		{ok, <<?CRC_EOP>>} ->
		    uart:send_char(Ctx#ctx.uart, ?STK_INSYNC),
		    uart:send_char(Ctx#ctx.uart, R),
		    Ctx;
		_ ->
		    uart:send_char(Ctx#ctx.uart, ?STK_NOSYNC),
		    Ctx#ctx { error = Ctx#ctx.error + 1 }
	    end;
	_ ->
	    uart:send_char(Ctx#ctx.uart, ?STK_FAILED),
	    Ctx
    end.

flash_read_buf(_Ctx, _Addr, I) when I =< 0 ->
    ok;
flash_read_buf(Ctx, Addr, I) ->
    L = spi_send(Ctx, ?AVR_READ_FLASH_LOW(Addr)), 
    uart:send_char(Ctx#ctx.uart, L),
    H = spi_send(Ctx, ?AVR_READ_FLASH_HIGH(Addr)),
    uart:send_char(Ctx#ctx.uart, H),
    flash_read_buf(Ctx, Addr+1, I-2).

flash_read_page(Ctx, Length) ->
    ok = flash_read_buf(Ctx, Ctx#ctx.here, Length),
    {?STK_OK, Ctx}.

eeprom_read_buf(_Ctx, _Addr, 0) ->
    ok;
eeprom_read_buf(Ctx, Addr, Len) ->
    EE = spi_send(Ctx, ?AVR_READ_EEPROM(Addr)),
    uart:send_char(Ctx#ctx.uart, EE),
    eeprom_read_buf(Ctx, Addr+1, Len-1).

eeprom_read_page(Ctx, Length) ->
    Start = Ctx#ctx.here * 2,
    ok = eeprom_read_buf(Ctx, Start, Length),
    {?STK_OK, Ctx}.


read_page(Ctx) ->
    case uart:recv(Ctx#ctx.uart, 3) of
	{ok, <<Length:16, $F, ?CRC_EOP>>} ->
	    uart:send_char(Ctx#ctx.uart, ?STK_INSYNC),
	    {R,Ctx1} = flash_read_page(Ctx, Length),
	    uart:send_char(Ctx#ctx.uart, R),
	    Ctx1;
	{ok, <<Length:16, $E, ?CRC_EOP>>} ->
	    uart:send_char(Ctx#ctx.uart, ?STK_INSYNC),
	    {R,Ctx1} = eeprom_read_page(Ctx, Length),
	    uart:send_char(Ctx#ctx.uart, R),
	    Ctx1;
	_ ->
	    uart:send_char(Ctx#ctx.uart, ?STK_NOSYNC),
	    Ctx#ctx { error = Ctx#ctx.error + 1 }
    end.

avrisp(Cmd, Ctx) ->
    case Cmd of
	$0 ->  %% signon
	    empty_reply(Ctx#ctx { error = 0 });
	$1 ->
	    case uart:recv(Ctx#ctx.uart, 1) of
		{ok,<<?CRC_EOP>>} ->
		    uart:send_char(Ctx#ctx.uart, ?STK_INSYNC),
		    uart:send(Ctx#ctx.uart, "AVR ISP"),
		    uart:send_char(Ctx#ctx.uart, ?STK_OK),
		    Ctx;
		_ ->
		    Ctx
	    end;
	$A ->
	    case uart:recv(Ctx#ctx.uart, 1) of    
		{ok, <<16#80>>} ->
		    breply(Ctx, ?HWVER);
		{ok, <<16#81>>} ->
		    breply(Ctx, ?SWMAJ);
		{ok, <<16#82>>} ->
		    breply(Ctx, ?SWMIN);
		{ok, <<16#93>>} ->
		    breply(Ctx, $S); %% serial programmer
		_ ->
		    breply(Ctx, 0)
	    end;
	$B ->
	    case uart:recv(Ctx#ctx.uart, 20) of
		{ok, <<DeviceCode,Revision,ProgType,ParMode,Polling,
		       SelfTimed,LockBytes,FuseBytes,FlashPoll,
		       EEPromPoll:16,
		       PageSize:16,
		       EEPromSize:16,
		       FlashSize:32>>} ->
		    P = #param { devicecode = DeviceCode,
				 revision = Revision,
				 progtype = ProgType,
				 parmode = ParMode,
				 polling = Polling,
				 selftimed = SelfTimed,
				 lockbytes = LockBytes,
				 fusebytes = FuseBytes,
				 flashpoll = FlashPoll,
				 eeprompoll = EEPromPoll,
				 pagesize = PageSize,
				 eepromsize = EEPromSize,
				 flashsize = FlashSize },
		    Ctx1 = Ctx#ctx { param = P },
		    empty_reply(Ctx1);
		_ ->
		    empty_reply(Ctx)
	    end;
	$E -> %% extended parameters - ignore for now
	    case uart:recv(Ctx#ctx.uart, 5) of
		{ok, _Buf} ->
		    empty_reply(Ctx);
		_ ->
		    empty_reply(Ctx)
	    end;
	$P ->
	    Ctx1 = start_pmode(Ctx),
	    empty_reply(Ctx1);
	$U ->
	    {ok,<<Here:16/little>>} = uart:recv(Ctx#ctx.uart, 2),
	    empty_reply(Ctx#ctx { here = Here });
	?STK_PROG_FLASH ->
	    {ok,<<Low,High>>} = uart:recv(Ctx#ctx.uart, 2),
	    empty_reply(Ctx#ctx { low=Low, high=High });
	?STK_PROG_DATA ->
	    {ok,<<Data>>} = uart:recv(Ctx#ctx.uart, 1),
	    empty_reply(Ctx#ctx { data=Data });
	?STK_PROG_PAGE ->
	    program_page(Ctx);
	?STK_READ_PAGE ->
	    read_page(Ctx);
	$V ->
	    universal(Ctx);
	$Q ->
	    Ctx1 = end_pmode(Ctx#ctx { error=0 }),
	    empty_reply(Ctx1);
	?STK_READ_SIGN->
	    read_signature(Ctx);
	?CRC_EOP ->
	    uart:send_char(Ctx#ctx.uart, ?STK_NOSYNC),
	    Ctx#ctx { error = Ctx#ctx.error + 1 };
	_ ->
	    case uart:recv(Ctx#ctx.uart, 1) of
		{ok, <<?CRC_EOP>>} ->
		    uart:send_char(Ctx#ctx.uart, ?STK_UNKNOWN);
		_ ->
		    uart:send_char(Ctx#ctx.uart, ?STK_NOSYNC)
	    end,
	    Ctx#ctx { error = Ctx#ctx.error + 1 }	    
    end.

	
