%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Attempt to drive ssd1289 from erlang over spi bus
%%% @end
%%% Created :  5 Mar 2014 by Tony Rogvall <tony@rogvall.se>

-module(ssd1289).

-export([start/0]).
-export([stop/1]).
-export([setup/1]).
-export([set_xy/3]).
-export([write_data/2]).

-define(SSD1289_REG_OSCILLATION,     16#00).
-define(SSD1289_REG_DRIVER_OUT_CTRL, 16#01).
-define(SSD1289_REG_LCD_DRIVE_AC,    16#02).
-define(SSD1289_REG_POWER_CTRL_1,    16#03).
-define(SSD1289_REG_DISPLAY_CTRL,    16#07).
-define(SSD1289_REG_FRAME_CYCLE,     16#0b).
-define(SSD1289_REG_POWER_CTRL_2,    16#0c).
-define(SSD1289_REG_POWER_CTRL_3,    16#0d).
-define(SSD1289_REG_POWER_CTRL_4,    16#0e).
-define(SSD1289_REG_GATE_SCAN_START, 16#0f).
-define(SSD1289_REG_SLEEP_MODE,      16#10).
-define(SSD1289_REG_ENTRY_MODE,      16#11).
-define(SSD1289_REG_POWER_CTRL_5,    16#1e).
-define(SSD1289_REG_GDDRAM_DATA,     16#22).
-define(SSD1289_REG_WR_DATA_MASK_1,  16#23).
-define(SSD1289_REG_WR_DATA_MASK_2,  16#24).
-define(SSD1289_REG_FRAME_FREQUENCY, 16#25).
-define(SSD1289_REG_GAMMA_CTRL_1,    16#30).
-define(SSD1289_REG_GAMME_CTRL_2,    16#31).
-define(SSD1289_REG_GAMMA_CTRL_3,    16#32).
-define(SSD1289_REG_GAMMA_CTRL_4,    16#33).
-define(SSD1289_REG_GAMMA_CTRL_5,    16#34).
-define(SSD1289_REG_GAMMA_CTRL_6,    16#35).
-define(SSD1289_REG_GAMMA_CTRL_7,    16#36).
-define(SSD1289_REG_GAMMA_CTRL_8,    16#37).
-define(SSD1289_REG_GAMMA_CTRL_9,    16#3a).
-define(SSD1289_REG_GAMMA_CTRL_10,   16#3b).
-define(SSD1289_REG_V_SCROLL_CTRL_1, 16#41).
-define(SSD1289_REG_V_SCROLL_CTRL_2, 16#42).
-define(SSD1289_REG_H_RAM_ADR_POS,   16#44).
-define(SSD1289_REG_V_RAM_ADR_START, 16#45).
-define(SSD1289_REG_V_RAM_ADR_END,   16#46).
-define(SSD1289_REG_FIRST_WIN_START, 16#48).
-define(SSD1289_REG_FIRST_WIN_END,   16#49).
-define(SSD1289_REG_SECND_WIN_START, 16#4a).
-define(SSD1289_REG_SECND_WIN_END,   16#4b).
-define(SSD1289_REG_GDDRAM_X_ADDR,   16#4e).
-define(SSD1289_REG_GDDRAM_Y_ADDR,   16#4f).

-define(PI32_LONG,  2#10000000).
-define(PI32_RESET, 2#00100000).
-define(PI32_RS,    2#00010000).
-define(PI32_BL,    2#00001000).
-define(PI32_RD,    2#00000100).

-define(MAX_SPEED_HZ, 125000000).

-record(ssd1289,
	{
	  bus = 0,
	  chip = 0,
	  max_speed_hz = ?MAX_SPEED_HZ,
	  rotate = 2,
	  backlight = true,
	  xres   = 320,
	  yres   = 240,
	  bits_per_pixel = 16  %% 565
	}).

start() ->
    application:start(spi),
    spi:open(0,0),
    #ssd1289 {}.

stop(S) ->
    spi:close(S#ssd1289.bus,S#ssd1289.chip).
    
setup(S) ->
    setup_(S,0).
    
setup_(S,_Code=0) ->
    reg_set(S,?SSD1289_REG_OSCILLATION, 16#0001),
    %% DCT=b1010=fosc/4 BT=b001=VGH:+6,VGL:-4
    %% DC=b1010=fosc/4 AP=b010=small to medium
    reg_set(S,?SSD1289_REG_POWER_CTRL_1, 16#a2a4),
    %% VRC=b100:5.5V
    reg_set(S,?SSD1289_REG_POWER_CTRL_2, 16#0004),
    %% VRH=b1000:Vref*2.165
    reg_set(S,?SSD1289_REG_POWER_CTRL_3, 16#0308),
    %% VCOMG=1 VDV=b1000:VLCD63*1.05
    reg_set(S,?SSD1289_REG_POWER_CTRL_4, 16#3000),
    %% nOTP=1 VCM=16#2a:VLCD63*0.77
    reg_set(S,?SSD1289_REG_POWER_CTRL_5, 16#00ea),
    %% RL=0 REV=1 CAD=0 BGR=1 SM=0 TB=1 MUX=16#13f=319
    reg_set(S,?SSD1289_REG_DRIVER_OUT_CTRL, 16#2b3f),
    %% FLD=0 ENWS=0 D/C=1 EOR=1 WSMD=0 NW=16#00=0
    reg_set(S,?SSD1289_REG_LCD_DRIVE_AC, 16#0600),
    %% SLP=0
    reg_set(S,?SSD1289_REG_SLEEP_MODE, 16#0000),
    %% VSMode=0 DFM=3:65k TRAMS=0 OEDef=0 WMode=0 Dmode=0
    %% TY=0 ID=2 AM=1 LG=0 (orig: ID=3, AM=0)
    %%reg_set(S,?SSD1289_REG_ENTRY_MODE, 16#6030),
    
    case S#ssd1289.rotate of 
	0 ->
	    reg_set(S,?SSD1289_REG_ENTRY_MODE, 16#6028);
	2 ->
	    reg_set(S,?SSD1289_REG_ENTRY_MODE, 16#6018)
    end,

    %% PT=0 VLE=1 SPT=0 GON=1 DTE=1 CM=0 D=3
    reg_set(S,?SSD1289_REG_DISPLAY_CTRL, 16#0233),
    %% NO=0 SDT=0 EQ=0 DIV=0 SDIV=1 SRTN=1 RTN=9:25 clock
    reg_set(S,?SSD1289_REG_FRAME_CYCLE, 16#0039),
    %% SCN=0
    reg_set(S,?SSD1289_REG_GATE_SCAN_START, 16#0000),
    
    %% PKP1=7 PKP0=7
    reg_set(S,?SSD1289_REG_GAMMA_CTRL_1, 16#0707),
    %% PKP3=2 PKP2=4
    reg_set(S,?SSD1289_REG_GAMME_CTRL_2, 16#0204),
    %% PKP5=2 PKP4=2
    reg_set(S,?SSD1289_REG_GAMMA_CTRL_3, 16#0204),
    %% PRP1=5 PRP0=2
    reg_set(S,?SSD1289_REG_GAMMA_CTRL_4, 16#0502),
    %% PKN1=5 PKN0=7
    reg_set(S,?SSD1289_REG_GAMMA_CTRL_5, 16#0507),
    %% PKN3=2 PNN2=4
    reg_set(S,?SSD1289_REG_GAMMA_CTRL_6, 16#0204),
    %% PKN5=2 PKN4=4
    reg_set(S,?SSD1289_REG_GAMMA_CTRL_7, 16#0204),
    %% PRN1=5 PRN0=2
    reg_set(S,?SSD1289_REG_GAMMA_CTRL_8, 16#0502),
    %% VRP1=3 VRP0=2
    reg_set(S,?SSD1289_REG_GAMMA_CTRL_9, 16#0302),
    %% VRN1=3 VRN0=2
    reg_set(S,?SSD1289_REG_GAMMA_CTRL_10, 16#0302),
    
    %% WMR=0 WMG=0
    reg_set(S,?SSD1289_REG_WR_DATA_MASK_1, 16#0000),
    %% WMB=0
    reg_set(S,?SSD1289_REG_WR_DATA_MASK_2, 16#0000),
    %% OSC=b1010:548k
    reg_set(S,?SSD1289_REG_FRAME_FREQUENCY, 16#a000),
    %% SS1=0
    reg_set(S,?SSD1289_REG_FIRST_WIN_START, 16#0000),
    %% SE1=319
    reg_set(S,?SSD1289_REG_FIRST_WIN_END, S#ssd1289.xres - 1),
    %% SS2=0
    reg_set(S,?SSD1289_REG_SECND_WIN_START, 16#0000),
    %% SE2=0
    reg_set(S,?SSD1289_REG_SECND_WIN_END, 16#0000),
    %% VL1=0
    reg_set(S,?SSD1289_REG_V_SCROLL_CTRL_1, 16#0000),
    %% VL2=0
    reg_set(S,?SSD1289_REG_V_SCROLL_CTRL_2, 16#0000),
    %% HEA=16#ef=239 HSA=0
    reg_set(S,?SSD1289_REG_H_RAM_ADR_POS, (S#ssd1289.yres-1) bsl 8),
    %% VSA=0
    reg_set(S,?SSD1289_REG_V_RAM_ADR_START, 16#0000),
    %% VEA=16#13f=319
    reg_set(S,?SSD1289_REG_V_RAM_ADR_END, S#ssd1289.xres - 1);
setup_(S,_Code) ->    
    reg_set(S,16#0028,16#0006),
    reg_set(S,16#0000,16#0001),
    reg_set(S,16#0003,16#aea4),    %% power control 1---line frequency and VHG,VGL voltage
    reg_set(S,16#000c,16#0004),    %% power control 2---VCIX2 output voltage
    reg_set(S,16#000d,16#000c),    %% power control 3---Vlcd63 voltage
    reg_set(S,16#000e,16#2800),    %% power control 4---VCOMA voltage VCOML=VCOMH*0.9475-VCOMA
    reg_set(S,16#001e,16#00b5),    %% POWER CONTROL 5---VCOMH voltage
    reg_set(S,16#0001,16#3b3f),     
    reg_set(S,16#0002,16#0600),
    reg_set(S,16#0010,16#0000),
    reg_set(S,16#0011,16#6098),
    reg_set(S,16#0005,16#0000),
    reg_set(S,16#0006,16#0000),
    reg_set(S,16#0016,16#ef1c),  
    reg_set(S,16#0007,16#0033),    %% Display control 1
    %% when GON=1 and DTE=0,all gate outputs become VGL
    %% when GON=1 and DTE=0,all gate outputs become VGH
    %% non-selected gate wires become VGL
    reg_set(S,16#000b,16#0000),
    reg_set(S,16#000f,16#0000),
    reg_set(S,16#0041,16#0000),
    reg_set(S,16#0042,16#0000),
    reg_set(S,16#0048,16#0000),
    reg_set(S,16#0049,16#013f),
    reg_set(S,16#004a,16#0000),
    reg_set(S,16#004b,16#0000), 
    reg_set(S,16#0044,16#ef00),	%% Horizontal RAM start and end address
    reg_set(S,16#0045,16#0000),	%% Vretical RAM start address
    reg_set(S,16#0046,16#013f),	%% Vretical RAM end address
    reg_set(S,16#004e,16#0000),	%% set GDDRAM x address counter
    reg_set(S,16#004f,16#0000),  %% set GDDRAM y address counter
    %% y control
    reg_set(S,16#0030,16#0707),
    reg_set(S,16#0031,16#0202),
    reg_set(S,16#0032,16#0204),
    reg_set(S,16#0033,16#0502),
    reg_set(S,16#0034,16#0507),
    reg_set(S,16#0035,16#0204),
    reg_set(S,16#0036,16#0204),
    reg_set(S,16#0037,16#0502),
    reg_set(S,16#003a,16#0302),
    reg_set(S,16#003b,16#0302), 
    reg_set(S,16#0023,16#0000),
    reg_set(S,16#0024,16#0000),
    reg_set(S,16#0025,16#8000),
    reg_set(S,16#0026,16#7000),
    reg_set(S,16#0020,16#b0eb),
    reg_set(S,16#0027,16#007c),
    write_reg16(S,16#0022).

set_xy(S,X,Y) ->
    case S#ssd1289.rotate of
	0 ->
	    reg_set(S, ?SSD1289_REG_GDDRAM_X_ADDR, (S#ssd1289.yres - 1) - Y),
	    reg_set(S, ?SSD1289_REG_GDDRAM_Y_ADDR, X);
	2 ->
	    reg_set(S, ?SSD1289_REG_GDDRAM_X_ADDR, Y),
	    reg_set(S, ?SSD1289_REG_GDDRAM_Y_ADDR, (S#ssd1289.xres-1) - X)
    end,
    write_reg16(S,?SSD1289_REG_GDDRAM_DATA).

reg_set(S, Reg, Value) ->
    write_reg16(S, Reg band 16#ff),
    write_data16(S, Value).

write_reg16(S, Value) ->
    Backlight = if S#ssd1289.backlight -> ?PI32_BL; true -> 0 end,
    Cmd = Backlight bor ?PI32_RESET bor ?PI32_RD,
    TxData = <<Cmd,Value:16>>,
    spi:transfer(S#ssd1289.bus,S#ssd1289.chip,TxData,3,0,
		 S#ssd1289.max_speed_hz,8,0).
    
write_data16(S, Value) ->
    Backlight = if S#ssd1289.backlight -> ?PI32_BL; true -> 0 end,
    Cmd = ?PI32_RS bor ?PI32_RESET bor ?PI32_RD bor Backlight,
    TxData = <<Cmd,Value:16>>,
    spi:transfer(S#ssd1289.bus,S#ssd1289.chip,TxData,3,0,
		 S#ssd1289.max_speed_hz,8,0).
      
write_data(S, Data) when is_binary(Data) ->
    Backlight = if S#ssd1289.backlight -> ?PI32_BL; true -> 0 end,
    Cmd = ?PI32_RS bor ?PI32_RESET bor ?PI32_RD bor ?PI32_LONG  bor Backlight,
    TxData = <<Cmd, Data/binary>>,
    spi:transfer(S#ssd1289.bus,S#ssd1289.chip,TxData,byte_size(TxData),0,
		 S#ssd1289.max_speed_hz,8,0).
