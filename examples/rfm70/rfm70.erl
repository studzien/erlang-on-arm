-module(rfm70).

%% API
-export([start/1]).

-define(SPI_FREQ, 8000000).

-include("rfm70.hrl").

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
start(Callbacks) ->
    init_pins(),
    init_bank0(),
    init_bank1(),
    receive after 100 -> ok end,
    bank(0),
    switch_rx(),
    self() ! ping,
    loop(Callbacks).

loop(Callbacks) ->
    receive
        ping ->
            handle_ping(),
            loop(Callbacks);
        interrupt ->
            maybe_receive_frame(Callbacks),
            loop(Callbacks);
        _ ->
            loop(Callbacks)
    end.

handle_ping() ->
    Data = "ping",
    send_frame(Data),
    erlang:send_after(1000, self(), ping).

maybe_receive_frame(Callbacks) ->
    %% receive frame iff it's a rx interrupt
    [Status] = register_read(?RFM70_REG_STATUS),
    case Status band 64 of
        64 ->
            receive_frame(Callbacks, Status);
        _ ->
            ok
    end.

receive_frame(Callbacks, Status) ->
    [Length] = register_read(?RFM70_CMD_R_RX_PL_WID),
    Payload = rw([?RFM70_CMD_R_RX_PAYLOAD], Length, 1),
    rw([?RFM70_CMD_FLUSH_RX, 0]),
    register_write(?RFM70_REG_STATUS, [Status bor 64]),
    [Callback(Payload) || Callback <- Callbacks].

send_frame(Data) ->
    switch_tx(),
    Cmd = [?RFM70_CMD_W_TX_PAYLOAD_NOACK] ++ Data,
    rw(Cmd),
    Reply = receive
        interrupt -> ok
    after 100 ->
        {error, timeout}
    end,
    %% clear tx interrupt
    [Status] = register_read(?RFM70_REG_STATUS),
    register_write(?RFM70_REG_STATUS, [(Status bor 32)]),
    switch_rx(),
    Reply.

switch_rx() ->
    rw([?RFM70_CMD_FLUSH_RX, 0]),
    lpc_gpio:low(?PORT_CE, ?PIN_CE),
    [Config] = register_read(?RFM70_REG_CONFIG),
    register_write(?RFM70_REG_CONFIG, [(Config bor 16#01)]),
    lpc_gpio:high(?PORT_CE, ?PIN_CE).

switch_tx() ->
    rw([?RFM70_CMD_FLUSH_TX, 0]),
    lpc_gpio:low(?PORT_CE, ?PIN_CE),
    [Config] = register_read(?RFM70_REG_CONFIG),
    register_write(?RFM70_REG_CONFIG, [(Config band 16#FE)]),
    lpc_gpio:high(?PORT_CE, ?PIN_CE).

init_pins() ->
    lpc_gpio:output(?PORT_CE, ?PIN_CE),
    lpc_gpio:output(?PORT_CSN, ?PIN_CSN),
    lpc_gpio:input(?PORT_IRQ, ?PIN_IRQ),
    lpc_gpio:interrupt(?PORT_IRQ, ?PIN_IRQ, falling),
    lpc_spi:init(?SPI_FREQ).

init_bank0() ->
    bank(0),
    %% initialize bank 0 registers
    [register_write(Register, Value) || {Register,Value} <- bank0()],
    %% initialize addresses for pipes
    [register_write(Register, Address) || {Register,Address} <- addresses()],
    %% enable extra features
    case register_read(?RFM70_REG_FEATURE) of
        [0] ->
            rw([?RFM70_CMD_ACTIVATE, 16#73]);
        _ ->
            ok
    end,
    %% enable dynamic payload
    Dynamic = dynamic_payload(),
    [register_write(Register, Value) || {Register,Value} <- Dynamic].

init_bank1() ->
    bank(1),
    [register_write(Register, Value) || {Register,Value} <- bank1()].

bank(Bank) ->
    [Status|_] = register_read(?RFM70_REG_STATUS),
    case (Status bsr 7) of 
        Bank ->
            ok;
        _    ->
            rw([?RFM70_CMD_ACTIVATE, 16#53]),
            ok
    end.

register_write(Register, Value) ->
    Cmd = ?RFM70_CMD_WRITE_REG bor Register,
    rw([Cmd] ++ Value).

register_read(Register) ->
    Cmd = ?RFM70_CMD_READ_REG bor Register,
    rw([Cmd], 1, 1).

rw(Data) ->
    lpc_gpio:low(?PORT_CSN, ?PIN_CSN),
    Value = [lpc_spi:rw(Byte) || Byte <- Data],
    lpc_gpio:high(?PORT_CSN, ?PIN_CSN),
    Value.

rw(Data, TPadding, RPadding) ->
    Data1 = Data ++ lists:duplicate(TPadding, 0),
    lpc_gpio:low(?PORT_CSN, ?PIN_CSN),
    Unpadded = [lpc_spi:rw(Byte) || Byte <- Data1],
    lpc_gpio:high(?PORT_CSN, ?PIN_CSN),
    lists:nthtail(RPadding, Unpadded).

%%%===================================================================
%%% Initialization data
%%%===================================================================
%% {RegNum, Value}
bank0() ->
    [{?RFM70_REG_CONFIG,     ?B0_CONFIG},
     {?RFM70_REG_EN_AA,      ?B0_EN_AA},
     {?RFM70_REG_EN_RXADDR,  ?B0_EN_RXADDR},
     {?RFM70_REG_SETUP_AW,   ?B0_SETUP_AW},
     {?RFM70_REG_SETUP_RETR, ?B0_SETUP_RETR},
     {?RFM70_REG_RF_CH,      ?B0_RF_CH},
     {?RFM70_REG_RF_SETUP,   ?B0_RF_SETUP},
     {?RFM70_REG_STATUS,     ?B0_STATUS},
     {?RFM70_REG_OBSERVE_TX, ?B0_OBSERVE_TX},
     {?RFM70_REG_RX_ADDR_P2, ?B0_RX_ADDR_P2},
     {?RFM70_REG_RX_ADDR_P3, ?B0_RX_ADDR_P3},
     {?RFM70_REG_RX_ADDR_P4, ?B0_RX_ADDR_P4},
     {?RFM70_REG_RX_ADDR_P5, ?B0_RX_ADDR_P5},
     {?RFM70_REG_RX_PW_P0,   ?B0_RX_PW_P0},
     {?RFM70_REG_RX_PW_P1,   ?B0_RX_PW_P1},
     {?RFM70_REG_RX_PW_P2,   ?B0_RX_PW_P2},
     {?RFM70_REG_RX_PW_P3,   ?B0_RX_PW_P3},
     {?RFM70_REG_RX_PW_P4,   ?B0_RX_PW_P4},
     {?RFM70_REG_RX_PW_P5,   ?B0_RX_PW_P5},
     {?RFM70_REG_FIFO_STATUS,?B0_FIFO_STATUS}].

dynamic_payload() ->
    [{?RFM70_REG_DYNPD,   ?B0_DYNPD},
     {?RFM70_REG_FEATURE, ?B0_FEATURE}].

addresses() ->
    [{?RFM70_REG_RX_ADDR_P0, ?B0_RX_ADDR_P0},
     {?RFM70_REG_RX_ADDR_P1, ?B0_RX_ADDR_P1},
     {?RFM70_REG_TX_ADDR,    ?B0_TX_ADDR}].

%% "it's a kind of magic"
bank1() ->
    [{16#00, [16#40,16#4B,16#01,16#E2]},
     {16#01, [16#C0,16#4B,16#00,16#00]},
     {16#02, [16#D0,16#FC,16#8C,16#02]},
     {16#03, [16#99,16#00,16#39,16#41]},
     {16#04, [16#D9,16#96,16#82,16#1B]},
     {16#05, [16#24,16#06,16#7F,16#A6]},
     {16#0C, [16#00,16#12,16#73,16#00]},
     {16#0D, [16#46,16#B4,16#80,16#00]},
     {16#0E, [16#41,16#20,16#08,16#04,16#81,16#20,16#CF,16#F7,16#FE,16#FF,16#FF]}].
