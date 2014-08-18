-define(PORT_CE, 0).
-define(PIN_CE, 24).
-define(PORT_CSN, 0).
-define(PIN_CSN, 23).
-define(PORT_IRQ, 0).
-define(PIN_IRQ, 1).

%% register CONFIG 0x00
-define(B0_MASK_RX_DR, 0).
-define(B0_MASK_TX_DS, 0).
-define(B0_MASK_MAX_RT, 1).
-define(B0_EN_CRC, 1).
-define(B0_CRCO, 1).
-define(B0_PWR_UP, 1).
-define(B0_PRIM_RX, 1).
%% register EN_AA 0x01
-define(B0_ENAA_P5, 1).
-define(B0_ENAA_P4, 1).
-define(B0_ENAA_P3, 1).
-define(B0_ENAA_P2, 1).
-define(B0_ENAA_P1, 1).
-define(B0_ENAA_P0, 1).
%% register EN_RXADDR 0x02
-define(B0_ERX_P5, 1).
-define(B0_ERX_P4, 1).
-define(B0_ERX_P3, 1).
-define(B0_ERX_P2, 1).
-define(B0_ERX_P1, 1).
-define(B0_ERX_P0, 1).
%% register SETUP_AW 0x03
-define(B0_AW, 3).
%% register SETUP_RETR 0x04
-define(B0_ARD, 0).
-define(B0_ARC, 0).
%% register RF_CH 0x05
-define(B0_RF_CH, [5]).
%% register RF_SETUP 0x06
-define(B0_RF_DR, 0).
-define(B0_RF_PWR, 3).
-define(B0_LNA_HCURR, 1).
%% register STATUS 0x07
-define(B0_RX_DR, 0).
-define(B0_TX_DS, 0).
-define(B0_MAX_RT, 0).
%% register RX_ADDR_P0 0x0A
-define(B0_RX_ADDR_P0, [16#34,16#43,16#10,16#10,16#01]).
%% register RX_ADDR_P1 0x0B
-define(B0_RX_ADDR_P1, [16#39,16#38,16#37,16#36,16#C2]).
%% register RX_ADDR_P2 0x0C
-define(B0_RX_ADDR_P2, [16#C3]).
%% register RX_ADDR_P3 0x0D
-define(B0_RX_ADDR_P3, [16#C4]).
%% register RX_ADDR_P4 0x0E
-define(B0_RX_ADDR_P4, [16#C5]).
%% register RX_ADDR_P5 0x0F
-define(B0_RX_ADDR_P5, [16#C6]).
%% register TX_ADDR 0x10
-define(B0_TX_ADDR, [16#34,16#43,16#10,16#10,16#01]).
%% register RX_PW_P0 0x11
-define(B0_RX_PW_P0, [32]).
%% register RX_PW_P1 0x12
-define(B0_RX_PW_P1, [32]).
%% register RX_PW_P2 0x13
-define(B0_RX_PW_P2, [32]).
%% register RX_PW_P3 0x14
-define(B0_RX_PW_P3, [32]).
%% register RX_PW_P4 0x15
-define(B0_RX_PW_P4, [32]).
%% register RX_PW_P5 0x16
-define(B0_RX_PW_P5, [32]).
%% register FIFO_STATUS 0x17
-define(B0_FIFO_STATUS, [0]).
%% register DYNPD 0x1C
-define(B0_DPL_P5, 1).
-define(B0_DPL_P4, 1).
-define(B0_DPL_P3, 1).
-define(B0_DPL_P2, 1).
-define(B0_DPL_P1, 1).
-define(B0_DPL_P0, 1).
%% register FEATURE
-define(B0_EN_DPL, 1).
-define(B0_EN_ACK_PAY, 1).
-define(B0_EN_DYN_ACK, 1).

%% BANK 0 registers
-define(B0_CONFIG, [0 bor (?B0_MASK_RX_DR bsl 6) bor (?B0_MASK_TX_DS bsl 5) bor (?B0_MASK_MAX_RT bsl 4) bor
                    (?B0_EN_CRC bsl 3) bor (?B0_CRCO bsl 2) bor (?B0_PWR_UP bsl 1) bor (?B0_PRIM_RX bsl 0)]).
-define(B0_EN_AA, [0 bor (?B0_ENAA_P5 bsl 5) bor (?B0_ENAA_P4 bsl 4) bor (?B0_ENAA_P3 bsl 3) bor
                   (?B0_ENAA_P2 bsl 2) bor (?B0_ENAA_P1 bsl 1) bor (?B0_ENAA_P0 bsl 0)]).
-define(B0_EN_RXADDR, [0 bor (?B0_ERX_P5 bsl 5) bor (?B0_ERX_P4 bsl 4) bor (?B0_ERX_P3 bsl 3) bor
                       (?B0_ERX_P2 bsl 2) bor (?B0_ERX_P1 bsl 1) bor (?B0_ERX_P0 bsl 0)]).
-define(B0_SETUP_AW, [?B0_AW]).
-define(B0_SETUP_RETR, [(?B0_ARD bsl 4) bor (?B0_ARC bsl 0)]).
-define(B0_RF_SETUP, [0 bor (?B0_RF_DR bsl 3) bor (?B0_RF_PWR bsl 1) bor (?B0_LNA_HCURR bsl 0)]).
-define(B0_STATUS, [0 bor (?B0_RX_DR bsl 6) bor (?B0_TX_DS bsl 5) bor (?B0_MAX_RT bsl 4)]).
-define(B0_OBSERVE_TX, [0]).
-define(B0_DYNPD, [0 bor (?B0_DPL_P5 bsl 5) bor (?B0_DPL_P4 bsl 4) bor (?B0_DPL_P3 bsl 3) bor
                   (?B0_DPL_P2 bsl 2) bor (?B0_DPL_P1 bsl 1) bor (?B0_DPL_P0 bsl 0)]).
-define(B0_FEATURE, [0 bor (?B0_EN_DPL bsl 2) bor (?B0_EN_ACK_PAY bsl 1) bor (?B0_EN_DYN_ACK bsl 0)]).

-define(RFM70_MAX_PACKET_LEN, 32).

-define(RFM70_CMD_READ_REG,  16#00).
-define(RFM70_CMD_WRITE_REG, 16#20).

-define(RFM70_CMD_R_RX_PAYLOAD, 16#61).
-define(RFM70_CMD_W_TX_PAYLOAD, 16#A0).
-define(RFM70_CMD_FLUSH_TX,     16#E1).
-define(RFM70_CMD_FLUSH_RX,     16#E2).
-define(RFM70_CMD_REUSE_TX_PL,  16#E3).
-define(RFM70_CMD_W_TX_PAYLOAD_NOACK, 16#B0).
-define(RFM70_CMD_W_ACK_PAYLOAD, 16#A8).
-define(RFM70_CMD_ACTIVATE, 16#50).
-define(RFM70_CMD_R_RX_PL_WID, 16#60).
-define(RFM70_CMD_NOP, 16#FF).

-define(RFM70_REG_CONFIG, 16#00).
-define(RFM70_REG_EN_AA,  16#01).
-define(RFM70_REG_EN_RXADDR, 16#02).
-define(RFM70_REG_SETUP_AW, 16#03).
-define(RFM70_REG_SETUP_RETR, 16#04).
-define(RFM70_REG_RF_CH,      16#05).
-define(RFM70_REG_RF_SETUP,   16#06).
-define(RFM70_REG_STATUS,     16#07).
-define(RFM70_REG_OBSERVE_TX, 16#08).
-define(RFM70_REG_CD,         16#09).
-define(RFM70_REG_RX_ADDR_P0, 16#0A).
-define(RFM70_REG_RX_ADDR_P1, 16#0B).
-define(RFM70_REG_RX_ADDR_P2, 16#0C).
-define(RFM70_REG_RX_ADDR_P3, 16#0D).
-define(RFM70_REG_RX_ADDR_P4, 16#0E).
-define(RFM70_REG_RX_ADDR_P5, 16#0F).
-define(RFM70_REG_TX_ADDR,    16#10).
-define(RFM70_REG_RX_PW_P0,   16#11).
-define(RFM70_REG_RX_PW_P1,   16#12).
-define(RFM70_REG_RX_PW_P2,   16#13).
-define(RFM70_REG_RX_PW_P3,   16#14).
-define(RFM70_REG_RX_PW_P4,   16#15).
-define(RFM70_REG_RX_PW_P5,   16#16).
-define(RFM70_REG_FIFO_STATUS, 16#17).
-define(RFM70_REG_DYNPD,      16#1C).
-define(RFM70_REG_FEATURE,    16#1D).

