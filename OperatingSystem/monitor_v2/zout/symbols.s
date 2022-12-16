;This file is generated by the build script.
;Do not make any changes here!

AY0_WRITE_REG equ 0x83
A_RTS_OFF equ 0x304
A_RTS_ON equ 0x30D
CMD_DASM equ 0x22B
CMD_EXEC equ 0x121
CMD_IO_READ equ 0x20F
CMD_IO_WRITE equ 0x1E6
CMD_SET equ 0x1AA
CMD_SET_END equ 0x1E0
CMD_SET_LOOP equ 0x1C5
CMD_SYNTAX_ERROR equ 0x264
CMD_VIEW equ 0x140
CMD_VIEW_END equ 0x1A4
CMD_VIEW_ROW equ 0x179
CMD_VIEW_ROW_LOOP equ 0x18B
CONSOLE_INIT equ 0x270
CONSOLE_INIT_CTC equ 0x270
CONSOLE_INIT_SIO equ 0x278
DHEX_TO_BYTE equ 0x316
DHEX_TO_BYTE_FAILED equ 0x332
EXEC_RST_08 equ 0xEB6
EXEC_RST_10 equ 0xEBA
EXEC_RST_18 equ 0xEC6
HEX_TO_BIN equ 0x336
HEX_TO_BIN_2 equ 0x344
HEX_TO_INVALID_2 equ 0x34B
INT_VEC equ 0x1B
Includes equ 0x270
MSG_CLEAR equ 0x2FC
MSG_ERROR equ 0x1057
MSG_START equ 0x1070
PROMPT_BEGIN equ 0x8B
PROMPT_BEGIN_READ_BACKSPACE equ 0xC8
PROMPT_BEGIN_READ_LOOP equ 0x97
PROMPT_BEGIN_READ_PROCESS equ 0xEE
RST_00 equ 0x00
RST_08 equ 0x08
RST_10 equ 0x10
RST_18 equ 0x18
STRCONV_BYTES_TO_HEX equ 0x34D
STRCONV_BYTES_TO_HEX_1 equ 0x35C
STRCONV_BYTES_TO_HEX_2 equ 0x366
STR_Banner_Start equ 0xEC9
STR_HEXDUMP_HEADER equ 0xEF9
STR_SyntaxError equ 0xEEF
STR_Unknown equ 0xEF4
dasm_00 equ 0xACE
dasm_01 equ 0xDBA
dasm_02 equ 0xB54
dasm_03 equ 0xD8C
dasm_08 equ 0xBD1
dasm_09 equ 0xD5F
dasm_0A equ 0xB36
dasm_0B equ 0xDA3
dasm_10 equ 0xB16
dasm_12 equ 0xB5F
dasm_18 equ 0xAD9
dasm_1A equ 0xB40
dasm_20 equ 0xAF3
dasm_22 equ 0xE00
dasm_27 equ 0xB99
dasm_28 equ 0xAEC
dasm_2A equ 0xDD1
dasm_2F equ 0xB9D
dasm_30 equ 0xAE4
dasm_32 equ 0xB6A
dasm_37 equ 0xBA9
dasm_38 equ 0xADD
dasm_3A equ 0xB4A
dasm_3F equ 0xBA5
dasm_76 equ 0xBAD
dasm_80C6 equ 0xC2A
dasm_BE equ 0xBC7
dasm_C3 equ 0xAD2
dasm_C88E equ 0xC50
dasm_C9 equ 0xB22
dasm_CD equ 0xB1C
dasm_D9 equ 0xBDE
dasm_DD equ 0xB04
dasm_DD_01 equ 0xDC1
dasm_DD_09 equ 0xD7A
dasm_DD_22 equ 0xE16
dasm_DD_23 equ 0xD91
dasm_DD_2A equ 0xDE8
dasm_DD_2B equ 0xDA8
dasm_DD_34 equ 0xD2A
dasm_DD_35 equ 0xD47
dasm_DD_86 equ 0xC32
dasm_DD_8E equ 0xC58
dasm_DD_96 equ 0xC7B
dasm_DD_9E equ 0xC9B
dasm_DD_A6 equ 0xCBC
dasm_DD_AE equ 0xCF3
dasm_DD_B6 equ 0xCD8
dasm_DD_BE equ 0xD0F
dasm_DD_E1 equ 0xE68
dasm_DD_E3 equ 0xBEE
dasm_DD_E5 equ 0xE52
dasm_DD_F9 equ 0xE38
dasm_E1 equ 0xE62
dasm_E3 equ 0xBE2
dasm_E5 equ 0xE4C
dasm_E9 equ 0xAFB
dasm_ED_42 equ 0xD71
dasm_ED_43 equ 0xE0C
dasm_ED_44 equ 0xBA1
dasm_ED_45 equ 0xB2C
dasm_ED_46 equ 0xBB8
dasm_ED_4A equ 0xD68
dasm_ED_4B equ 0xDDD
dasm_ED_4D equ 0xB27
dasm_ED_56 equ 0xBBD
dasm_ED_5E equ 0xBC2
dasm_ED_A0 equ 0xC06
dasm_ED_A1 equ 0xC18
dasm_ED_A8 equ 0xC0F
dasm_ED_A9 equ 0xC21
dasm_ED_B0 equ 0xC0A
dasm_ED_B1 equ 0xC1C
dasm_ED_B8 equ 0xC13
dasm_ED_B9 equ 0xC25
dasm_F3 equ 0xBB2
dasm_F9 equ 0xE2E
dasm_FB equ 0xBB5
dasm_FD equ 0xB0D
dasm_FD_01 equ 0xDC9
dasm_FD_09 equ 0xD83
dasm_FD_22 equ 0xE22
dasm_FD_23 equ 0xD9A
dasm_FD_2A equ 0xDF4
dasm_FD_2B equ 0xDB1
dasm_FD_34 equ 0xD36
dasm_FD_35 equ 0xD53
dasm_FD_86 equ 0xC41
dasm_FD_8E equ 0xC67
dasm_FD_96 equ 0xC87
dasm_FD_9E equ 0xCA9
dasm_FD_A6 equ 0xCC8
dasm_FD_AE equ 0xCFF
dasm_FD_B6 equ 0xCE3
dasm_FD_BE equ 0xD1A
dasm_FD_E1 equ 0xE70
dasm_FD_E3 equ 0xBFA
dasm_FD_E5 equ 0xE5A
dasm_FD_F9 equ 0xE42
dasm_FF equ 0xB31
dasm_UU equ 0xE78
dasm_UW equ 0xE7C
dasm__AND equ 0xCB7
dasm__CP equ 0xD0B
dasm__DEC equ 0xD42
dasm__ED_47 equ 0xB89
dasm__ED_4F equ 0xB91
dasm__ED_57 equ 0xB79
dasm__ED_5F equ 0xB81
dasm__INC equ 0xD25
dasm__LD equ 0xB75
dasm__OR equ 0xCD4
dasm__SBC equ 0xC93
dasm__SUB equ 0xC76
dasm__XOR equ 0xCEE
dasm_opcode_table equ 0x676
dasm_print16hex_addr equ 0x4A3
dasm_print8hex equ 0x4BB
dasm_printFlags_table equ 0xE81
dasm_printRegister8_table equ 0xE91
dasm_printRegister8_table_HL equ 0xEB1
dasm_printRegisterIX_table equ 0xE99
dasm_printRegisterIY_table equ 0xEA1
dasm_printRegisterSP_table equ 0xEA9
disassemble equ 0x367
disassemble_continue equ 0x44D
disassemble_err equ 0x43D
disassemble_next equ 0x36B
disassemble_print_opcode_params_end equ 0x43A
disassemble_print_opcode_params_loop equ 0x3E6
disassemble_print_opcode_raw equ 0x3A8
disassemble_print_opcode_raw_fill equ 0x3BC
disassemble_table_first_match equ 0x47F
disassemble_table_found equ 0x499
disassemble_table_notfound equ 0x49D
disassemble_table_seek equ 0x45B
disassemble_table_seek_loop equ 0x45F
mon_start_complete equ 0x76
mon_start_init_ctc equ 0x50
mon_start_init_serial equ 0x5F
mon_start_init_sound equ 0x50
mon_start_ram equ 0x62
mon_start_ram_loop equ 0x6B
mon_var_template equ 0x44
mon_var_template_end equ 0x402A
param_01 equ 0x4C9
param_02 equ 0x4F0
param_03 equ 0x50D
param_03_done equ 0x545
param_03_neg equ 0x52E
param_04 equ 0x549
param_04_i equ 0x559
param_05 equ 0x55F
param_06 equ 0x56C
param_07 equ 0x586
param_08 equ 0x593
param_09 equ 0x5A8
param_09_0A equ 0x5AE
param_0A equ 0x59F
param_10 equ 0x5BD
param_11 equ 0x5CD
param_11_12 equ 0x5DC
param_11_12_all equ 0x5FC
param_11_12_def equ 0x5EF
param_11_12_ix equ 0x5F4
param_11_12_iy equ 0x5F9
param_12 equ 0x5D5
param_13 equ 0x60D
param_80 equ 0x62C
param_80_seek equ 0x633
param_81 equ 0x623
param_comma equ 0x667
param_printRegister equ 0x642
param_printRegisterA equ 0x660
param_printRegisterHL equ 0x658
print_a_hex equ 0x2D9
print_char equ 0x2A9
print_clear equ 0x2BC
print_newLine equ 0x2C3
print_str equ 0x2B1
print_str_end equ 0x2BB
print_wait_out equ 0x2CE
read_char equ 0x2EA
var_buffer equ 0x4029
var_buffer_len equ 0x4000
var_curserchar equ 0x4006
var_curserlastaddr equ 0x4007
var_curseron equ 0x4005
var_curserstate equ 0x4004
var_curserx equ 0x4002
var_cursery equ 0x4003
var_last_char equ 0x4001
var_ps2mem equ 0x4019
var_scratch equ 0x4009
xmodem_ack equ 0x108A
xmodem_await_conn equ 0xF43
xmodem_end equ 0xFEE
xmodem_err equ 0xFB4
xmodem_init equ 0xF15
xmodem_int equ 0xFF5
xmodem_int_cont equ 0x1014
xmodem_nak equ 0x1098
xmodem_out equ 0x1046
xmodem_packet equ 0xF4E
xmodem_packet_EOT equ 0xFAC
xmodem_packet_get equ 0xF60
xmodem_packet_get_crc equ 0xF99
xmodem_packet_get_data equ 0xF8A
xmodem_read_wait equ 0x1019
xmodem_read_wait_loop equ 0x1027
xmodem_read_wait_timeout equ 0x1044
xmodem_wait equ 0x10A6
xmodem_wait_1 equ 0x10AC
xmodem_wait_out equ 0x104C
