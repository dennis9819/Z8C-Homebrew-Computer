;This file is generated by the build script.
;Do not make any changes here!

?a0000 equ 0x14C3
?a0001 equ 0x1580
ADDR_RTC equ 0xD0
A_RTS_OFF equ 0x1DA
A_RTS_ON equ 0x1E3
BOOT_PHASE0 equ 0x86
BOOT_PHASE1 equ 0xB7
BOOT_PHASE1_LOOP equ 0xC0
BOOT_PHASE2 equ 0xCB
B_BEEP equ 0x50
B_DSKSEL equ 0x74
B_FATCD equ 0x7D
B_FATCREATE equ 0x80
B_FATOPEN equ 0x77
B_FATREAD equ 0x7A
B_FATWRITE equ 0x83
B_IICRECV equ 0x56
B_IICSEND equ 0x53
B_KEYREAD equ 0x6B
B_KEYREADASCII equ 0x6E
B_KEYSEND equ 0x71
B_PRINTAHEX equ 0x62
B_PRINTCHAR equ 0x59
B_PRINTINLINE equ 0x5F
B_PRINTLN equ 0x65
B_PRINTSTR equ 0x5C
B_READCHAR equ 0x68
COMMAND equ 0x1AA4
COMMAND_ABORT equ 0x1AA1
COMMAND_BACKSPACE equ 0x1AE5
COMMAND_LUT equ 0x1A0F
COMMAND_PROCESS equ 0x1B0B
COMMAND_PROCESS_FOUND equ 0x1B46
COMMAND_PROCESS_LOOP equ 0x1B0E
COMMAND_PROCESS_LOOP_STR1 equ 0x1B17
COMMAND_PROCESS_LOOP_STR2 equ 0x1B2D
COMMAND_PROCESS_NEXT_ENTRY equ 0x1B32
COMMAND_PROCESS_NEXT_ENTRYI equ 0x1B31
COMMAND_PROCESS_NOT_FOUND equ 0x1B3D
COMMAND_READ equ 0x1AB6
CONSOLE_INIT equ 0xEF
CS_APU_CTRL equ 0xF9
CS_APU_DATA equ 0xF8
CS_BANK equ 0x00
CS_CTC_0 equ 0x04
CS_CTC_1 equ 0x05
CS_CTC_2 equ 0x06
CS_CTC_3 equ 0x07
CS_DIP equ 0x01
CS_PIA_CR equ 0x33
CS_PIA_PA equ 0x30
CS_PIA_PB equ 0x31
CS_PIA_PC equ 0x32
CS_PIO_AC equ 0xF6
CS_PIO_AD equ 0xF4
CS_PIO_BC equ 0xF7
CS_PIO_BD equ 0xF5
CS_SIO_A_C equ 0x09
CS_SIO_A_D equ 0x08
CS_SIO_B_C equ 0x0B
CS_SIO_B_D equ 0x0A
CS_VT82C42_CTRL equ 0xF1
CS_VT82C42_DATA equ 0xF0
DHEX_TO_BYTE equ 0x251
DHEX_TO_BYTE_FAILED equ 0x26D
ERR_SYNTAX equ 0x1B5C
EXEC_RST_08 equ 0xDF1
EXEC_RST_10 equ 0xDF5
EXEC_RST_18 equ 0xE01
HEX_TO_BIN equ 0x271
HEX_TO_BIN_2 equ 0x27F
HEX_TO_INVALID_2 equ 0x286
IDE_CMD_IDENT equ 0xEC
IDE_CMD_READSEC equ 0x20
IDE_CMD_WRITESEC equ 0x30
IDE_DEV_TABLE equ 0x1325
IDE_RD equ 0x40
IDE_REG_ALTSTS equ 0x16
IDE_REG_CMDSTS equ 0x0F
IDE_REG_DATA equ 0x08
IDE_REG_DRVADDR equ 0x17
IDE_REG_ERROR equ 0x09
IDE_REG_HCYL equ 0x0D
IDE_REG_HEAD equ 0x0E
IDE_REG_LBA0 equ 0x0B
IDE_REG_LBA1 equ 0x0C
IDE_REG_LBA2 equ 0x0D
IDE_REG_LBA3 equ 0x0E
IDE_REG_LCYL equ 0x0C
IDE_REG_SECTOR equ 0x0A
IDE_REG_SSECTOR equ 0x0B
IDE_RST equ 0x80
IDE_WR equ 0x20
IIC_CLK equ 0x01
IIC_DATA equ 0x02
INT_PIO_ADDRC equ 0xF6
INT_PIO_ADDRD equ 0xF4
INT_VEC equ 0x1B
INT_VEC_TABLE equ 0x4000
IO_AY0_ADDR equ 0x40
IO_AY0_DATA equ 0x41
IO_AY1_ADDR equ 0x42
IO_AY1_DATA equ 0x43
IO_REG0 equ 0x20
IO_RTC_AMPM equ 0x25
IO_RTC_CTR_D equ 0x2D
IO_RTC_CTR_E equ 0x2E
IO_RTC_CTR_F equ 0x2F
IO_RTC_DAY0 equ 0x26
IO_RTC_DAY1 equ 0x27
IO_RTC_HOUR equ 0x24
IO_RTC_MIN0 equ 0x22
IO_RTC_MIN1 equ 0x23
IO_RTC_MON0 equ 0x28
IO_RTC_MON1 equ 0x29
IO_RTC_SEC0 equ 0x20
IO_RTC_SEC1 equ 0x21
IO_RTC_WEEK equ 0x2C
IO_RTC_YEAR1 equ 0x2B
IO_RTC_YERR0 equ 0x2A
MEM_FAT_AMOUNT equ 0x45BC
MEM_FAT_CLUSTERLEN equ 0x45BF
MEM_FAT_COUNT1 equ 0x45C0
MEM_FAT_CURDIR equ 0x45E3
MEM_FAT_CURRDIR equ 0x4643
MEM_FAT_DATASTART equ 0x45C5
MEM_FAT_DIRSEC equ 0x45D1
MEM_FAT_EXEC_COUNT equ 0x4649
MEM_FAT_EXEC_CURR equ 0x4647
MEM_FAT_EXEC_START equ 0x464B
MEM_FAT_FILEREMAIN equ 0x45CD
MEM_FAT_OF0_ATTRIBUTE equ 0x4633
MEM_FAT_OF0_CCLUST equ 0x4635
MEM_FAT_OF0_DATREM equ 0x463F
MEM_FAT_OF0_DATSEC equ 0x463B
MEM_FAT_OF0_FATSEC equ 0x4637
MEM_FAT_RESERVED equ 0x45BA
MEM_FAT_ROOTSTART equ 0x45C9
MEM_FAT_SECTORS equ 0x45BD
MEM_FAT_TMPFNAME equ 0x45D3
MEM_FAT_TMPPOINTER equ 0x45C1
MEM_IDE_BUFFER equ 0x43BA
MEM_IDE_DEVICE equ 0x42CF
MEM_IDE_DEV_TABLE equ 0x42D9
MEM_IDE_FSBUFFER equ 0x45BA
MEM_IDE_PARTITION equ 0x42D1
MEM_IDE_POINTER equ 0x42D5
MEM_IDE_SELECTED equ 0x4319
MEM_IDE_STATUS equ 0x42D0
MEM_IDE_STRING_0 equ 0x431A
MEM_IDE_STRING_1 equ 0x4342
MEM_IDE_STRING_2 equ 0x436A
MEM_IDE_STRING_3 equ 0x4392
MSG_CLEAR equ 0x1D2
NOT_IMPLEMENTED equ 0x1B55
OP_CALL equ 0x1BB7
OP_CD equ 0x1F87
OP_CLR equ 0x1D4B
OP_DASM equ 0x1C38
OP_DIR equ 0x1F83
OP_DUMP equ 0x1BD7
OP_EXEC equ 0x1B9E
OP_FSEXEC equ 0x1FA4
OP_IIC_IN equ 0x1CFF
OP_IIC_OUT equ 0x1C9F
OP_IO_IN equ 0x1C68
OP_IO_OUT equ 0x1C7E
OP_LSDSK equ 0x1F60
OP_RTIME equ 0x1D4F
OP_SELDSK equ 0x1F64
OP_SET equ 0x1C07
OP_STIME equ 0x1EE7
POST_CHECK_APU equ 0x1FDD
POST_CHECK_IDE_30 equ 0x2008
POST_CHECK_IDE_40 equ 0x201B
POST_CHECK_PIO equ 0x1FB8
POST_START equ 0x1FA8
POST_TEST_RTC equ 0x2035
PRG_RAM_START equ 0x4110
PRG_RAM_TOP equ 0xFF00
PRINTINLINE equ 0x1EC
PROG_MEM_START equ 0x4000
PROG_ROM_START equ 0x100
RST_00 equ 0x00
RST_08 equ 0x08
RST_10 equ 0x10
RST_18 equ 0x18
STACK_RAM_TOP equ 0xFFFF
START_ADDR equ 0x8000
START_ROM equ 0x00
STRCONV_BYTES_TO_HEX equ 0x288
STRCONV_BYTES_TO_HEX_1 equ 0x297
STRCONV_BYTES_TO_HEX_2 equ 0x2A1
STR_Banner_Start equ 0x27E9
STR_PD_HEADER equ 0x19C8
STR_keyboard_init_err equ 0x8123
STR_keyboard_init_failed equ 0x8147
STR_keyboard_init_missing equ 0x8169
STR_keyboard_init_okay equ 0x8108
SYS_BUF_START equ 0x4010
SYS_RAM_START equ 0x4000
VAR_CONSOLE_BAUD equ 0x18
VAR_CONSOLE_CONF equ 0x0F
VDP_MEM equ 0x80
VDP_REG equ 0x81
_COMMAND_PROCESS_FOUND equ 0x1B54
_OP_CALL equ 0x1BD6
_OP_IIC_ACK_ERR equ 0x1CE4
_OP_IIC_ACK_ERR_str equ 0x1CEB
_OP_IIC_IN_LOOP equ 0x1D31
_OP_IIC_IN_LOOP_TEXT equ 0x1D3E
_OP_IIC_OUT_LOOP equ 0x1CB5
_OP_IIC_OUT_SEND equ 0x1CD1
_OP_RTIME_NN equ 0x1D52
_OP_RTIME_RD_CMD equ 0x1E0A
_OP_SELDSK_INVALID equ 0x1F73
_OP_SELDSK_INVALID_STR equ 0x1F8B
_OP_SET_LOOP equ 0x1C1F
_OP_STIME_INVALID equ 0x1EAD
_OP_STIME_PROMPT equ 0x1EC4
_OP_STIME_PROMPT_ERR equ 0x1EDC
_OP_STIME_STR_DAY equ 0x1E0B
_OP_STIME_STR_HOUR equ 0x1E5C
_OP_STIME_STR_MIN equ 0x1E77
_OP_STIME_STR_MON equ 0x1E26
_OP_STIME_STR_SEC equ 0x1E92
_OP_STIME_STR_YEAR equ 0x1E41
_POST_CHECK_APU_FAILED equ 0x1FFC
_POST_CHECK_IDE_FAILED equ 0x202E
_POST_CHECK_PIO_FAILED equ 0x1FD1
_POST_TEST_RTC_INVALID equ 0x206F
_POST_TEST_RTC_NOTFOUND equ 0x2068
_STR_NOT_FOUND equ 0x1B77
_STR_NOT_IMPLEMENTED equ 0x1B63
_STR_SYNTAX equ 0x1B8B
_beep_pause_l1 equ 0xE1F
_compare_filename_loop equ 0x24AA
_compare_filename_nomatch equ 0x24B8
_eof equ 0x818F
_fat_cd_navigate equ 0x265F
_fat_cd_navigate_end equ 0x26A5
_fat_cd_navigate_errfile equ 0x26CD
_fat_cd_navigate_errfile_str equ 0x26FF
_fat_cd_navigate_error equ 0x26B7
_fat_cd_navigate_error_str equ 0x26D5
_fat_cd_navigate_findsec equ 0x2664
_fat_cd_navigate_findsec_skipslash equ 0x2690
_fat_cd_navigate_goback_fl equ 0x261B
_fat_cd_navigate_inerror equ 0x26C5
_fat_cd_navigate_inerrorS equ 0x26BF
_fat_cd_navigate_inerrore equ 0x26C6
_fat_cd_navigate_inputerr_str equ 0x26EC
_fat_cd_navigate_l2 equ 0x2696
_fat_exec_notexec equ 0x27C5
_fat_exec_notfound equ 0x27AC
_fat_exec_read_done equ 0x2794
_fat_exec_readloop1 equ 0x2776
_fat_get_root_table_invalid equ 0x229A
_fat_getfatsec_notroot equ 0x22D9
_fat_increment_32 equ 0x2430
_fat_lfs_loop equ 0x23A6
_fat_lfs_loop_compare_end equ 0x23E2
_fat_lfs_loop_compare_match equ 0x23E6
_fat_lfs_loop_compare_next_sector equ 0x23B8
_fat_math_add32 equ 0x2444
_fat_math_mul32 equ 0x2475
_fat_math_mul32_l equ 0x247C
_fat_math_mul32_noadd equ 0x2489
_fat_math_sector_add_16 equ 0x241C
_fat_print_directory_dir equ 0x259A
_fat_print_directory_loop equ 0x252D
_fat_print_directory_loop_break equ 0x25F9
_fat_print_directory_loop_file equ 0x2549
_fat_print_directory_loop_next equ 0x25C5
_fat_print_directory_loop_next_sector equ 0x25D0
_format_filename_fat16_clean equ 0x24C1
_format_filename_fat16_loop equ 0x24C8
_format_filename_fat16_loop_copy equ 0x24DB
_format_filename_fat16_loop_skip_8 equ 0x24D2
_ide_readsector_512_floop equ 0xE7B
_ideif_drv_sel_fail equ 0x15F0
_ideif_drv_sel_fstr0 equ 0x16AD
_ideif_drv_sel_pstr equ 0x16A4
_ideif_drv_sel_sstr0 equ 0x16BB
_ideif_drv_sel_syn equ 0x16C8
_ideif_init_drive_charloop equ 0x14B7
_ideif_init_drive_found equ 0x1499
_ideif_init_drive_loop equ 0x1481
_ideif_init_drive_nodrv equ 0x1494
_ideif_init_drive_prt_fnd equ 0x1515
_ideif_init_drive_prt_l1 equ 0x1503
_ideif_init_drive_prt_ln equ 0x150D
_ideif_prnt_devtable_hdr equ 0x1630
_ideif_prnt_devtable_l1 equ 0x1391
_ideif_prnt_devtable_l1_e2 equ 0x1457
_ideif_prnt_devtable_l1_es equ 0x13D6
_ideif_prnt_devtable_l1_ms equ 0x1454
_ideif_prnt_devtable_l1_nxt equ 0x139E
_ideif_prnt_devtable_l1_s00 equ 0x13C4
_ideif_prnt_devtable_l1_s01 equ 0x13C9
_ideif_prnt_devtable_l1_s02 equ 0x13CE
_ideif_prnt_devtable_l1_sFF equ 0x13D3
_ideif_prnt_devtable_l1_sel equ 0x139C
_ideif_prnt_devtable_master equ 0x1694
_ideif_prnt_devtable_s00 equ 0x1668
_ideif_prnt_devtable_s01 equ 0x1673
_ideif_prnt_devtable_s02 equ 0x167E
_ideif_prnt_devtable_sFF equ 0x1689
_ideif_prnt_devtable_slave equ 0x169C
_int_invalid_int equ 0x185A
_intctrl_init_fill_loop equ 0x183B
_isr_pio equ 0x184E
_isr_pio_test equ 0x8181
_keyb_cmd_rd_l1 equ 0x80E0
_keyb_enable_int_flush_buffer equ 0x80CF
_keyb_wr_wait_ack_l1 equ 0x80FD
_keyboard_init_dev_missing equ 0x80AA
_keyboard_init_failed equ 0x80A3
_keyboard_init_flush_buffer equ 0x8065
_keyboard_init_okay equ 0x80B1
_read_bcd_invalid equ 0x165
_shift4 equ 0x1F5B
_str_invalid_interrupt equ 0x1864
_str_pio_interrupt equ 0x189B
beep equ 0xE04
beep_loop equ 0xE07
beep_pause equ 0xE1E
com_header equ 0x8000
com_prg equ 0x8040
compare_filename equ 0x24A6
con_rb_init equ 0x18E
con_rb_read equ 0x199
con_rb_read_empty equ 0x1B6
con_rb_write equ 0x1BF
consio_init_a equ 0x18CE
consio_init_a_int equ 0x1906
consio_init_a_sio equ 0x18D5
consio_init_ctc_a equ 0x191D
consio_isr equ 0x1960
consio_rx_a equ 0x1933
consio_rx_a_sts equ 0x1956
consio_rx_rts_a_off equ 0x194D
consio_rx_rts_a_on equ 0x1944
consio_tx_a equ 0x1926
consio_tx_a_waitout equ 0x1928
dasm_00 equ 0xA09
dasm_01 equ 0xCF5
dasm_02 equ 0xA8F
dasm_03 equ 0xCC7
dasm_08 equ 0xB0C
dasm_09 equ 0xC9A
dasm_0A equ 0xA71
dasm_0B equ 0xCDE
dasm_10 equ 0xA51
dasm_12 equ 0xA9A
dasm_18 equ 0xA14
dasm_1A equ 0xA7B
dasm_20 equ 0xA2E
dasm_22 equ 0xD3B
dasm_27 equ 0xAD4
dasm_28 equ 0xA27
dasm_2A equ 0xD0C
dasm_2F equ 0xAD8
dasm_30 equ 0xA1F
dasm_32 equ 0xAA5
dasm_37 equ 0xAE4
dasm_38 equ 0xA18
dasm_3A equ 0xA85
dasm_3F equ 0xAE0
dasm_76 equ 0xAE8
dasm_80C6 equ 0xB65
dasm_BE equ 0xB02
dasm_C3 equ 0xA0D
dasm_C88E equ 0xB8B
dasm_C9 equ 0xA5D
dasm_CD equ 0xA57
dasm_D9 equ 0xB19
dasm_DD equ 0xA3F
dasm_DD_01 equ 0xCFC
dasm_DD_09 equ 0xCB5
dasm_DD_22 equ 0xD51
dasm_DD_23 equ 0xCCC
dasm_DD_2A equ 0xD23
dasm_DD_2B equ 0xCE3
dasm_DD_34 equ 0xC65
dasm_DD_35 equ 0xC82
dasm_DD_86 equ 0xB6D
dasm_DD_8E equ 0xB93
dasm_DD_96 equ 0xBB6
dasm_DD_9E equ 0xBD6
dasm_DD_A6 equ 0xBF7
dasm_DD_AE equ 0xC2E
dasm_DD_B6 equ 0xC13
dasm_DD_BE equ 0xC4A
dasm_DD_E1 equ 0xDA3
dasm_DD_E3 equ 0xB29
dasm_DD_E5 equ 0xD8D
dasm_DD_F9 equ 0xD73
dasm_E1 equ 0xD9D
dasm_E3 equ 0xB1D
dasm_E5 equ 0xD87
dasm_E9 equ 0xA36
dasm_ED_42 equ 0xCAC
dasm_ED_43 equ 0xD47
dasm_ED_44 equ 0xADC
dasm_ED_45 equ 0xA67
dasm_ED_46 equ 0xAF3
dasm_ED_4A equ 0xCA3
dasm_ED_4B equ 0xD18
dasm_ED_4D equ 0xA62
dasm_ED_56 equ 0xAF8
dasm_ED_5E equ 0xAFD
dasm_ED_A0 equ 0xB41
dasm_ED_A1 equ 0xB53
dasm_ED_A8 equ 0xB4A
dasm_ED_A9 equ 0xB5C
dasm_ED_B0 equ 0xB45
dasm_ED_B1 equ 0xB57
dasm_ED_B8 equ 0xB4E
dasm_ED_B9 equ 0xB60
dasm_F3 equ 0xAED
dasm_F9 equ 0xD69
dasm_FB equ 0xAF0
dasm_FD equ 0xA48
dasm_FD_01 equ 0xD04
dasm_FD_09 equ 0xCBE
dasm_FD_22 equ 0xD5D
dasm_FD_23 equ 0xCD5
dasm_FD_2A equ 0xD2F
dasm_FD_2B equ 0xCEC
dasm_FD_34 equ 0xC71
dasm_FD_35 equ 0xC8E
dasm_FD_86 equ 0xB7C
dasm_FD_8E equ 0xBA2
dasm_FD_96 equ 0xBC2
dasm_FD_9E equ 0xBE4
dasm_FD_A6 equ 0xC03
dasm_FD_AE equ 0xC3A
dasm_FD_B6 equ 0xC1E
dasm_FD_BE equ 0xC55
dasm_FD_E1 equ 0xDAB
dasm_FD_E3 equ 0xB35
dasm_FD_E5 equ 0xD95
dasm_FD_F9 equ 0xD7D
dasm_FF equ 0xA6C
dasm_UU equ 0xDB3
dasm_UW equ 0xDB7
dasm__AND equ 0xBF2
dasm__CP equ 0xC46
dasm__DEC equ 0xC7D
dasm__ED_47 equ 0xAC4
dasm__ED_4F equ 0xACC
dasm__ED_57 equ 0xAB4
dasm__ED_5F equ 0xABC
dasm__INC equ 0xC60
dasm__LD equ 0xAB0
dasm__OR equ 0xC0F
dasm__SBC equ 0xBCE
dasm__SUB equ 0xBB1
dasm__XOR equ 0xC29
dasm_opcode_table equ 0x5B1
dasm_print16hex_addr equ 0x3DE
dasm_print8hex equ 0x3F6
dasm_printFlags_table equ 0xDBC
dasm_printRegister8_table equ 0xDCC
dasm_printRegister8_table_HL equ 0xDEC
dasm_printRegisterIX_table equ 0xDD4
dasm_printRegisterIY_table equ 0xDDC
dasm_printRegisterSP_table equ 0xDE4
disassemble equ 0x2A2
disassemble_continue equ 0x388
disassemble_err equ 0x378
disassemble_next equ 0x2A6
disassemble_print_opcode_params_end equ 0x375
disassemble_print_opcode_params_loop equ 0x321
disassemble_print_opcode_raw equ 0x2E3
disassemble_print_opcode_raw_fill equ 0x2F7
disassemble_table_first_match equ 0x3BA
disassemble_table_found equ 0x3D4
disassemble_table_notfound equ 0x3D8
disassemble_table_seek equ 0x396
disassemble_table_seek_loop equ 0x39A
dump_pretty equ 0x196D
dump_pretty_ascii equ 0x1997
dump_pretty_ascii_cont equ 0x19B5
dump_pretty_ascii_loop equ 0x199F
dump_pretty_ascii_none equ 0x19B0
dump_pretty_col equ 0x198A
dump_pretty_end equ 0x19C7
dump_pretty_nextrow equ 0x19BB
dump_pretty_row equ 0x1975
endPrint equ 0x1FA
fat_cd_single equ 0x25FB
fat_copy_lba_pointer equ 0x249D
fat_exec equ 0x2716
fat_get_root_table equ 0x21CD
fat_getfatsec equ 0x22C2
fat_openfile equ 0x2377
fat_openfile_noprepare equ 0x237F
fat_print_directory equ 0x24E2
fat_readfilesec equ 0x234A
fat_reset_pointer equ 0x248E
format_filename_fat16 equ 0x24BC
ide_printerror equ 0xEB2
ide_readsector_512_fast equ 0xE67
ide_readsector_timeout equ 0xE9C
ide_regread_8 equ 0xE51
ide_regwrite_8 equ 0xE37
ide_reset equ 0xE2B
ide_writesector_256 equ 0xEB1
ideif_drv_sel equ 0x15B8
ideif_get_drv_pointer equ 0x1558
ideif_init_all equ 0x156C
ideif_init_devtable equ 0x1365
ideif_init_drive equ 0x1478
ideif_prnt_devtable equ 0x1383
iic_init equ 0x173A
iic_read_ack equ 0x1781
iic_receive_buffer equ 0x170B
iic_receive_buffer_done equ 0x172E
iic_receive_buffer_err equ 0x1734
iic_receive_buffer_loop equ 0x171C
iic_receive_byte equ 0x1807
iic_receive_byte_loop equ 0x1814
iic_send_ack equ 0x17A7
iic_send_buffer equ 0x16E1
iic_send_buffer_done equ 0x16FF
iic_send_buffer_err equ 0x1705
iic_send_buffer_loop equ 0x16F1
iic_send_byte equ 0x17E1
iic_send_byte_loop equ 0x17ED
iic_send_ebit equ 0x1760
iic_send_nack equ 0x17C4
iic_send_sbit equ 0x1747
initctrl_int_abandon equ 0x184E
initctrl_int_register equ 0x184E
intctrl_init equ 0x182E
interrupt_vectors equ 0x4000
keyb_cmd_enable equ 0x80B8
keyb_cmd_rd equ 0x80DE
keyb_cmd_wr equ 0x80D8
keyb_enable_int equ 0x80C8
keyb_wait_ibf_empty equ 0x80EB
keyb_wait_obf equ 0x80F2
keyb_wr_wait_ack equ 0x80F8
keyboard_init equ 0x805D
mon_var_template equ 0x44
mon_var_template_end equ 0x613
mon_var_template_sof equ 0x444
nxtILC equ 0x1EF
param_01 equ 0x404
param_02 equ 0x42B
param_03 equ 0x448
param_03_done equ 0x480
param_03_neg equ 0x469
param_04 equ 0x484
param_04_i equ 0x494
param_05 equ 0x49A
param_06 equ 0x4A7
param_07 equ 0x4C1
param_08 equ 0x4CE
param_09 equ 0x4E3
param_09_0A equ 0x4E9
param_0A equ 0x4DA
param_10 equ 0x4F8
param_11 equ 0x508
param_11_12 equ 0x517
param_11_12_all equ 0x537
param_11_12_def equ 0x52A
param_11_12_ix equ 0x52F
param_11_12_iy equ 0x534
param_12 equ 0x510
param_13 equ 0x548
param_80 equ 0x567
param_80_seek equ 0x56E
param_81 equ 0x55E
param_comma equ 0x5A2
param_printRegister equ 0x57D
param_printRegisterA equ 0x59B
param_printRegisterHL equ 0x593
print_16_hex equ 0x181
print_32_hex equ 0x168
print_a_hex equ 0x122
print_bcd equ 0x134
print_char equ 0xF6
print_clear equ 0x105
print_newLine equ 0x10C
print_reg equ 0x1FF
print_str equ 0xFC
print_str_fixed equ 0x1A07
print_wait_out equ 0x117
read_bcd equ 0x14F
read_char equ 0x14C
read_char_raw equ 0x13A
read_lba_sector equ 0x157F
str_dev_done equ 0x1628
str_dev_waitready equ 0x1610
str_error_start equ 0xEE7
str_error_start1 equ 0xF04
str_error_start2 equ 0xF0D
str_error_time equ 0xF16
str_post_apu equ 0x20C7
str_post_ide_30 equ 0x2076
str_post_ide_40 equ 0x2091
str_post_nd equ 0x20FD
str_post_ok equ 0x2135
str_post_pio equ 0x20AC
str_post_rtc equ 0x20E2
str_post_rtc_iv equ 0x210B
var_apu_present equ 0x420E
var_buffer_conin_data equ 0x4100
var_buffer_conin_in equ 0x4200
var_buffer_conin_out equ 0x4201
var_buffer_conin_sts equ 0x4202
var_buffer_conout equ 0x4203
var_buffer_len equ 0x4204
var_bytes_count equ 0x411A
var_curserchar equ 0x420A
var_curserlastaddr equ 0x420B
var_curseron equ 0x4209
var_curserstate equ 0x4208
var_curserx equ 0x4206
var_cursery equ 0x4207
var_dir equ 0x422F
var_idebuffer equ 0x42CF
var_input equ 0x427F
var_last_char equ 0x4205
var_opcode equ 0x4114
var_opcode_length equ 0x4116
var_opcode_pcount equ 0x4119
var_opcode_start equ 0x4110
var_opcode_string equ 0x4117
var_opcode_table equ 0x4112
var_opcode_x equ 0x4115
var_pio_present equ 0x420D
var_ps2mem equ 0x421F
var_scratch equ 0x420F
