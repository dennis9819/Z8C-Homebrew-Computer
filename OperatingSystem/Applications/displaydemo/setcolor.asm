.include "extern_symbols.s" ;include monitor symbols.
    START_ADDR   .EQU 0x8000
    
    org START_ADDR  
com_header:
    jp com_prg
    db 0x00
    dw 0x00, 0x00, 0x00   ;always 0
    dw [START_ADDR]     ;start addr
    dw [_eof]           ;end of file
    dw [_eof - START_ADDR]  ;length
    dc 48,0x00
    
com_prg:
; Program start
    call VDP_INIT_TEXT2
    call VTERM_INIT
    ld hl,[STR_Banner_Start_Test]
    call VTERM_PRINT_STR
    ld hl,[STR_Banner_Start_Test]
    call VTERM_PRINT_STR
    ld hl,[STR_Banner_Start_Test]
    call VTERM_PRINT_STR
    ld hl,[STR_Banner_Start_Test]
    call VTERM_PRINT_STR
    ld hl,[STR_Banner_Start_Test]
    call VTERM_PRINT_STR
    ld hl,[STR_Banner_Start_Test]
    call VTERM_PRINT_STR
    ld hl,[STR_Banner_Start_Test]
    call VTERM_PRINT_STR
    ld hl,[STR_Banner_Start_Test]
    call VTERM_PRINT_STR
    ld hl,[STR_Banner_Start_Test]
    call VTERM_PRINT_STR

    

    call VTERM_CURSOR_ON

    ld b, TmsBlack
    ld c, TmsWhite
    call VDP_COLOR

TEST_ECHO:
    call read_char
    jp z, TEST_ECHO
    call VTERM_PRINT
    jr TEST_ECHO
    ret

STR_Banner_Start_Test:
    db 10,13,"Z8C Monitor V2.1 by Dennis Gunia (2022-2024)",10,13,"TEST",10,13,"NEW LINE",10,13,"TEST > ",0


    .include "kdrv_vdpterm.s"
    .include "krdv_vdp.s"
    .include "kdrv_vt82c42.s"
    .include "kdrv_int.s"
_eof:


