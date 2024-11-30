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
    VDP_REGISTER .EQU 0xE1   
    VDP_RAM     .EQU 0xE0  

    VDPR_MODE0  .EQU 0
    VDPR_MODE1  .EQU 1
    VDPR_MODE2  .EQU 8
    VDPR_MODE3  .EQU 9
    VDPR_COLOR  .EQU 7

    VDPR_PATNMEBASE    .EQU 2
    VDPR_PATGENBASE    .EQU 4
    VDPR_COLTBBASE0    .EQU 3
    VDPR_COLTBBASE1    .EQU 10


TmsTransparent:         equ 0
TmsBlack:               equ 1
TmsMediumGreen:         equ 2
TmsLightGreen:          equ 3
TmsDarkBlue:            equ 4
TmsLightBlue:           equ 5
TmsDarkRed:             equ 6
TmsCyan:                equ 7
TmsMediumRed:           equ 8
TmsLightRed:            equ 9
TmsDarkYellow:          equ 0ah
TmsLightYellow:         equ 0bh
TmsDarkGreen:           equ 0ch
TmsMagenta:             equ 0dh
TmsGray:                equ 0eh
TmsWhite:               equ 0fh


TMS_R8_MS equ 128   ;Mouse: when set to 1, sets the color bus into input mode and enables mouse. If set to 0, sets color bus into output mode and disables mouse
TMS_R8_LP equ 64    ;Light pen: when set to 1, enables light pen
TMS_R8_TP equ 32    ;Sets the color of code 0 to the color of the palette
TMS_R8_CB equ 16    ;Color bus: when set to 1, sets color bus into input mode. If set to 0, sets color bus into output mode
TMS_R8_VR equ 8     ;If set to 1, VRAM is 64Kx1Bit or 64Kx4bits. If set to 0, VRAM is 16Kx1Bit or 16Kx4Bits
TMS_R8_SPD equ 2    ;if set to 1, sprites are not displayed and related VRAM reads are not performed
TMS_R8_BW equ 1     ;if set to 1, output is grayscale in 32 tones

    call tms_init_text2



    ret


; Set a VDP Register (dircet access)
; a = data
; e = register
tms_setregister:
    out (VDP_REGISTER), a
    ld a,e
    or 80h
    out (VDP_REGISTER), a
    ret

; ahl contains pointer to VRAM address

tms_mem_setpointer_rd:
    rlc h
    rla
    rlc h
    rla
    srl h
    srl h
    out (VDP_REGISTER),a
    ld a,14 + 128
    out (VDP_REGISTER),a
    ld a,l
    out (VDP_REGISTER),a
    ld a,h
    out (VDP_REGISTER),a
    ret

; ahl contains pointer to VRAM address
tms_mem_setpointer_wr:
    rlc h
    rla
    rlc h
    rla
    srl h
    srl h
    out (VDP_REGISTER),a
    ld a,14 + 128
    out (VDP_REGISTER),a
    ld a,l
    out (VDP_REGISTER),a
    ld a,h
    or 64
    out (VDP_REGISTER),a
    ret

;b back, c: front
tms_set_color
    ld a,c      ;load front color to a
    rlca
    rlca
    rlca
    rlca
    or b        ;combine back color
    ld e, VDPR_COLOR
    jp tms_setregister

tms_init_text2:
    ; init vdp (80col text)
    ld e, VDPR_MODE0
    ld a, 00000100b ;TEXT2
    call tms_setregister

    ld e, VDPR_MODE1    ;blank screen wit 64K enabled
    ld a, 00001000b
    call tms_setregister

    ld e, VDPR_MODE2
    ld a, TMS_R8_SPD
    call tms_setregister

    ld e, VDPR_MODE3
    ld a, 00000010b
    call tms_setregister
    ; set memory layout
    ; set pattern name table
    ld e, VDPR_PATNMEBASE
    ld a, 00000011b
    call tms_setregister

    ld e, VDPR_PATGENBASE
    ld a, 00000010b
    call tms_setregister

    ld e, VDPR_COLTBBASE0
    ld a, 00101111b
    call tms_setregister

    ld e, VDPR_COLTBBASE1
    ld a, 00000000b
    call tms_setregister

    ;enable cpu wait
    ld e,25
    ld a,4
    call tms_setregister


    ;setup cursor:
    ld e, 13    ; set blink rate
    ld a, 0x22
    call tms_setregister

    ld e, 12    ; set blink color
    ld a, 0xFF
    call tms_setregister

    ld de, 0x0000
    ld bc, 0x2000
    ld a, 0
    call tms_fill_data

    ;load font
    ld hl, [TmsFont]
    ld bc, 256*8
    ld de, 0x1000
    call tms_load_data

    ld e, VDPR_MODE1    ;enable screen wit 64K enabled
    ld a, 01010000b
    call tms_setregister

    ;WRITE CHAR
    ld hl, 0
    ld a,0
    call tms_mem_setpointer_wr
    ld a,'R'

    call VTERM_INIT
    ld hl,[STR_Banner_Start_Test]
    call VTERM_PRINT_STR
    call VTERM_CURSOR_ON

    ld b, TmsBlack
    ld c, TmsWhite
    call tms_set_color

TEST_ECHO:
    call read_char
    jp z, TEST_ECHO
    call VTERM_PRINT
    jr TEST_ECHO
    ret

STR_Banner_Start_Test:
    db 10,13,"Z8C Monitor V2.1 by Dennis Gunia (2022-2024)",10,13,"TEST",10,13,"NEW LINE",10,13,"LOLLOLLLOLOLOLOLOL",0

;hl location in sysram
;de location in vram (destination)
;bc amount of bytes (byte counter)
tms_load_data:
    ;setup address
    push hl
    ex de,hl
    ld a,0
    call tms_mem_setpointer_wr
    pop hl
    
    ;load data
tms_load_data_loop:
    ld a,(hl)   ;load byte from system
    inc hl
    out (VDP_RAM),a
    dec bc          ;decrement counter
    ld a,b          ;check if zero
    or c
    jr nz, tms_load_data_loop   ;if not loop
    ret             ;else exit

tms_read_statusreg:
    out (VDP_RAM),a
    ld a,15 + 128
    out (VDP_RAM),a
    in a,(VDP_RAM)
    ex af,af'
    xor a           ; ld a,0
    out (VDP_RAM),a
    ld a,15 + 128
    out (VDP_RAM),a
    ex af,af'
    ret

;a  bytes to fill
;de location in vram (destination)
;bc amount of bytes (byte counter)
tms_fill_data:
    ;setup address
    push af
    ex de,hl
    call tms_mem_setpointer_wr
    
tms_fill_data_loop:
    pop af
    out (VDP_RAM),a
    push af
    dec bc          ;decrement counter
    ld a,b          ;check if zero
    or c
    jr nz, tms_fill_data_loop   ;if not loop
    pop af
    ret             ;else exit


    .include "font80.s"
    .include "vdpterm.s"


;

;hl location in sysmem
;de location in vram (destination)
;bc amount of bytes (byte counter)
tms_read_data:
    push hl
    push bc
    ex de,hl
    call tms_mem_setpointer_rd
    pop bc
    pop hl
tms_read_data_loop:
    in a,(VDP_RAM)
    ld (hl),a
    inc hl
    dec bc          ;decrement counter
    ld a,b          ;check if zero
    or c
    jr nz, tms_read_data_loop   ;if not loop
    ret             ;else exit
_eof:

