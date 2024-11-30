;----------------------------------------------------------------
;BIOS Driver for VDP V9958
;by Dennis Gunia (07/2024)
;----------------------------------------------------------------
    
;================================================================
; I/O registers
;================================================================
    VDP_REGISTER .EQU 0xE1   
    VDP_RAM      .EQU 0xE0  

;================================================================
; VDP Registers
;================================================================
    ; registers
    VDPR_MODE0  .EQU 0
    VDPR_MODE1  .EQU 1
    VDPR_MODE2  .EQU 8
    VDPR_MODE3  .EQU 9
    VDPR_COLOR  .EQU 7
    VDPR_PATNMEBASE    .EQU 2
    VDPR_PATGENBASE    .EQU 4
    VDPR_COLTBBASE0    .EQU 3
    VDPR_COLTBBASE1    .EQU 10
    ; register bits
    TMS_R8_MS   .EQU 128   ;Mouse: when set to 1, sets the color bus into input mode and enables mouse. If set to 0, sets color bus into output mode and disables mouse
    TMS_R8_LP   .EQU 64    ;Light pen: when set to 1, enables light pen
    TMS_R8_TP   .EQU 32    ;Sets the color of code 0 to the color of the palette
    TMS_R8_CB   .EQU 16    ;Color bus: when set to 1, sets color bus into input mode. If set to 0, sets color bus into output mode
    TMS_R8_VR   .EQU 8     ;If set to 1, VRAM is 64Kx1Bit or 64Kx4bits. If set to 0, VRAM is 16Kx1Bit or 16Kx4Bits
    TMS_R8_SPD  .EQU 2    ;if set to 1, sprites are not displayed and related VRAM reads are not performed
    TMS_R8_BW   .EQU 1     ;if set to 1, output is grayscale in 32 tones
    ; colors
    TmsTransparent     .EQU 0
    TmsBlack           .EQU 1
    TmsMediumGreen     .EQU 2
    TmsLightGreen      .EQU 3
    TmsDarkBlue        .EQU 4
    TmsLightBlue       .EQU 5
    TmsDarkRed         .EQU 6
    TmsCyan            .EQU 7
    TmsMediumRed       .EQU 8
    TmsLightRed        .EQU 9
    TmsDarkYellow      .EQU 0ah
    TmsLightYellow     .EQU 0bh
    TmsDarkGreen       .EQU 0ch
    TmsMagenta         .EQU 0dh
    TmsGray            .EQU 0eh
    TmsWhite           .EQU 0fh
;================================================================
; I/O access functions 
;================================================================

;------------------------------------------------------------------------------
; set vdp register
;
; inputs: a (value), e (register)
;------------------------------------------------------------------------------
VDP_SETREG:
    out (VDP_REGISTER), a
    ld a,e
    or 80h
    out (VDP_REGISTER), a
    ret

;------------------------------------------------------------------------------
; read vdp status register
;
; inputs: a (register)
;------------------------------------------------------------------------------
VDP_STS:
    out (VDP_RAM),a ;write addr
    ld a,15 + 128
    out (VDP_RAM),a ;selct reg for read
    in a,(VDP_RAM)
    ret

;------------------------------------------------------------------------------
; set vdp ram pointer for read
;
; inputs: ahl (address)
;------------------------------------------------------------------------------
VDP_RAMADDR_RD:
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

;------------------------------------------------------------------------------
; set vdp ram pointer for write
;
; inputs: ahl (address)
;------------------------------------------------------------------------------
VDP_RAMADDR_WR:
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

;------------------------------------------------------------------------------
; set vdp console color
;
; inputs: b (back color), c (front color)
; destroys: e, af
;------------------------------------------------------------------------------
VDP_COLOR:
    ld a,c      ;load front color to a
    rlca
    rlca
    rlca
    rlca
    or b        ;combine back color
    ld e, VDPR_COLOR
    call VDP_SETREG
    ; set blink color
    ld e, 12    ; set blink color
    ld a,b      ;load front color to a
    rlca
    rlca
    rlca
    rlca
    or c        ;combine back color
    call VDP_SETREG
    ret

;------------------------------------------------------------------------------
; copy block of memory to vdp
;
; inputs:   hl (source in local ram)
;           de (destination in vram)
;           bc (byte counter)
; destroys: af
;------------------------------------------------------------------------------
VDP_LOADRAM:
    ;setup address
    push hl
    ex de,hl
    ld a,0
    call VDP_RAMADDR_WR
    pop hl
_VDP_LOADRAM_LOOP:
    ld a,(hl)   ;load byte from system
    inc hl
    out (VDP_RAM),a
    dec bc          ;decrement counter
    ld a,b          ;check if zero
    or c
    jr nz, _VDP_LOADRAM_LOOP   ;if not loop
    ret             ;else exit


;------------------------------------------------------------------------------
; fill block of memory in vdp
;
; inputs:   a (value to write)
;           de (destination in vram)
;           bc (byte counter)
; destroys: hl
;------------------------------------------------------------------------------
VDP_FILL:
    push af
    ex de,hl
    call VDP_RAMADDR_WR
_VDP_FILL_LOOP:
    pop af
    out (VDP_RAM),a
    push af
    dec bc          ;decrement counter
    ld a,b          ;check if zero
    or c
    jr nz, _VDP_FILL_LOOP   ;if not loop
    pop af
    ret    


;================================================================
; Init screen modes
;================================================================
VDP_INIT_TEXT2:
    ; init vdp (80col text)
    ld e, VDPR_MODE0
    ld a, 00000100b ;TEXT2
    call VDP_SETREG
    ld e, VDPR_MODE1    ;blank screen wit 64K enabled
    ld a, 00001000b
    call VDP_SETREG
    ld e, VDPR_MODE2
    ld a, TMS_R8_SPD
    call VDP_SETREG
    ld e, VDPR_MODE3
    ld a, 00000010b
    call VDP_SETREG
    ; set memory layout
    ; set pattern name table
    ld e, VDPR_PATNMEBASE
    ld a, 00000011b
    call VDP_SETREG
    ld e, VDPR_PATGENBASE
    ld a, 00000010b
    call VDP_SETREG
    ld e, VDPR_COLTBBASE0
    ld a, 00101111b
    call VDP_SETREG
    ld e, VDPR_COLTBBASE1
    ld a, 00000000b
    call VDP_SETREG
    ;enable cpu wait (for fast writes. Required!!!!)
    ld e,25
    ld a,4
    call VDP_SETREG
    ;setup cursor:
    ld e, 13    ; set blink rate
    ld a, 0x22
    call VDP_SETREG
    ;fill nametable to 0
    ld de, 0x0000
    ld bc, 0x2000
    ld a, 0
    call VDP_FILL
    ;load font
    ld hl, [VDP_FONT_6x8_80COL]
    ld bc, 256*8
    ld de, 0x1000
    call VDP_LOADRAM
    ;enable screen wit 64K enabled
    ld e, VDPR_MODE1    
    ld a, 01010000b
    call VDP_SETREG
    ret

;================================================================
; Fonts
;================================================================
VDP_FONT_6x8_80COL:
    .include "font80.s"