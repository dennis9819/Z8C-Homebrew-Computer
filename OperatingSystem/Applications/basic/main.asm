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
    ;shadow rom
    call B_PRINTINLINE
    db 10,13,"Shadowing ROM @ 0x0000-0x3FFF ...",10,13,0 
    ld a,0x01       ;set bit1 to switch to ram
    out (0x00),a    ;store to bank select register
    ld hl,[prg_start]
    ld de,0
    ld bc,[prg_end-prg_start]
    ldir
    jp 0x0000


prg_start:
    phase 0x0000

BASRST_00: ;Hardware Restart
    jp CSTART    
    defs 5
    ;org 0x0008
BASRST_08  ;Print Char
    jp _BASRST_08
    defs 5
    ;org 0x0010
BASRST_10  ;receive char
    jp _BASRST_10
    defs 5
    ;org 0x0018
BASRST_18  ;Buffer length
    jp _BASRST_18
    defs 5
    ;org 0x0044

_BASRST_08:
    out (CS_SIO_A_D),a
_wait:
    ld a,1
    out (CS_SIO_A_C),A
    in A,(CS_SIO_A_C) ;read RRx
    bit 0,A
    jr z,_wait
    ret

_BASRST_10:
    ;RTS ON
    ld a,005h ;write into WR0: select WR5
    out (CS_SIO_A_C),A
    ld a,0EAh ;DTR active, TX 8bit, BREAK off, TX on, RTS active
    out (CS_SIO_A_C),A
    nop
    ;read if avail
    xor a               ; a = 0
    out (CS_SIO_A_C), a ; select reg 0
    in a, (CS_SIO_A_C)  ; read reg 0
    and	1               ; mask D0 (recieve char available)
    ;RTS OFF
    push af
    ld a,005h ;write into WR0: select WR5
    out (CS_SIO_A_C),A
    ld a,068h ;DTR inactive, TX 8bit, BREAK off, TX on, RTS inactive
    out (CS_SIO_A_C),A
    pop af
    ret	Z               ; return 0 if no char
    ;read data
    in a, (CS_SIO_A_D)  ; read char if avail
    ret                 ; return

_BASRST_18:
    ld a,0
    ret

BASSTART:
.include "basic.s"
_WORKSPACE:

    dephase
prg_end:

_eof: