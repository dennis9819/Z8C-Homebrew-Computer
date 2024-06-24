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


    ld hl,[_str]
_l1:
    ld a, (hl)
    or a
    ret z
    out (CS_SIO_A_D),a
_wait:
    ld a,1
    out (CS_SIO_A_C),A
    in A,(CS_SIO_A_C) ;read RRx
    bit 0,A
    jr z,_wait
    inc hl
    jr _l1

_str:
    db 10,13,"Hello World",10,13,0


_eof: