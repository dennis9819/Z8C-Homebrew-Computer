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
    ld hl, [_isr_pio_test]
    ld (INT_VEC_TABLE + 2), hl

    ;setup PIO
    ld a, 11001111b ;setup as controll port
    out (INT_PIO_ADDRC), a
    ld a, 11110111b      ;set all as inputs
    out (INT_PIO_ADDRC), a
    ld a, 00000010b ;set interrupt vector
    out (INT_PIO_ADDRC), a
    ld a, 11010111b ;enable interrupt; logic or, mask follows
    out (INT_PIO_ADDRC), a
    ld a, 00001011b ;set interrupt mask
    ;out (INT_PIO_ADDRC), a
    ;in a,(INT_PIO_ADDRD); clear buffer
    ld a, 10000011b ;enable interrupt
    out (INT_PIO_ADDRC), a
    ;call keyboard_init
    ;call keyb_cmd_enable
    ;call keyb_enable_int
    
    ret

    .include "kdrv_vt82c42.s"
    .include "kdrv_int.s"
_eof:


