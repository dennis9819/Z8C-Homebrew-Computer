
INT_VEC_TABLE .equ [interrupt_vectors]
INT_PIO_ADDRD .equ CS_PIO_AD
INT_PIO_ADDRC .equ CS_PIO_AC

;initialize interrupt controller
intctrl_init:
    ;disable interrupt (just to make sure)
    di
    ;setup interrupt table
    ld a,high [INT_VEC_TABLE]
    ld i,a

    ;setup PIO
    ld a, 10000011b ;enable interrupt
    out (INT_PIO_ADDRC), a
    ld a, 00000010b ;set interrupt vector
    out (INT_PIO_ADDRC), a
    ld a, 11001111b ;setup as controll port
    out (INT_PIO_ADDRC), a
    ld a, 11110111b      ;set all as inputs
    out (INT_PIO_ADDRC), a
    ld a, 10010111b ;enable interrupt; logic or, mask follows
    out (INT_PIO_ADDRC), a
    ld a, 11000000b ;set interrupt mask
    out (INT_PIO_ADDRC), a

    ; set port
    xor a
    out (INT_PIO_ADDRD), a 

    ;fill table
    ld hl,[INT_VEC_TABLE]
    ld b,128
_intctrl_init_fill_loop:
    ld a, low [_int_invalid_int]
    ld (hl),a
    inc hl
    ld a, high [_int_invalid_int]
    ld (hl),a
    inc hl
    djnz _intctrl_init_fill_loop

    ;set int vector for PIO
    ld hl, [_isr_pio]
    ld (INT_VEC_TABLE + 2), hl

    ;enable interrupts
    im 2
    ret
    
;------------------------------------------------------------------------------
; setup interrupt for PIO pin
; inputs:   a (interrupt pin)
;           hl (jump addr)
;------------------------------------------------------------------------------
initctrl_int_register:

;------------------------------------------------------------------------------
; removes interrupt for PIO pin
; inputs:   a (interrupt pin)
;------------------------------------------------------------------------------
initctrl_int_abandon:

; jumps to isr. MUST be exited with RETI opcode!
_isr_pio:
    di
    ld hl, [_str_pio_interrupt]
    call print_str
    ;get int from pio
    in a,(INT_PIO_ADDRD)
    call print_a_hex

_int_invalid_int:
    di
    ld hl, [_str_invalid_interrupt]
    call print_str
    ei
    reti

_str_invalid_interrupt:
    db 10,13,"[KERNEL] INT: Invalid interrupt call! Exiting ISR.",10,13,0

_str_pio_interrupt:
    db 10,13,"[KERNEL] INT: (DEBUG) Interrupt call from PIO.",10,13,0