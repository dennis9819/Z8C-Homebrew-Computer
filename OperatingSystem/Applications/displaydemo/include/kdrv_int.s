
INT_VEC_TABLE .equ [interrupt_vectors]
INT_PIO_ADDRD .equ CS_PIO_AD
INT_PIO_ADDRC .equ CS_PIO_AC


; jumps to isr. MUST be exited with RETI opcode!

_isr_pio_test:
    di
    ld hl, [_str_pio_interrupt]
    call print_str
    ;get int from pio
    in a,(INT_PIO_ADDRD)
    call print_a_hex
    reti