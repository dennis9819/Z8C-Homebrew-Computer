;----------------------------------------------------------------
;BIOS Driver for Serial Console
;by Dennis Gunia (07/2024)
;
;----------------------------------------------------------------

;================================================================
; I/O access functions 
;================================================================
;initialize SIO Port A

consio_init_a:
    call consio_init_ctc_a
    call consio_init_a_sio
    ;call consio_init_a_int
    ret

consio_init_a_sio:
    ld A, 00110000b      ;write into WR0: error reset, select WR0
    out (CS_SIO_A_C), a
    ld a, 018h           ;write into WR0: channel reset
    out (CS_SIO_A_C), a
    ld a, 004h           ;write into WR0: select WR4
    out (CS_SIO_A_C), a
    ld a, 01000100b      ;write into WR4: clkx16,1 stop bit, no parity
    out (CS_SIO_A_C), a
    ld a, 005h           ;write into WR0: select WR5
    out (CS_SIO_A_C), a
    ld a, 11101000b      ;DTR inactive, TX 8bit, BREAK off, TX on, RTS inactive
    out (CS_SIO_A_C), a
    ld a, 01h            ;write into WR0: select WR1
    out (CS_SIO_A_C), a
    ld a, 00000100b      ;no interrupt in CH B, special RX condition affects vect
    out (CS_SIO_A_C), a
    ld a, 02h            ;write into WR0: select WR2
    out (CS_SIO_A_C), a
    ld a, 0h             ;write into WR2: cmd line int vect (see int vec table)
                         ;bits D3,D2,D1 are changed according to RX condition
    out (CS_SIO_A_C), a
    ld a, 003h           ;write into WR0: select WR3
    out (CS_SIO_A_C), a
    ld a, 0C1h           ;RX 8bit, auto enable off, RX on
    out (CS_SIO_A_C), a
    ret

consio_init_a_int:
    ;setup SIO interrupt vector
    ld a, 2 ;write WR2
    out (CS_SIO_B_C), a
    ld a, 00000100b ;write Vector
    out (CS_SIO_B_C), a
    ;setup SIO 
    ld a, 1 ;write WR1
    out (CS_SIO_A_C), a
    ld a, 00011000b ;enable INT on all RX
    out (CS_SIO_A_C), a
    ;set ISR addr
    ld hl, [consio_isr]
    ld (INT_VEC_TABLE + 4), hl
    ret

consio_init_ctc_a:
    ld a, 01001111b; External Trigger, Time COnstant Follows
    out (CS_CTC_0),a
    in a, (CS_DIP) ; Read BAUD from DIP-Switches
    out (CS_CTC_0),a
    ret


consio_tx_a:
    out (CS_SIO_A_D),a
consio_tx_a_waitout:
    ; check for TX buffer empty
    sub a ;clear a, write into WR0: select RR0
    inc a ;select RR1
    out (CS_SIO_A_C),A
    in A,(CS_SIO_A_C) ;read RRx
    bit 0,A
    jr z,consio_tx_a_waitout
    ret

consio_rx_a:
    call consio_rx_rts_a_on
    xor a               ; a = 0
    out (CS_SIO_A_C), a ; select reg 0
    in a, (CS_SIO_A_C)  ; read reg 0
    and	1               ; mask D0 (recieve char available)
    call A_RTS_OFF
    ret	Z               ; return 0 if no char
    in a, (CS_SIO_A_D)  ; read char if avail
    ret                 ; return

consio_rx_rts_a_on:
    ld a,005h ;write into WR0: select WR5
    out (CS_SIO_A_C),A
    ld a,0EAh ;DTR active, TX 8bit, BREAK off, TX on, RTS active
    out (CS_SIO_A_C),A
    ret

consio_rx_rts_a_off:
    ld a,005h ;write into WR0: select WR5
    out (CS_SIO_A_C),A
    ld a,068h ;DTR inactive, TX 8bit, BREAK off, TX on, RTS inactive
    out (CS_SIO_A_C),A
    ret

consio_rx_a_sts:
    out (CS_SIO_A_C), a ; select reg 0
    in a, (CS_SIO_A_C)  ; read reg 0
    and	1               ; mask D0 (recieve char available)
    ret z
    ld a, 0xFF
    ret


;Interrupt service routine
consio_isr:
    di
    ld a, 00111000b ;RET fro INT
    out (CS_SIO_B_C), a ;read data
    in a, (CS_SIO_A_D)
    call con_rb_write   ;write to ringbuffer
    ei
    reti