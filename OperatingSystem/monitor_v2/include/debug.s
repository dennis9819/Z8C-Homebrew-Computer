

debug_init:
    ld A,VAR_CONSOLE_CONF   ; Setup CTC
    out (IO_CTC0_C0),A      ; Controll word, software reset, time constant follows, CLK/TRG starts timer
    ld A,VAR_CONSOLE_BAUD   ; Setup timer const
    out (IO_CTC0_C0),A      ; Load timer const into CTC (Setup Baud generator)
    ;set up TX and RX:W
    ld a,00110000b      ;write into WR0: error reset, select WR0
    out (IO_SIO0A_C),A
    ld a,018h           ;write into WR0: channel reset
    out (IO_SIO0A_C),A
    ld a,004h           ;write into WR0: select WR4
    out (IO_SIO0A_C),A
    ld a,04h            ;44h write into WR4: clkx1,1 stop bit, no parity
    out (IO_SIO0A_C),A
    ld a,005h           ;write into WR0: select WR5
    out (IO_SIO0A_C),A
    ld a,068h           ;DTR active, TX 8bit, BREAK off, TX on, RTS inactive
    out (IO_SIO0A_C),A
    ld a,01h            ;write into WR0: select WR1
    out (IO_SIO0A_C),A
    ld a,00000100b      ;no interrupt in CH B, special RX condition affects vect
    out (IO_SIO0A_C),A
    ;enable SIO channel A RX
    ld a,003h ;write into WR0: select WR3
    out (IO_SIO0A_C),A
    ld a,0C1h ;RX 8bit, auto enable off, RX on
    out (IO_SIO0A_C),A
    ;Channel A RX active
    ld hl,[MSG_START_DBG]
    call debug_print_str
    ret

debug_print_char:
    out (IO_SIO0A_D),a
    call debug_wait_out
    ret

debug_print_str:
    ld a, (hl)
    or a
    ret z
    call debug_print_char
    inc hl
    jr debug_print_str

debug_print_newLine:
    ld a,10
    call debug_print_char
    ld a,13
    call debug_print_char
    ret

debug_wait_out:
    ; check for TX buffer empty
    sub a ;clear a, write into WR0: select RR0
    inc a ;select RR1
    out (IO_SIO0A_C),A
    in A,(IO_SIO0A_C) ;read RRx
    bit 0,A
    jr z,debug_wait_out
    ret

MSG_START_DBG:
    db "Debug interface active!",13,10,0

debug_a_hex:
    push BC
    push AF
    call STRCONV_BYTES_TO_HEX
    ld a, b
    call debug_print_char
    ld a, c
    call debug_print_char
    pop AF
    pop BC
    ret