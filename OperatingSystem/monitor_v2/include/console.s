;DIP SWICTHES
;1843200 CLK / x16 SIO CLOCK MODE = 115200
;MAX BAUD RATE = 115200
;DIP VALUE = 115200/<BAUD>
;
;9600 -> 12 / 00110000
;

CONSOLE_INIT:
CONSOLE_INIT_CTC:
    ;LD A,00001111b      ; Set /16 Divider, CPU Trigger, Time COnstant Follows
    LD A,01001111b      ; External Trigger, Time COnstant Follows
    OUT (CS_CTC_0),A    
    IN A,(CS_DIP)       ; Read BAUD from DIP-Switches
    ;LD A,39
    OUT (CS_CTC_0),A
CONSOLE_INIT_SIO:
    LD A,00110000b      ;write into WR0: error reset, select WR0
    OUT (CS_SIO_A_C),A
    LD a,018h           ;write into WR0: channel reset
    OUT (CS_SIO_A_C),A
    LD a,004h           ;write into WR0: select WR4
    OUT (CS_SIO_A_C),A
    LD a,01000100b       ;write into WR4: clkx16,1 stop bit, no parity
    OUT (CS_SIO_A_C),A
    LD a,005h ;write into WR0: select WR5
    OUT (CS_SIO_A_C),A
    LD a,11101000b ;DTR inactive, TX 8bit, BREAK off, TX on, RTS inactive
    OUT (CS_SIO_A_C),A
    LD a,01h ;write into WR0: select WR1
    OUT (CS_SIO_A_C),A
    LD a,00000100b ;no interrupt in CH B, special RX condition affects vect
    OUT (CS_SIO_A_C),A
    LD a,02h ;write into WR0: select WR2
    OUT (CS_SIO_A_C),A
    LD a,0h ;write into WR2: cmd line int vect (see int vec table)
            ;bits D3,D2,D1 are changed according to RX condition
    OUT (CS_SIO_A_C),A
    LD a,003h ;write into WR0: select WR3
    OUT (CS_SIO_A_C),A
    LD a,0C1h ;RX 8bit, auto enable off, RX on
    OUT (CS_SIO_A_C),A
    ;Channel A RX active
    RET

; A contains char
; Destroys A
print_char:
    push af
    out (CS_SIO_A_D),a
    call print_wait_out
    pop af
    ;call print_char
    ret
; HL contains pointer to string
; Destroy A, HL

print_str:
    ld a, (hl)
    or a
    jr z,print_str_end
    call print_char
    inc hl
    jr print_str
print_str_end:
    ret

print_clear:
    ld hl, [MSG_CLEAR]
    call print_str
    ret

print_newLine:
    ld a,10
    call print_char
    ld a,13
    call print_char
    ret
; destroys a
print_wait_out:
    ; check for TX buffer empty
    sub a ;clear a, write into WR0: select RR0
    inc a ;select RR1
    out (CS_SIO_A_C),A
    in A,(CS_SIO_A_C) ;read RRx
    bit 0,A
    jr z,print_wait_out
    ret

print_a_hex:
    push af
    push bc
    push de
    call STRCONV_BYTES_TO_HEX
    ld a,b
    call print_char
    ld a,c
    call print_char
    pop de
    pop bc
    pop af
    ret

print_bcd:
	ADD 48	;offset for ascii number
	call print_char
	ret

read_char:
    call A_RTS_ON
    nop
    xor a               ; a = 0
    out (CS_SIO_A_C), a ; select reg 0
    in a, (CS_SIO_A_C)  ; read reg 0
    and	1               ; mask D0 (recieve char available)
    call A_RTS_OFF
    ret	Z               ; return 0 if no char
    in a, (CS_SIO_A_D)  ; read char if avail
    ret                 ; return

read_bcd;
	call read_char
	jp z, read_bcd
	call print_char
    sbc 48  ;remove ascii offset
    jp c, _read_bcd_invalid  ;if carry, wrong input
    cp 10 
    jp z, _read_bcd_invalid ;if equal or greater than 10, also error
	and 0x0F	;mask unused bits
    ret
_read_bcd_invalid
	ld a, 0xFF
	ret


;MSG_CRSR_0:
;    db 0x1B, "[?25h",0
;MSG_CRSR_1:
;    db 0x1B, "[?25l",0
MSG_CLEAR:
    db 27, '[2J', 27, '[H',0



; Serial Util Functions
A_RTS_OFF:
    ld a,005h ;write into WR0: select WR5
    out (CS_SIO_A_C),A
    ld a,068h ;DTR inactive, TX 8bit, BREAK off, TX on, RTS inactive
    out (CS_SIO_A_C),A
    ret
A_RTS_ON:
    ld a,005h ;write into WR0: select WR5
    out (CS_SIO_A_C),A
    ld a,0EAh ;DTR active, TX 8bit, BREAK off, TX on, RTS active
    out (CS_SIO_A_C),A
    ret