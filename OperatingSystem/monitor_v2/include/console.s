;DIP SWICTHES
;1843200 CLK / x16 SIO CLOCK MODE = 115200
;MAX BAUD RATE = 115200
;DIP VALUE = 115200/<BAUD>
;
;9600 -> 12 / 00110000
;


CONSOLE_INIT:
    call con_rb_init; initialize ring buffer
    call consio_init_a
    ret


; A contains char
; Destroys A
print_char:
    push af
    call consio_tx_a
    pop af
    ret
; HL contains pointer to string
; Destroy A, HL

print_str:
    ld a, (hl)
    or a
    ret z
    call print_char
    inc hl
    jr print_str


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

read_char_raw:
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

read_char:
    jp consio_rx_a
    
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

print_32_hex:
    ld a,(ix+3)
    call print_a_hex
    ld a,(ix+2)
    call print_a_hex
    ld a,(ix+1)
    call print_a_hex
    ld a,(ix+0)
    call print_a_hex
    ret

print_16_hex:
    ld a,(ix+1)
    call print_a_hex
    ld a,(ix+0)
    call print_a_hex
    ret

;input ringbuffer
;initialize ringbuffer
con_rb_init:
    xor a
    ld (var_buffer_conin_in),a  
    ld (var_buffer_conin_out),a  
    ld (var_buffer_conin_sts),a
    ret

con_rb_read:
    push hl
    push de
    ld a,(var_buffer_conin_in)
    ld b,a
    ld a,(var_buffer_conin_out)
    cp b    ;check if equal
    jp z, con_rb_read_empty
    ;if not equal, buffer contians data
    ld h, high [var_buffer_conin_data]  ;load high byte for pointer
    ld l, a
    ld a,(hl)
    push af
    ;move pointer
    ld a,(var_buffer_conin_out)
    inc a
    ld (var_buffer_conin_out),a
    pop af
    pop de
    pop hl
    ret
con_rb_read_empty:
    ld a,1
    ld (var_buffer_conin_sts),a
    xor a
    pop de
    pop hl
    ret

con_rb_write:
    push hl
    push af
    ld h, high [var_buffer_conin_data]  ;load high byte for pointer
    ld a,(var_buffer_conin_in)
    ld l,a
    pop af
    ld (hl),a
    ld a,(var_buffer_conin_in)
    inc a
    ld (var_buffer_conin_in),a 
    ;TODO: check for overflow
    pop hl
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

;------------------------------------------------------------------------------
; PRINTINLINE
;
; String output function
;
; Prints in-line data (bytes immediately following the PRINTINLINE call)
; until a string terminator is encountered (0 - null char).
;------------------------------------------------------------------------------
PRINTINLINE:
		EX 		(SP),HL 			; PUSH HL and put RET ADDress into HL
		PUSH 	AF
		PUSH 	BC
nxtILC:
		LD 		A,(HL)
		CP		0
		JR		Z,endPrint
		CALL 	print_char
		INC 	HL
		JR		nxtILC
endPrint:
		INC 	HL 					; Get past "null" terminator
		POP 	BC
		POP 	AF
		EX 		(SP),HL 			; PUSH new RET ADDress on stack and restore HL
        RET

print_reg:
    push af
    push de
    push bc
    push hl
    push af
    call PRINTINLINE
    db 10,13,"A:  ",0
    pop af
    call print_a_hex
    call PRINTINLINE
    db 10,13,"BC: ",0
    ld a,b
    call print_a_hex
    ld a,c
    call print_a_hex
    call PRINTINLINE
    db 10,13,"DE: ",0
    ld a,d
    call print_a_hex
    ld a,e
    call print_a_hex
    pop af
    call PRINTINLINE
    db 10,13,"HL: ",0
    ld a,h
    call print_a_hex
    ld a,l
    call print_a_hex
    call print_newLine
    pop hl
    pop bc
    pop de
    pop af
    ret