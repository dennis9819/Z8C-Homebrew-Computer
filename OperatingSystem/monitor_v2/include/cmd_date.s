ADDR_RTC .equ 0xD0

OP_RTIME:
    call print_newLine
_OP_RTIME_NN:
	;set pointer
	ld hl,[_OP_RTIME_RD_CMD]
	ld b, 1
	ld c, ADDR_RTC
	call iic_send_buffer
    or a
    jp nz, _OP_IIC_ACK_ERR
	;read RTC data
	ld hl,[var_scratch]
	ld b,8
	ld c, ADDR_RTC
	call iic_receive_buffer
    or a
    jp nz, _OP_IIC_ACK_ERR
	;process display hours
	ld a,(var_scratch+2)
	and 00110000b
	RRA
	RRA
	RRA
	RRA
	call print_bcd
	ld a,(var_scratch+2)
	and 00001111b
	call print_bcd
	ld a,':'
	call print_char
	;display minutes
	ld a,(var_scratch+1)
	and 01110000b
	RRA
	RRA
	RRA
	RRA
	call print_bcd
	ld a,(var_scratch+1)
	and 00001111b
	call print_bcd
	ld a,':'
	call print_char
	;display seconds
	ld a,(var_scratch+0)
	and 01110000b
	RRA
	RRA
	RRA
	RRA
	call print_bcd
	ld a,(var_scratch+0)
	and 00001111b
	call print_bcd
	ld a,' '
	call print_char
	;display date
	ld a,(var_scratch+4)
	and 00110000b
	RRA
	RRA
	RRA
	RRA
	call print_bcd
	ld a,(var_scratch+4)
	and 00001111b
	call print_bcd
	ld a,'.'
	call print_char
	ld a,(var_scratch+5)
	and 00010000b
	RRA
	RRA
	RRA
	RRA
	call print_bcd
	ld a,(var_scratch+5)
	and 00001111b
	call print_bcd
	ld a,'.'
	call print_char
	ld a,'2'
	call print_char
	ld a,'0'
	call print_char
	ld a,(var_scratch+6)
	and 11110000b
	RRA
	RRA
	RRA
	RRA
	call print_bcd
	ld a,(var_scratch+6)
	and 00001111b
	call print_bcd

	RET
_OP_RTIME_RD_CMD:
	db 0x00

_OP_STIME_STR_DAY:  db 10,13,"Enter Date    (00-31) : ",0
_OP_STIME_STR_MON:  db 10,13,"Enter Month   (00-12) : ",0
_OP_STIME_STR_YEAR: db 10,13,"Enter Year    (00-99) : ",0
_OP_STIME_STR_HOUR: db 10,13,"Enter Hours   (00-23) : ",0
_OP_STIME_STR_MIN:  db 10,13,"Enter Minutes (00-59) : ",0
_OP_STIME_STR_SEC:  db 10,13,"Enter Seconds (00-59) : ",0
_OP_STIME_INVALID:  db " invaild input. Retry!",0
;HL contains pointer to stack
;BC returns value (BCD)
_OP_STIME_PROMPT:
    push hl
    call print_str
    pop hl
    call read_bcd
	cp 0xFF	;if failed
	jp Z, _OP_STIME_PROMPT_ERR
	ld b,a
    call read_bcd
	cp 0xFF	;if failed
	jp Z, _OP_STIME_PROMPT_ERR
	ld c,a
	ret
_OP_STIME_PROMPT_ERR:
    push HL
    ld hl, [_OP_STIME_INVALID]
    call print_str
    pop hl
    jp _OP_STIME_PROMPT



OP_STIME:
    xor a
	ld (var_scratch),a	;set pointer
	;date
	ld hl, [_OP_STIME_STR_DAY]
	call _OP_STIME_PROMPT
	ld a,b
	call _shift4
	and 00110000b	;mask bits; bit6 low -> 24h mode
	or c			;add second digit
	ld (var_scratch+5),a
	;month
	ld hl, [_OP_STIME_STR_MON]
	call _OP_STIME_PROMPT
	ld a,b
	call _shift4
	and 00010000b	;mask bits; bit6 low -> 24h mode
	or c			;add second digit
	ld (var_scratch+6),a
	;year
	ld hl, [_OP_STIME_STR_YEAR]
	call _OP_STIME_PROMPT
	ld a,b
	call _shift4
	and 11110000b	;mask bits; bit6 low -> 24h mode
	or c			;add second digit
	ld (var_scratch+7),a
	;hours
	ld hl, [_OP_STIME_STR_HOUR]
	call _OP_STIME_PROMPT
	ld a,b
	call _shift4
	and 00110000b	;mask bits; bit6 low -> 24h mode
	or c			;add second digit
	ld (var_scratch+3),a
	; minutes
	ld hl, [_OP_STIME_STR_MIN]
	call _OP_STIME_PROMPT
	ld a,b
	call _shift4
	and 01110000b	;mask bits
	or c			;add second digit
	ld (var_scratch+2),a
	; seconds
	ld hl, [_OP_STIME_STR_SEC]
	call _OP_STIME_PROMPT
	ld a,b
	call _shift4
	and 01110000b	;mask bits / bit6 low -> 24h mode (enable clock)
	or c			;add second digit
	ld (var_scratch+1),a
	inc de	;next

	ld c, ADDR_RTC
	ld b, 8
	ld hl,[var_scratch]
	call iic_send_buffer
    or a
    jp nz, _OP_IIC_ACK_ERR

	ret

_shift4:
	RLCA
	RLCA
	RLCA
	RLCA
	ret