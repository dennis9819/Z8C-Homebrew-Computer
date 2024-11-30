CS_PIO_BD .EQU 0xF5
CS_PIO_BC .EQU 0xF7
CS_PIO_AD .EQU 0xF4
CS_PIO_AC .EQU 0xF6
.include "extern_symbols.s" ;include monitor symbols.   
    .org 0x8000

PRINTSPEED .EQU 0x7FFE


fancy_print:
    call print_clear
    ld bc, 65535
    call fancy_print_delay
    ld hl,[fancy_print_demo_text]
    ld bc, 4500
    ld (PRINTSPEED), bc
fancy_print_loop:
    ld a,(hl)
    inc hl
    or a
    ret z
    cp 0xFE
    jr z, fancy_print_loop_wait
    cp 0xFD
    jr z, fancy_print_loop_speed
    jr fancy_print_loop_char
fancy_print_loop_speed:
    ld a,(hl)
    inc hl
    ld c,a
    ld a,(hl)
    inc hl
    ld b,a
    ld (PRINTSPEED), bc
    jr fancy_print_loop

fancy_print_loop_wait:
    ld a,(hl)
    inc hl
    ld c,a
    ld a,(hl)
    inc hl
    ld b,a
    call fancy_print_delay
    jr fancy_print_loop

fancy_print_loop_char:
    call fancy_print_char
    jr fancy_print_loop

fancy_print_char:
    call print_char
    ld DE, 0xA0
    ld bc, 12
    call fancy_print_beep
    ld bc, (PRINTSPEED)
    call fancy_print_delay
    ret

fancy_print_beep:
    push    AF
    push    DE
fancy_print_beep_lp:
    LD      A,0x08
    OUT     (CS_PIO_AD), A
    call fancy_print_beep_pause
    LD      A,0x00
    OUT     (CS_PIO_AD), A
    call fancy_print_beep_pause
    DEC     DE
    ld      A,D
    or      E
    jr      NZ, fancy_print_beep_lp
    pop     de
    pop     af
	ret

fancy_print_beep_pause:
    PUSH BC
_fancy_print_beep_pause:
    NEG									; 8 T-states
	NEG									; 8 T-states
	DEC		BC							; 6 T-states
	LD		A,C							; 9 T-states
	OR		B							; 4 T-states
	JP		NZ,_fancy_print_beep_pause  ; 10 T-states
    POP     BC
	RET									; Pause complete, RETurn

; bc contians length
fancy_print_delay:
_fancy_print_delay_loop:
    push AF
    pop AF
    push AF
    pop AF
    dec bc
    ld a,b
    or c
    jr nz, _fancy_print_delay_loop
    ret

;; 0x00 exits the print
;; 0xFE <uint16: delay> pauses the print
;; 0xFD <uint16: delay> changes the print speed
fancy_print_demo_text:
    db "Hello Dennis ... ",0xFE
    dw 65535
    db "It's been a ", 0xFD
    dw 24500
    db "long long time.", 0xFD
    dw 4500
    db 10,13,0xFE
    dw 65535
    db 0xFE
    dw 65535
    db "I almost thought",0xFE
    dw 65535
    db 0xFD
    dw 50000
    db " ... ", 0xFD
    dw 4500
    db "you forgot me ...",0xFE
    dw 65535
    db 0xFE
    dw 65535
    db 10,13
    db 10,13
    db "I'm glad you are back!"
    db 10,13
    db 10,13
    db 0

