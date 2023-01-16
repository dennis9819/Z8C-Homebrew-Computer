;------------------------------------------------------------------------------
; beep
;
; Beeps the speaker
; DE sets duration
;------------------------------------------------------------------------------
beep:
    push    AF
    push    BC
    push    DE
beep_loop:
	LD      A,0x08
    OUT     (CS_PIO_AD), A
    LD      BC, 0x28
    CALL    beep_pause
    LD      A,0x00
    OUT     (CS_PIO_AD), A
    LD      BC, 0x24
    CALL    beep_pause
    DEC     DE
    ld      A,D
    or      E
    jr      NZ, beep_loop
    pop     de
    pop     bc
    pop     af
	ret

beep_pause:
    NEG									; 8 T-states
	NEG									; 8 T-states
	NEG									; 8 T-states
	NEG									; 8 T-states
	DEC		BC							; 6 T-states
	LD		A,C							; 9 T-states
	OR		B							; 4 T-states
	JP		NZ,beep_pause					; 10 T-states
	RET									; Pause complete, RETurn