;------------------------------------------------------------------------------
; beep
;
; Beeps the speaker
; DE sets duration
;------------------------------------------------------------------------------
beep:
    push    AF
    push    DE
    di
beep_loop:
	LD      A,0x08
    OUT     (CS_PIO_AD), A
    CALL    beep_pause
    LD      A,0x00
    OUT     (CS_PIO_AD), A
    CALL    beep_pause
    DEC     DE
    ld      A,D
    or      E
    jr      NZ, beep_loop
    pop     de
    pop     af
    ei
	ret

beep_pause:
    PUSH    BC
_beep_pause_l1:
    ;NEG									; 8 T-states
	;NEG									; 8 T-states
	NEG									; 8 T-states
	NEG									; 8 T-states
	DEC		BC							; 6 T-states
	LD		A,C							; 9 T-states
	OR		B							; 4 T-states
	JP		NZ,_beep_pause_l1					; 10 T-states
    POP     BC
	RET									; Pause complete, RETurn