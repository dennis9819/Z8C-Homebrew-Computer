; HL Contains Address to string
; E is 0xFF if error
; E is 0x00 if okay
; A returns byte
; A,DE are destroyed
DHEX_TO_BYTE:
    PUSH HL             ;Backup pointer
    ; Load First Byte
    LD A,(HL)           ;Load first char (high)
    CALL HEX_TO_BIN     ;Conv chart to 4bit
    jr C, DHEX_TO_BYTE_FAILED   ;If error jmp to DHEX_TO_BYTE_FAILED
    SLA A               ;shift result to upper 4 bits
    SLA A
    SLA A
    SLA A
    LD D,A              ;store result in D

    INC HL              ;next byte
    LD A,(HL)           ;load 2nd char (lower)
    CALL HEX_TO_BIN     ;Conv chart to 4bit
    jr C, DHEX_TO_BYTE_FAILED   ;If error jmp to DHEX_TO_BYTE_FAILED
    OR D                ;merge D with accumulator

    POP HL              ;restor original pointer
    LD E,0x00           ;set error to 0x00 = no error
    RET
DHEX_TO_BYTE_FAILED:
    LD E,0xFF           ;set error to oxFF
    ;LD A,0x00
    POP HL
    RET

; REG A Contains CHAR-Low
; If failed, Carry is set
HEX_TO_BIN:
    SUB 48 ; 0 is 0
    jp PE, HEX_TO_INVALID_2; to low (SUB overflow)
    CP 10 ; Value is between 0 and 9
    JR C, HEX_TO_BIN_2
    SUB 7
    jp PE, HEX_TO_INVALID_2; to low (SUB overflow)
HEX_TO_BIN_2:
    CP 16
    JR NC, HEX_TO_INVALID_2; if bigger than 15-> fail
    AND 0x0F
    RET
HEX_TO_INVALID_2:
    SCF ;set carry flag
    RET


;*****************
; Convert Byte to Hex Char (ASCII)
;*****************
;Description: the StrLength function inline
;
;Inputs: A contains input byte
;
;Outputs: BC contains 2 Bytes of ASCII (HEX)
;
;Destroys: A, BC,
STRCONV_BYTES_TO_HEX:
    PUSH AF;Backup A Register
    RRA
    RRA
    RRA
    RRA
    CALL STRCONV_BYTES_TO_HEX_1
    ;LD A,D
    LD B,A  
    POP AF ; Reload first Byte
    CALL STRCONV_BYTES_TO_HEX_1
    ;LD A,D
    LD C,A
    RET

STRCONV_BYTES_TO_HEX_1:
    AND 0x0F
    ADD 48
    CP 58 ; Check if less than 58 (less than ASCII 9)
    ;JR C, STRCONV_BYTES_TO_HEX_2
    RET C
    ADD 7 ; A-F
    RET
STRCONV_BYTES_TO_HEX_2:
    ;LD D,A
    RET


