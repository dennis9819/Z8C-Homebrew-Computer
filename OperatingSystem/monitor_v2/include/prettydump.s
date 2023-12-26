;----------------------------------------------------------------
;HEX and ASCII dump function
;by Dennis Gunia (01/2023)
;----------------------------------------------------------------

;------------------------------------------------------------------------------
; dump_pretty
;
; Dumps memory content
; B contains amount of rows
; HL contains start address
; Destroys BC, HL
;------------------------------------------------------------------------------
dump_pretty:
    PUSH HL
    LD HL,[STR_PD_HEADER]  ;Print header
    CALL print_str
    POP HL
dump_pretty_row:
    LD A,B                  ;Check row counter
    OR A
    JP Z, dump_pretty_end   ;If counter is 0, exit
    DEC B                   ;Decrement row counter by 1
    LD C, 16                ;Load column counter
    LD A, H                 ;Print base address
    CALL print_a_hex
    LD A, L
    CALL print_a_hex
    LD A, ' '
    CALL print_char
dump_pretty_col:            ;Loop for column
    LD A,(HL)               ;Load byte to disply
    CALL print_a_hex
    LD A, ' '
    CALL print_char
    INC HL
    DEC C                   ;Decrement column counter
    JR NZ, dump_pretty_col  ;Loop if not 0
dump_pretty_ascii:
    PUSH BC
    PUSH HL
    LD B,0
    LD C,16
    SBC HL,BC   ;Reset HL by column count
dump_pretty_ascii_loop:
    LD A,(HL)
    INC HL
    CP 32
    JP C, dump_pretty_ascii_none    ;if less than 32, it is not a char
    CP 127
    JP NC, dump_pretty_ascii_none   ;if greater or equal than 128, it is not a char
    call print_char
    jr dump_pretty_ascii_cont
dump_pretty_ascii_none:
    LD A,'.'
    call print_char
dump_pretty_ascii_cont:
    DEC C
    JP NZ, dump_pretty_ascii_loop


    POP HL
    POP BC
dump_pretty_nextrow:
    LD A,10                 ;New line
    CALL print_char
    LD A,13
    CALL print_char
    JR dump_pretty_row      ;Else next line
dump_pretty_end:
    RET

STR_PD_HEADER:
    db 13,10,'BASE 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F  ASCII',13,10,0


;------------------------------------------------------------------------------
; print_str_fixed
;
; Prints string with fixed length
; B contains length
; HL contains start address
;------------------------------------------------------------------------------
print_str_fixed:
    LD A,(HL)
    INC HL
    CALL print_char
    DJNZ print_str_fixed
    RET

