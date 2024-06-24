regdump:
    
    PUSH BC
    PUSH DE
    PUSH HL
    PUSH AF

    CALL	PRINTINLINE
	defb	"REGDUMP",10,13,"A: 0x",0
    call print_a_hex    
    CALL	PRINTINLINE
	defb	" F: 0x",0
    POP BC
    PUSH BC
    LD A,C
    call print_a_hex    

    CALL	PRINTINLINE
	defb	13,10,"BC: 0x",0
    

    
    ld a,b
    call print_a_hex
    ld a,c
    call print_a_hex
    CALL	PRINTINLINE
	defb	13,10,0

    POP AF
    POP HL
    POP DE
    POP BC
    