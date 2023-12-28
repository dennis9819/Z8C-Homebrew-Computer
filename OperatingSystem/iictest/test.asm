.include "include/extern_symbols.s" ;include monitor symbols.
    org 0x8000  

IIC_RTC equ 11010000b
;Testing code

    CALL iic_init

    ;JP PROMPT_BEGIN

    LD		BC,$1000
	CALL	_pause_loop

    JP PROMPT_BEGIN

    LD      DE, 0xC000   ; Set I2C Buffer Location
    LD      A,0x00       ; store string in buffer
    LD      (DE),A
    LD      B, IIC_RTC   ; Set I2C Address
    LD      A, 1         ; Set I2C Buffer length
    call    iic_send




    JP PROMPT_BEGIN

.include "include/kdrv_iic.s"


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