    .include "extern_symbols.s" ;include monitor symbols.
    org 0xB000  
CS_PIO_BD .EQU 0xF5
CS_PIO_BC .EQU 0xF7
CS_PIO_AD .EQU 0xF4
CS_PIO_AC .EQU 0xF6

CS_I2C_S1 .EQU 0xF3
CS_I2C_SX .EQU 0xF2

IIC_RTC equ 11010000b

IIC_INIT:
    LD A,0xCF
    OUT (CS_PIO_AC), A
    LD A,11110101b
    OUT (CS_PIO_AC), A

    LD A,00000000b      ; Reset PCF8584 minimum 30 clock cycles
    OUT (CS_PIO_AD), A
    LD BC,0x1000
    CALL PAUSE_LOOP
    LD A,0000010b
    OUT (CS_PIO_AD), A

    NOP
    NOP
    NOP

    LD A, 0x80 ;S1 -> Select S0, PIN disabled, ESO = 0, Interrupt disabled, STA, STA, ACK = 0
    OUT (CS_I2C_S1),A 
    CALL SlowAccess
    CALL SlowAccess

    LD A,0x55 ;S0 -> Loads byte 55H into register S0'; effective own address becomes AAH
    OUT (CS_I2C_SX),A
    CALL SlowAccess

    LD A, 0xA0 ;S1 -> Loads byte A0H into register S1, i.e. next byte will be loaded into the clock control register S2.
    OUT (CS_I2C_S1),A
    CALL SlowAccess


    LD A,0x00 ;Load 18H into S2 register (clock control - 8 MHz, 90 KHz)
    OUT (CS_I2C_SX),A
    CALL SlowAccess

    LD A,0xC1    ;S1 -> loads byte C1H into register S1; register enable
                ;serial interface, set I 2C-bus into idle mode;
                ;SDA and SCL are HIGH. The next write or read
                ;operation will be to/from data transfer register
                ;S0 if A0 = LOW.;
    OUT (CS_I2C_S1),A
    CALL SlowAccess



    ;CALL force_stop
    JP PROMPT_BEGIN

    LD		BC,$0100
	CALL	PAUSE_LOOP
  

    ; Send test message to RTC
    
    LD      DE, 0xC000   ; Set I2C Buffer Location
    LD      A,0x00
    LD      (DE),A
    ;call regdump

    LD      B, IIC_RTC   ; Set I2C Address
    LD      A, 1         ; Set I2C Buffer length
    call    i2c_send
    
    LD      DE, 0xC010
    LD      B, IIC_RTC
    LD      A, 7
    call    i2c_read



    JP PROMPT_BEGIN


;CLK_ENABLE:
;    LD      DE, 0xC000   ; Set I2C Buffer Location
;    LD      A,0x00
;    LD      (0xC000),A
;    LD      (0xC001),A
;    ;call regdump
;
;    LD      B, IIC_RTC   ; Set I2C Address
;    LD      A, 2         ; Set I2C Buffer length
;    call    i2c_send
;    JP PROMPT_BEGIN

;------------------------------------------------------------------------------
; i2c_send
;
; Sends data over the i2c bus
; A contains BYTE COUNTER
; B contains ADDRESS
; DE contains location of Data Buffer
;------------------------------------------------------------------------------
i2c_send:
;    CALL	PRINTINLINE;
;	defb	"SEND A",10,13,0
    PUSH    BC
    PUSH    AF
    CALL    i2c_bus_rdy
;    CALL	PRINTINLINE
;	defb	"SEND START",10,13,0
    LD      A,B     ;Load 'slave address' into S0 register:
    OUT		(CS_I2C_SX),A
    CALL    SlowAccess

    LD      A, 0xC5 ;Load C5H into S1. 'C5H' = PCF8584 generates
                    ;the 'START' condition and clocks out the slave
                    ;address and the clock pulse for slave acknowledgement.
    OUT     (CS_I2C_S1),A
    POP     AF
    LD      C,A
    INC C
i2c_send_1:         ; LOOP 1 : Wait for bus ready
    IN		A,(CS_I2C_S1)	; Read byte from S1 register
	BIT		7,A			    ; Is bus free? (S1 ~BB=1?)
	JR		NZ,i2c_send_1	; No - loop
    BIT     4,A             ; slave acknowledged? (LRB = 0?)
    JR      NZ, i2c_send_stop   ; if not, cancel transmission
    LD      A,(DE)               ; Load next byte from buffer
    INC     DE
    DEC     C
    JR      Z, i2c_send_stop    ; if counter = 0, exit loop
    OUT		(CS_I2C_SX),A       ; Send byte
    JR      i2c_send_1          ; if counter > 0, loop again
i2c_send_stop:
    LD      A, 0xC3         ;STOP
    OUT     (CS_I2C_S1),A
    CALL    SlowAccess
    POP     BC
    RET


;------------------------------------------------------------------------------
; i2c_read
;
; Sends data over the i2c bus
; A contains BYTE COUNTER
; B contains ADDRESS
; DE contains location of Data Buffer
;------------------------------------------------------------------------------
i2c_read:
    PUSH    DE
    PUSH    BC
    PUSH    AF
    LD      A,B     ;Load 'slave address' into S0 register:
    OR      0x01    ;Set RW Bit for read operation
    OUT		(CS_I2C_SX),A
    CALL    SlowAccess
    CALL    i2c_bus_rdy ; Is bus ready
    LD      A, 0xC5 ;Load C5H into S1. 'C5H' = PCF8584 generates
                    ;the 'START' condition and clocks out the slave
                    ;address and the clock pulse for slave acknowledgement.
    OUT     (CS_I2C_S1),A
     ;Setup counter
    POP     AF
    LD      C,A ; Load BYTE COUNTER into C
    INC     C   ; Offset C by 1
i2c_read_1: ;Wait for PIN = 0
    IN		A,(CS_I2C_S1)	; Read byte from S1 register
	BIT		7,A			    ; S1 PIN=1?
	JR		NZ,i2c_read_1	; No - loop
    BIT		3,A			    ; S1 LRB=0? slave ACK?
    JR      NZ, i2c_read_error  ; No ACK -> an error has occured
    DEC     C
    LD      A, C
    DEC     A       ;If n = m − 1?
    JR      Z, i2c_read_last
    IN      A,(CS_I2C_SX)
    LD      (DE),A
    INC     DE
    JR      i2c_read_1
i2c_read_last:  ;read last byte
    LD      A, 0x40
    OUT		(CS_I2C_S1),A
    CALL    SlowAccess
    IN      A,(CS_I2C_SX)   ;receives the final data byte. Neg. ACK is also sent.
    LD      (DE),A
    INC     DE
i2c_read_last_1:
    IN		A,(CS_I2C_S1)	; Read byte from S1 register
	BIT		7,A			    ; S1 PIN=1?
    JR		NZ,i2c_read_last_1	; No - loop

i2c_read_error:
    NOP
i2c_read_stop:
    LD      A, 0xC3
    OUT		(CS_I2C_S1),A
    CALL    SlowAccess
    IN      A,(CS_I2C_SX)   ;transfers the final data byte from the 
                            ;data buffer to accumulator.
    CALL    SlowAccess
    LD      (DE),A
    POP     BC
    POP DE
    RET


i2c_stop_force:

;------------------------------------------------------------------------------
; i2c_rdy
;
; Waits until the PCF8584 signals a byte transmission/reception is complete.
;------------------------------------------------------------------------------
i2c_rdy:
	PUSH	AF
i2c_rlp:
	IN		A,(CS_I2C_S1)	; Read byte from S1 register
	BIT		7,A			; Is Tx/Rx complete? (S1 PIN=0?)
    call    print_a_hex
	JR		NZ,i2c_rlp	; 	No - loop
i2crlpex:
	POP		AF
	RET

;------------------------------------------------------------------------------
; i2c_bus_rdy
;
; Waits until the I2C bus is free before RETurning
;------------------------------------------------------------------------------
i2c_bus_rdy:
	PUSH	AF
i2c_blp:
	IN		A,(CS_I2C_S1)	; Read byte from S1 register
    PUSH    AF
    call    print_a_hex
    POP AF
	BIT		0,A			; Is bus free? (S1 ~BB=1?)
	JR		Z,i2c_blp	; 	No - loop
i2cblpex:
	POP		AF
	RET

;------------------------------------------------------------------------------				 
; PAUSE_LOOP
;
; Timer function
;
; 16-bit (BC) decrement counter, performing 4xNEG loop until BC
; reaches zero.
;
; 61 T-states in loop = 15.25uS per loop @ 4 MHz - near enough
; a second delay for 65,535 iterations.
;
; Set iteration count in BC before calling this function.
; Destroys: BC
;------------------------------------------------------------------------------
PAUSE_LOOP:
	PUSH	AF							; 11 T-states
pau_lp:
	;NEG									; 8 T-states
	;NEG									; 8 T-states
	;NEG									; 8 T-states
	;NEG									; 8 T-states
    PUSH    BC                          ; 11 T-states
    POP     BC                          ; 10 T-states
    PUSH    BC                          ; 11 T-states
    POP     BC                          ; 10 T-states
    DEC		BC	                        ; 6 T-states
	LD		A,C							; 9 T-states
	OR		B							; 4 T-states
	JP		NZ,pau_lp					; 10 T-states
	POP		AF							; 10 T-states
	RET									; Pause complete, RETurn

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

SlowAccess:
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    RET


    ;.include "regdump.s"

force_stop:
    IN A,(CS_I2C_S1)
    BIT 0, A
    RET NZ
    LD A, 11000011b
    OUT (CS_I2C_S1),A
    NOP
    NOP
    JR force_stop