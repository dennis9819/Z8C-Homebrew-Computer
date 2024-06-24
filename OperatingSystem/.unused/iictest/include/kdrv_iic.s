CS_PIO_BD .EQU 0xF5
CS_PIO_BC .EQU 0xF7
CS_PIO_AD .EQU 0xF4
CS_PIO_AC .EQU 0xF6

CS_I2C_S1 .EQU 0xF3
CS_I2C_SX .EQU 0xF2

iic_init:
    LD A,0xCF
    OUT (CS_PIO_AC), A
    LD A,11110101b
    OUT (CS_PIO_AC), A

    LD A,00000000b      ; Reset PCF8584 minimum 30 clock cycles
    OUT (CS_PIO_AD), A

    LD BC,0x1000
    CALL _pause_loop

    LD A,0000010b
    OUT (CS_PIO_AD), A

    LD BC,0x2000
    CALL _pause_loop

    LD A, 0x80 ;S1 -> Select S0, PIN disabled, ESO = 0, Interrupt disabled, STA, STA, ACK = 0
    OUT (CS_I2C_S1),A 
    CALL _slow_access
    ;CALL _slow_access

    LD A,0x55 ;S0 -> Loads byte 55H into register S0'; effective own address becomes AAH
    OUT (CS_I2C_SX),A
    CALL _slow_access


    LD A, 0xA0 ;S1 -> Loads byte A0H into register S1, i.e. next byte will be loaded into the clock control register S2.
    OUT (CS_I2C_S1),A
    CALL _slow_access

    ; 000100000
    LD A,0x18 ;Load 18H into S2 register (clock control - 4.43 MHz, 90 KHz)
    LD A,0x00 ;Load 18H into S2 register (clock control - 4.43 MHz, 90 KHz)
    OUT (CS_I2C_SX),A
    CALL _slow_access
    ;CALL _slow_access
    ;CALL _slow_access
    ;CALL _slow_access

    LD A,0xC1    ;S1 -> loads byte C1H into register S1; register enable
                ;serial interface, set I 2C-bus into idle mode;
                ;SDA and SCL are HIGH. The next write or read
                ;operation will be to/from data transfer register
                ;S0 if A0 = LOW.;
    OUT (CS_I2C_S1),A
    CALL _slow_access
    RET

;------------------------------------------------------------------------------
; iic_send
;
; Sends data over the i2c bus
; A contains BYTE COUNTER
; B contains ADDRESS
; DE contains location of Data Buffer
;------------------------------------------------------------------------------
iic_send:
    ;CALL	PRINTINLINE;
	;defb	"SEND A",10,13,0
    PUSH    BC
    PUSH    AF
    CALL    iic_bus_rdy
    ;CALL	PRINTINLINE
	;defb	"SEND START",10,13,0
    LD      A,B     ;Load 'slave address' into S0 register:
    OUT		(CS_I2C_SX),A
    CALL    _slow_access

    LD      A, 0xC5 ;Load C5H into S1. 'C5H' = PCF8584 generates
                    ;the 'START' condition and clocks out the slave
                    ;address and the clock pulse for slave acknowledgement.
    OUT     (CS_I2C_S1),A
    POP     AF
    LD      C,A
    INC C
_iic_send_1:         ; LOOP 1 : Wait for bus ready
    IN		A,(CS_I2C_S1)	; Read byte from S1 register
	BIT		7,A			    ; Is bus free? (S1 ~BB=1?)
	JR		NZ,_iic_send_1	; No - loop
    BIT     4,A             ; slave acknowledged? (LRB = 0?)
    JR      NZ, _iic_send_stop   ; if not, cancel transmission
    LD      A,(DE)               ; Load next byte from buffer
    INC     DE
    DEC     C
    JR      Z, _iic_send_stop    ; if counter = 0, exit loop
    OUT		(CS_I2C_SX),A       ; Send byte
    JR      _iic_send_1          ; if counter > 0, loop again
_iic_send_stop:
    LD      A, 0xC3         ;STOP
    OUT     (CS_I2C_S1),A
    CALL    _slow_access
    POP     BC
    RET


;------------------------------------------------------------------------------
; iic_read
;
; Sends data over the i2c bus
; A contains BYTE COUNTER
; B contains ADDRESS
; DE contains location of Data Buffer
;------------------------------------------------------------------------------
iic_read:
    PUSH    DE
    PUSH    BC
    PUSH    AF
    LD      A,B     ;Load 'slave address' into S0 register:
    OR      0x01    ;Set RW Bit for read operation
    OUT		(CS_I2C_SX),A
    CALL    _slow_access
    CALL    iic_bus_rdy ; Is bus ready
    LD      A, 0xC5 ;Load C5H into S1. 'C5H' = PCF8584 generates
                    ;the 'START' condition and clocks out the slave
                    ;address and the clock pulse for slave acknowledgement.
    OUT     (CS_I2C_S1),A
     ;Setup counter
    POP     AF
    LD      C,A ; Load BYTE COUNTER into C
    INC     C   ; Offset C by 1
_iic_read_1: ;Wait for PIN = 0
    IN		A,(CS_I2C_S1)	; Read byte from S1 register
	BIT		7,A			    ; S1 PIN=1?
	JR		NZ,_iic_read_1	; No - loop
    BIT		3,A			    ; S1 LRB=0? slave ACK?
    JR      NZ, _iic_read_error  ; No ACK -> an error has occured
    DEC     C
    LD      A, C
    DEC     A       ;If n = m − 1?
    JR      Z, _iic_read_last
    IN      A,(CS_I2C_SX)
    LD      (DE),A
    INC     DE
    JR      _iic_read_1
_iic_read_last:  ;read last byte
    LD      A, 0x40
    OUT		(CS_I2C_S1),A
    CALL    _slow_access
    IN      A,(CS_I2C_SX)   ;receives the final data byte. Neg. ACK is also sent.
    LD      (DE),A
    INC     DE
_iic_read_last_1:
    IN		A,(CS_I2C_S1)	; Read byte from S1 register
	BIT		7,A			    ; S1 PIN=1?
    JR		NZ,_iic_read_last_1	; No - loop

_iic_read_error:
    NOP
_iic_read_stop:
    LD      A, 0xC3
    OUT		(CS_I2C_S1),A
    CALL    _slow_access
    IN      A,(CS_I2C_SX)   ;transfers the final data byte from the 
                            ;data buffer to accumulator.
    CALL    _slow_access
    LD      (DE),A
    POP     BC
    POP DE
    RET


;------------------------------------------------------------------------------
; iic_rdy
;
; Waits until the PCF8584 signals a byte transmission/reception is complete.
;------------------------------------------------------------------------------
iic_rdy:
	PUSH	AF
_iic_rdy_loop:
	IN		A,(CS_I2C_S1)	; Read byte from S1 register
	BIT		7,A			; Is Tx/Rx complete? (S1 PIN=0?)
    ;call    print_a_hex
	JR		NZ,_iic_rdy_loop	; 	No - loop
_iic_rdy_done:
	POP		AF
	RET

;------------------------------------------------------------------------------
; i2c_bus_rdy
;
; Waits until the I2C bus is free before RETurning
;------------------------------------------------------------------------------
iic_bus_rdy:
	PUSH	AF
_iic_blp:
	IN		A,(CS_I2C_S1)	; Read byte from S1 register
    PUSH    AF
    call    print_a_hex
    POP AF
	BIT		0,A			; Is bus free? (S1 ~BB=1?)
	JR		Z,_iic_blp	; 	No - loop
	POP		AF
	RET


;------------------------------------------------------------------------------				 
; _pause_loop
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
_pause_loop:
	PUSH	AF							; 11 T-states
_pause_loop_lp:
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
	JP		NZ,_pause_loop_lp					; 10 T-states
	POP		AF							; 10 T-states
	RET									; Pause complete, RETurn


iic_force_stop:
    IN A,(CS_I2C_S1)
    BIT 0, A
    RET NZ
    LD A, 11000011b
    OUT (CS_I2C_S1),A
    NOP
    NOP
    JR iic_force_stop

_slow_access:
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    PUSH AF
    POP AF
    PUSH AF
    POP AF
    PUSH AF
    POP AF
    PUSH AF
    POP AF
    PUSH AF
    POP AF
    PUSH AF
    POP AF
    POP AF
    RET