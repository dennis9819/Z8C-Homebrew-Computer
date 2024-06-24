;----------------------------------------------------------------
;BIOS Driver for I2C Protocol (Software)
;by Dennis Gunia (01/2024)
;
; SCL is connected to PA1 of PIO (U6) with pull-up
; SDA is connected to PA0 of PIO (U6) with pull-up
;----------------------------------------------------------------


;================================================================
; I/O registers
;================================================================


;================================================================
; I/O pins
;================================================================
    IIC_CLK  .EQU 00000001b
    IIC_DATA .EQU 00000010b

;================================================================
; basic access functions 
;================================================================
;HL contains buffer location
;B  defines amount of bytes to send
;C  contains device address
iic_send_buffer:
    CALL iic_send_sbit  ;Send startbit
    LD A,C
    AND 0xFE            ;Mask R/W bit (must be 0 for write)
    CALL iic_send_byte  ;Send Address
    CALL iic_read_ack   
    OR A        ; if no ack, error
    JP NZ, iic_send_buffer_err
iic_send_buffer_loop;
    LD A,(HL)
    INC HL
    CALL iic_send_byte
	CALL iic_read_ack
    OR A        ; if no ack, error
    JP NZ, iic_send_buffer_err
    DJNZ iic_send_buffer_loop   ;loop for remaining bytes
iic_send_buffer_done:
    CALL iic_send_ebit
    LD A,0
    RET
iic_send_buffer_err:
    CALL iic_send_ebit
    LD A,1
    RET

;HL contains buffer location
;B  defines amount of bytes to send
;C  contains device address
iic_receive_buffer:
    DEC B   
    CALL iic_send_sbit  ;Send startbit
    LD A,C
    OR 0x01             ;set R/W bit (must be 1 for read)
    CALL iic_send_byte  ;Send Address
    CALL iic_read_ack   
    OR A        ; if no ack, error
    JP NZ, iic_receive_buffer_err
iic_receive_buffer_loop:
    CALL iic_receive_byte
    LD (HL),A
    INC HL
    CALL iic_send_ack
    DJNZ iic_receive_buffer_loop
    ; last time:
    CALL iic_receive_byte
    LD (HL),A
    INC HL
    CALL iic_send_nack

iic_receive_buffer_done:
    CALL iic_send_ebit
    LD A,0
    RET
iic_receive_buffer_err:
    CALL iic_send_ebit
    LD A,1
    RET



;================================================================
; I/O access functions 
;================================================================

;Reset PIO configuration
iic_init:
    ;SCL HIGH, SDA HIGH
    LD A,0x03
    OUT (CS_PIO_AD), A
    ;Set port to controll mode (MODE3)
    LD A,0xCF
    OUT (CS_PIO_AC), A
    ;Set inputs/outputs
    LD A,0xF0
    OUT (CS_PIO_AC), A
    RET

; send start bit
iic_send_sbit:
    ;SCL HIGH, SDA HIGH
    LD A,0x03
    OUT (CS_PIO_AD), A
    ;Set port to controll mode (MODE3)
    LD A,0xCF
    OUT (CS_PIO_AC), A
    ;Set inputs/outputs (SDA and SCL is now output)
    LD A,0xFC
    OUT (CS_PIO_AC), A
    ;SCL HIGH, SDA LOW
    LD A,0x02
    OUT (CS_PIO_AD), A
    NOP
    NOP
    LD A,0x00
    OUT (CS_PIO_AD), A
    NOP
    NOP
    RET

; send end/stop bit
iic_send_ebit:
    ;Set port to controll mode (MODE3)
    LD A,0xCF
    OUT (CS_PIO_AC), A
    
    ;Set inputs/outputs (SDA and SCL is now output)
    LD A,0xFC
    OUT (CS_PIO_AC), A
    ;SCL HIGH, SDA LOW
    LD A,0x02
    OUT (CS_PIO_AD), A
    NOP
    NOP
    LD A,0x03   ; both high
    OUT (CS_PIO_AD), A
    NOP
    NOP
    ;release bus
    ;Set port to controll mode (MODE3)
    LD A,0xCF
    OUT (CS_PIO_AC), A
    NOP
    NOP
    ;Set inputs/outputs (SDA and SCL is now input, sound enabled)
    LD A,11110111b
    OUT (CS_PIO_AC), A
    NOP
    NOP
    RET


iic_read_ack:
    LD A,0xCF
    OUT (CS_PIO_AC), A
    ;Set inputs/outputs (SCL is now output, SDA input)
    LD A,0xFD
    OUT (CS_PIO_AC), A
    NOP
    NOP
    LD A,0x00       ;set SCL LOW
    OUT (CS_PIO_AD), A
    NOP
    NOP
    XOR 0x02         ;set SCL HIGH
    OUT (CS_PIO_AD), A
    NOP
    IN A,(CS_PIO_AD)    ; Read SDA
    NOP
    NOP
    PUSH AF
    AND 0xFE            ; Filter input
    XOR 0x02         ;set SCL LOW
    OUT (CS_PIO_AD), A
    NOP
    NOP
    POP AF
    AND 1
    RET

iic_send_ack:
    ;Set port to controll mode (MODE3)
    LD A,0xCF
    OUT (CS_PIO_AC), A
    ;Set inputs/outputs (SDA and SCL is now output)
    LD A,0xFC
    OUT (CS_PIO_AC), A
    NOP
    NOP
    LD A,0x00   ; SCL LOW, SDA LOW
    OUT (CS_PIO_AD), A
    NOP
    NOP
    LD A,0x02   ; SCL HIGH, SDA LOW
    OUT (CS_PIO_AD), A
    NOP
    NOP
    LD A,0x00   ; SCL LOW, SDA LOW
    OUT (CS_PIO_AD), A
    NOP
    NOP
    RET

iic_send_nack:
    ;Set port to controll mode (MODE3)
    LD A,0xCF
    OUT (CS_PIO_AC), A
    ;Set inputs/outputs (SDA and SCL is now output)
    LD A,0xFC
    OUT (CS_PIO_AC), A
    NOP
    NOP
    LD A,0x02   ; SCL LOW, SDA HIGH
    OUT (CS_PIO_AD), A
    NOP
    NOP
    LD A,0x03   ; both high
    OUT (CS_PIO_AD), A
    NOP
    NOP
    LD A,0x02   ; SCL LOW, SDA HIGH
    OUT (CS_PIO_AD), A
    NOP
    NOP
    RET

;A contains byte
iic_send_byte:
    PUSH BC
    LD C,A  ;buffer
    ;Set port to controll mode (MODE3)
    LD A,0xCF
    OUT (CS_PIO_AC), A
    ;Set inputs/outputs (SDA and SCL is now output)
    LD A,0xFC
    OUT (CS_PIO_AC), A
    LD B,8  ;bit counter
    
iic_send_byte_loop:
    ;prepare data
    RL C
    LD A,0
    RLA     ; set SCA bit from carry, SCL LOW
    OUT (CS_PIO_AD), A
    NOP
    NOP
    XOR 0x02         ;set SCL HIGH
    OUT (CS_PIO_AD), A
    NOP
    NOP
    XOR 0x02         ;set SCL LOW
    OUT (CS_PIO_AD), A
    NOP
    NOP
    DJNZ iic_send_byte_loop ;loop until counter is 0
    ;transmittion end / end loop
    LD A,C
    POP BC
    RET

iic_receive_byte:
    PUSH BC
    ;Set port to controll mode (MODE3)
    LD A,0xCF
    OUT (CS_PIO_AC), A
    ;Set inputs/outputs (SCL is now output, SDA input)
    LD A,0xFD
    OUT (CS_PIO_AC), A
    LD B,8  ;bit counter
    LD C,0
iic_receive_byte_loop:
    XOR A   ;set SCL LOW
    OUT (CS_PIO_AD), A
    NOP
    NOP
    LD A,2  ;set SCL HIGH
    OUT (CS_PIO_AD), A
    NOP
    IN A, (CS_PIO_AD)
    NOP
    RRA     ;read SDA bit
    RL C     ;store bit
    XOR A   ;set SCL LOW again
    OUT (CS_PIO_AD), A
    NOP
    NOP
    DJNZ iic_receive_byte_loop
    LD A,C
    POP BC
    RET