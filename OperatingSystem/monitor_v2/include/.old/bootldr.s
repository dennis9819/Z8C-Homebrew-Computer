; Z8C Bootloader
; 2022 by Dennis Gunia

BTLDR_ENTRY:    
                ;ld SP, 0ffffh       ; set stack pointer

                ;Setup Serial Interface
                LD A,01001111b      ; External Trigger, Time Constant Follows
                OUT (CS_CTC_0),A    
                IN A,(CS_DIP)       ; Read BAUD from DIP-Switches
                OUT (CS_CTC_0),A
                LD A,00110000b      ;write into WR0: error reset, select WR0
                OUT (CS_SIO_A_C),A
                LD a,018h           ;write into WR0: channel reset
                OUT (CS_SIO_A_C),A
                LD a,004h           ;write into WR0: select WR4
                OUT (CS_SIO_A_C),A
                ;LD a,04h            ;write into WR4: clkx1,1 stop bit, no parity
                LD a,01000100b       ;write into WR4: clkx16,1 stop bit, no parity
                OUT (CS_SIO_A_C),A
                LD a,005h ;write into WR0: select WR5
                OUT (CS_SIO_A_C),A
                LD a,11101000b ;DTR inactive, TX 8bit, BREAK off, TX on, RTS inactive
                OUT (CS_SIO_A_C),A
                LD a,01h ;write into WR0: select WR1
                OUT (CS_SIO_A_C),A
                LD a,00000100b ;no interrupt in CH B, special RX condition affects vect
                OUT (CS_SIO_A_C),A
                LD a,02h ;write into WR0: select WR2
                OUT (CS_SIO_A_C),A
                LD a,0h ;write into WR2: cmd line int vect (see int vec table)
                        ;bits D3,D2,D1 are changed according to RX condition
                OUT (CS_SIO_A_C),A
                LD a,003h ;write into WR0: select WR3
                OUT (CS_SIO_A_C),A
                LD a,0C1h ;RX 8bit, auto enable off, RX on
                OUT (CS_SIO_A_C),A

BTLDR_STARTUP_MSG:
                LD HL,[S_BTLDR_STARTUP_MSG]
    BTLDR_STARTUP_MSG_LOOP:
                LD A,(HL)
                OR A
                JR Z, BTLDR_INPUT
                CALL BTLDR_SUB_WRITEA
                INC HL
                JR BTLDR_STARTUP_MSG_LOOP
BTLDR_INPUT:
        ; Byte 1 & 2 = Length
        CALL BTLDR_SUB_INPUT_READ
        LD B, A
        CALL BTLDR_SUB_INPUT_READ
        LD C, A
        
        ; Byte 3 & 4 = Offset / Start address
        CALL BTLDR_SUB_INPUT_READ
        LD H, A
        CALL BTLDR_SUB_INPUT_READ
        LD L, A

        ; Byte 5+ = Payload
    BTLDR_INPUT_1:
        CALL BTLDR_SUB_INPUT_READ
        LD (HL),A   ;Store byte
        DEC BC
        LD A,H
        OR A.L
        JR Z, BTLDR_DONE
        INC HL
        JR BTLDR_INPUT_1
        
BTLDR_DONE:
    RET


; Strings
S_BTLDR_STARTUP_MSG:
    db "Z8C BTLDR.S V0.1 by Dennis Gunia [RDY] ",0


; Subroutines
BTLDR_SUB_WRITEA:
                OUT (CS_SIO_A_D),A
    BTLDR_SUB_WRITEA_WAIT:
                XOR A
                INC A
                OUT (CS_SIO_A_C),A
                IN A,(CS_SIO_A_C) 
                BIT 0,A
                JR Z, BTLDR_SUB_WRITEA_WAIT
                RET

BTLDR_SUB_RTS_OFF:
    ld a,005h ;write into WR0: select WR5
    out (CS_SIO_A_C),A
    ld a,0E8h ;DTR active, TX 8bit, BREAK off, TX on, RTS inactive
    out (CS_SIO_A_C),A
    ret
BTLDR_SUB_RTS_ON:
    ld a,005h ;write into WR0: select WR5
    out (CS_SIO_A_C),A
    ld a,0EAh ;DTR active, TX 8bit, BREAK off, TX on, RTS active
    out (CS_SIO_A_C),A
    ret

BTLDR_SUB_INPUT_READ:
    CALL BTLDR_SUB_RTS_ON
    XOR A
    OUT (CS_SIO_A_C), a
    IN A, (CS_SIO_A_C)
    AND 1
    JR Z, BTLDR_SUB_INPUT_READ  ;LOOP IF BUFFER EMPTY
    CALL BTLDR_SUB_RTS_OFF
    IN A, (CS_SIO_A_D)
    RET