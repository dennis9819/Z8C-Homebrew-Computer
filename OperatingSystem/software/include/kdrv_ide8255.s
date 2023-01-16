;----------------------------------------------------------------
;BIOS Driver for IDE Interface 82C55
;by Dennis Gunia (01/2023)
;----------------------------------------------------------------

;================================================================
; I/O registers
;================================================================
    CS_PIA_PA .EQU 0x30 ; D0-7
    CS_PIA_PB .EQU 0x31 ; D8-15
    CS_PIA_PC .EQU 0x32 ; Controll Lines
    CS_PIA_CR .EQU 0x33

;================================================================
; I/O pins
;================================================================
    IDE_WR   .EQU 00100000b
    IDE_RD   .EQU 01000000b
    IDE_RST  .EQU 10000000b

;================================================================
; IDE registers
;================================================================
    IDE_REG_DATA    .EQU 01000b   ;data I/O register (16-bits) 
    IDE_REG_ERROR   .EQU 01001b   ;error information register when read; write precompensation register when written.
    IDE_REG_SECTOR  .EQU 01010b   ;Sector counter register
    IDE_REG_SSECTOR .EQU 01011b   ;Start sector register
    IDE_REG_LCYL    .EQU 01100b   ;Low byte of the cylinder number
    IDE_REG_HCYL    .EQU 01101b   ;High two bits of the cylinder number
    IDE_REG_HEAD    .EQU 01110b   ;Head and device select register
    IDE_REG_CMDSTS  .EQU 01111b   ;command/status register
    IDE_REG_ALTSTS  .EQU 10110b   ;Alternate Status/Digital Output
    IDE_REG_DRVADDR .EQU 10111b   ;Drive Address  

;================================================================
; I/O access functions 
;================================================================

;------------------------------------------------------------------------------
; ide_reset
;
; resets drives on bus
;------------------------------------------------------------------------------
ide_reset:
    LD A, 10000000b    ;CommandByte-A, Mode 0, PA Out, PC Out, PB Out
    OUT (CS_PIA_CR), A  ;Set Data direction to out
    LD A, IDE_RST
    OUT (CS_PIA_PC), A  ;Reset IDE Device
    NOP
    XOR A
    OUT (CS_PIA_PC), A  ;end device reset
    RET


;------------------------------------------------------------------------------
; ide_regwrite_8
;
; Sends data to the IDE device
; A contains DATA
; B contains register number
;------------------------------------------------------------------------------
ide_regwrite_8:
    PUSH AF             ;store date to stack
    ; Prepare PIA Data Direction
    LD A, 10000000b    ;CommandByte-A, Mode 0, PA Out, PC Out, PB Out
    OUT (CS_PIA_CR), A  ;Set Data direction to out
    ; Write Data out
    POP AF
    OUT (CS_PIA_PA), A  ;Write Data to bit 0-7
    ;Prepare Address
    LD  A, B        ;Load register address
    AND 00011111b  ;Mask unused bits
    OUT (CS_PIA_PC), A  ;Write Data to bit controll lines
    OR  IDE_WR      ;Set Write bit
    OUT (CS_PIA_PC), A  ;Set write signal
    NOP                 ;delay to wait for processing
    LD  A, B        ;Load register address
    AND 00011111b  ;Mask unused bits
    OUT (CS_PIA_PC), A  ;disable write signal
    NOP
    XOR A               ;clear register A
    OUT (CS_PIA_PC), A  ;clear controll lines
    RET


;------------------------------------------------------------------------------
; ide_regread_8
;
; Sends data to the IDE device
; B contains register number
; A returns data
;------------------------------------------------------------------------------
ide_regread_8:
    LD A, 10010010b    ;CommandByte-A, Mode 0, PA IN, PC Out, PB IN
    OUT (CS_PIA_CR), A  ;Set Data direction to in
    ;Prepare Address
    LD  A, B        ;Load register address
    AND 00011111b  ;Mask unused bits
    OUT (CS_PIA_PC), A  ;Write Data to bit controll lines
    OR  IDE_RD      ;Set Write bit
    OUT (CS_PIA_PC), A  ;Write Data to bit controll lines
    NOP                 ;delay to wait for processing
    PUSH AF
    POP AF
    PUSH AF
    POP AF
    PUSH AF
    POP AF
    PUSH AF
    POP AF
    IN  A,(CS_PIA_PA)   ;read data from ide device to b (because a is used later)
    PUSH AF
    XOR A               ;clear register A
    OUT (CS_PIA_PC), A  ;clear controll lines
    POP AF          ;put data in accumulator
    RET


;------------------------------------------------------------------------------
; ide_readsector_256
;
; Reads IDE Data
; HL contains destination address
;------------------------------------------------------------------------------
ide_readsector_256:
    LD C,255    ;Setup counter for 256 words

ide_readsector_256_waitloop:
    LD B, IDE_REG_CMDSTS
    CALL ide_regread_8
    BIT 0,a                             ;Error Bit set.
    JP NZ, ide_printerror  
    BIT 3,a                             ;DRQ Bit set. If set, disk has data
    JR Z, ide_readsector_256_waitloop    ;If not set, wait

    LD A, 10010010b    ;CommandByte-A, Mode 0, PA IN, PC Out, PB IN
    OUT (CS_PIA_CR), A ;Set Data direction to IN
    LD A, IDE_REG_DATA ;CS0 and A=0 -> I/O register
    OUT (CS_PIA_PC), A ;set register
    OR  IDE_RD         ;Set Read bit
    OUT (CS_PIA_PC), A ;Write Read to bit controll lines
    NOP
    NOP
    NOP
    IN A,(CS_PIA_PB)    ;Load 16-Bit data to buffer
    LD (HL), A
    INC HL
    IN A,(CS_PIA_PA)
    LD (HL), A
    INC HL

    LD A,C
    OR A
    JP Z,ide_readsector_256_done
    DEC C
    JR ide_readsector_256_waitloop

ide_readsector_256_done:
    RET

ide_readsector_256_inv:
    LD C,255    ;Setup counter for 256 words

ide_readsector_256_inv_waitloop:
    LD B, IDE_REG_CMDSTS
    CALL ide_regread_8
    BIT 0,a                             ;Error Bit set.
    JP NZ, ide_printerror  
    BIT 3,a                             ;DRQ Bit set. If set, disk has data
    JR Z, ide_readsector_256_inv_waitloop    ;If not set, wait

    LD A, 10010010b    ;CommandByte-A, Mode 0, PA IN, PC Out, PB IN
    OUT (CS_PIA_CR), A ;Set Data direction to IN
    LD A, IDE_REG_DATA ;CS0 and A=0 -> I/O register
    OUT (CS_PIA_PC), A ;set register
    OR  IDE_RD         ;Set Read bit
    OUT (CS_PIA_PC), A ;Write Read to bit controll lines
    NOP
    NOP
    NOP
    IN A,(CS_PIA_PA)    ;Load 16-Bit data to buffer
    LD (HL), A
    INC HL
    IN A,(CS_PIA_PB)
    LD (HL), A
    INC HL

    LD A,C
    OR A
    JP Z,ide_readsector_256_done
    DEC C
    JR ide_readsector_256_inv_waitloop



;------------------------------------------------------------------------------
; ide_writesector_256
;
; Writes 512 bytes (256 words) of IDE Data
; HL contains data start address
;------------------------------------------------------------------------------
ide_writesector_256:
    RET     ;NOT IMPLEMENTED


;================================================================
; utility functions 
;================================================================

;------------------------------------------------------------------------------
; ide_printerror
;
; prints IDE error to console
;------------------------------------------------------------------------------
ide_printerror:
    LD HL, [str_error_start]
    CALL print_str
    LD B, IDE_REG_CMDSTS
    CALL ide_regread_8
    CALL print_a_hex
    LD HL, [str_error_start1]
    CALL print_str
    LD A,(MEM_IDE_DEVICE)
    CALL print_a_hex
    LD HL, [str_error_start2]
    CALL print_str
    LD B, IDE_REG_ERROR
    CALL ide_regread_8
    CALL print_a_hex
    LD A,10
    CALL print_char
    LD A,13
    CALL print_char

    RET

str_error_start:
    db 13,10,"Disk I/O error. Status: 0x",0
str_error_start1:
    db " Dev: 0x",0
str_error_start2:
    db " Err: 0x",0

