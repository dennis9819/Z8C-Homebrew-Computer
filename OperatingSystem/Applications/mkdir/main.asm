    IDE_CMD_WRITESEC .EQU 0x30


.include "extern_symbols.s" ;include monitor symbols.
    START_ADDR   .EQU 0x8000
    
    org START_ADDR  
com_header:
    jp com_prg
    db 0x00
    dw 0x00, 0x00, 0x00   ;always 0
    dw [START_ADDR]     ;start addr
    dw [_eof]           ;end of file
    dw [_eof - START_ADDR]  ;length
    dc 48,0x00
    
com_prg:
; Program starts here






;------------------------------------------------------------------------------
; write_lba_sector
;
; Writes A*512 byte sector ro disk
; HL contains pointer to LBA address
; DE contains data location
; A contains sector count
;------------------------------------------------------------------------------
write_lba_sector:
    push af
    ide_wait_rdy
    pop af
    ;setup registers
    LD B,IDE_REG_SECTOR ;amount of sectores
    CALL ide_regwrite_8
    LD A,(HL)   
    LD B,IDE_REG_LBA0
    CALL ide_regwrite_8
    INC HL
    LD A,(HL)   
    LD B,IDE_REG_LBA1
    CALL ide_regwrite_8
    INC HL
    LD A,(HL)   
    LD B,IDE_REG_LBA2
    CALL ide_regwrite_8
    INC HL
    LD A,(HL)   
    AND 00001111b
    OR  11100000b
    LD B,IDE_REG_LBA3
    CALL ide_regwrite_8

    LD A,IDE_CMD_WRITESEC ;send read command
    LD B,IDE_REG_CMDSTS
    CALL ide_regwrite_8
    jp ide_writesector_512_fast

ide_writesector_512_fast:
    ld b, IDE_REG_CMDSTS    ;check status
    call ide_regread_8
    bit 0,a                 ;Error Bit set
    jp nz, ide_printerror   ;then abort
    bit 3,a                 ;wait for drq
	jr z,ide_writesector_512_fast

    ld b,0             ;256x
    ld a, 10000000b    ;CommandByte-A, Mode 0, PA Out, PC Out, PB Out
    out (CS_PIA_CR), a ;Set Data direction to IN
_ide_writesector_512_loop:
    ld a, IDE_REG_DATA ;CS0 and A=0 -> I/O register
    out (CS_PIA_PC), a ;set register
    ld a,(hl)
    out (CS_PIA_PA),a
    inc hl
    ld a,(hl)
    out (CS_PIA_PB),a
    inc hl
    or  IDE_WR         ;Set Read bit
    out (CS_PIA_PC), a ;Write Read to bit controll lines
    ld a, IDE_REG_DATA ;CS0 and A=0 -> I/O register
    out (CS_PIA_PC), a ;set register
    djnz _ide_writesector_512_loop  ; loop for 512 bytes
_ide_writesector_512_waitready:
    ld b, IDE_REG_CMDSTS;check drive status
    call ide_regread_8 
    bit 0,a                 ;Error Bit set
    jp nz, ide_printerror   ;then abort
    bit 7,a                 ;test if data is written
    jp nz _ide_writesector_512_waitready    ; if still writing, loop
    bit 3,a                 ;test if drive awaits next sector
    jp nz, ide_writesector_512_fast
    ret z

; Program ends here
_eof: