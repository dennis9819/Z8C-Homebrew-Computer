;----------------------------------------------------------------
;BIOS Driver for IDE Access
;by Dennis Gunia (01/2023)
;----------------------------------------------------------------

;================================================================
; IDE commands
;================================================================
    IDE_CMD_IDENT   .EQU 0xEC    ;Identify drive.
    IDE_CMD_READSEC .EQU 0x20    ;Read sectors.

;================================================================
; IDE Variables
;================================================================
    MEM_IDE_BASE    .EQU 0x5000
    MEM_IDE_DEVICE  .EQU MEM_IDE_BASE       ;1Byte:   Device ID for IDE-Port, Controller and Master/Slave
    MEM_IDE_STATUS  .EQU MEM_IDE_BASE + 1   ;1Byte:   0x00 if status is okay
    MEM_IDE_BUFFER  .EQU MEM_IDE_BASE + 2   ;512Byte: buffer for read/write data

;================================================================
; IDE funtions
;================================================================

;------------------------------------------------------------------------------
; ideif_init_drive
;
; initializes drive
;------------------------------------------------------------------------------
ideif_init_drive:
    xor a
    ld (MEM_IDE_DEVICE),A   ;Set device to 0    
    ld (MEM_IDE_STATUS),A   ;Set status to 0 (OK)
    di              ;disable interrupt 
    call ide_reset  ;Reset drives on bus
    ld hl, [str_dev_waitready]
    call print_str  ;print waiting message
    ld DE, 0x1FFF   ;preload timeout counter
ideif_init_drive_loop1:
    ld b, IDE_REG_CMDSTS 
    call ide_regread_8  ;read drive status register
    OR A
    JR Z, ideif_init_drive_nodrv    ;no drive found
    BIT 6,A             ;Wait for device ready
    JR NZ, ideif_init_drive_detected 
    DEC DE  ; decrement timeout
    LD A,D
    OR E
    JR Z, ideif_init_drive_nodrv

    JR ideif_init_drive_loop1

ideif_init_drive_nodrv:
    ld hl, [str_dev_notfound]
    call print_str
    RET

ideif_init_drive_detected:
    ld hl, [str_dev_ready]
    call print_str
    LD B, IDE_REG_CMDSTS    ;Get drive identification
    LD A, IDE_CMD_IDENT
    call ide_regwrite_8     ;Write command to drive
    LD HL,MEM_IDE_BUFFER    ;set read/write buffer start address
    call ide_readsector_256 ;read 256 words from device
    LD HL,MEM_IDE_BUFFER + 20  ;print device serial
    LD B, 20
    CALL print_str_fixed
    ld hl, [str_dev_ready2] 
    call print_str
    LD HL,MEM_IDE_BUFFER + 54  ;print device name
    LD B, 40
    CALL print_str_fixed
    LD A,10                 ;New line
    CALL print_char
    LD A,13
    CALL print_char
    RET


;================================================================
; IDE strings
;===============================================================

str_dev_waitready:
    db 13,10,"Seek HDD ... ",0

str_dev_ready:
    db "Device Found!",13,10,"Serial: ",0
str_dev_ready2:
    db "  Name: ",0

str_dev_notfound:
    db "no drive detected",13,10,0