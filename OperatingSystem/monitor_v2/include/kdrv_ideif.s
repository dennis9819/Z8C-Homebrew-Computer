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
    phase var_idebuffer
MEM_IDE_DEVICE:
    defs 1
MEM_IDE_STATUS:
    defs 1   ;1Byte:   0x00 if status is okay
MEM_IDE_PARTITION:
    defs 4  ;4*16Bytes: LBA first sector
MEM_IDE_POINTER:
    defs 4 ;4*16Bytes: LBA first sector
MEM_IDE_DEV_TABLE:
    defs 16*4
MEM_IDE_SELECTED:
    defs 1
MEM_IDE_STRING_0:
    defs 40
MEM_IDE_STRING_1:
    defs 40
MEM_IDE_STRING_2:
    defs 40
MEM_IDE_STRING_3:
    defs 40
MEM_IDE_BUFFER:
    defs 512
MEM_IDE_FSBUFFER:
    defs 256
    dephase
    ;DEV-Table layout
    ;<STATUS> <TYPE> <FIRST-SECTOR (4-byte)> <Length (4-byte)> <I/O Addr> <Master/Slave> 0x00 0x00
    ;Status: 0x00 -> Ready
    ;        0x01 -> Not found
    ;        0x02 -> No supported filesystem
    ;I/O Addr: Base addr of 82C55
    ;Type: File system type


IDE_DEV_TABLE:
    db 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x00, [MEM_IDE_STRING_0], [MEM_IDE_STRING_0]>>8, 0x00, 0x00
    db 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x01, [MEM_IDE_STRING_1], [MEM_IDE_STRING_1]>>8, 0x00, 0x00
    db 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, [MEM_IDE_STRING_2], [MEM_IDE_STRING_2]>>8, 0x00, 0x00
    db 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x01, [MEM_IDE_STRING_3], [MEM_IDE_STRING_3]>>8, 0x00, 0x00

;================================================================
; IDE funtions
;================================================================

;------------------------------------------------------------------------------
; ideif_init_table
;
; initializes drive table
;------------------------------------------------------------------------------
ideif_init_devtable:
    ;copy default values to ram
    ld hl,[IDE_DEV_TABLE]
    ld de,[MEM_IDE_DEV_TABLE]
    ld bc,16*4
    ldir
    ;set selected device
    ld a,0
    ld (MEM_IDE_SELECTED),a
    ;set empty names
    xor a
    ld (MEM_IDE_STRING_0),a
    ld (MEM_IDE_STRING_1),a
    ld (MEM_IDE_STRING_2),a
    ld (MEM_IDE_STRING_3),a
    ret

;------------------------------------------------------------------------------
; ideif_prnt_devtable
;
; prints drive table
;------------------------------------------------------------------------------
ideif_prnt_devtable:
    call print_newLine
    ld hl,[_ideif_prnt_devtable_hdr]
    call print_str
    ld hl,[MEM_IDE_DEV_TABLE]
    ld b,0  ;device number
_ideif_prnt_devtable_l1:    ;loop 1 -> for each device
;print if selected
    ld a,(MEM_IDE_SELECTED)
    cp b
    jp z,_ideif_prnt_devtable_l1_sel
    ld a, ' '
    jr _ideif_prnt_devtable_l1_nxt
_ideif_prnt_devtable_l1_sel:
    ld a, '*'
_ideif_prnt_devtable_l1_nxt:
    call print_char
;print drive letter
    ld a,b
    add 69  ;offset letter to D
    call print_char
    ld a, ':'
    call print_char
    ld a, ' '
    call print_char
;print status
    push hl
    ld a,(HL)
    or a
    jr z, _ideif_prnt_devtable_l1_s00
    cp 0x01
    jr z, _ideif_prnt_devtable_l1_s01
    cp 0x02
    jr z, _ideif_prnt_devtable_l1_s02
    cp 0xFF
    jr z, _ideif_prnt_devtable_l1_sFF
    jr _ideif_prnt_devtable_l1_sFF
_ideif_prnt_devtable_l1_s00
    ld hl,[_ideif_prnt_devtable_s00]
    jr _ideif_prnt_devtable_l1_es
_ideif_prnt_devtable_l1_s01
    ld hl,[_ideif_prnt_devtable_s01]
    jr _ideif_prnt_devtable_l1_es
_ideif_prnt_devtable_l1_s02
    ld hl,[_ideif_prnt_devtable_s02]
    jr _ideif_prnt_devtable_l1_es
_ideif_prnt_devtable_l1_sFF
    ld hl,[_ideif_prnt_devtable_sFF]
_ideif_prnt_devtable_l1_es
    call print_str
    pop hl
    inc hl
;print FS-Type
    ld a,'0'
    call print_char
    ld a,'x'
    call print_char
    ld a,(HL)
    call print_a_hex
    ld a,' '
    call print_char
    inc hl
;print first sector
    push hl
    pop ix

    inc hl
    inc hl
    inc hl
    inc hl
    inc hl
    inc hl
    inc hl
    inc hl

    ld a,(ix+3)
    call print_a_hex
    ld a,(ix+2)
    call print_a_hex
    ld a,(ix+1)
    call print_a_hex
    ld a,(ix+0)
    call print_a_hex
    ld a,' '
    call print_char
;print length
    ld a,(ix+7)
    call print_a_hex
    ld a,(ix+6)
    call print_a_hex
    ld a,(ix+5)
    call print_a_hex
    ld a,(ix+4)
    call print_a_hex
    ld a,' '
    call print_char
;print Port
    ld a,'0'
    call print_char
    ld a,'x'
    call print_char
    ld a,(HL)
    call print_a_hex
    ld a,' '
    call print_char
    inc hl
;print M/S
    ld a,(HL)
    push hl
    or a
    jp z,_ideif_prnt_devtable_l1_ms
    ld hl,[_ideif_prnt_devtable_slave]
    jp _ideif_prnt_devtable_l1_e2
_ideif_prnt_devtable_l1_ms
    ld hl,[_ideif_prnt_devtable_master]
_ideif_prnt_devtable_l1_e2
    call print_str
    pop hl
    inc hl
;print str
    push bc
    ld a,(hl)
    ld c,a
    inc hl
    ld a,(hl)
    ld b,a
    inc hl
    push hl
    ld h,b
    ld l,c
    call print_str
    call print_newLine
    pop hl
    pop bc
;next
    inc hl
    inc hl
    inc b
    ld a,b
    cp 4
    ret z
    jp _ideif_prnt_devtable_l1
;------------------------------------------------------------------------------
; ideif_init_drive
;
; initializes selected drive in table
;------------------------------------------------------------------------------
ideif_init_drive:
    call ideif_get_drv_pointer
    ; load addresses. not used atm
    ;ld a,(IX+6) ;load IO Addr 
    ;ld d,a
    ;ld a,(IX+7) ;load Master/Slave bit
    ;ld e,a
    call ide_reset
    
    ld bc, 0x5FFF   ;preload timeout counter
_ideif_init_drive_loop:
    ld b, IDE_REG_CMDSTS 
    call ide_regread_8  ;read drive status register
    or a
    jr z,_ideif_init_drive_nodrv   ;no drive found
    bit 6,a
    jr nz, _ideif_init_drive_found
    dec de
    ld a,d
    or e
    jr z, _ideif_init_drive_nodrv
    jr _ideif_init_drive_loop

_ideif_init_drive_nodrv:
    ld(ix+0),0x01
    ret

_ideif_init_drive_found:
    ld (ix+0),0x02
    ;get drive name
    ld b, IDE_REG_CMDSTS    ;Get drive identification
    ld a, IDE_CMD_IDENT
    call ide_regwrite_8     ;Write command to drive
    ld hl, MEM_IDE_BUFFER   ;set read/write buffer start address
    call ide_readsector_256 ;read 256 words from device
    ld hl,MEM_IDE_BUFFER + 54  ;print device serial
    ld a,(ix+12)  ;load str pointer into de
    ld e,a
    ld a,(ix+13)
    ld d,a
    ld bc,40    ;copy 40 char
    ldir
    ;get partition table
    ;read bootsector
    ld a,1                  ;read 1 sector
    ld B,IDE_REG_SECTOR
    call ide_regwrite_8
    ld a,1                  ;read sector 0
    ld b,IDE_REG_SSECTOR
    call ide_regwrite_8
    ld a,0                  ;read cylinder 0
    ld b,IDE_REG_LCYL
    call ide_regwrite_8
    ld a,0                  
    ld b,IDE_REG_HCYL
    call ide_regwrite_8
    ld a,10100000b          ;read head 0
    ld b,IDE_REG_HEAD
    call ide_regwrite_8
    ld a,IDE_CMD_READSEC    ;send read command
    ld b,IDE_REG_CMDSTS
    call ide_regwrite_8
    ld hl, MEM_IDE_BUFFER   ;set read/write buffer start address
    call ide_readsector_512_inv ;read 256 words from device
    ;prepare partitions
    ld b,4                      ;Partition table length
    ld c,0                      ;Partition ID counter
    ld iy,MEM_IDE_BUFFER+446    ;Load offest of first partition table entry
_ideif_init_drive_prt_l1:
    ld a,(iy+4) ;load status byte
    or a
    jp NZ, _ideif_init_drive_prt_fnd    ;If not zero, jump to print function
    jp _ideif_init_drive_prt_ln
_ideif_init_drive_prt_ln: 
    ld de,16
    add iy,de
    djnz _ideif_init_drive_prt_l1    
    ret
_ideif_init_drive_prt_fnd;
    ld a,(iy+4)
    ld (ix+1),a ;store partition type
    cp 0x0E     ;if not 0xE0, continue with next entry
    jr nz, _ideif_init_drive_prt_ln
    ;get start LBA
    ld a,(iy+0x08)
    ld (ix+0x02),a
    ld a,(iy+0x09)
    ld (ix+0x03),a
    ld a,(iy+0x0A)
    ld (ix+0x04),a
    ld a,(iy+0x0B)
    ld (ix+0x05),a
    ld (ix+0),0x00
    ;get count LBA
    ld a,(iy+0x0C)
    ld (ix+0x06),a
    ld a,(iy+0x0D)
    ld (ix+0x07),a
    ld a,(iy+0x0E)
    ld (ix+0x08),a
    ld a,(iy+0x0F)
    ld (ix+0x09),a
    ld (ix+0),0x00
    ret
    
;------------------------------------------------------------------------------
; ideif_get_drv_pointer
;
; gets pointer to selected drive in table in IX
;------------------------------------------------------------------------------
ideif_get_drv_pointer:
    push af
    push bc
    ;get selected drive
    ld a,(MEM_IDE_SELECTED)
    ;multiply a *8
    rlca ;*2
    rlca ;*4
    rlca ;*8
    ld b,0
    ld c,a
    ld ix,[MEM_IDE_DEV_TABLE]
    add ix,bc
    pop bc
    pop af
    ret

;------------------------------------------------------------------------------
; ideif_init_all
;
; initializes interface
;------------------------------------------------------------------------------
ideif_init_all:
    ld hl, [str_dev_waitready]
    call print_str  ;print waiting message
   
    call ideif_init_devtable
    call ideif_init_drive

    ld hl, [str_dev_done]
    call print_str
    RET


;------------------------------------------------------------------------------
; ideif_drv_sel
;
; Selects drive from table and initializes the fat16 file system
; A contains drive id
;------------------------------------------------------------------------------



;------------------------------------------------------------------------------
; read_lba_sector
;
; Reads A*512 byte sector into memory
; HL contains pointer to LBA address
; DE contains destination location
; A contains sector count
;------------------------------------------------------------------------------
read_lba_sector:
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

    LD A,IDE_CMD_READSEC    ;send read command
    LD B,IDE_REG_CMDSTS
    CALL ide_regwrite_8
    ;LD HL,MEM_IDE_BUFFER    ;set read/write buffer start address
    EX DE,HL                ;transfer destination in DE to HL
    call ide_readsector_512_inv ;read 256 words from device
    ret
    
;------------------------------------------------------------------------------
; ideif_drv_sel
;
; Select drive from table
; Prepare variables
;
; A contains drive number
;------------------------------------------------------------------------------
ideif_drv_sel:
    ld (MEM_IDE_SELECTED),a
    push af
    call ideif_get_drv_pointer  ;test if drive is marked as available
    ld a,(ix+0)
    or a
    jp nz, _ideif_drv_sel_fail  ;if not-> fail
    
    call fat_get_root_table     ;else get root table

    ld hl,[_ideif_drv_sel_pstr] ;print success message
    call print_str
    pop af
    add 69
    ld (var_dir),a  ;store drive letter
    call print_char
    ld a, ':'
    ld (var_dir+1),a
    ld a, '\'
    ld (var_dir+2),a
    xor a   ;set dir to empty
    ld (var_dir+3),a
    ld (var_dir+79),a ;set depth counter
    ld hl,[_ideif_drv_sel_sstr0]
    call print_str
    ret
_ideif_drv_sel_fail:
    ld hl,[_ideif_drv_sel_pstr]
    call print_str
    pop af
    add 69
    call print_char
    ld hl,[_ideif_drv_sel_fstr0]
    call print_str
    xor a   ;set dir to empty
    ld (var_dir),a
    LD DE,0x20
    LD BC,0x70
    CALL beep
    ret




;================================================================
; IDE strings
;===============================================================

str_dev_waitready:
    db 13,10,"Detecting drives ... ",0
str_dev_done:
    db "done!",13,10,0

_ideif_prnt_devtable_hdr:
    db "DRV Status    Type LBA      Length   Port M/S    Name",10,13,0
_ideif_prnt_devtable_s00:
    db "Avail     ",0
_ideif_prnt_devtable_s01:
    db "Not Found ",0
_ideif_prnt_devtable_s02:
    db "Unkown FS ",0
_ideif_prnt_devtable_sFF:
    db "Ctrl. Err ",0
_ideif_prnt_devtable_master:
    db "Master ",0
_ideif_prnt_devtable_slave:
    db "Slave  ",0
_ideif_drv_sel_pstr:
    db 10,13,"Drive ",0
_ideif_drv_sel_fstr0:
    db ": not ready",10,13,0
_ideif_drv_sel_sstr0:
    db ": selected",10,13,0
_ideif_drv_sel_syn:
    db 10,13,"Invalid drive letter",10,13,0
