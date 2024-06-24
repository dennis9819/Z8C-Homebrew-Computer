; VARS

    phase MEM_IDE_FSBUFFER
MEM_FAT_RESERVED:   ; Reserved sectors (2byte)
    defs 2  
MEM_FAT_AMOUNT:     ; Amount of FATs (1byte)
    defs 1  
MEM_FAT_SECTORS:    ; Length of FAT (2byte)
    defs 2  
MEM_FAT_CLUSTERLEN: ; Length of Cluster (1byte)
    defs 1  
MEM_FAT_COUNT1:     ; Counter Var for reading FAT (2byte)
    defs 1
MEM_FAT_TMPPOINTER: ; Temporary working pointer
    defs 4
MEM_FAT_DATASTART: ; Start of data area
    defs 4
MEM_FAT_ROOTSTART: ; Start of Root directory
    defs 4
MEM_FAT_FILEREMAIN: ; Remaining sectors in file
    defs 4
MEM_FAT_DIRSEC:   ; Sectors per directory
    defs 2  
MEM_FAT_TMPFNAME:   ; Temporary filename
    defs 16
MEM_FAT_CURDIR:   ; Current Directory
    defs 80
MEM_FAT_OF0_ATTRIBUTE: ;Current file attribute
    defw 0
MEM_FAT_OF0_CCLUST: ;Current cluster of file
    defw 0
MEM_FAT_OF0_FATSEC: ;Current sector in FAT
    defs 4
MEM_FAT_OF0_DATSEC: ;Current sector in Data
    defs 4
MEM_FAT_OF0_DATREM: ;Remaining bytes in Data
    defs 4
MEM_FAT_CURRDIR: ;Current directory
    defs 4
MEM_FAT_EXEC_CURR:
    defw 0
MEM_FAT_EXEC_COUNT:
    defw 0
MEM_FAT_EXEC_START:
    defw 0

    dephase



;-------------------------------------
; Get FAT Root-Table position
;-------------------------------------
fat_get_root_table:
    call fat_reset_pointer ;reset fat pointer

    ; Load first sector on active partition
    LD HL, MEM_IDE_POINTER  ; pointer to LBA address
    LD A,1                  ;read 1 sector
    LD DE, MEM_IDE_BUFFER   ;where to store data?
    call read_lba_sector

    ; check for valid Boot sector
    ld a,(MEM_IDE_BUFFER)
    cp 0xEB ;first byte should be 0xEB
    jp nz, _fat_get_root_table_invalid

    ; Read and store FS Properties
    LD IX,MEM_IDE_BUFFER
    LD A,(IX+0x0D)
    LD (MEM_FAT_CLUSTERLEN),A
    LD A,(IX+0x0E)
    LD (MEM_FAT_RESERVED),A
    LD A,(IX+0x0F)
    LD (MEM_FAT_RESERVED+1),A
    LD A,(IX+0x10)
    LD (MEM_FAT_AMOUNT),A
    LD A,(IX+0x16)
    LD (MEM_FAT_SECTORS),A
    LD A,(IX+0x17)
    LD (MEM_FAT_SECTORS+1),A

    ;Get Data Start Sector
    ;calculate fat length
    ld bc,(MEM_FAT_SECTORS)
    ld a,(MEM_FAT_AMOUNT)       ;add fat to cluster number
    ld d,0
    ld e,a
    call _fat_math_mul32
    ; BCHL contains result -> store to PTR.MEM_FAT_ROOTSTART
    ld (MEM_FAT_ROOTSTART+0),hl
    ld (MEM_FAT_ROOTSTART+2),bc

    ;add offset (reserved sectors)
    ld hl,(MEM_IDE_BUFFER +0x0E) ; load sectors into hl
    ld (MEM_FAT_TMPPOINTER), hl
    xor a
    ld (MEM_FAT_TMPPOINTER+2),a
    ld (MEM_FAT_TMPPOINTER+3),a

    ld bc,[MEM_FAT_ROOTSTART]
    ld de,[MEM_FAT_TMPPOINTER]
    call _fat_math_add32    ;MEM_FAT_ROOTSTART now contains the first sector
                            ;of the Root directory

    ;add offset (partition location)
    call ideif_get_drv_pointer
    inc ix
    inc ix
    push ix
    pop de  ;copy poiter to hl
    ld bc,[MEM_FAT_ROOTSTART]
    call _fat_math_add32    ;MEM_FAT_OF0_DATSEC now contains the first sector
                            ;of the cluster
    ;copy value from MEM_FAT_ROOTSTART to MEM_IDE_POINTER
    ld hl,MEM_FAT_ROOTSTART
    ld de,MEM_IDE_POINTER
    ldi 
    ldi
    ldi
    ldi

    ;copy value from MEM_FAT_ROOTSTART to MEM_IDE_POINTER
    ld hl,MEM_FAT_ROOTSTART
    ld de,MEM_FAT_DATASTART
    ldi 
    ldi
    ldi
    ldi

    ld hl,MEM_FAT_ROOTSTART
    ld de,MEM_FAT_CURRDIR
    ldi 
    ldi
    ldi
    ldi

    ;add offset to data area
    ;multiply cluster by length of cluster

    ;calculate sectors for root dir
    ld hl,(MEM_IDE_BUFFER+0x11)  ;load Maximum root directory entries 
    ld a,h
    ld l,a
    xor a   ;set a 0, clear carry flag
    ld h,a  ;shift right by 8 bit = /512

    ;last step: multiply by 16
    ex de,hl
    ld bc,16
    call _fat_math_mul32
    ; BCHL contains result -> store to PTR.MEM_FAT_TMPPOINTER
    ld (MEM_FAT_TMPPOINTER+0),hl
    ld (MEM_FAT_TMPPOINTER+2),bc

    ld (MEM_FAT_DIRSEC),hl
    ; add offset to MEM_FAT_DATASTART
    ld de,[MEM_FAT_TMPPOINTER]
    ld bc,[MEM_FAT_DATASTART]
    call _fat_math_add32    ;MEM_FAT_DATASTART now contains the correct sector
                            ;at teh beginnig of the data area


    ;done all FS vars populated

    ;navigate to root directory
    ld a,'\'
    ld(MEM_FAT_CURDIR),a
    xor a
    ld(MEM_FAT_CURDIR+1),a

    ret

_fat_get_root_table_invalid:
    call PRINTINLINE
    db 10,13,"Cannot find boot sector.",10,13,0
    call ideif_get_drv_pointer
    ld (ix+0),0x02
    ret

;-------------------------------------
; fat_getfatsec
;
; gets sector in FAT table for the cluster stored in MEM_FAT_OF0_CCLUST
;
; store result in MEM_FAT_OF0_FATSEC
; stores next cluster in MEM_FAT_OF0_CCLUST
;-------------------------------------
fat_getfatsec:
    ld HL,(MEM_FAT_OF0_CCLUST)  ;load cluster 
    ld a,h  ;if not 0x0000
    or l
    jp nz, _fat_getfatsec_notroot
    ;if 0x0000, goto root directory
    ld hl,MEM_FAT_ROOTSTART
    ld de,MEM_FAT_OF0_DATSEC
    ldi ;quick and dirty hack to go back to root directory
    ldi
    ldi
    ldi
    ret

_fat_getfatsec_notroot:
    ld HL,(MEM_FAT_OF0_CCLUST)  ;load cluster 
    ;each sector contains 256 clusters
    ;first 8bits are not needed (/256)
    ld a,h  ;divide by 256
    ld l,a
    xor a
    ld h,a

    ld bc,(MEM_FAT_RESERVED)    ;add reserved sectors
    add hl,bc
    ld(MEM_FAT_OF0_FATSEC+0),hl;store sector 
    xor a
    ld(MEM_FAT_OF0_FATSEC+2),a
    ld(MEM_FAT_OF0_FATSEC+3),a

    call ideif_get_drv_pointer
    inc ix
    inc ix
    push ix
    pop de  ;copy poiter to hl
    ld bc,[MEM_FAT_OF0_FATSEC]
    call _fat_math_add32    ;MEM_FAT_OF0_FATSEC now contains the correct sector
                            ;in the FAT

    ;read FAT sector
    ld hl,MEM_FAT_OF0_FATSEC   ;read next sector
    ld b,1
    ld a,1
    LD DE, MEM_IDE_BUFFER   ;where to store data?
    call read_lba_sector

    ;calculate data sector
    ;multiply cluster by length of cluster
    xor a   ;clear carry
    ld a,(MEM_FAT_CLUSTERLEN)
    ld b,0
    ld c,a
    ld de,(MEM_FAT_OF0_CCLUST)  ;load cluster number
    dec de  ; sub 2 becaus fat starts at 3    
    dec de
    call _fat_math_mul32
    ; BCHL contains result -> store to PTR.MEM_FAT_OF0_DATSEC
    ld (MEM_FAT_OF0_DATSEC+0),hl
    ld (MEM_FAT_OF0_DATSEC+2),bc

    ; add start of data region to addr
    ld bc,[MEM_FAT_OF0_DATSEC]
    ld de,[MEM_FAT_DATASTART]
    call _fat_math_add32    ;MEM_FAT_OF0_FATSEC now contains the correct sector
                            ;in the FAT
    ;MEM_FAT_OF0_DATSEC now has the first sector of the selected cluster

    ;reset MEM_FAT_OF0_DATREM to default cluster length
    ld a,(MEM_FAT_CLUSTERLEN)
    ld l,a
    ld h,0
    ld (MEM_FAT_OF0_DATREM), hl

    ;get next cluster
    ;calculate offset address
    ld a,(MEM_FAT_OF0_CCLUST)
    RLA ;shift to left (x2)
    ld l, a
    ld a,0
    RLA ;shift in carry flag
    ld h,a
    ld de,MEM_IDE_BUFFER
    add hl,de
    ;copy pointer (hl to de)
    ld de,MEM_FAT_OF0_CCLUST
    ldi ;copy byte for next cluster from FAT
    ldi
    ret
    ;store data

;-------------------------------------
; fat_readfilesec
;
; reads single sector of file
; must run fat_readfilesec before to initialize
; if a ix 0x00, success
; if a is 0xFF, end reached
;
; DE contains destination address
;-------------------------------------
fat_readfilesec:
    ;call fat_print_dbg
    ld hl,[MEM_FAT_OF0_DATSEC]
    ld b,1
    ld a,1
    ;LD DE, MEM_IDE_BUFFER   ;where to store data?
    call read_lba_sector        ;read sectore
    ld hl,[MEM_FAT_OF0_DATSEC]  ;increment pointer to next sector
    call _fat_increment_32      ;***
    ld hl,(MEM_FAT_OF0_DATREM)  ;reduce counter
    xor a
    ld de,1
    sbc hl,de ;decrement counter
    ld (MEM_FAT_OF0_DATREM),hl  ;store decremented counter
    ret nz                      ;when not zero, exit function
    ;if zero: 
    ld a, 0xFF                  ;preload error code
    ld hl,(MEM_FAT_OF0_CCLUST)  ;check next chunk
    ld de,0xFFFF    ;end mark
    sbc hl,de       ;if Z match
    ret z                       ;If 0xFFFF, end is reched. Return 
    ;if next cluster available:
    xor a
    call fat_getfatsec    ; read next cluster information
    ret


;-------------------------------------
; fat_openfile
; search for entry in current directory
;
; DE pointer to file name
; sets:
; - MEM_FAT_OF0_CCLUST
; - MEM_FAT_OF0_ATTRIBUTE
; - MEM_FAT_FILEREMAIN
; if a is 0x00, success
; if a is 0xFF, end reached
;-------------------------------------
fat_openfile:
    PUSH DE
    ;MEM_FAT_TMPFNAME now has valid text to compare
    LD HL,[MEM_FAT_TMPFNAME]
    call format_filename_fat16
    POP DE
fat_openfile_noprepare:
    PUSH DE
    ;prepare pointer
    ld hl,MEM_FAT_CURRDIR
    ld de,MEM_IDE_POINTER
    ldi 
    ldi
    ldi
    ldi

    LD A,(MEM_FAT_DIRSEC) ;init counter for FAT sectors
    LD (MEM_FAT_COUNT1),A

    LD HL,MEM_IDE_POINTER   ;read first sector
    LD B,1
    ld a,1
    LD DE, MEM_IDE_BUFFER   ;where to store data?
    call read_lba_sector

    LD HL, MEM_IDE_BUFFER   ;set buffer start
    LD C,16                 ;set entries counter

_fat_lfs_loop:
    LD DE,[MEM_FAT_TMPFNAME]
    CALL compare_filename
    JR C, _fat_lfs_loop_compare_match   ;on match

    ; prepare next entry
    DEC C   ;next sector after 16 entries
    JR Z,_fat_lfs_loop_compare_next_sector
    LD DE, 32   ;length of entry
    ADD HL,DE   ;increment
    JP _fat_lfs_loop

_fat_lfs_loop_compare_next_sector:
    ld hl,[MEM_IDE_POINTER]
    call _fat_increment_32 ;increment sector

    LD A,(MEM_FAT_COUNT1)  ; decrement sector count (max FAT length)
    DEC A
    LD (MEM_FAT_COUNT1),A
    JP Z, _fat_lfs_loop_compare_end ; if DE is 0, mmax is reached. End here
    ;call print_a_hex

    LD HL,MEM_IDE_POINTER   ;read next sector
    LD B,1
    ld a,1
    LD DE, MEM_IDE_BUFFER   ;where to store data?
    call read_lba_sector

    LD HL, MEM_IDE_BUFFER   ;set buffer start
    LD C,16                  ;set entries counter

    ld a,(HL)
    or a
    jp z, _fat_lfs_loop_compare_end ;skip empty sectors

    JP _fat_lfs_loop

_fat_lfs_loop_compare_end:
    POP DE
    ld a,0xFF
    RET

_fat_lfs_loop_compare_match:
    ; get entry
    POP DE

    ; HL points to Start of Table item
    PUSH HL
    POP IX
    ; get important information
    ld a,(ix+0x1B)  ;first cluster number
    ld (MEM_FAT_OF0_CCLUST+1),a
    ld a,(ix+0x1A)
    ld (MEM_FAT_OF0_CCLUST+0),a
    ld a,(ix+0x0B)
    ld (MEM_FAT_OF0_ATTRIBUTE+0),a

    xor a   ;clear carry    ;set MEM_FAT_OF0_DATREM to remaining sectors
    ld a,(ix+0x1F)  ;cluste length  shift by 256
    rra
    ld (MEM_FAT_FILEREMAIN+2),a
    ld a,(ix+0x1E)
    rra
    ld (MEM_FAT_FILEREMAIN+1),a
    ld a,(ix+0x1D)
    rra
    ld (MEM_FAT_FILEREMAIN+0),a
    ld a,0
    ld (MEM_FAT_FILEREMAIN+3),a
    call fat_getfatsec  ;get sector information

    xor a
    RET


;=================== UTIL Functions ===========================
; 32 Bit addition to pointer
; HL has value
;deprecated!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
_fat_math_sector_add_16:
    ld (MEM_FAT_TMPPOINTER), hl
    xor a
    ld (MEM_FAT_TMPPOINTER+2),a
    ld (MEM_FAT_TMPPOINTER+3),a

    ld de,[MEM_FAT_TMPPOINTER]
    ld bc,[MEM_IDE_POINTER]
    call _fat_math_add32
    ret
;deprecated!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ;hl contains pointer
_fat_increment_32
    ld a,(HL)   ; byte 0
    add 1
    ld (hl),a
    inc hl
    ld a,(HL)   ; byte 1
    adc 0
    ld (hl),a
    inc hl
    ld a,(HL)   ; byte 2
    adc 0
    ld (hl),a
    inc hl
    ld a,(HL)   ; byte 3
    adc 0
    ld (hl),a
    ret
    ;bc contains pointer to a (also result)
    ;de contains pointer to b
_fat_math_add32
    push hl
    push bc
    push de
    ld a,(de)   ; load lower 16bit for B int from (DE) to HL
    ld l,a
    inc de
    ld a,(de)
    ld h,a
    inc de
    ; HL, DE dirty
    ld a,(bc)   ; load lower 16bit for A int from (BC) to DE
    ld e,a
    inc bc
    ld a,(bc)
    ld d,a
    ; HL now contains A, DE now contains D
    add hl,de   ;add lower bytes, store carry
    pop de  ;restore pointers
    pop bc  ;both now cointain first byte of long-value
    ld a,l      ;store lower result in (bc)
    ld (bc),a
    inc bc
    ld a,h
    ld (bc),a
    inc bc      
    inc de      ;also increment de to next byte
    inc de
    ; DE and HL now start at the upper byte
    push bc
    push de
    ld a,(de)   ; load upper 16bit for B
    ld l,a
    inc de
    ld a,(de)
    ld h,a
    inc de
    ld a,(bc)   ; load upper 16bit for A
    ld e,a
    inc bc
    ld a,(bc)
    ld d,a
    adc hl,de   ;add upper bytes, store carry
    pop de
    pop bc
    ld a,l      ;store lower result in (bc)
    ld(bc),a
    inc bc
    ld a,h
    ld(bc),a
    pop hl
    ret


; Multiply 16-bit values (with 32-bit result)
; Operands BC, DE
; Result -> BCHL
_fat_math_mul32:
    ld a,c
    ld c,b
    ld hl,0
    ld b,16
_fat_math_mul32_l:
    add hl,hl
    rla
    rl c
    jr nc,_fat_math_mul32_noadd
    add hl,de
    adc a,0
    jp nc,_fat_math_mul32_noadd
    inc c
_fat_math_mul32_noadd:
    djnz _fat_math_mul32_l
    ld b,c
    ld c,a
    ret

; reset LBA pointer to first sector in selected partition
fat_reset_pointer:
    call ideif_get_drv_pointer
    inc ix
    inc ix
    push ix
    pop hl  ;copy poiter to hl
    ld de, MEM_IDE_POINTER
    jr fat_copy_lba_pointer

; resets LBA pointer (4-byte) to partition start
; HL = from here
; DE = to this destimation
fat_copy_lba_pointer:
    PUSH BC
    LD B,0
    LD C,4
    LDIR
    POP BC
    ret

; compares filenames
; HL points to name1
; DE points to name2
; Carry is set if match
; Destroys DE, AF
compare_filename:
    PUSH HL
    push BC
    LD B, 11    ;Counter
_compare_filename_loop:
    LD A,(DE)
    LD C,A
    LD A,(HL)
    INC HL
    INC DE
    XOR C   ;check if identical (should return 0)
    JR NZ, _compare_filename_nomatch   
    djnz _compare_filename_loop   ;if not last, continue
    POP BC
    POP HL
    SCF
    RET
_compare_filename_nomatch:
    POP BC
    POP HL
    XOR A   ; clear carry flag
    RET

; formats filename to 8+3 format
; DE points to source filename to string
; HL points to destination
format_filename_fat16:
    LD B, 11 ;counter
    PUSH HL
    LD A, ' '
_format_filename_fat16_clean:
    LD (HL),A
    INC HL
    DJNZ _format_filename_fat16_clean
    POP HL ; continue with copy
    LD B, 13   
_format_filename_fat16_loop:
    LD A, (DE)  ; load byte
    OR A
    RET Z ;exit on 0byte
    DEC B                               ;reduce counter
    RET Z ;exit after 12 bytes 8+.+3
    CP '.' ; check if dot 
    JR NZ, _format_filename_fat16_loop_copy ; if not continue as usual
    INC DE  ;else skip char
_format_filename_fat16_loop_skip_8:
    LD A,B
    CP 5
    JR C, _format_filename_fat16_loop
    INC HL
    DEC B
    JR _format_filename_fat16_loop_skip_8

_format_filename_fat16_loop_copy:
    LD A, (DE)  ; load byte
    LD (HL), A  ; copy byte
    INC HL
    INC DE
    JP _format_filename_fat16_loop