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
; Print current fat directory of MEM_FAT_CURRDIR
;-------------------------------------
fat_print_directory:
    ld hl,MEM_FAT_CURRDIR
    ld de,MEM_IDE_POINTER
    ldi 
    ldi
    ldi
    ldi
    
    LD DE,(MEM_FAT_SECTORS)
    LD (MEM_FAT_COUNT1),DE
    LD HL,MEM_IDE_POINTER   ;read first sector
    LD B,1
    LD DE, MEM_IDE_BUFFER   ;where to store data?
    call read_lba_sector

    call PRINTINLINE
    db 10,13,"  Filename     Cluster Size",10,13,0

    LD HL, MEM_IDE_BUFFER   ;set buffer start
    LD C,16                  ;set entries counter

_fat_print_directory_loop:  ;loop over each entry (32byte)
    LD A,(HL) ; check first byte
    PUSH HL ;backup start of entry
    POP IX
    PUSH HL
    ;ignore unwanted entries
    CP 0x41 ;skip invisible
    JP Z, _fat_print_directory_loop_next    
    CP 0xE5 ;skip deleted
    JP Z, _fat_print_directory_loop_next
    CP 0x00 ;reached end
    JP Z, _fat_print_directory_loop_break

    ;check file attribute
    ld a,(IX+0x0B)
    cp 0x10 ;if subdirectors
    jp z, _fat_print_directory_dir  ;print dir 
    ;else print file
_fat_print_directory_loop_file
    ;print filename
    ld a,' '
    call print_char
    ld a,' '
    call print_char
    LD B,8
    call print_str_fixed
    ld A,'.'
    call print_char
    LD B,3
    call print_str_fixed

    call PRINTINLINE
    db " 0x",0
    ;first cluster number
    ld a,(ix+0x1B)
    call print_a_hex
    ld a,(ix+0x1A)
    call print_a_hex
    call PRINTINLINE
    db "  0x",0
    ld a,(ix+0x1F)
    call print_a_hex
    ld a,(ix+0x1E)
    call print_a_hex
    ld a,(ix+0x1D)
    call print_a_hex
    ld a,(ix+0x1C)
    call print_a_hex

    LD A,10                 ;New line
    CALL print_char
    LD A,13
    CALL print_char
    jr _fat_print_directory_loop_next
_fat_print_directory_dir
    ld a,'D'
    call print_char
    ld a,' '
    call print_char
    LD B,8
    call print_str_fixed
    call PRINTINLINE
    db "     0x",0
    ;first cluster number
    ld a,(ix+0x1B)
    call print_a_hex
    ld a,(ix+0x1A)
    call print_a_hex

    LD A,10                 ;New line
    CALL print_char
    LD A,13
    CALL print_char
    jr _fat_print_directory_loop_next

_fat_print_directory_loop_next: ; read next entry
    DEC C   ;next sector after 32 entries
    JR Z,_fat_print_directory_loop_next_sector
    POP HL      ;restore start
    LD DE, 32   ;length of entry
    ADD HL,DE   ;increment
    JP _fat_print_directory_loop

_fat_print_directory_loop_next_sector:  ; end fo sector. read next sector from disk
    POP HL      ;clear stack from old hl
    LD H,0
    LD L,1      
    call _fat_math_sector_add_16 ;increment sector

    LD DE,(MEM_FAT_COUNT1)  ; decrement sector count (max FAT length)
    DEC DE
    LD (MEM_FAT_COUNT1),DE
    LD A,D
    OR E
    JP Z, _fat_print_directory_loop_break_dirty ; if DE is 0, mmax is reached. End here
    
    LD HL,MEM_IDE_POINTER   ;read next sector
    LD B,1
    
    LD DE, MEM_IDE_BUFFER   ;where to store data?
    call read_lba_sector

    LD HL, MEM_IDE_BUFFER   ;set buffer start
    LD C,16                  ;set entries counter
    JP _fat_print_directory_loop

_fat_print_directory_loop_break
    POP HL
_fat_print_directory_loop_break_dirty
 ;   ld hl, [str_sum] 
 ;   call print_str  ;print 
 ;   ld a,c
 ;   call print_a_hex
 ;   ld hl, [str_files]
 ;   call print_str  ;print 
    ret

; fat change directory
; relative path 
; DE pointer to path
fat_cd_single:
    push de
    ; check if user wants to go back (input = '..')
    ld a,(de)
    cp '.'
    jr nz,  _fat_cd_navigate; if not, skip
    inc de  ;check next
    ld a,(de)
    cp '.'
    jr nz,  _fat_cd_navigate; if not, skip
    ld a,(var_dir+79)   ;last byte contains depth
    or a;   Test if 0
    jp z, _fat_cd_navigate_error    ;cannot go back any more (already at root)
    ; check if .. exists in directory
    ld a,'.'    ;prepare filename buffer
    ld hl,[MEM_FAT_TMPFNAME]
    ld (hl),a
    inc hl
    ld (hl),a
    inc hl
    ld a,0x20   ;clear char 3-11
    ld b,11
_fat_cd_navigate_goback_fl:
    ld (hl),a
    inc hl
    djnz _fat_cd_navigate_goback_fl ;fill loop end
    call fat_openfile_noprepare     ;load file table (only 1st sector needed)
    or a                            ;check for error
    jp nz, _fat_cd_navigate_error   ;entry not found exception


    ; find end of path
    ld hl,[var_dir+3]   ;current position
    ld bc,76
    ld a,0x00           ;termination char
    cpir                ;find end
    jp po,_fat_cd_navigate_inerror    ;in case of error, abort
    ;hl is now at end of string
    ld bc,76
    ld a,'\'            ;seperation char
    cpdr                ;serach backwards for "/"
    jp po,_fat_cd_navigate_inerror    ;in case of error, abort
    ;hl is now at end of string
    inc hl
    xor a
    ld (hl),a           ;set termination char
    inc hl
    ld (hl),a           ;set termination char
    ld a,(var_dir+79)
    dec a
    ld (var_dir+79),a   ;decrement dir depth counter

    pop de

    ld hl,[var_dir+2]   
    ld a,'\'
    ld (hl),a           ;set first /

    ld hl,MEM_FAT_OF0_DATSEC        ;setup directory pointer
    ld de,MEM_FAT_CURRDIR
    ldi 
    ldi
    ldi
    ldi

    ret

_fat_cd_navigate
    pop de              ;get pointer to directory namme
    push de             ;and re-store it for next use
    call fat_openfile   ;find 'file' in current directory
_fat_cd_navigate_findsec
    or a
    jp nz, _fat_cd_navigate_error   ;entry not found
    ld a, (MEM_FAT_OF0_ATTRIBUTE)
    cp 0x10
    jp nz, _fat_cd_navigate_errfile
    ld a,(var_dir+79)
    inc a
    ld (var_dir+79),a   ;increment dir depth counter
    ld hl,[var_dir+2]   ;load start of path string
    ld a,0              ;load termination char
    ld bc,76            ;max length of string
    cpir                ;find end of path string
    dec hl
    jp po,_fat_cd_navigate_inerror    ;in case of error, abort
    ;HL now has last element, BC has remaining max length
    ld a,(var_dir+79)   ;last byte contains depth
    cp 1                ;if first path, skip /
    jr z, _fat_cd_navigate_findsec_skipslash
    ld a,'\'
    ld (hl),a
    inc hl
_fat_cd_navigate_findsec_skipslash
    pop de              ;get argument from stack
    ex de,hl
    push de             ;store start to stack
    ;HL now has start of input string, DE has end of current path
    ld bc,09            ;maximum length of directory name +1
_fat_cd_navigate_l2:    ;copy new subdirectory
    ldi                 ;copy
    jp po,_fat_cd_navigate_inerrorS    ;in case of error, abort    
    ld a,(hl)           ;check next char
    cp '\'              ;end at '\'
    jr z, _fat_cd_navigate_end  ;else next byte
    or a                ;or and at 0x00
    jr z, _fat_cd_navigate_end  ;else next byte
    jr _fat_cd_navigate_l2
_fat_cd_navigate_end:
    xor a
    ld (de),a           ;set last byte to 0x00 (termination)
    ld hl,MEM_FAT_OF0_DATSEC
    ;setup directory pointer
    ld de,MEM_FAT_CURRDIR
    ldi 
    ldi
    ldi
    ldi
    pop de              ;stack cleanup
    ret

_fat_cd_navigate_error:
    ld hl,[_fat_cd_navigate_error_str]
    call print_str
    pop de
    ret

_fat_cd_navigate_inerrorS:  ;with path reset
    pop de              ;restore former path
    dec de              ;change pointer to remove previous '\' as well
    xor a               ;clear a to 0x00
    ld (de),a           ;set last byte to 0x00 (termination)
    jr _fat_cd_navigate_inerrore
_fat_cd_navigate_inerror:   ;without path reset
    pop de
_fat_cd_navigate_inerrore:  
    ld hl,[_fat_cd_navigate_inputerr_str]
    call print_str
    ret
_fat_cd_navigate_errfile:
    pop de
    ld hl,[_fat_cd_navigate_errfile_str]
    call print_str
    ret

_fat_cd_navigate_error_str:
    db 10,13,"No such directory!",10,13,0
_fat_cd_navigate_inputerr_str:
    db 10,13,"Invalid input!",10,13,0
_fat_cd_navigate_errfile_str:
    db 10,13,"Cannot cd to file!",10,13,0

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

