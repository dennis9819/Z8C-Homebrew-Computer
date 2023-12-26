; VARS

    MEM_FAT_RESERVED    .EQU MEM_IDE_BASE + 10  ; Reserved sectors (2byte)
    MEM_FAT_AMOUNT      .EQU MEM_IDE_BASE + 12  ; Amount of FATs (1byte)
    MEM_FAT_SECTORS     .EQU MEM_IDE_BASE + 13  ; Length of FAT (2byte)

    MEM_FAT_COUNT1      .EQU MEM_IDE_BASE + 15  ;Counter Var for reading FAT (2byte)

;-------------------------------------
; Get FAT Root-Table position
;-------------------------------------
fat_get_root_table:
    call fat_reset_pointer ;reset fat pointer
    ; Load first sector on active partition
    LD HL, MEM_IDE_PARTITION    ; pointer to LBA address
    LD A,1                  ;read 1 sector
    call read_lba_sector

    ; Read and store FS Properties
    LD IX,MEM_IDE_BUFFER
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

    ;Get Root FAT
    LD A, (MEM_FAT_SECTORS+1) ; load FAT Sector size to DE
    LD D,A
    LD A, (MEM_FAT_SECTORS)
    LD E,A
    XOR A   ; clear HL
    LD H,A
    LD L,A
    LD A,(MEM_FAT_AMOUNT)  ; Load counter for multiplication
    LD B,A
_fat_get_root_table_loop:  ; multiply
    ADD HL,DE
    DJNZ _fat_get_root_table_loop

    ; add reserved sectors
    LD D,0
    LD A,(MEM_FAT_RESERVED)
    LD E,A
    ADD HL,DE

    ; add 
    call _fat_math_sector_add_16
    ret

; reset LBA pointer to first sector in partition
fat_reset_pointer:
    LD HL,MEM_IDE_PARTITION
    LD DE,MEM_IDE_POINTER
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

;-------------------------------------
; Print current fat directory of MEM_IDE_POINTER
;-------------------------------------
fat_print_directory:
    LD A,10                 ;New line
    CALL print_char
    LD A,13
    CALL print_char
    
    LD DE,(MEM_FAT_SECTORS)
    LD (MEM_FAT_COUNT1),DE
    LD HL,MEM_IDE_POINTER   ;read first sector
    LD B,1
    call read_lba_sector

    LD HL, MEM_IDE_BUFFER   ;set buffer start
    LD C,16                  ;set entries counter

_fat_print_directory_loop:  ;loop over each entry (32byte)
    LD A,(HL) ; check first byte
    PUSH HL ;backup start of entry
    ;ignore unwanted entries
    CP 0x41 ;skip invisible
    JR Z, _fat_print_directory_loop_next    
    CP 0xE5 ;skip deleted
    JR Z, _fat_print_directory_loop_next
    CP 0x00 ;reached end
    JP Z, _fat_print_directory_loop_break
    ;print filename
    LD B,8
    call print_str_fixed
    ld A,'.'
    call print_char
    LD B,3
    call print_str_fixed

    LD A,(HL) ; print attribute
    call print_char

    LD A,10                 ;New line
    CALL print_char
    LD A,13
    CALL print_char
    

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
    call read_lba_sector

    LD HL, MEM_IDE_BUFFER   ;set buffer start
    LD C,16                  ;set entries counter
    JP _fat_print_directory_loop

_fat_print_directory_loop_break
    POP HL
_fat_print_directory_loop_break_dirty
    ld hl, [str_sum] 
    call print_str  ;print 
    ld a,c
    call print_a_hex
    ld hl, [str_files]
    call print_str  ;print 

    ret
;=================== UTIL Functions ===========================
; 32 Bit addition to pointer
; HL has value
_fat_math_sector_add_16:
    LD IX,MEM_IDE_POINTER; LOAD IX to sector pointer in memory
    LD A,L
    ADD A,(IX+3)
    LD (IX+3),A
    JR NC, _fat_math_sector_add_16_2 ;if no carry, continue
    LD A,1
    ADD A,(IX+2)
_fat_math_sector_add_16_2:
    LD A,h
    ADD A,(IX+2)
    LD (IX+2),A
    RET NC  ;done when no carry
    LD A,1
    ADD A,(IX+1)
    LD (IX+1),A
    RET