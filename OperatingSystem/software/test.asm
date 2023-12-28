.include "extern_symbols.s" ;include monitor symbols.
    org 0xB000  
;Testing code

    ;LD HL,MEM_IDE_BUFFER 
    ;LD B,32
    ;call dump_pretty
    call find_partition
    ;call fat_get_root_table
    ;call fat_print_directory

    call fat_get_root_table
    LD DE, [str1]
    CALL fat_lfs
    
    JP PROMPT_BEGIN

str1:
    db "ILLUSION.PSG",0
str2:
    db "HALLOWLT.TXT",0
str3:
    db "TEST",0
str4:
    db ".ORG",0

delay_small:
    PUSH AF
    POP AF
    PUSH AF
    POP AF
    RET

find_partition:
    ;read bootsector
    LD A,1                  ;read 1 sector
    LD B,IDE_REG_SECTOR
    CALL ide_regwrite_8

    LD A,1                  ;read sector 0
    LD B,IDE_REG_SSECTOR
    CALL ide_regwrite_8

    LD A,0                  ;read cylinder 0
    LD B,IDE_REG_LCYL
    CALL ide_regwrite_8
    LD A,0                  
    LD B,IDE_REG_HCYL
    CALL ide_regwrite_8

    LD A,10100000b          ;read head 0
    LD B,IDE_REG_HEAD
    CALL ide_regwrite_8

    LD A,IDE_CMD_READSEC    ;send read command
    LD B,IDE_REG_CMDSTS
    CALL ide_regwrite_8

    LD HL,MEM_IDE_BUFFER    ;set read/write buffer start address
    call ide_readsector_512_inv ;read 256 words from device

    LD B,4                      ;Partition table length
    LD C,0                      ;Partition ID counter
    LD IX,MEM_IDE_BUFFER+446    ;Load offest of first partition table entry
find_partition_loop:
    LD A,(IX+4) ;load status byte
    OR A
    JP NZ, find_partition_process    ;If not zero, jump to print function
    jp find_partition_next

find_partition_next:
    LD A,10                 ;New line
    CALL print_char
    LD A,13
    CALL print_char
    LD DE,16
    ADD IX,DE
    DJNZ find_partition_loop    
    RET

find_partition_process: ; process table entry
    ld hl, [str_part_seek_1]
    call print_str  ;print 
    LD A,(IX+0x04) ;load type
    call print_a_hex
    LD A,(IX+0x04) ;load type
    CP 0x0E
    JR NZ, find_partition_next

    ld hl, [str_part_seek_2]
    call print_str  ;print 
    ld hl, [str_part_seek_3]
    call print_str  ;print 

    LD A,(IX+0x08) ;load start LBA
    LD (MEM_IDE_PARTITION+3),A
    LD A,(IX+0x09) ;load start LBA
    LD (MEM_IDE_PARTITION+2),A
    LD A,(IX+0x0A) ;load start LBA
    LD (MEM_IDE_PARTITION+1),A
    LD A,(IX+0x0B) ;load start LBA
    LD (MEM_IDE_PARTITION+0),A

    LD A,(MEM_IDE_PARTITION+3)
    call print_a_hex
    LD A,(MEM_IDE_PARTITION+2)
    call print_a_hex
    LD A,(MEM_IDE_PARTITION+1)
    call print_a_hex
    LD A,(MEM_IDE_PARTITION+0)
    call print_a_hex
    ld hl, [str_part_seek_4]
    call print_str  ;print 
    LD A,(IX+0x0C) ;load count LBA
    call print_a_hex
    LD A,(IX+0x0D) ;load count LBA
    call print_a_hex
    LD A,(IX+0x0E) ;load count LBA
    call print_a_hex
    LD A,(IX+0x0F) ;load count LBA
    call print_a_hex
    LD A,10                 ;New line
    CALL print_char
    LD A,13
    CALL print_char
    RET

str_part_seek_1:
    db "- Type: 0x",0
str_part_seek_2:
    db " State: ",0
str_part_seek_3:
    db " LBA: 0x",0
str_part_seek_4:
    db " Len: 0x",0
str_sum:
    db "------------",10,13,0
str_files:
    db " Files",10,13,0

.include "include/fat16.s"