fat_print_dbg:
    call PRINTINLINE
    db 10,13,"PTR.MEM_IDE_POINTER:    0x",0
    ld ix,MEM_IDE_POINTER
    call print_32_hex
    call PRINTINLINE
    db "  |  PTR.MEM_IDE_PARTITION:   0x",0
    ld ix,MEM_IDE_PARTITION
    call print_32_hex

    call PRINTINLINE
    db 10,13,"PTR.MEM_FAT_TMPPOINTER: 0x",0
    ld ix,MEM_FAT_TMPPOINTER
    call print_32_hex
    call PRINTINLINE
    db "  |  PTR.MEM_FAT_DATASTART:   0x",0
    ld ix,MEM_FAT_DATASTART
    call print_32_hex

    call PRINTINLINE
    db 10,13,"VAL.MEM_FAT_RESERVED:   0x",0
    ld ix,MEM_FAT_RESERVED
    call print_16_hex
    call PRINTINLINE
    db "      |  VAL.MEM_FAT_AMOUNT:      0x",0
    ld a,(MEM_FAT_AMOUNT)
    call print_a_hex

    call PRINTINLINE
    db 10,13,"VAL.MEM_FAT_SECTORS:    0x",0
    ld ix,MEM_FAT_SECTORS
    call print_16_hex
    call PRINTINLINE
    db "      |  VAL.MEM_FAT_COUNT1:      0x",0
    ld a,(MEM_FAT_COUNT1)
    call print_a_hex

    call PRINTINLINE
    db 10,13,"VAL.MEM_FAT_OF0_CCLUST: 0x",0
    ld ix,MEM_FAT_OF0_CCLUST
    call print_16_hex
    call PRINTINLINE
    db "      |  PTR.MEM_FAT_OF0_FATSEC:  0x",0
    ld ix,MEM_FAT_OF0_FATSEC
    call print_32_hex

    call PRINTINLINE
    db 10,13,"VAL.MEM_FAT_OF0_DATSEC: 0x",0
    ld ix,MEM_FAT_OF0_DATSEC
    call print_32_hex
    call PRINTINLINE
    db "  |  PTR.MEM_FAT_OF0_DATREM:  0x",0
    ld ix,MEM_FAT_OF0_DATREM
    call print_16_hex

    call PRINTINLINE
    db 10,13,"VAL.MEM_FAT_ROOTSTART:  0x",0
    ld ix,MEM_FAT_ROOTSTART
    call print_32_hex
    call PRINTINLINE
    db "  |  VAL.MEM_FAT_CLUSTERLEN:  0x",0
    ld a,(MEM_FAT_CLUSTERLEN)
    call print_a_hex

    call PRINTINLINE
    db 10,13,"VAL.MEM_FAT_FILEREMAIN: 0x",0
    ld ix,MEM_FAT_FILEREMAIN
    call print_32_hex
    call PRINTINLINE
    db "  |  VAL.MEM_FAT_DIRSEC:      0x",0
    ld ix,MEM_FAT_DIRSEC
    call print_16_hex


    call print_newLine
    ret

