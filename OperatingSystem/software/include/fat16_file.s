

;1. find sector for given cluster
;2. read sector
;3. store first data sector to MEM_FAT_OF0_DATSEC
;4. set MEM_FAT_OF0_DATREM to amount uf sectors per cluster
;5. find next cluster in FAt and update MEM_FAT_OF0_CCLUST


; gets sector in FAT table for the cluster stored in MEM_FAT_OF0_CCLUST
; store result in MEM_FAT_OF0_FATSEC
; stores next cluster in MEM_FAT_OF0_CCLUST
fat_getfatsec:
    ld HL,(MEM_FAT_OF0_CCLUST)  ;load cluster 
    ;each sector contains 256 clusters
    ;first 8bits are not needed (/256)

    ld a,h  ;divide by 256
    ld l,a
    xor a
    ld h,a

    ld bc,(MEM_FAT_RESERVED)    ;add reserved sectors
    add hl,bc
    ld(MEM_FAT_OF0_FATSEC+0),hl;store sector to MEM_FAT_TMPPOINTER1
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

    call fat_print_dbg

    ;read FAT sector
    ld hl,MEM_FAT_OF0_FATSEC   ;read next sector
    ld b,1
    call read_lba_sector

    ld hl, MEM_IDE_BUFFER
    ld b,20
    call dump_pretty

    ;calculate offset address
    ld a,(MEM_FAT_OF0_CCLUST)
    RLA ;shift to left (x2)
    ld l, a
    ld a,0
    RLA ;shift in carry flag
    ld h,a
    ld de,MEM_IDE_BUFFER
    add hl,de
    ;copy pointer
    ld de,MEM_FAT_OF0_CCLUST
    ldi ;copy byte for next cluster from FAT
    ldi


    call fat_print_dbg

    ret
    ;store data

