

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

; reads single sector of file
; must run fat_readfilesec before to initialize
; if a ix 0x00, success
; if a is 0xFF, end reached
fat_readfilesec:
    call fat_print_dbg
    ld hl,[MEM_FAT_OF0_DATSEC]
    ld b,1
    LD DE, MEM_IDE_BUFFER   ;where to store data?
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
; FAT open file
;
; DE pointer to file name
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
    ;LD HL, [str_file_notfound] 
    ;CALL print_str  ;print 
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
    ;call print_newLine

    ;LD B,8
    ;call print_str_fixed
    ;ld A,'.'
    ;call print_char
    ;LD B,3
    ;call print_str_fixed
    ;LD HL, [str_file_found]
    ;CALL print_str  ;print 
    xor a
    RET


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

str_file_notfound:
    db "File not found!",13,10,0

str_file_found:
    db " File located!",13,10,0