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
    call print_newLine
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
    call print_newLine
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
    RET Z ; if DE is 0, mmax is reached. End here
    
    LD HL,MEM_IDE_POINTER   ;read next sector
    LD B,1
    
    LD DE, MEM_IDE_BUFFER   ;where to store data?
    call read_lba_sector

    LD HL, MEM_IDE_BUFFER   ;set buffer start
    LD C,16                  ;set entries counter
    JP _fat_print_directory_loop

_fat_print_directory_loop_break
    POP HL
    ret

;-------------------------------------
; Changes current fat directory of MEM_FAT_CURRDIR
; input is relativ path
; DE points to path
;-------------------------------------
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

fat_exec:
    push de
    ;DE has pointer to arguments
    call fat_openfile
    or a
    jp nz, _fat_exec_notfound   ;if not found, abort
    ;call fat_print_dbg
    ;load header
    ld de, MEM_IDE_BUFFER
    call fat_readfilesec


    ;ld hl, MEM_IDE_BUFFER       ;print sector
    ;ld b,0x20
    ;call dump_pretty

    ld a,(MEM_IDE_BUFFER)
    cp 0xC3
    jp nz, _fat_exec_notexec

    call PRINTINLINE
    db 10,13,"Loading ",0
    ld hl,[var_input+6]
    call print_str
    call PRINTINLINE
    db " to 0x",0
    ;get start address
    ld bc,(MEM_IDE_BUFFER + 10)
    ld a,b
    call print_a_hex
    ld a,c
    call print_a_hex
    call PRINTINLINE
    db " ... ",0
    ;bc has start addr
    ld (MEM_FAT_EXEC_CURR),bc
    ld (MEM_FAT_EXEC_START),bc

    ;get amount of sectors to load
    ld hl,(MEM_IDE_BUFFER + 14)
    ld l,h
    srl l
    ld h,0  ;divide by 512
    inc hl  ;increment because first sector is always loaded
    ; hl contains sector count
    ld (MEM_FAT_EXEC_COUNT), hl

    pop de  ; restore filename
    call fat_openfile   ;reset file information
    ;start reading
_fat_exec_readloop1:
    ld de,(MEM_FAT_EXEC_CURR)
    call fat_readfilesec
    ld hl,(MEM_FAT_EXEC_CURR)
    ld de,512
    add hl,de
    ld (MEM_FAT_EXEC_CURR),hl

    ld hl,(MEM_FAT_EXEC_COUNT)
    dec hl
    ld (MEM_FAT_EXEC_COUNT),hl
    ld a,h
    or l
    jr z, _fat_exec_read_done
    jr _fat_exec_readloop1
_fat_exec_read_done:
    call PRINTINLINE
    db "Load complete!",10,13,0
    ld hl,(MEM_FAT_EXEC_START)
    jp (hl)


_fat_exec_notfound:
    pop de
    call PRINTINLINE
    db 10,13,"File not found!",10,13,0
    ret

_fat_exec_notexec:
    pop de
    call PRINTINLINE
    db 10,13,"File is not an executable!",10,13,0
    ret