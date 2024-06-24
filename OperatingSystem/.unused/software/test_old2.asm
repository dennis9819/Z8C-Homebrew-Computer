.include "extern_symbols.s" ;include monitor symbols.
    org 0x6000  

MEM_FAT_EXEC_CURR   .equ     var_scratch+10
MEM_FAT_EXEC_COUNT  .equ     var_scratch+12
MEM_FAT_EXEC_START  .equ     var_scratch+14

fat_exec:
    ld de,[var_input+6]   ;prepare input like to mimic rom behaviour
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
    call PRINTINLINE
    db 10,13,"File not found!",10,13,0
    ret

_fat_exec_notexec:
    call PRINTINLINE
    db 10,13,"File is not an executable!",10,13,0
    ret
  


_test_loop:
    call fat_readfilesec
    push af
    ld hl, MEM_IDE_BUFFER       ;print sector
    ld b,0x20
    call dump_pretty
    ;call PRINTINLINE
    ;db 10,13,"SECREAD",10,13,0
    pop af
    or a
    jp z, _test_loop

    ;check if end of file


    ret
