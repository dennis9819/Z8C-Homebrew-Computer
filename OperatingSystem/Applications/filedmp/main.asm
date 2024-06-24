.include "extern_symbols.s" ;include monitor symbols.
    START_ADDR   .EQU 0x8000
    

    org START_ADDR  
com_header:
    jp com_prg
    db 0x00
    dw 0x00, 0x00, 0x00   ;always 0
    dw [START_ADDR]     ;start addr
    dw [_eof]           ;end of file
    dw [_eof - START_ADDR]  ;length
    dc 48,0x00
    
com_prg:
    ld hl,[var_input]   ;find end of command name
    ld bc,80
    ld a,' '
    cpir  
    ld bc,80
    ld a,' '
    cpir  
    push hl

    pop hl
    ;HL now has pointer to file name
    ex de,hl
    call fat_openfile       ;find file
    or a
    jp nz, _com_prg_fail    ;if not found, exit
    ;prepare 32bit counter
    xor a
    ld (var_scratch), a
    ld (var_scratch+1), a
    ld (var_scratch+2), a
    ld (var_scratch+3), a

    ;printloop
_com_prg_loop:              ;main loop, load sector
    ld de,[MEM_IDE_BUFFER]
    call fat_readfilesec    ;read sector
    push af
    ld hl, [_com_prg_fail_str_header]
    call print_str          ;print header
    ld hl, [MEM_IDE_BUFFER]
    ld de, 512              ; set sector byte counter
    ld (var_scratch+4),de
_com_prg_loop_row:
    call print_newLine
    ld a, (var_scratch+3)   ;print offset
    call print_a_hex
    ld a, (var_scratch+2)
    call print_a_hex
    ld a, (var_scratch+1)
    call print_a_hex
    ld a, (var_scratch)
    call print_a_hex
    ld a, ' '
    call print_char
    ld a, '|'
    call print_char
    ld a, ' '
    call print_char
    ld a,(var_scratch)      ;increment total byte counter
    add 16
    ld (var_scratch),a
    ld a,(var_scratch+1)   ; byte 1
    adc 0
    ld (var_scratch+1),a
    ld a,(var_scratch+2)   ; byte 2
    adc 0
    ld (var_scratch+2),a
    ld a,(var_scratch+3)   ; byte 3
    adc 0
    ld (var_scratch+3),a
    ;now start printing data (512 bytes)
    ld b, 16                ; bytes per column
    push hl
_com_prg_loop_column:
    ld a, (hl)              ;print value
    call print_a_hex
    ld a, ' '               ;print seperator
    call print_char
    inc hl                  ;increment current byte pointer
    ld de,(var_scratch+4)   ;decrement sector byte counter
    dec de
    ld (var_scratch+4),de
    djnz _com_prg_loop_column   ;loop 16 times
    ld a, '|'
    call print_char
    ld a, ' '
    call print_char
    pop hl
    ld b, 16                ; bytes per column
_com_prg_loop_column_ascii:
    ld a, (hl) 
    inc hl
    cp 32
    jp c, _com_prg_loop_column_ascii_none    ;if less than 32, it is not a char
    cp 127
    jp nc, _com_prg_loop_column_ascii_none   ;if greater or equal than 128, it is not a char
    call print_char
    jr _com_prg_loop_column_ascii_le
_com_prg_loop_column_ascii_none:
    ld a,'.'
    call print_char
_com_prg_loop_column_ascii_le:
    djnz _com_prg_loop_column_ascii
    ld a, ' '
    call print_char
    ld a, '|'
    call print_char
    ;next row:
    ld a,d                  ;if sector byte counter is not 0
    or e
    jp nz, _com_prg_loop_row ;next row
    ;else read next sector or exit
    pop af                  ;if status from sector read
    or a
    jp z, _com_prg_loop     ;sector available
    ;else exit
    call print_newLine
    ret

_com_prg_fail:
    ld hl,[_fat_exec_notfound]
    call print_str
    ret


_com_prg_fail_str_header:
    db 10,13,'OFFSET   | 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F  | ASCII',0
prg_end:

_eof: