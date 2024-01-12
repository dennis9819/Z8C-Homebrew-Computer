OP_EXEC:
    ;DE contains pointer
    push DE
    pop HL
    call DHEX_TO_BYTE
    ld b,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, ERR_SYNTAX
    inc HL
    inc HL
    call DHEX_TO_BYTE
    ld c,a
    ld a,e  ;check for error
    and a
    jp nz, ERR_SYNTAX
    ld h,b
    ld l,c
    jp (hl)
OP_CALL:
    ;DE contains pointer
    push DE
    pop HL
    call DHEX_TO_BYTE
    ld b,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, ERR_SYNTAX
    inc HL
    inc HL
    call DHEX_TO_BYTE
    ld c,a
    ld a,e  ;check for error
    and a
    jp nz, ERR_SYNTAX
    ld h,b
    ld l,c
    call _OP_CALL
    call print_newLine
    ret
_OP_CALL
    jp (hl)

OP_DUMP:
    ;DE contains pointer
    push DE
    pop HL
    call DHEX_TO_BYTE       ;parse start address  
    ld b,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, ERR_SYNTAX
    inc HL
    inc HL
    call DHEX_TO_BYTE
    ld c,a
    ld a,e  ;check for error
    and a
    jp nz, ERR_SYNTAX
    inc HL
    inc HL
    ld a,(HL)
    cp ' '
    jp nz, ERR_SYNTAX
    inc HL
    call DHEX_TO_BYTE
    push af
    ld a,e  ;check for error
    and a
    jp nz, ERR_SYNTAX  
    pop af

    ld h,b
    ld l,c
    ld b,a

    call dump_pretty
    ret

OP_SET:
    ;DE contains pointer
    push DE
    pop HL
    call DHEX_TO_BYTE       ;parse start address  
    ld b,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, ERR_SYNTAX
    inc HL
    inc HL
    call DHEX_TO_BYTE
    ld c,a
    ld a,e  ;check for error
    and a
    jp nz, ERR_SYNTAX
    ;bc now contains the start address
    INC HL
    INC HL
    ;hl now cointains start addr of data bytes
_OP_SET_LOOP:
    ld a,(hl)
    cp 0    ;if 0 then end
    RET Z
    cp ' '
    jp nz, ERR_SYNTAX
    inc hl  ;next byte
    call DHEX_TO_BYTE
    ld (bc),a   ;load byte to 
    ld a,e
    and a
    jp nz, ERR_SYNTAX    
    inc bc
    inc hl
    inc hl
    jp _OP_SET_LOOP

OP_DASM:
    push DE
    pop HL
    call DHEX_TO_BYTE       
    ld b,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, ERR_SYNTAX
    inc hl
    inc hl
    call DHEX_TO_BYTE
    ld c,a
    ld a,e  ;check for error
    and a
    jp nz, ERR_SYNTAX  
    inc hl
    inc hl
    ld a,(HL)
    cp ' '
    jp nz, ERR_SYNTAX
    inc hl
    call DHEX_TO_BYTE
    push af
    ld a,e  ;check for error
    and a
    jp nz, ERR_SYNTAX  
    ld h,b
    ld l,c
    pop af  ;restore af
    ld b,a
    call disassemble
    ret