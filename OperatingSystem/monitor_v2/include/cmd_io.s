OP_IO_IN:
    push DE
    pop HL
    call DHEX_TO_BYTE  
    ld c,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, ERR_SYNTAX
    in a,(c)
    push af
    call print_newLine
    pop af
    call print_a_hex
    ret

OP_IO_OUT:
    push DE
    pop HL
    call DHEX_TO_BYTE  
    ld c,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, ERR_SYNTAX
    inc hl
    inc hl
    ld a,(hl)
    cp ' '
    jp nz, ERR_SYNTAX
    inc hl
    call DHEX_TO_BYTE 
    push af
    ld a,e                  ;check for error
    and a
    jp nz, ERR_SYNTAX
    pop af
    out (c),a    
    ret

OP_IIC_OUT:
    push DE
    pop HL
    call DHEX_TO_BYTE       ;load start addr
    ld (var_scratch+1),A    ;store result in ram
    ld a,e                  ;check for error
    and a
    jp nz, ERR_SYNTAX
    inc hl
    inc hl
    LD BC,[var_scratch + 2]
    XOR A
    LD (var_scratch),A  
_OP_IIC_OUT_LOOP:
    ld a,(hl)
    cp 0    ;if 0 then end
    jp z,_OP_IIC_OUT_SEND
    cp ' '
    jp nz, ERR_SYNTAX
    inc hl  ;next byte

    call DHEX_TO_BYTE
    ld (bc),a
    inc bc  ;incerement pointer
    ld a,(var_scratch)
    inc a   ;increment  counter
    ld (var_scratch),a
    inc HL
    inc HL
    jr _OP_IIC_OUT_LOOP
_OP_IIC_OUT_SEND:
    ld hl,[var_scratch + 2]
    ld a,(var_scratch)      ;load amount of bytes
    ld b,a
    ld a,(var_scratch+1)    ;load start addr
    ld c,a
    call iic_send_buffer
    or a
    jp nz, _OP_IIC_ACK_ERR
    ret

_OP_IIC_ACK_ERR:
    LD HL,[_OP_IIC_ACK_ERR_str]
    call print_str
    ret
_OP_IIC_ACK_ERR_str:
    db 10,13,"bus-error: no ACK",0

OP_IIC_IN:
    push DE
    pop HL
    call DHEX_TO_BYTE       ;load start addr
    ld C,a                  ;store start addr to B
    ld a,e                  ;check for error
    and a
    jp nz, ERR_SYNTAX
    inc hl
    inc hl
    ld a,(hl)
    cp ' '
    jp nz, ERR_SYNTAX
    inc hl      
    call DHEX_TO_BYTE       ;read length
    ld b,a                  ;store length in B
    push bc
    ld a,e                  ;check for error
    and a
    jp nz, ERR_SYNTAX
    ld hl,[var_scratch]
    call iic_receive_buffer
    pop bc
    or a
    jp nz, _OP_IIC_ACK_ERR
    ld hl,[_OP_IIC_IN_LOOP_TEXT]
    call print_str
    ld hl,[var_scratch]
    ;print data
_OP_IIC_IN_LOOP:
    ld a,(hl)
    call print_a_hex
    ld a, ' '
    call print_char
    inc hl
    djnz _OP_IIC_IN_LOOP
    ret

_OP_IIC_IN_LOOP_TEXT:
    db 10,13,"rec-buff: ",0

OP_CLR:
    call print_clear
    ret