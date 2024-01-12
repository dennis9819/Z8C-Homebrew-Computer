; HL contains start address
; B  contains length
disassemble:
    call dasm_print16hex_addr   ;print address  (HL)
    call disassemble_table_seek
    xor a
    or b
    or c
    jr z, disassemble_err   ;if bc==0000h

    ld a,(hl)   ;load first byte





    call dasm_print8hex         ;print value
    ld a,(hl)
    cp 0xC3         ;C3 JPnn
    jp z, dasm_C3

    ;start getting value

disassemble_err:
    ld a,(hl)
    call dasm_print8hex         ;print value
    push hl
    ld hl, [dasm_UU]
    call print_str
    pop hl
disassemble_continue:
    call print_newLine
    inc hl
    dec b
    jp nz, disassemble
    ret



;A contains char
;BC contains returned position
disassemble_table_seek:
    push hl
    ld c,a
disassemble_table_seek_loop:
    ld a,(hl)
    cp c    ; if match
    jr z, disassemble_table_found
    or a    ; if null
    jp z, disassemble_table_notfound
    ld b,0
    ld c,4
    add hl,bc
    ld a,(hl)
    ld c,a
    add hl,bc
    inc hl
    jr disassemble_table_seek_loop


disassemble_table_found
    ld b,H
    ld c,l
    pop hl
    ret

disassemble_table_notfound
    ld b,0
    ld c,0
    pop hl
    ret



dasm_C3: ;JP nn (276)
    inc hl
    ld e, (HL)
    inc hl
    ld d, (HL)
    inc hl
    ;de now has jmp value
    push hl
    ld hl, [dasm_C3_str]
    call print_str
    ld h,d
    ld l,e
    call dasm_print16hex_addr
    pop hl
    jp disassemble_continue
dasm_C3_str:
    db "JP ",0x00

dasm_JPccnn: ;JP nn (276)
    rra
    rra
    rra
    and 0x07
    call dasm_printFlags
    ld a, ","
    call print_char
    inc hl
    ld e, (HL)
    inc hl
    ld d, (HL)
    inc hl
    ;de now has jmp value
    push hl
    ld hl, [dasm_C3_str]
    call print_str
    ld h,d
    ld l,e
    call dasm_print16hex_addr
    pop hl
    jp disassemble_continue

dasm_18: ;JR e
    inc hl
    ld e, (HL)
    inc hl
    ;de now has jmp value
    push hl
    ld hl, [dasm_18_str]
    call print_str
    ld a,e
    call dasm_print8relhex
    pop hl
    jp disassemble_continue
dasm_18_str:
    db "JR ",0x00

dasm_38: ;JR C,e
    inc hl
    ld e, (HL)
    inc hl
    ;de now has jmp value
    push hl
    ld hl, [dasm_38_str]
    call print_str
    ld a,e
    call dasm_print8relhex
    pop hl
    jp disassemble_continue
dasm_38_str:
    db "JR C, ",0x00

dasm_30: ;JR NC,e
    inc hl
    ld e, (HL)
    inc hl
    ;de now has jmp value
    push hl
    ld hl, [dasm_30_str]
    call print_str
    ld a,e
    call dasm_print8relhex
    pop hl
    jp disassemble_continue
dasm_30_str:
    db "JR NC, ",0x00

dasm_28: ;JR Z,e
    inc hl
    ld e, (HL)
    inc hl
    ;de now has jmp value
    push hl
    ld hl, [dasm_28_str]
    call print_str
    ld a,e
    call dasm_print8relhex
    pop hl
    jp disassemble_continue
dasm_28_str:
    db "JR Z, ",0x00

dasm_20: ;JR NZ,e
    inc hl
    ld e, (HL)
    inc hl
    ;de now has jmp value
    push hl
    ld hl, [dasm_20_str]
    call print_str
    ld a,e
    call dasm_print8relhex
    pop hl
    jp disassemble_continue
dasm_20_str:
    db "JR NZ, ",0x00

dasm_E9: ;JR NZ,e
    inc hl
    push hl
    ld hl, [dasm_20_str]
    call print_str
    pop hl
    jp disassemble_continue
dasm_E9_str:
    db "JP (HL), ",0x00

dasm_E9: ;JP (HL)
    inc hl
    push hl
    ld hl, [dasm_E9_str]
    call print_str
    pop hl
    jp disassemble_continue
dasm_E9_str:
    db "JP (HL)",0x00

dasm_E9: ;JP (IX)
    inc hl
    push hl
    ld hl, [dasm_E9_str]
    call print_str
    pop hl
    jp disassemble_continue
dasm_E9_str:
    db "JP (IX)",0x00

dasm_E9: ;JP (IY)
    inc hl
    push hl
    ld hl, [dasm_E9_str]
    call print_str
    pop hl
    jp disassemble_continue
dasm_E9_str:
    db "JP (IY)",0x00


dasm_00: ;JP nn (276)
    inc hl
    push hl
    ld hl, [dasm_00_str]
    call print_str
    pop hl
    jp disassemble_continue
dasm_00_str:
    db "NOP",0x00

dasm_FF: ;JP nn (276)
    inc hl
    push hl
    ld hl, [dasm_FF_str]
    call print_str
    pop hl
    jp disassemble_continue
dasm_FF_str:
    db "---",0x00


dasm_print16hex_addr:
    ld a,"$"
    call print_char
    ld a,h 
    call print_a_hex
    ld a,l
    call print_a_hex
    ld a,"h"
    call print_char
    ld a," "
    call print_char
    ret

dasm_print8hex:
    call print_a_hex
    ld a,"h"
    call print_char
    ld a," "
    call print_char
    ret

dasm_print8relhex:
    push af
    and 0x80
    jp nz, dasm_print8relhex_neg
    ld a,"$"
    call print_char
    ld a,"+"
    call print_char
    pop af
    call print_a_hex
    ld a,"h"
    call print_char
    ret

dasm_print8relhex_neg:
    ld a,"$"
    call print_char
    ld a,"-"
    call print_char
    pop af
    neg
    call print_a_hex
    ld a,"h"
    call print_char
    ret


dasm_printFlags:
    push hl
    ld hl, [dasm_printFlags_table]
    rlca
    ld b,0
    ld c,a
    add hl,bc
    call print_str
    ld a, " "
    call print_char
    pop hl
    ret

dasm_printFlags_table:
    db "NZ",0
    db "Z",0,0
    db "NC"
    db "C",0,0
    db "PO",0
    db "PE",0
    db "P",0,0
    db "M",0,0