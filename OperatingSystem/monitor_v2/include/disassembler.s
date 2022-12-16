var_opcode_start    equ PRG_RAM_START       ;16 bit pointer to opcode in mem
var_opcode_table    equ PRG_RAM_START+2     ;16 bit pointer to opcode in table
var_opcode          equ PRG_RAM_START+4     ;8 bit opcode value
var_opcode_x        equ PRG_RAM_START+5     ;8 bit opcode extension value
var_opcode_length   equ PRG_RAM_START+6     ;8 bit opcode length (in bytes)
var_opcode_string   equ PRG_RAM_START+7     ;16 bit pointer to opcode string
var_opcode_pcount   equ PRG_RAM_START+9    ;8 bit opcode param count
var_bytes_count     equ PRG_RAM_START+10    ;bytes to examine
; HL contains start address
; B  contains length
disassemble:
    ld a,b
    ld (var_bytes_count),a
disassemble_next: 
    

    ld (var_opcode_start), hl   ;16 bit pointer to opcode in mem
    ld a,(hl)                   ;load opcode to find in A
    ld (var_opcode), a          ;8 bit opcode value
    inc hl
    ld a,(hl)                   ;load opcode to find in A
    ld (var_opcode_x), a          ;8 bit opcode extended value
    dec hl
    call disassemble_table_seek
    ld a,b
    or c
    jp z, disassemble_err   ;if bc==0000h

    ld (var_opcode_table), bc   ;16 bit pointer to opcode in table

    ;load params
    inc bc
    inc bc
    inc bc
    inc bc
    ld a,(bc)
    ld (var_opcode_length),A    ;8 bit opcode length (in bytes)
    inc bc
    ld a, (bc)
    ld e,a
    inc bc
    ld a, (bc)
    ld d,a
    ld (var_opcode_string),de   ;16 bit pointer to opcode string
    inc bc
    ld a,(bc)
    ld (var_opcode_pcount),A    ;8 bit opcode param count

    ;values are prepared. Continue with print

    ld hl,(var_opcode_start)    ;print address
    call dasm_print16hex_addr

    ld a,(var_opcode_length)
    ld b, a

    ;print up to 4 opcode bytes
    ld hl,(var_opcode_start)    

disassemble_print_opcode_raw
    ld a,(hl)   ;load first byte
    call print_a_hex
    inc hl
    djnz disassemble_print_opcode_raw

    ld a,"h"
    call print_char
    ;fill empty spots
    ld a,(var_opcode_length)
    ld b,a
    ld a,6
    sub b
    ld b,a
    
disassemble_print_opcode_raw_fill:
    ld a," "
    call print_char
    ld a," "
    call print_char
    djnz disassemble_print_opcode_raw_fill
    ld a," "
    call print_char

    push hl
    ;print opcode
    ld hl,(var_opcode_string)
    call print_str
    

    ;print params
    ld a,(var_opcode_pcount)
    or a
    jp z, disassemble_print_opcode_params_end   ;skip if no params

    ld hl,(var_opcode_table)
    ld bc, 8
    add hl,bc   ;hl now has address of first param
    ld a,(var_opcode_pcount)
    ld b,a
disassemble_print_opcode_params_loop:
    ;ld a,(hl)   ;load param
    ;call print_a_hex
    ld a,(hl)   ;load param

    cp 0x01     
    call z, param_01
    cp 0x02
    call z, param_02
    cp 0x03
    call z, param_03
    cp 0x04
    call z, param_04
    cp 0x05
    call z, param_05
    cp 0x06
    call z, param_06
    cp 0x07
    call z, param_07
    cp 0x08
    call z, param_08
    cp 0x09
    call z, param_09
    cp 0x10
    call z, param_10
    cp 0x11
    call z, param_11
    cp 0x12
    call z, param_12
    cp 0x13
    call z, param_13
    cp 0x0A
    call z, param_0A
    ;strings
    cp 0x80
    call z, param_80
    cp 0x81
    call z, param_81
    inc hl
    djnz disassemble_print_opcode_params_loop
disassemble_print_opcode_params_end:
    pop hl
    jr disassemble_continue


disassemble_err:
    call dasm_print16hex_addr
    ld a,(hl)
    call dasm_print8hex         ;print value
    inc hl
    push hl
    ld hl, [dasm_UU]
    call print_str
    pop hl

disassemble_continue:
    
    call print_newLine
    ;inc hl
    ld a,(var_bytes_count)
    dec a
    ld (var_bytes_count),a
    jp nz, disassemble_next
    ret



;A contains opcode
;BC contains returned position
disassemble_table_seek:
    push hl
    ld hl, [dasm_opcode_table]
disassemble_table_seek_loop:
    ld a,(var_opcode)
    ld c,a
    ld a,(hl)
    cp 0xFF ; if null
    jp z, disassemble_table_notfound
    
    ;apply mask
    push af
    inc hl
    ld b,(hl)   ;load mask
    dec hl
    ld a,c
    and b       ;apply mask
    ld c,a
    pop af

    cp c    ; if match
    jr z, disassemble_table_first_match
    ld b,0
    ld c,7
    add hl,bc
    ld a,(hl)
    ld c,a
    add hl,bc
    inc hl
    jr disassemble_table_seek_loop

disassemble_table_first_match
    inc hl
    inc hl
    ld c,(hl) ;load opcode x from table
    inc hl
    ld a,(var_opcode_x) ;load current opcode x
    ld b,(hl)   ;load mask
    and b   ;apply mask
    cp c    ;compare to table
    dec hl
    dec hl
    dec hl
    jr z, disassemble_table_found   ;IF FOUND
    ld b,0  ;else continue with next

    ld c,7
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







param_01:   ; 0x01 16bit address pointer
    push hl
    ld de,(var_opcode_start)
    inc de
    ld a,(de)
    ld l,a
    inc de
    ld a,(de)
    ld h,a

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
    call param_comma
    pop hl
    ret

param_02:
    push bc
    ld de,(var_opcode_start)
    ld a,(de)
    rra
    rra
    and 0x0E

    push hl
    ld hl, [dasm_printFlags_table]
    ld b,0
    ld c,a
    add hl,bc
    call print_str
    ld a, " "
    call print_char
    pop hl
    pop bc
    ret


param_03:
    ld de,(var_opcode_start)
    inc de
    ld a,(de)

    push af
    and 0x80
    jp nz, param_03_neg
    ld a,"$"
    call print_char
    ld a,"+"
    call print_char
    pop af
    call print_a_hex
    ld a,"h"
    call print_char
    jr param_03_done

param_03_neg:
    ld a,"$"
    call print_char
    ld a,"-"
    call print_char
    pop af
    neg
    call print_a_hex
    ld a,"h"
    call print_char
    jr param_03_done

param_03_done:
    call param_comma
    ret


param_04:
    ld de,(var_opcode_start)
    inc de
    ld a,(de)
    cp 0x4D
    jr z,param_04_i
    ld a,"N"
    call print_char
    ret
param_04_i:
    ld a,"I"
    call print_char
    ret

param_05:
    push bc
    ld de,(var_opcode_start)
    ld a,(de)
    and 0x38
    ; print hex char
    call dasm_print8hex
    pop bc
    ret


param_06:
    push bc
    ld de,(var_opcode_start)
    ld a,(de)
    rra
    rra
    rra
    and 0x07
    call param_printRegister
    ld a," "
    call print_char
    ld a,","
    call print_char
    pop bc
    ret

param_07:
    push bc
    ld de,(var_opcode_start)
    ld a,(de)
    and 0x07

    call param_printRegister
    pop bc
    ret

param_08:
    push bc
    ld de,(var_opcode_start)
    inc de
    ld a,(de)
    call dasm_print8hex
    pop bc
    ret


param_0A:
    push hl
    ld de,(var_opcode_start)
    inc de
    inc de
    jr param_09_0A
param_09:
    push hl
    ld de,(var_opcode_start)
    inc de
param_09_0A:
    ld a,(de)
    ld l,a
    inc de
    ld a,(de)
    ld h,a
    ld a,h 
    call print_a_hex
    ld a,l
    call print_a_hex
    pop hl
    ret

param_10:
    push bc
    ld de,(var_opcode_start)
    ld a,(de)
    rra
    rra
    rra
    and 0x07
    call param_printRegister
    pop bc
    ret


param_11:
    push hl
    push bc
    ld de,(var_opcode_start)
    jr param_11_12
param_12:
    push hl
    push bc
    ld de,(var_opcode_start)
    inc de

param_11_12:
    ld a,(de)
    rra
    rra
    rra
    and 0x06
    push af
    ;check which table to use
    ld hl, (var_opcode_start)
    ld a,(hl)
    cp 0xDD
    jr z,param_11_12_ix
    cp 0xFD
    jr z,param_11_12_iy
param_11_12_def:
    ld hl, [dasm_printRegister8_table]
    jr param_11_12_all
param_11_12_ix:
    ld hl, [dasm_printRegisterIX_table]
    jr param_11_12_all
param_11_12_iy:
    ld hl, [dasm_printRegisterIY_table]
param_11_12_all:
    pop af
    ld b,0
    ld c,a
    add hl, bc
    ld a,(hl)
    call print_char
    inc hl
    ld a,(hl)
    call print_char
    pop bc
    pop hl
    ret

param_13:
    push hl
    push bc
    ld de,(var_opcode_start)
    ld a,(de)
    rra
    rra
    rra
    and 0x06
    push af
    ;check which table to use
    ld hl, (var_opcode_start)
    ld a,(hl)
    ld hl, [dasm_printRegisterSP_table]
    jr param_11_12_all  ;reuse code from 11_12


param_81:
    push hl
    push bc
    ld hl, (var_opcode_string)
    ld b,2
    jr param_80_seek
param_80:
    push hl
    push bc
    ld hl, (var_opcode_string)
    ld b,1
param_80_seek:
    ld a,(hl)
    inc hl
    and a
    jr nz, param_80_seek
    ;found
    dec b   ;found but counter too high
    jp nz, param_80_seek

    call print_str  
    pop bc
    pop hl
    ret



param_printRegister:
    push hl
    cp 0x06
    jr z, param_printRegisterHL
    cp 0x07
    jr z, param_printRegisterA
    ld hl, [dasm_printRegister8_table]
    ld b,0
    ld c,a
    add hl,bc
    ld a, (hl)
    call print_char
    pop hl
    ret

param_printRegisterHL:
    ld hl, [dasm_printRegister8_table_HL]
    call print_str
    pop hl
    ret

param_printRegisterA:
    ld a,"A"
    call print_char
    pop hl
    ret


param_comma:
    ld a,b
    cp 1
    ret z
    ld a," "
    call print_char
    ld a,","
    call print_char
    ret