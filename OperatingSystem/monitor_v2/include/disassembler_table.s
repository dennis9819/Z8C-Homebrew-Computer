;disassembler tables

dasm_opcode_table:
    ;byte 0 = opcode
    ;byte 1 = opcode mask
    ;byte 2 = opcode extended
    ;byte 3 = opcode extended mask
    ;byte 4 = length
    ;byte 5+6 = pointer to string
    ;byte 7 = params = count of paramters
    ;byte 8+ = paramters        
    ; 0x01 16bit address pointer
    ; 0x02 flag bit 3-5
    ; 0x03 relative jmp address
    ; 0x04 RETI/RETN
    ; 0x05 RST Vector
    ; 0x06 register (r)
    ; 0x07 register (r')
    ; 0x08 8-Bit value
    ; 0x09 16-Bit value
    ; 0x0A 16-bit value with offset +1
    ; 0x10 same as 0x06 without ","
    ; 0x11 print 16 bit register from 1st byte
    ; 0x12 print 16 bit register from 2nd byte
    ; 0x13 push/pop register lookup
    ; 0x80 print string suffix
    ; 0x81 print string suffix 2
    defb 0x00, 0xFF, 0x00, 0x00, 1, [dasm_00], [dasm_00]>>8,0 ;NOP
    ;General-Purpose Arithmetic and CPU Control Groups
    defb 0x27, 0xFF, 0x00, 0x00, 1, [dasm_27], [dasm_27]>>8, 0 ;DAA
    defb 0x2F, 0xFF, 0x00, 0x00, 1, [dasm_2F], [dasm_2F]>>8, 0 ;CPL
    defb 0xED, 0xFF, 0x44, 0xFF, 2, [dasm_ED_44], [dasm_ED_44]>>8, 0 ;NEG
    defb 0x3f, 0xFF, 0x00, 0x00, 1, [dasm_3F], [dasm_3F]>>8, 0 ;CCF
    defb 0x37, 0xFF, 0x00, 0x00, 1, [dasm_37], [dasm_37]>>8, 0 ;SCF
    ;defb 0x00, 0xFF, 0x00, 0x00, 1, [dasm_00], [dasm_00]>>8,0 ;NOP -> already at top for performance reasons
    defb 0x76, 0xFF, 0x00, 0x00, 1, [dasm_76], [dasm_76]>>8, 0 ;HALT
    defb 0xF3, 0xFF, 0x00, 0x00, 1, [dasm_F3], [dasm_F3]>>8, 0 ;DI
    defb 0xFB, 0xFF, 0x00, 0x00, 1, [dasm_FB], [dasm_FB]>>8, 0 ;EI
    defb 0xED, 0xFF, 0x46, 0xFF, 2, [dasm_ED_46], [dasm_ED_46]>>8, 0 ;IM 0
    defb 0xED, 0xFF, 0x56, 0xFF, 2, [dasm_ED_56], [dasm_ED_56]>>8, 0 ;IM 1
    defb 0xED, 0xFF, 0x5E, 0xFF, 2, [dasm_ED_5E], [dasm_ED_5E]>>8, 0 ;IM 2

    ;Exchange, Block Transfer, and Search Group
    defb 0xEB, 0xFF, 0x00, 0x00, 1, [dasm_BE], [dasm_BE]>>8, 0 ;EX DE, HL
    defb 0x08, 0xFF, 0x00, 0x00, 1, [dasm_08], [dasm_08]>>8, 0 ;EX AF, AF′
    defb 0xD9, 0xFF, 0x00, 0x00, 1, [dasm_D9], [dasm_D9]>>8, 0 ;EXX
    defb 0xE3, 0xFF, 0x00, 0x00, 1, [dasm_E3], [dasm_E3]>>8, 0 ;EX (SP), HL
    defb 0xDD, 0xFF, 0xE3, 0xFF, 2, [dasm_DD_E3], [dasm_DD_E3]>>8, 0 ;EX (SP), IX
    defb 0xFD, 0xFF, 0xE3, 0xFF, 2, [dasm_FD_E3], [dasm_FD_E3]>>8, 0 ;EX (SP), IY
    defb 0xED, 0xFF, 0xA0, 0xFF, 2, [dasm_ED_A0], [dasm_ED_A0]>>8, 0 ;LDI
    defb 0xED, 0xFF, 0xB0, 0xFF, 2, [dasm_ED_B0], [dasm_ED_B0]>>8, 0 ;LDIR
    defb 0xED, 0xFF, 0xA8, 0xFF, 2, [dasm_ED_A8], [dasm_ED_A8]>>8, 0 ;LDD
    defb 0xED, 0xFF, 0xB8, 0xFF, 2, [dasm_ED_B8], [dasm_ED_B8]>>8, 0 ;LDDR
    defb 0xED, 0xFF, 0xA1, 0xFF, 2, [dasm_ED_A1], [dasm_ED_A1]>>8, 0 ;CPI
    defb 0xED, 0xFF, 0xB1, 0xFF, 2, [dasm_ED_B1], [dasm_ED_B1]>>8, 0 ;CPIR
    defb 0xED, 0xFF, 0xA9, 0xFF, 2, [dasm_ED_A9], [dasm_ED_A9]>>8, 0 ;CPD
    defb 0xED, 0xFF, 0xB9, 0xFF, 2, [dasm_ED_B9], [dasm_ED_B9]>>8, 0 ;CPDR

    ;JUMP Group
    defb 0xC3, 0xFF, 0x00, 0x00, 3, [dasm_C3], [dasm_C3]>>8,1, 0x01 ;JP nn
    defb 0xC2, 0xC7, 0x00, 0x00, 3, [dasm_C3], [dasm_C3]>>8,3, 0x02, 0x80, 0x01 ;JP cc,nn
    defb 0x18, 0xFF, 0x00, 0x00, 2, [dasm_18], [dasm_18]>>8,1, 0x03 ;JR e
    defb 0x38, 0xFF, 0x00, 0x00, 2, [dasm_38], [dasm_38]>>8,1, 0x03 ;JR C,e
    defb 0x30, 0xFF, 0x00, 0x00, 2, [dasm_30], [dasm_30]>>8,1, 0x03 ;JR NC,e
    defb 0x28, 0xFF, 0x00, 0x00, 2, [dasm_28], [dasm_28]>>8,1, 0x03 ;JR Z,e
    defb 0x20, 0xFF, 0x00, 0x00, 2, [dasm_20], [dasm_20]>>8,1, 0x03 ;JR NZ,e
    defb 0xE9, 0xFF, 0x00, 0x00, 2, [dasm_E9], [dasm_E9]>>8,0 ;JP (HL)
    defb 0xDD, 0xFF, 0xE9, 0xFF, 2, [dasm_DD], [dasm_DD]>>8,0 ;JP (IX)
    defb 0xFD, 0xFF, 0xE9, 0xFF, 2, [dasm_FD], [dasm_FD]>>8,0 ;JP (IY)
    defb 0x10, 0xFF, 0x00, 0x00, 2, [dasm_10], [dasm_10]>>8,1, 0x03 ;DJNZ, e
    ;Call and Return Group
    defb 0xCD, 0xFF, 0x00, 0x00, 3, [dasm_CD], [dasm_CD]>>8,1, 0x01 ;CALL nn
    defb 0xC4, 0xC7, 0x00, 0x00, 3, [dasm_CD], [dasm_CD]>>8,2, 0x02, 0x01 ;CALL cc,nn
    defb 0xC9, 0xFF, 0x00, 0x00, 1, [dasm_C9], [dasm_C9]>>8,0 ;RET
    defb 0xC0, 0xC7, 0x00, 0x00, 1, [dasm_C9], [dasm_C9]>>8,1, 0x02 ;RET cc
    defb 0xED, 0xFF, 0x4D, 0xFF, 2, [dasm_ED_4D], [dasm_ED_4D]>>8,0 ;RETI
    defb 0xED, 0xFF, 0x45, 0xFF, 2, [dasm_ED_45], [dasm_ED_45]>>8,0 ;RETN

    defb 0xC7, 0xC7, 0x00, 0x00, 1, [dasm_FF], [dasm_FF]>>8,1, 0x05 ;RST
    ;8-Bit load group
    defb 0x0A, 0xFF, 0x00, 0x00, 1, [dasm_0A], [dasm_0A]>>8, 0 ;LD A, (BC)
    defb 0x1A, 0xFF, 0x00, 0x00, 1, [dasm_1A], [dasm_1A]>>8, 0 ;LD A, (DE)
    defb 0x3A, 0xFF, 0x00, 0x00, 3, [dasm_3A], [dasm_3A]>>8, 2, 0x09, 0x80 ;LD A, (nn)
    defb 0x02, 0xFF, 0x00, 0x00, 1, [dasm_02], [dasm_02]>>8, 0 ;LD (BC), A
    defb 0x12, 0xFF, 0x00, 0x00, 1, [dasm_12], [dasm_12]>>8, 0 ;LD (DE), A
    defb 0x32, 0xFF, 0x00, 0x00, 3, [dasm_32], [dasm_32]>>8, 2, 0x09, 0x80 ;LD (nn), A
    defb 0xED, 0xFF, 0x57, 0xFF, 2, [dasm__ED_57], [dasm__ED_57]>>8, 0 ;LD A, I
    defb 0xED, 0xFF, 0x5F, 0xFF, 2, [dasm__ED_5F], [dasm__ED_5F]>>8, 0 ;LD A, R
    defb 0xED, 0xFF, 0x47, 0xFF, 2, [dasm__ED_47], [dasm__ED_47]>>8, 0 ;LD I, A
    defb 0xED, 0xFF, 0x4F, 0xFF, 2, [dasm__ED_4F], [dasm__ED_4F]>>8, 0 ;LD R, A

    defb 0x06, 0xC7, 0x00, 0x00, 2, [dasm__LD], [dasm__LD]>>8, 2, 0x06, 0x08 ;LD r, n
    defb 0x40, 0xC0, 0x00, 0x00, 1, [dasm__LD], [dasm__LD]>>8, 2, 0x06, 0x07 ;LD r, r' / LD r, (HL) / LD (HL), r
    
    ;8-Bit Arithmetic Group
    defb 0x80, 0xF8, 0x00, 0x00, 1, [dasm_80C6], [dasm_80C6]>>8, 1, 0x07 ;ADD A, r / ADD A, (HL)
    defb 0xC6, 0xFF, 0x00, 0x00, 2, [dasm_80C6], [dasm_80C6]>>8, 1, 0x08 ;ADD A, n
    defb 0xDD, 0xFF, 0x86, 0xFF, 3, [dasm_DD_86], [dasm_DD_86]>>8, 2, 0x08, 0x80 ;ADD A, (IX + d)
    defb 0xFD, 0xFF, 0x86, 0xFF, 3, [dasm_FD_86], [dasm_FD_86]>>8, 2, 0x08, 0x80 ;ADD A, (IY + d)
    defb 0xC8, 0xF8, 0x00, 0x00, 1, [dasm_C88E], [dasm_C88E]>>8, 1, 0x07 ;ADC A, r / ADC A, (HL)
    defb 0x8E, 0xF8, 0x00, 0x00, 2, [dasm_C88E], [dasm_C88E]>>8, 1, 0x08 ;ADC A, n
    defb 0xDD, 0xFF, 0x8E, 0xFF, 3, [dasm_DD_8E], [dasm_DD_8E]>>8, 2, 0x08, 0x80 ;ADC A, (IX + d)
    defb 0xFD, 0xFF, 0x8E, 0xFF, 3, [dasm_FD_8E], [dasm_FD_8E]>>8, 2, 0x08, 0x80 ;ADC A, (IY + d)
    defb 0x90, 0xF8, 0x00, 0x00, 1, [dasm__SUB], [dasm__SUB]>>8, 1, 0x07 ;SUB r / SUB A, (HL)
    defb 0xD6, 0xFF, 0x00, 0x00, 2, [dasm__SUB], [dasm__SUB]>>8, 1, 0x08 ;SUB n
    defb 0xDD, 0xFF, 0x96, 0xFF, 3, [dasm_DD_96], [dasm_DD_96]>>8, 2, 0x08, 0x80 ;SUB (IX + d)
    defb 0xFD, 0xFF, 0x96, 0xFF, 3, [dasm_FD_96], [dasm_FD_96]>>8, 2, 0x08, 0x80 ;SUB (IY + d)
    defb 0x94, 0xF8, 0x00, 0x00, 1, [dasm__SBC], [dasm__SBC]>>8, 1, 0x07 ;SBC A,r / SBC A, (HL)
    defb 0xDE, 0xFF, 0x00, 0x00, 2, [dasm__SBC], [dasm__SBC]>>8, 1, 0x08 ;SBC A,n
    defb 0xDD, 0xFF, 0x9E, 0xFF, 3, [dasm_DD_9E], [dasm_DD_9E]>>8, 2, 0x08, 0x80 ;SBC A,(IX + d)
    defb 0xFD, 0xFF, 0x9E, 0xFF, 3, [dasm_FD_9E], [dasm_FD_9E]>>8, 2, 0x08, 0x80 ;SBC A,(IY + d)
    defb 0xA0, 0xF8, 0x00, 0x00, 1, [dasm__AND], [dasm__AND]>>8, 1, 0x07 ;AND A,r / AND A, (HL)
    defb 0xE6, 0xFF, 0x00, 0x00, 2, [dasm__AND], [dasm__AND]>>8, 1, 0x08 ;AND A,n
    defb 0xDD, 0xFF, 0xA6, 0xFF, 3, [dasm_DD_A6], [dasm_DD_A6]>>8, 2, 0x08, 0x80 ;AND A,(IX + d)
    defb 0xFD, 0xFF, 0xA6, 0xFF, 3, [dasm_FD_A6], [dasm_FD_A6]>>8, 2, 0x08, 0x80 ;AND A,(IY + d)
    defb 0xB0, 0xF8, 0x00, 0x00, 1, [dasm__OR], [dasm__OR]>>8, 1, 0x07 ;OR A,r / OR A, (HL)
    defb 0xF6, 0xFF, 0x00, 0x00, 2, [dasm__OR], [dasm__OR]>>8, 1, 0x08 ;OR A,n
    defb 0xDD, 0xFF, 0xB6, 0xFF, 3, [dasm_DD_B6], [dasm_DD_B6]>>8, 2, 0x08, 0x80 ;OR A,(IX + d)
    defb 0xFD, 0xFF, 0xB6, 0xFF, 3, [dasm_FD_B6], [dasm_FD_B6]>>8, 2, 0x08, 0x80 ;OR A,(IY + d)
    defb 0xA8, 0xF8, 0x00, 0x00, 1, [dasm__XOR], [dasm__XOR]>>8, 1, 0x07 ;XOR A,r / XOR A, (HL)
    defb 0xEE, 0xFF, 0x00, 0x00, 2, [dasm__XOR], [dasm__XOR]>>8, 1, 0x08 ;XOR A,n
    defb 0xDD, 0xFF, 0xAE, 0xFF, 3, [dasm_DD_AE], [dasm_DD_AE]>>8, 2, 0x08, 0x80 ;XOR A,(IX + d)
    defb 0xFD, 0xFF, 0xAE, 0xFF, 3, [dasm_FD_AE], [dasm_FD_AE]>>8, 2, 0x08, 0x80 ;XOR A,(IY + d)
    defb 0xB8, 0xF8, 0x00, 0x00, 1, [dasm__CP], [dasm__CP]>>8, 1, 0x07 ;CP A,r / CP A, (HL)
    defb 0xFE, 0xFF, 0x00, 0x00, 2, [dasm__CP], [dasm__CP]>>8, 1, 0x08 ;CP A,n
    defb 0xDD, 0xFF, 0xBE, 0xFF, 3, [dasm_DD_BE], [dasm_DD_BE]>>8, 2, 0x08, 0x80 ;CP A,(IX + d)
    defb 0xFD, 0xFF, 0xBE, 0xFF, 3, [dasm_FD_BE], [dasm_FD_BE]>>8, 2, 0x08, 0x80 ;CP A,(IY + d)
    defb 0x04, 0xC7, 0x00, 0x00, 1, [dasm__INC], [dasm__INC]>>8, 1, 0x10 ;INC r / INC (HL)
    defb 0xDD, 0xFF, 0x34, 0xFF, 3, [dasm_DD_34], [dasm_DD_34]>>8, 2, 0x08, 0x80 ;INC (IX + d)
    defb 0xFD, 0xFF, 0x34, 0xFF, 3, [dasm_FD_34], [dasm_FD_34]>>8, 2, 0x08, 0x80 ;INC (IY + d)
    defb 0x05, 0xC7, 0x00, 0x00, 1, [dasm__DEC], [dasm__DEC]>>8, 1, 0x10 ;DEC r / DEC (HL)
    defb 0xDD, 0xFF, 0x35, 0xFF, 3, [dasm_DD_35], [dasm_DD_35]>>8, 2, 0x08, 0x80 ;DEC (IX + d)
    defb 0xFD, 0xFF, 0x35, 0xFF, 3, [dasm_FD_35], [dasm_FD_35]>>8, 2, 0x08, 0x80 ;DEC (IY + d)
    ;16-Bit Arithmetic Group
    defb 0x09, 0xCF, 0x00, 0x00, 1, [dasm_09], [dasm_09]>>8, 1, 0x11 ;ADD HL, ss
    defb 0xED, 0xFF, 0x4A, 0xCF, 2, [dasm_ED_4A], [dasm_ED_4A]>>8, 1, 0x12 ;ADC HL, ss
    defb 0xED, 0xFF, 0x42, 0xCF, 2, [dasm_ED_42], [dasm_ED_42]>>8, 1, 0x12 ;SBC HL, ss
    defb 0xDD, 0xFF, 0x09, 0xCF, 2, [dasm_DD_09], [dasm_DD_09]>>8, 1, 0x12 ;ADD IX, ss
    defb 0xFD, 0xFF, 0x09, 0xCF, 2, [dasm_FD_09], [dasm_FD_09]>>8, 1, 0x12 ;ADD IY, ss
    defb 0x03, 0xCF, 0x00, 0x00, 1, [dasm_03], [dasm_03]>>8, 1, 0x11 ;INC ss
    defb 0xDD, 0xFF, 0x23, 0xFF, 2, [dasm_DD_23], [dasm_DD_23]>>8, 0 ;INC IX
    defb 0xFD, 0xFF, 0x23, 0xFF, 2, [dasm_FD_23], [dasm_FD_23]>>8, 0 ;INC IY
    defb 0x0B, 0xCF, 0x00, 0x00, 1, [dasm_0B], [dasm_0B]>>8, 1, 0x11 ;DEC ss
    defb 0xDD, 0xFF, 0x2B, 0xFF, 2, [dasm_DD_2B], [dasm_DD_2B]>>8, 0 ;DEC IX
    defb 0xFD, 0xFF, 0x2B, 0xFF, 2, [dasm_FD_2B], [dasm_FD_2B]>>8, 0 ;DEC IY
    ;16-Bit Load Group
    defb 0x01, 0xCF, 0x00, 0x00, 3, [dasm_01], [dasm_01]>>8, 3, 0x11, 0x80, 0x09 ;LD dd, nn
    defb 0xDD, 0xFF, 0x21, 0xFF, 4, [dasm_DD_01], [dasm_DD_01]>>8, 1, 0x0A ;LD IX, nn
    defb 0xFD, 0xFF, 0x21, 0xFF, 4, [dasm_FD_01], [dasm_FD_01]>>8, 1, 0x0A ;LD IY, nn
    defb 0x2A, 0xFF, 0x00, 0x00, 3, [dasm_2A], [dasm_2A]>>8, 2, 0x09, 0x80 ;LD HL, (nn)
    defb 0xED, 0xFF, 0x4B, 0xCF, 4, [dasm_ED_4B], [dasm_ED_4B]>>8, 4, 0x12, 0x80, 0x0A, 0x81 ;LD dd, (nn)
    defb 0xDD, 0xFF, 0x2A, 0xFF, 4, [dasm_DD_2A], [dasm_DD_2A]>>8, 1, 0x0A ;LD IX, (nn)
    defb 0xFD, 0xFF, 0x2A, 0xFF, 4, [dasm_FD_2A], [dasm_FD_2A]>>8, 1, 0x0A ;LD IY, (nn)
    defb 0x22, 0xFF, 0x00, 0x00, 3, [dasm_22], [dasm_22]>>8, 2, 0x0A, 0x80 ;LD (nn), HL
    defb 0xED, 0xFF, 0x43, 0xCF, 4, [dasm_ED_43], [dasm_ED_43]>>8, 3, 0x0A, 0x80, 0x12 ;LD (nn), dd
    defb 0xDD, 0xFF, 0x22, 0xCF, 4, [dasm_DD_22], [dasm_DD_22]>>8, 2, 0x0A, 0x80 ;LD (nn), IX
    defb 0xFD, 0xFF, 0x22, 0xCF, 4, [dasm_FD_22], [dasm_FD_22]>>8, 2, 0x0A, 0x80 ;LD (nn), IY
    defb 0xF9, 0xFF, 0x00, 0x00, 1, [dasm_F9], [dasm_F9]>>8, 0 ;LD SP, HL
    defb 0xDD, 0xFF, 0xF9, 0xFF, 2, [dasm_DD_F9], [dasm_DD_F9]>>8, 0 ;LD SP, IX
    defb 0xFD, 0xFF, 0xF9, 0xFF, 2, [dasm_FD_F9], [dasm_FD_F9]>>8, 0 ;LD SP, IY
    defb 0xC5, 0xCF, 0x00, 0x00, 1, [dasm_E5], [dasm_E5]>>8, 1, 0x13 ;PUSH qq
    defb 0xDD, 0xFF, 0xE5, 0xFF, 2, [dasm_DD_E5], [dasm_DD_E5]>>8, 0 ;PUSH IX
    defb 0xFD, 0xFF, 0xE5, 0xFF, 2, [dasm_FD_E5], [dasm_FD_E5]>>8, 0 ;PUSH IY
    defb 0xC1, 0xCF, 0x00, 0x00, 1, [dasm_E1], [dasm_E1]>>8, 1, 0x13 ;POP qq
    defb 0xDD, 0xFF, 0xE1, 0xFF, 2, [dasm_DD_E1], [dasm_DD_E1]>>8, 0 ;POP IX
    defb 0xFD, 0xFF, 0xE1, 0xFF, 2, [dasm_FD_E1], [dasm_FD_E1]>>8, 0 ;POP IY


dasm_00:    db "NOP",0x00
;JUMP Group
dasm_C3:    db "JP ",0x00,", ",0x00
dasm_18:    db "JR ",0x00
dasm_38:    db "JR C, ",0x00
dasm_30:    db "JR NC, ",0x00
dasm_28:    db "JR Z, ",0x00
dasm_20:    db "JR NZ, ",0x00
dasm_E9:    db "JP (HL) ",0x00
dasm_DD:    db "JP (IX) ",0x00
dasm_FD:    db "JP (IY) ",0x00
dasm_10:    db "DJNZ ",0x00
;Call and Return Group
dasm_CD:    db "CALL ",0x00
dasm_C9:    db "RET ",0x00
dasm_ED_4D: db "RETI",0x00
dasm_ED_45: db "RETN",0x00
dasm_FF:    db "RST ",0x00
;8-Bit load group
dasm_0A:    db "LD A,(BC)",0x00
dasm_1A:    db "LD A,(DE)",0x00
dasm_3A:    db "LD A,(",0x00, "h)",0x00
dasm_02:    db "LD (BC), A",0x00
dasm_12:    db "LD (DE), A",0x00
dasm_32:    db "LD (",0x00, "h), A",0x00
dasm__LD:   db "LD ",0x00
dasm__ED_57: db "LD A, I",0x00
dasm__ED_5F: db "LD A, R",0x00
dasm__ED_47: db "LD I, A",0x00
dasm__ED_4F: db "LD R, A",0x00
;General-Purpose Arithmetic and CPU Control Groups
dasm_27:    db "DAA",0x00
dasm_2F:    db "CPL",0x00
dasm_ED_44: db "NEG",0x00
dasm_3F:    db "CCF",0x00
dasm_37:    db "SCF",0x00
dasm_76:    db "HALT",0x00
dasm_F3:    db "DI",0x00
dasm_FB:    db "EI",0x00
dasm_ED_46: db "IM 0",0x00
dasm_ED_56: db "IM 1",0x00
dasm_ED_5E: db "IM 2",0x00
;Exchange, Block Transfer, and Search Group
dasm_BE:    db "EX DE, HL",0x00
dasm_08:    db "EX AF, AF′",0x00
dasm_D9:    db "EXX",0x00
dasm_E3:    db "EX (SP), HL",0x00
dasm_DD_E3: db "EX (SP), IX",0x00
dasm_FD_E3: db "EX (SP), IY",0x00
dasm_ED_A0: db "LDI",0x00
dasm_ED_B0: db "LDIR",0x00
dasm_ED_A8: db "LDD",0x00
dasm_ED_B8: db "LDDR",0x00
dasm_ED_A1: db "CPI",0x00
dasm_ED_B1: db "CPIR",0x00
dasm_ED_A9: db "CPD",0x00
dasm_ED_B9: db "CPDR",0x00
;8-Bit Arithmetic Group
dasm_80C6:    db "ADD A, ", 0x00
dasm_DD_86:   db "ADD A, (IX+", 0x00, "h)",0x00
dasm_FD_86:   db "ADD A, (IY+", 0x00, "h)",0x00
dasm_C88E:    db "ADC A, ", 0x00
dasm_DD_8E:   db "ADC A, (IX+", 0x00, "h)",0x00
dasm_FD_8E:   db "ADC A, (IY+", 0x00, "h)",0x00
dasm__SUB:    db "SUB ", 0x00
dasm_DD_96:   db "SUB (IX+", 0x00, "h)",0x00
dasm_FD_96:   db "SUB (IY+", 0x00, "h)",0x00
dasm__SBC:    db "SBC A, ", 0x00
dasm_DD_9E:   db "SBC A,(IX+", 0x00, "h)",0x00
dasm_FD_9E:   db "SBC A,(IY+", 0x00, "h)",0x00
dasm__AND:    db "AND ", 0x00
dasm_DD_A6:   db "AND (IX+", 0x00, "h)",0x00
dasm_FD_A6:   db "AND (IY+", 0x00, "h)",0x00
dasm__OR:     db "OR ", 0x00
dasm_DD_B6:   db "OR (IX+", 0x00, "h)",0x00
dasm_FD_B6:   db "OR (IY+", 0x00, "h)",0x00
dasm__XOR:    db "XOR ", 0x00
dasm_DD_AE:   db "XOR (IX+", 0x00, "h)",0x00
dasm_FD_AE:   db "XOR (IY+", 0x00, "h)",0x00
dasm__CP:     db "CP ", 0x00
dasm_DD_BE:   db "CP (IX+", 0x00, "h)",0x00
dasm_FD_BE:   db "CP (IY+", 0x00, "h)",0x00
dasm__INC:    db "INC ", 0x00
dasm_DD_34:   db "INC (IX+", 0x00, "h)",0x00
dasm_FD_34:   db "INC (IY+", 0x00, "h)",0x00
dasm__DEC:    db "DEC ", 0x00
dasm_DD_35:   db "DEC (IX+", 0x00, "h)",0x00
dasm_FD_35:   db "DEC (IY+", 0x00, "h)",0x00
;16-Bit Arithmetic Group
dasm_09:      db "ADD HL, ",0x00
dasm_ED_4A:   db "ADC HL, ",0x00
dasm_ED_42:   db "SBC HL, ",0x00
dasm_DD_09:   db "ADD IX, ",0x00
dasm_FD_09:   db "ADD IY, ",0x00
dasm_03:      db "INC ",0x00
dasm_DD_23:   db "INC IX, ",0x00
dasm_FD_23:   db "INC IY, ",0x00
dasm_0B:      db "DEC ",0x00
dasm_DD_2B:   db "DEC IX, ",0x00
dasm_FD_2B:   db "DEC IY, ",0x00
;16-Bit Load Group
dasm_01:      db "LD ",0x00, ", ",0x00
dasm_DD_01:   db "LD IX, ",0x00
dasm_FD_01:   db "LD IY, ",0x00
dasm_2A:      db "LD HL, (",0x00,"h)",0x00
dasm_ED_4B:   db "LD ",0x00,", (",0x00,"h)",0x00
dasm_DD_2A:   db "LD IX, (",0x00,"h)",0x00
dasm_FD_2A:   db "LD IY, (",0x00,"h)",0x00
dasm_22:      db "LD (",0x00,"h), HL",0x00
dasm_ED_43:   db "LD (",0x00,"h), ",0x00
dasm_DD_22:   db "LD (",0x00,"h), IX",0x00
dasm_FD_22:   db "LD (",0x00,"h), IY",0x00
dasm_F9:      db "LD SP, HL",0x00
dasm_DD_F9:   db "LD SP, IX",0x00
dasm_FD_F9:   db "LD SP, IY",0x00
dasm_E5:      db "PUSH ",0x00
dasm_DD_E5:   db "PUSH IX",0x00
dasm_FD_E5:   db "PUSH IY",0x00
dasm_E1:      db "PUSH ",0x00
dasm_DD_E1:   db "PUSH IX",0x00
dasm_FD_E1:   db "PUSH IY",0x00

;Misc
dasm_UU:    db ".?.",0x00
dasm_UW:    db "    ",0x00

dasm_printFlags_table:
    db "NZ"
    db "Z",0
    db "NC"
    db "C",0
    db "PO"
    db "PE"
    db "P",0
    db "M",0

dasm_printRegister8_table:
    db "B"
    db "C"
    db "D"
    db "E"
    db "H"
    db "L"
    db "S"  ;only 18 bit (SP)
    db "P"  ;only 18 bit (SP)

dasm_printRegisterIX_table:
    db "BC"
    db "DE"
    db "IX"
    db "SP"
dasm_printRegisterIY_table:
    db "BC"
    db "DE"
    db "IY"
    db "SP"

dasm_printRegisterSP_table:
    db "BC"
    db "DE"
    db "HL"
    db "AF"

dasm_printRegister8_table_HL:
    db "(HL)", 0