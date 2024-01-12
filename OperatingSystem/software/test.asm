.include "extern_symbols.s" ;include monitor symbols.
    org 0x8000  

sel_dsk:
    call ideif_drv_sel
    call fat_print_dbg
    ret

    org 0x8010
    call fat_print_dbg
    ret

    org 0x8020  
    call fat_print_directory
    ret

    org 0x8030
    ld hl,0x0006
    ld (MEM_FAT_OF0_CCLUST),hl
    call fat_getfatsec
    ret

fat_print_dbg:
    call PRINTINLINE
    db 10,13,"PTR.MEM_IDE_POINTER:    0x",0
    ld ix,MEM_IDE_POINTER
    call print_32_hex
    call PRINTINLINE
    db "  |  PTR.MEM_IDE_PARTITION:   0x",0
    ld ix,MEM_IDE_PARTITION
    call print_32_hex

    call PRINTINLINE
    db 10,13,"PTR.MEM_FAT_TMPPOINTER: 0x",0
    ld ix,MEM_FAT_TMPPOINTER
    call print_32_hex
    call PRINTINLINE
    db "  |  PTR.MEM_FAT_TMPPOINTER1: 0x",0
    ld ix,MEM_FAT_TMPPOINTER1
    call print_32_hex

    call PRINTINLINE
    db 10,13,"VAL.MEM_FAT_RESERVED:   0x",0
    ld ix,MEM_FAT_RESERVED
    call print_16_hex
    call PRINTINLINE
    db "      |  VAL.MEM_FAT_AMOUNT:      0x",0
    ld a,(MEM_FAT_AMOUNT)
    call print_a_hex

    call PRINTINLINE
    db 10,13,"VAL.MEM_FAT_SECTORS:    0x",0
    ld ix,MEM_FAT_SECTORS
    call print_16_hex
    call PRINTINLINE
    db "      |  VAL.MEM_FAT_COUNT1:      0x",0
    ld a,(MEM_FAT_COUNT1)
    call print_a_hex

    call PRINTINLINE
    db 10,13,"VAL.MEM_FAT_OF0_CCLUST: 0x",0
    ld ix,MEM_FAT_OF0_CCLUST
    call print_16_hex
    call PRINTINLINE
    db "      |  PTR.MEM_FAT_OF0_FATSEC:  0x",0
    ld ix,MEM_FAT_OF0_FATSEC
    call print_32_hex

    call PRINTINLINE
    db 10,13,"VAL.MEM_FAT_OF0_DATSEC: 0x",0
    ld ix,MEM_FAT_OF0_DATSEC
    call print_32_hex
    call PRINTINLINE
    db "  |  PTR.MEM_FAT_OF0_DATREM:  0x",0
    ld ix,MEM_FAT_OF0_DATREM
    call print_16_hex

    call print_newLine
    ret

print_32_hex:
    ld a,(ix+3)
    call print_a_hex
    ld a,(ix+2)
    call print_a_hex
    ld a,(ix+1)
    call print_a_hex
    ld a,(ix+0)
    call print_a_hex
    ret

print_16_hex:
    ld a,(ix+1)
    call print_a_hex
    ld a,(ix+0)
    call print_a_hex
    ret

; a contains drive to select
; populate fs vars as well
ideif_drv_sel:
    ld (MEM_IDE_SELECTED),a
    push af
    call ideif_get_drv_pointer  ;test if drive is marked as available
    ld a,(ix+0)
    or a
    jp nz, _ideif_drv_sel_fail  ;if not-> fail
    
    call fat_get_root_table     ;else get root table
    ;backup tmp pointer
    ld hl,(MEM_IDE_POINTER)
    ld de,(MEM_IDE_PARTITION)   ;use MEM_IDE_PARTITION to backup the pointer
    call fat_copy_lba_pointer
    ld hl,[_ideif_drv_sel_pstr] ;print success message
    call print_str
    pop af
    add 69
    call print_char
    ld hl,[_ideif_drv_sel_sstr0]
    call print_str
    ret
_ideif_drv_sel_fail:
    ld hl,[_ideif_drv_sel_pstr]
    call print_str
    pop af
    add 69
    call print_char
    ld hl,[_ideif_drv_sel_fstr0]
    call print_str
    LD DE,0x20
    LD BC,0x70
    CALL beep
    ret

_ideif_drv_sel_pstr:
    db 10,13,"Drive ",0
_ideif_drv_sel_fstr0:
    db ": not ready",10,13,0
_ideif_drv_sel_sstr0:
    db ": selected",10,13,0
_ideif_drv_sel_syn:
    db 10,13,"Invalid drive letter",10,13,0
.include "fat16.s" ;include monitor symbols.
.include "fat16_file.s" ;include monitor symbols.

;------------------------------------------------------------------------------
; PRINTINLINE
;
; String output function
;
; Prints in-line data (bytes immediately following the PRINTINLINE call)
; until a string terminator is encountered (0 - null char).
;------------------------------------------------------------------------------
PRINTINLINE:
		EX 		(SP),HL 			; PUSH HL and put RET ADDress into HL
		PUSH 	AF
		PUSH 	BC
nxtILC:
		LD 		A,(HL)
		CP		0
		JR		Z,endPrint
		CALL 	print_char
		INC 	HL
		JR		nxtILC
endPrint:
		INC 	HL 					; Get past "null" terminator
		POP 	BC
		POP 	AF
		EX 		(SP),HL 			; PUSH new RET ADDress on stack and restore HL
        RET