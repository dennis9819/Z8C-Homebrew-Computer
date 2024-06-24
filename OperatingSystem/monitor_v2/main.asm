;----------------------------------------------------------------
;Z8C Homebrew Computer Monitor 
;by Dennis Gunia (01/2023)
;----------------------------------------------------------------

;================================================================
; Memory areas
;================================================================
SYS_RAM_START   equ 0x4000
SYS_BUF_START   equ 0x4010
PRG_RAM_START   equ 0x4110
PRG_RAM_TOP     equ 0xFF00
STACK_RAM_TOP   equ 0xFFFF

;================================================================
; Terminal configuration
;================================================================
VAR_CONSOLE_CONF    equ 0x0F  ;CPU/16  Clock @ 230402.5625/s
VAR_CONSOLE_BAUD    equ 24  ;BAUD timer constant
                            ;CLK/TRG Clock @ 1843220.5/s
                            ; -> 0x16 : 14400
                            ;CPU/16  Clock @ 230402.5625/s
                            ; -> 12 : 19200
                            ; -> 24 : 9600

;================================================================
; Includes (1/2)
;================================================================
; include IO device addresses
.include "addresses.s"

;================================================================
; RST Vectors for BASIC
;================================================================
    org 0x0000
RST_00: ;Hardware Restart
    jp BOOT_PHASE0    

    org 0x0008
RST_08  ;Print Char
    jp EXEC_RST_08

    org 0x0010
RST_10  ;receive char
    jp EXEC_RST_10

    org 0x0018
RST_18  ;Buffer length
    jp EXEC_RST_18

;================================================================
; Default interrupt vectors
;================================================================
INT_VEC:    
    org 0x0044
    ;DEFW EXEC_INT_VDP

;================================================================
; Memory layout
;================================================================
mon_var_template:
    phase SYS_RAM_START
interrupt_vectors:
    defs 256
var_buffer_len:
    defb 0
var_last_char:
    defb 0
var_curserx:
    defb 0
var_cursery:
    defb 0
var_curserstate:
    defb 0
var_curseron:
    defb 0
var_curserchar:
    defb 0
var_curserlastaddr:
    defw 0
var_pio_present:
    defb 0
var_apu_present:
    defb 0
var_scratch:
    defs 16 ;16 bytes space for scratch vars
var_ps2mem:
    defs 16 ;16 bytes space for scratch vars
var_dir:
    defs 80 
var_input:
    defs 80 
var_idebuffer:
    defs 768

    dephase
mon_var_template_end:
    nop

;================================================================
; Start of monitor
;================================================================
    org 0x0050
    .include "ref.s"    ;static bios calls for programs
    
BOOT_PHASE0:     ;Setup Hardware
    ;Setup Stack-Pointer
    ld sp, STACK_RAM_TOP
    ;Disable Interrupts
    di
    ;Setup PIO on MIO Board
    LD A,0xCF
    OUT (CS_PIO_AC), A
    LD A,11110111b      ;All pins inputs except speaker
    OUT (CS_PIO_AC), A
    LD A,00000011B      ;Preset I2C pins to high
    OUT (CS_PIO_AC), A  

    ;Set variables    
    ld (var_curserstate),a
    ld (var_curseron),a
    ld a, " "
    ld (var_curserchar),a

    ;setup interrupt table
    ld a,[interrupt_vectors]>>8
    ld i,a

    ;Initialize Console (Serial-Port)
    call CONSOLE_INIT

BOOT_PHASE1:    ;Copy default values to RAM
    ld hl,mon_var_template
    ld de,mon_var_template_end
    ld bc,SYS_RAM_START
BOOT_PHASE1_LOOP:
    ld a,(hl)  ;copy values
    ld (bc),a
    inc hl      
    inc bc
    push hl    ;check if end is reached
    sbc hl,de
    pop hl
    jp nz, BOOT_PHASE1_LOOP
    ;template copy done

    
BOOT_PHASE2:    ;Hardware initialized.
    ; Print banner
    call print_clear
    ld hl, [STR_Banner_Start]
    call print_str

    ; Power-On Self Tests
    call POST_START

    ; Detect IDE drives
    call ideif_init_all

    ; Beep after start
    LD DE,0x40
    LD BC,0x48
    CALL beep

    xor a   ;set dir to empty
    ld (var_dir),a
    ld (var_dir+1),a
    ; Start commandline
    jp COMMAND
    
    ; This instruction should never be reached
    halt
   
;================================================================
; Includes (2/2)
;================================================================
.include "console.s"
.include "conversions.s"
.include "disassembler.s"
.include "disassembler_table.s"
.include "rst.s"
.include "beep.s" 
.include "kdrv_ide8255.s" ;include ide interface driver.
.include "kdrv_ideif.s" ;include ide driver.
.include "kdrv_siic.s"
.include "prettydump.s"
.include "command.s"
.include "cmd_mem.s"
.include "cmd_io.s"
.include "cmd_date.s"
.include "cmd_disk.s"
.include "post.s"
.include "fat16.s"
.include "fat16_cmd.s"

;================================================================
; Strings
;================================================================
STR_Banner_Start:
    db "Z8C Monitor V2.1 by Dennis Gunia (2022-2024)",0
