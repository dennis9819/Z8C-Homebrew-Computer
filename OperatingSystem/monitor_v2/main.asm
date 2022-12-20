SYS_RAM_START   equ 0x4000
SYS_BUF_START   equ 0x4010
PRG_RAM_START   equ 0x4110
PRG_RAM_TOP     equ 0xFF00
STACK_RAM_TOP   equ 0xFFFF

;VAR_CONSOLE_CONF    equ 0x07  ;CLK/TRG Clock @ 1843220.5/s
VAR_CONSOLE_CONF    equ 0x0F  ;CPU/16  Clock @ 230402.5625/s

VAR_CONSOLE_BAUD    equ 24  ;BAUD timer constant
                            ;CLK/TRG Clock @ 1843220.5/s
                            ; -> 0x16 : 14400
                            ;CPU/16  Clock @ 230402.5625/s
                            ; -> 12 : 19200
                            ; -> 24 : 9600

; include IO device addresses
.include "addresses.s"

    org 0x0000
RST_00: ;Hardware Restart
    jp mon_start_init_sound    

    org 0x0008
RST_08  ;Print Char
    jp EXEC_RST_08

    org 0x0010
RST_10  ;receive char
    jp EXEC_RST_10

    org 0x0018
RST_18  ;Buffer length
    jp EXEC_RST_18


INT_VEC:    
    org 0x0044
    ;DEFW EXEC_INT_VDP

;memory var template
mon_var_template:
    phase SYS_RAM_START

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
var_scratch:
    defs 16 ;16 bytes space for scratch vars
var_ps2mem:
    defs 16 ;16 bytes space for scratch vars
var_buffer:
    defb 0  ;var lentgh 

mon_var_template_end:
    dephase
;end memory var template
    org 0x0050
mon_start_init_sound:
    ;CALL BTLDR_ENTRY  ;call bootloader first
    ;ld	  D,0x08	; Select register #8
    ;ld	  A,0x00	; Volume channel A 0
    ;call AY0_WRITE_REG
    ;ld	  D,0x09	; Select register #9
    ;ld	  A,0x00	; Volume channel B 0
    ;call AY0_WRITE_REG
    ;ld	  D,0x0A	; Select register #10
    ;ld	  A,0x00	; Volume channel C 0
    ;call AY0_WRITE_REG

mon_start_init_ctc:
    ld sp, STACK_RAM_TOP
    ; Set CTC Ch2 Interrupt Vector
    ;LD A,40h    ; it vector defined in bit 7­3,bit 2­1 don't care, bit 0 = 0
    ;OUT (IO_CTC0_C0),A
    ; Init CTC Channel 2
    ;LD A,10100111b
    ;OUT (IO_CTC0_C2),A
    ;LD A,0x34 ; 55Hz ISR
    ;LD A,0xFF ; 55Hz ISR
    ;OUT (IO_CTC0_C2),A

    ;INIT PIO
    LD A,0xCF
    OUT (CS_PIO_AC), A
    LD A,11110101b
    OUT (CS_PIO_AC), A


    xor a
    ;ld i, a
    ;im 2    ;set int mode 2
    
    ld (var_curserstate),a
    ld (var_curseron),a
    ld a, " "
    ld (var_curserchar),a


    ;ei      ; Enable Interrupts

    ;jr mon_start_ram    ;skip serial, cause not used atm
mon_start_init_serial:
    call CONSOLE_INIT
mon_start_ram:
    ld hl,mon_var_template
    ld de,mon_var_template_end
    ld bc,SYS_RAM_START
mon_start_ram_loop:
    ld a,(hl)  ;copy values
    ld (bc),a
    inc hl      
    inc bc
    push hl    ;check if end is reached
    sbc hl,de
    pop hl
    jp nz, mon_start_ram_loop
    ;template copy done

mon_start_complete:
    
    ;call keyboard_init_io
    ;call vdpconsole_init

    ;call print_str
    

    ;jp splash_run

    ;jp ps2demo_run
    ;call debug_init
    ;call vdp_cursor_on
    ;jp COLD

    call print_clear
    ld hl, [STR_Banner_Start]
    call print_str
    
    ;halt

    LD DE,0x40
    CALL beep

    ;call vdp_cursor_on
    call PROMPT_BEGIN
    
    ;halt CPU if prompt exits
    halt
    
; Misc Functions
AY0_WRITE_REG:
    LD B,A
    LD A,D
    OUT (IO_AY0_ADDR),A
    LD A,B
    OUT (IO_AY0_DATA),A
    RET

PROMPT_BEGIN:
    call print_newLine
    ;call A_RTS_ON
    ld a,'>'
    call print_char
    xor a  ;reset buffer len
    ld (var_buffer_len),a

PROMPT_BEGIN_READ_LOOP:
    call read_char
    ;call keybd_read_ascii
    jp z, PROMPT_BEGIN_READ_LOOP    ; wait until char avail
    push af
    pop af
    ; process special ops
    cp 13   ; enter
    jp z,PROMPT_BEGIN_READ_PROCESS
    cp 10
    jp z, PROMPT_BEGIN_READ_LOOP; skip LF for file load
    cp 0x08 ; backspace 0x08 VT102 0x7f Putty
    jp z,PROMPT_BEGIN_READ_BACKSPACE

    push af
    ; a contains latest char
    ld hl,[var_buffer]
    ld d,0
    ld a,(var_buffer_len)
    ld e,a
    add hl,de   ;hl now contains pointer to last position in buffer
    inc a
    ld (var_buffer_len),a ;store incremented buffer length

    pop af
    ld (hl),a
    call print_char
    inc hl
    xor a       ;a = 0
    ld (hl),a   ;always add null termination after last char
    jp PROMPT_BEGIN_READ_LOOP

PROMPT_BEGIN_READ_BACKSPACE:
    ld a,(var_buffer_len)
    and a
    jp z, PROMPT_BEGIN_READ_LOOP    ; do not continue if already at char 0
    dec a       ;decrement length
    ld (var_buffer_len),a   ;and store it
    ld e,a      ;load de with decremented value
    ld d,0
    ld hl,[var_buffer]
    add hl,de   ;hl now contains pointer to last position in buffer
    xor a       ; store null byte to current location
    ld (hl),a
    ;call print_delete
    ld a, 0x08
    call print_char
    ld a, 0x20
    call print_char
    ld a, 0x08
    call print_char
    jp PROMPT_BEGIN_READ_LOOP

PROMPT_BEGIN_READ_PROCESS:
    ;call print_newLine
    ;ld hl,var_buffer
    ;call print_str

    ld a,([var_buffer])
    cp '$'              ;jump to addr
    jp z, CMD_EXEC
    cp '?'              ;print hexdump
    jp z, CMD_VIEW
    cp '!'              ;set memory
    jp z, CMD_SET
    cp 'i'              ;in IO
    jp z, CMD_IO_READ
    cp 'o'              ;out IO
    jp z, CMD_IO_WRITE
    cp 'd'              ;disassemble
    jp z, CMD_DASM
    cp 'x'              ;start xmodem
    jp z, xmodem_init


    call print_newLine
    ld hl, [STR_Unknown]
    call print_str
    jp PROMPT_BEGIN

    ret

CMD_EXEC:
    xor a                   ;write null byte to buffer pos 0 to prevent reexecute the last command
    ld (var_buffer),a

    ld hl,var_buffer+1      ;load 1st byte
    call DHEX_TO_BYTE       
    ld b,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR

    ld hl,var_buffer+3      ;load 2nd byte
    call DHEX_TO_BYTE
    ld c,a
    ld a,e  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR

    ld h,b
    ld l,c
    jp (hl)

CMD_VIEW:
    call print_newLine
    ld hl,var_buffer+1      ;load 1st byte
    call DHEX_TO_BYTE       
    ld b,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR

    ld hl,var_buffer+3      ;load 2nd byte
    call DHEX_TO_BYTE
    ld c,a
    ld a,e  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR  


    ld a,(var_buffer+5)
    cp ' '
    jp nz, CMD_SYNTAX_ERROR

    ld hl,var_buffer+6      ;load length
    call DHEX_TO_BYTE
    push af
    ld a,e  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR  
    
    ;draw header
    ld hl,[STR_HEXDUMP_HEADER]
    call print_str

    pop af
    ;loading vars done. display results
    ld h, b ;pointer to current byte
    ld l, c ;pointer to current byte
    ld b, a                 ;bytes counter

    ;draw row
CMD_VIEW_ROW:
    call print_newLine
    ld a,h              ;print start
    call print_a_hex
    ld a,l
    call print_a_hex
    ld a, ' '
    call print_char
    ld c, 8                 ;column counter
CMD_VIEW_ROW_LOOP:
    ld a,(hl)
    call print_a_hex
    
    inc hl  ;increment pointer
    dec b   ;decrement byte counter
    dec c   ;decrement column counter

    ld a,b
    and a
    jp z,CMD_VIEW_END   ;if byte counter = 0 -> end reached

    ld a,c
    and a
    jp z,CMD_VIEW_ROW   ;else if column counter = 0 -> 16 chars printed. next row

    ld a, ' '
    call print_char
    jp CMD_VIEW_ROW_LOOP    ;else
CMD_VIEW_END:
    call print_newLine
    jp PROMPT_BEGIN

CMD_SET:
    ld hl,var_buffer+1      ;load 1st byte
    call DHEX_TO_BYTE       
    ld b,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR

    ld hl,var_buffer+3      ;load 2nd byte
    call DHEX_TO_BYTE
    ld c,a
    ld a,e  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR    
    ;bc now contains the start address

    ld hl,var_buffer+5
CMD_SET_LOOP:
    ld a,(hl)
    cp 0    ;if 0 then end
    jp z, CMD_SET_END
    cp ' '
    jp nz, CMD_SYNTAX_ERROR
    inc hl  ;next byte
    call DHEX_TO_BYTE
    ld (bc),a   ;load byte to 
    ld a,e
    and a
    jp nz, CMD_SYNTAX_ERROR    
    inc bc
    inc hl
    inc hl
    jp CMD_SET_LOOP
CMD_SET_END:
    call print_newLine
    jp PROMPT_BEGIN


CMD_IO_WRITE:
    ld hl,var_buffer+1      ;load 1st byte
    call DHEX_TO_BYTE       
    ld c,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR

    ld a,(var_buffer+3)
    cp ' '
    jp nz, CMD_SYNTAX_ERROR

    ld hl,var_buffer+4      ;load 1st byte
    call DHEX_TO_BYTE       
    push af
    ld a,e                  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR

    pop af
    out (c),a    
    call print_newLine
    jp PROMPT_BEGIN

CMD_IO_READ:
    ld hl,var_buffer+1      ;load 1st byte
    call DHEX_TO_BYTE       
    ld c,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR

    in a,(c)
    push af
    call print_newLine
    pop af
    call print_a_hex
    call print_newLine
    jp PROMPT_BEGIN

CMD_DASM:
    call print_newLine
    ld hl,var_buffer+1      ;load 1st byte
    call DHEX_TO_BYTE       
    ld b,a                  ;store result in b
    ld a,e                  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR

    ld hl,var_buffer+3      ;load 2nd byte
    call DHEX_TO_BYTE
    ld c,a
    ld a,e  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR  


    ld a,(var_buffer+5)
    cp ' '
    jp nz, CMD_SYNTAX_ERROR

    ld hl,var_buffer+6      ;load length
    call DHEX_TO_BYTE
    push af
    ld a,e  ;check for error
    and a
    jp nz, CMD_SYNTAX_ERROR  
    
    ld h,b
    ld l,c

    pop af  ;restore af
    ld b,a

    call disassemble

    
    jp PROMPT_BEGIN  

CMD_SYNTAX_ERROR:
    call print_newLine
    ld hl, [STR_SyntaxError]
    call print_str
    jp PROMPT_BEGIN



Includes:
;.include "bootldr.s"
.include "console.s"
.include "conversions.s"
;.include "basic.s"
.include "disassembler.s"
.include "disassembler_table.s"
.include "rst.s"
.include "beep.s" 
; Strings
STR_Banner_Start:
    db "Z8C Monitor V2 by Dennis Gunia (2022)",0
STR_SyntaxError:
    db "syn?",0
STR_Unknown:
    db "cmd?",0
STR_HEXDUMP_HEADER:
    db 'BASE 0  1  2  3  4  5  6  7',0

.include "xmodem.s"
;.include "debug.s"