COMMAND_LUT:
    db "date", 0 , [OP_RTIME], [OP_RTIME]>>8    ;Read time
    db "setdate", 0 , [OP_STIME], [OP_STIME]>>8    ;Read time
    db "pin ", 0 , [OP_IO_IN], [OP_IO_IN]>>8    ;Read port
    db "dump ",0, [OP_DUMP], [OP_DUMP]>>8       ;print pretty hexdump
    db "pout ", 0 , [OP_IO_OUT], [OP_IO_OUT]>>8    ;Write port
    db "iin ", 0, [OP_IIC_IN], [OP_IIC_IN]>>8      ;Read IIC
    db "iout ", 0, [OP_IIC_OUT], [OP_IIC_OUT]>>8     ;Write IIC
    db "call ", 0, [OP_CALL], [OP_CALL]>>8     ;Call to addr
    db "clr", 0, [OP_CLR], [OP_CLR]>>8     ;Call to addr
    db "dasm ", 0, [OP_DASM], [OP_DASM]>>8     ;Call to addr
    db "jp ", 0,[OP_EXEC], [OP_EXEC]>>8         ;jump to addr
    db "rst", 0,0x00,0x00         ;soft reset
    db "lsdsk", 0,[OP_LSDSK], [OP_LSDSK]>>8         ;list disks
    db "seldsk ", 0,[OP_SELDSK], [OP_SELDSK]>>8         ;select disk
    db "cd ", 0 , [OP_CD], [OP_CD]>>8    ;Read time
    db "ls", 0 , [OP_DIR], [OP_DIR]>>8    ;Read time
    db "run ", 0 , [OP_FSEXEC], [OP_FSEXEC]>>8    ;Read time
    db "$", 0, [OP_EXEC], [OP_EXEC]>>8          ;jump to addr
    db "i", 0, [OP_IO_IN], [OP_IO_IN]>>8       ;Read port
    db "o", 0, [OP_IO_OUT], [OP_IO_OUT]>>8       ;Write port
    db "!", 0, [OP_SET], [OP_SET]>>8        ;Write memory
    db "?", 0, [OP_DUMP], [OP_DUMP]>>8        ;Print memory
    db 0xFF             ;End of Table

COMMAND_ABORT:
    ;cleanup stack
    ld sp, STACK_RAM_TOP
    ; return to prompt
COMMAND:
    call print_newLine
    ld hl,[var_dir]
    call print_str
    ld a,'>'
    call print_char
    xor a  ;reset buffer len
    ld (var_buffer_len),a   ;set buffer len to 0
COMMAND_READ:
    call read_char
    jp z, COMMAND_READ   ;wait for input
    cp 13   ; enter
    jp z,COMMAND_PROCESS
    cp 10
    jp z, COMMAND_READ; skip LF for file load
    cp 0x08 ; backspace 0x08 VT102 0x7f Putty
    jp z,COMMAND_BACKSPACE

    push af
    ; a contains latest char
    ld hl,[var_input]
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
    jp COMMAND_READ


COMMAND_BACKSPACE:
    ld a,(var_buffer_len)
    and a
    jp z, COMMAND_READ    ; do not continue if already at char 0
    dec a       ;decrement length
    ld (var_buffer_len),a   ;and store it
    ld e,a      ;load de with decremented value
    ld d,0
    ld hl,[var_input]
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
    jp COMMAND_READ
COMMAND_PROCESS:
    ;compare
    LD HL,[COMMAND_LUT] ;Lookup table
COMMAND_PROCESS_LOOP:
    LD DE,[var_input]  ;Buffer
    LD A,(HL)           ;Load first byte of entry
    CP 0xFF
    JP Z,COMMAND_PROCESS_NOT_FOUND ;if first byte is 0xFF, End is reached
    ; compare string loop
COMMAND_PROCESS_LOOP_STR1:
    LD A,(DE)
    LD B,A
    LD A,(HL) 
    OR B    ;not 0 -> match
    JP Z, COMMAND_PROCESS_FOUND  ;match

    LD A,(DE)
    LD B,A
    ;LD A,(HL)           ;Load first byte of entry
    ;call print_a_hex
    LD A,(HL) 
    ;compare byte
    XOR B
    OR A    ;if identical = resoult shopuld be zero
    JP Z, COMMAND_PROCESS_LOOP_STR2  ;then continue 
    ; if not identical
    LD A,(HL)
    OR A    ;if reached end of compare string
    JP Z, COMMAND_PROCESS_FOUND  ;match
    JR COMMAND_PROCESS_NEXT_ENTRY ;next entry on no match

COMMAND_PROCESS_LOOP_STR2:   ;continue with next char
    INC HL
    INC DE
    JR COMMAND_PROCESS_LOOP_STR1

COMMAND_PROCESS_NEXT_ENTRYI: ;do not jump here
    INC HL  
COMMAND_PROCESS_NEXT_ENTRY:  ;jump here
    LD A,(HL)
    OR A
    JP NZ,COMMAND_PROCESS_NEXT_ENTRYI    ;loop until end of string
    INC HL                              ;skip pointer
    INC HL
    INC HL
    JP COMMAND_PROCESS_LOOP
COMMAND_PROCESS_NOT_FOUND:
    LD HL,[_STR_NOT_FOUND]
    CALL print_str
    JP COMMAND

COMMAND_PROCESS_FOUND:
    PUSH HL
    POP BC
    INC BC
    LD A,(BC)
    LD L,A
    INC BC
    LD A,(BC)
    LD H,A 

    ;HL: pointer to start of routine
    ;DE: buffer start of arguments
    CALL _COMMAND_PROCESS_FOUND
    JP COMMAND
_COMMAND_PROCESS_FOUND
    JP (HL)



NOT_IMPLEMENTED:
    LD HL,[_STR_NOT_IMPLEMENTED]
    CALL print_str
    RET

ERR_SYNTAX:
    LD HL,[_STR_SYNTAX]
    CALL print_str
    RET

_STR_NOT_IMPLEMENTED:
    db 10,13,"not implemented",10,13,0

_STR_NOT_FOUND:
    db 10,13,"invalid command",10,13,0

_STR_SYNTAX:
    db 10,13,"invalid syntax",10,13,0