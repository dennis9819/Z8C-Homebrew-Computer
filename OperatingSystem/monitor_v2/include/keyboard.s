;----------------------------------------------------------------
;Keyboard input library for Z8C 
;by Dennis Gunia (04/2022)
;----------------------------------------------------------------

var_ps2_extension   equ var_ps2mem+4    ;extension code
var_ps2_shift       equ var_ps2mem+5    ;shift down = 0xFF, up = 0x00
var_ps2_raw         equ var_ps2mem+6    ;raw scan code

keybd_read:
    xor a
    ld (var_ps2_extension), a
    call keybd_read_wait
    cp 0xE0
    jr z, keybd_read_extended   ;handle extended scancodes
    cp 0xE1
    jr z, keybd_read_extended_E1   ;handle extended scancodes for stupid pause button ....
    ;else fall through to keybd_read_simple
    
keybd_read_simple:
    cp 0xF0             ;check if break 
    jr z, keybd_read_break  ;handle break code logic
    cp 0x12
    jr z, keybd_shift_down
    cp 0x59
    jr z, keybd_shift_down
    ;else process key
    jr keybd_read_return

keybd_read_extended:
    ld (var_ps2_extension), a   ;store first byte to ram
    call keybd_read_wait        ;read key
    cp 0xF0                     ;check if break 
    jr z, keybd_read_break_extended      ;handle break code logic
    ;else process key
    jr keybd_read_return

keybd_read_extended_E1: ;pause key
    call keybd_read_wait  ;read realeased key
    call keybd_read_wait  ;read realeased key
    call keybd_read_wait  ;read realeased key
    call keybd_read_wait  ;read realeased key
    call keybd_read_wait  ;read realeased key
    call keybd_read_wait  ;read realeased key
    ;fall-thorugh for last byte
keybd_read_break_extended:
    call keybd_read_wait  ;read realeased key
    jr keybd_read

keybd_read_break:
    call keybd_read_wait  ;read realeased key
    cp 0x12
    jr z, keybd_shift_up
    cp 0x59
    jr z, keybd_shift_up
    jp keybd_read

keybd_read_return:
    ;a contains scan code
    ret


keybd_shift_down:
    ld a,0x01
    ld (var_ps2_shift),a
    jr keybd_read
keybd_shift_up:
    xor a
    ld (var_ps2_shift),a
    jr keybd_read


;wait for keyboard input
keybd_read_wait:
    call keyboard_read
    jr z, keybd_read_wait    ;read again if error
    ret


keybd_read_ascii:
    call keybd_read
    ;push af
    ;call print_a_hex
    ;pop af

    ld ix, [keybd_lut]  ;LUT base address
    ld bc, 4            ;increments
    ld d, a             ;Load scancode into d
keybd_read_ascii_seekloop:
    ld a, (ix)          ;load scancode from table
    or a                ;test if scancode is 0x00 -> indicates end of list
    jr z, keybd_read_ascii_notfound ;reached end of list wizhout match
    cp d                ;compare scancode
    jr z, keybd_read_ascii_match    ;found entry
    add ix,bc
    jr keybd_read_ascii_seekloop

keybd_read_ascii_match:
    ld b,0
    ld a, (var_ps2_shift)   ;if shift offset+1
    inc a                   ;add ofset for column1
    ld c,a      
    add ix,bc               ;add column address to row address
    ld a,(ix)
    ret

keybd_read_ascii_notfound:
    jr keybd_read_ascii

keybd_lut:
    defb 0x1C, "a", "A", "@"
    defb 0x32, "b", "B", 0x00
    defb 0x21, "c", "C", 0x00
    defb 0x23, "d", "D", 0x00
    defb 0x24, "e", "E", 0x00
    defb 0x2B, "f", "F", 0x00
    defb 0x34, "g", "G", 0x00
    defb 0x33, "h", "H", 0x00
    defb 0x43, "i", "I", 0x00
    defb 0x3B, "j", "J", 0x00
    defb 0x42, "k", "K", 0x00
    defb 0x4B, "l", "L", 0x00
    defb 0x3A, "m", "M", 0x00
    defb 0x31, "n", "N", 0x00
    defb 0x44, "o", "O", 0x00
    defb 0x4D, "p", "P", 0x00
    defb 0x15, "q", "Q", 0x00
    defb 0x2D, "r", "R", 0x00
    defb 0x1B, "s", "S", 0x00
    defb 0x2C, "t", "T", 0x00
    defb 0x3C, "u", "U", 0x00
    defb 0x2A, "v", "V", 0x00
    defb 0x1D, "w", "W", 0x00
    defb 0x22, "x", "X", 0x00
    defb 0x1A, "y", "Y", 0x00
    defb 0x35, "z", "Z", 0x00
    defb 0x45, "0", "=", "}"
    defb 0x16, "1", "!", 0x00
    defb 0x1E, "2", 0x22, 0x00
    defb 0x26, "3", "3", 0x00
    defb 0x25, "4", "$", 0x00
    defb 0x2E, "5", "%", 0x00
    defb 0x36, "6", "&", 0x00
    defb 0x3D, "7", "/", "{"
    defb 0x3E, "8", "(", "["
    defb 0x46, "9", ")", "]"
    defb 0x41, ",", ";", 0x00
    defb 0x49, ".", ":", 0x00
    defb 0x4A, "-", "_", 0x00
    defb 0x5D, "#", "'", 0x00
    defb 0x5B, "+", "*", "~"
    defb 0x4E, "s", "?", 0x5C
    defb 0x61, "<", ">", "|"
    defb 0x5A, 13, 13, 13   ;enter
    defb 0x66, 0x08, 0x08, 0x08 ;backspace
    defb 0x29, " ", " ", 0x00   ;space
    defb 0x00 ;end of LUT
 