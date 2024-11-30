;; VDP Terminal Driver

__VDPBUFFER_COUNTER equ var_idebuffer + 767
__VDPBUFFER_CURSORADDR equ var_idebuffer + 768
__VDPBUFFER_LINE equ var_idebuffer + 770


__VTERM_MAX_LINES equ 23
__VTERM_MAX_CPL equ 81

VTERM_INIT:
    ; init variables
    xor a
    ld (var_curserx),a
    ld (var_cursery),a
    ld (var_curseron),a
    ld hl, 0x0A00
    ld (__VDPBUFFER_CURSORADDR),hl
    call _VTERM_SET_CURSORADDR
    ret

; --- Console enable cursor ---
; destroy a
VTERM_CURSOR_ON:
    push hl
    push bc
    push de
    ld a,1
    ld (var_curseron),a
    call _VTERM_MOVE_CURSOR
    pop de
    pop bc
    pop hl
    ret
; --- Console disable cursor ---
; destroy a
VTERM_CURSOR_OFF:
    push hl
    push bc
    xor a
    ld (var_curseron),a
    ld hl,(__VDPBUFFER_CURSORADDR)
    call VDP_RAMADDR_WR
    xor a
    out (VDP_RAM),a ;clear old cursor
    call _VTERM_SET_CURSORADDR
    pop bc
    pop hl
    ret
; --- Console print string (null terminated) ---
; inputs: hl (pointer to string)
; modify hl
; destroy a
VTERM_PRINT_STR:
    ld a,(hl)
    or a
    ret z
    call VTERM_PRINT
    inc hl
    jr VTERM_PRINT_STR

; --- Console print char ---
; inputs: a (ascii char)
VTERM_PRINT:
    push af
    push bc
    push de
    push hl

    push af
    call print_a_hex
    pop af
    cp 0x08
    jp z,_VTERM_PRINT_BACKSPC
    cp 13
    jp z, _VTERM_PRINT_CR
    cp 10
    jp z, _VTERM_PRINT_LF
    ; else print a
    call _VTERM_CHAROUT
_VTERM_PRINT_END:
    call _VTERM_MOVE_CURSOR
    pop hl
    pop de
    pop bc
    pop af
    ret
_VTERM_PRINT_BACKSPC:
    call _VTERM_CURSOR_BACK
    ld a,' '
    out (VDP_RAM),a
    jp _VTERM_PRINT_END
_VTERM_PRINT_CR:
    call _VTERM_CURSOR_START
    jp _VTERM_PRINT_END
_VTERM_PRINT_LF:
    call _VTERM_CURSOR_NEWLINE
    jp _VTERM_PRINT_END

; only called by VTERM_PRINT
; destroys af,bc,de,hl
_VTERM_CHAROUT:
    push af             ;store char
    ld a,(var_curserx)  ;test for line wrap
    inc a
    cp __VTERM_MAX_CPL +1
    jr nz, __VTERM_CHAROUT_NOBREAK  ; if linewrap:
    call _VTERM_CURSOR_START        ; do new line
    call _VTERM_CURSOR_NEWLINE
__VTERM_CHAROUT_NOBREAK:            ; if not linewrap
    ld a,(var_curserx)  ; move curser by one
    inc a
    ld (var_curserx),a
    pop af
    out (VDP_RAM),a
    ret

; destroys af,bc,de,hl
_VTERM_CURSOR_BACK:
    ld a,(var_curserx)
    or a
    ret z;if line wrap, ignore
    dec a
    ld (var_curserx),a
    jp _VTERM_MOVE_CURSOR

; --- Move cursor to start of line ---
; destroys af,bc,de,hl
_VTERM_CURSOR_START:
    xor a
    ld (var_curserx),a
    jp _VTERM_MOVE_CURSOR

; --- Move cursor to next line ---
; destroys af,bc,de,hl
_VTERM_CURSOR_NEWLINE:
    ld a,(var_cursery)
    cp __VTERM_MAX_LINES
    jp z, _VTERM_SCROLL
    inc a
    ld (var_cursery),a
    jp _VTERM_MOVE_CURSOR

; --- Calculate 16bit VRAM adress of current char ---
; destroy AF,DE
; outputs HL
_VTERM_CALC_CURSORADDR:
    ld d,0
    ld a,(var_cursery)
    ld e,a
    ld hl,0
    add     hl, de                  ; Y x 1
    add     hl, hl                  ; Y x 2
    add     hl, hl                  ; Y x 4
    add     hl, de                  ; Y x 5
    add     hl, hl                  ; Y x 10
    add     hl, hl                  ; Y x 20
    add     hl, hl                  ; Y x 40
    add     hl, hl                  ; Y x 80
    ld a,(var_curserx)
    ld e,a
    add     hl, de                  ; add X for final address
    ret

; destroy AF,DE,HL
_VTERM_SET_CURSORADDR:
    call _VTERM_CALC_CURSORADDR
    jp VDP_RAMADDR_WR

; destroy AF,DE,HL
_VTERM_SET_CURSORADDR_RD:
    call _VTERM_CALC_CURSORADDR
    jp VDP_RAMADDR_RD

; destroys af,bc,de,hl
_VTERM_MOVE_CURSOR:
    ld a,(var_curseron)
    or a
    jp z, _VTERM_SET_CURSORADDR ;if cursor disable, only set new memory location
    ;if cursor is enabled
    ld hl,(__VDPBUFFER_CURSORADDR)
    call VDP_RAMADDR_WR
    xor a
    out (VDP_RAM),a ;clear old cursor
    ; calulate new position
    call _VTERM_CALC_CURSORADDR
    ld a,l      ;load lower byte
    and 0x07    ;mask lower 3 bits
    ld b,a      ;save lower 3 bits
    srl h
    rr l
    srl h
    rr l
    srl h
    rr l
    ld de,0x0A00
    add hl,de
    ld (__VDPBUFFER_CURSORADDR), hl
    call VDP_RAMADDR_WR
    ld a,128
_VTERM_MOVE_CURSOR_LOOP:
    rrca
    djnz _VTERM_MOVE_CURSOR_LOOP
    out (VDP_RAM),a
    jp _VTERM_SET_CURSORADDR

; --- Memory Scroll ---
_VTERM_SCROLL:
    ;COPY / Scroll up using VDP commands 
    ld e,15 ;select status register
    ld a,2
    call VDP_SETREG
    in a,(VDP_REGISTER)
    and 0x40    ;Vertical retrace flag
    jr z, _VTERM_SCROLL; if not loop
    ; else setup address
    xor a
    ld (var_curserx),a
    ld (__VDPBUFFER_COUNTER),a
_VTERM_SCROLL_LINE:
    ld a,(__VDPBUFFER_COUNTER) ;set next line
    inc a
    ld (var_cursery),a
    call _VTERM_SET_CURSORADDR_RD
    ld b,__VTERM_MAX_CPL
    ld hl,__VDPBUFFER_LINE   ;buffer location
    ld c, VDP_RAM
_VTERM_SCROLL_LINE_L1:
    ini    ;load 80 bytes to ram
    jr nz, _VTERM_SCROLL_LINE_L1
    ;setup target row
    ld a,(__VDPBUFFER_COUNTER) ;set next line
    ld (var_cursery),a
    call _VTERM_SET_CURSORADDR
    ld b,__VTERM_MAX_CPL
    ld hl,__VDPBUFFER_LINE   ;buffer location
_VTERM_SCROLL_LINE_L2:
    outi    ;store to vdp ram
    jr nz, _VTERM_SCROLL_LINE_L2
;done with one line
    ld a,(__VDPBUFFER_COUNTER)
    inc a
    ld (__VDPBUFFER_COUNTER),a
    cp __VTERM_MAX_LINES
    jp nz, _VTERM_SCROLL_LINE
    ld (var_cursery),a
    call _VTERM_SET_CURSORADDR
    ;fill 
    ld b,__VTERM_MAX_CPL
_VTERM_SCROLL_LINE_FILL:
    xor a
    out (VDP_RAM),a
    djnz _VTERM_SCROLL_LINE_FILL
    jp _VTERM_SET_CURSORADDR