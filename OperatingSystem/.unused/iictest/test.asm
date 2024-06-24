.include "include/extern_symbols.s" ;include monitor symbols.
    org 0x8000  

	call TermInit
	ret
;-------------------------------------------------
; Z8C/MEGA80 VDP Console IO Functions
; by Dennis Gunia 2022/02
; Version 0.1
;
; For Z8C Monitor
;-------------------------------------------------

; Constant Values
TermVarMemoryLocation        equ 0x40E0


; Dynamic VARs
TermVarTemplate:
    phase TermVarMemoryLocation  ;Change pointers to this template to the specified location
TermCurrentX:   ; Start sector
    defb 0x01
TermCurrentY:   ; Start sector
    defb 0x04
TermCursorState:
    defb 0x00
TermCursorEnabled:
    defb 0x00
TermVarTemplateEnd:
    dephase



TermInit:
    ; Copy TermVarTemplate to memory
    LD HL, TermVarMemoryLocation     ;Load destination into HL
    LD BC, [TermVarTemplate]         ;Load source into BC
    LD DE, [TermVarTemplateEnd]      ;Load end into DE
TermInit_copy_loop:
    LD A,(BC)       ;Copy value
    LD (HL),A       
    INC BC          ;Increment pointers
    INC HL
    PUSH HL         ;Backup HL
    SBC HL,DE       ;Check if next pointer matches end
    POP HL          ;Restore HL
    JP NZ, TermInit_copy_loop    ;Loop if pointer != end
    ; template copy done


    ; install font
    call    setup_vars
    ld      hl, TmsFont
    call    TmsTextMode                     ; initialize text mode

    ld      a, TmsDarkRed                  ; set colors
    call    TmsBackground
    ld      a, TmsWhite
    call    TmsTextColor

    ld      a, 0                           ; put title at 11, 1
    ld      e, 0
    call    TmsTextPos
    ld      hl, TitleMessage                ; output title
    call    TmsStrOut

    ld      a, 0                           ; put title at 11, 1
    ld      e, 4
    call    TmsTextPos


    ret

TermSetCursor:
    ld      a, (TermCurrentY)
    ld      e, a
    ld      a, (TermCurrentX)                           
    
    call    TmsTextPos
    ret

TermPrintStr:
    ; BC contains String offset
    ; Prints null terminated string
    PUSH AF
    PUSH BC
TermPrintStr_LOOP:
    LD A,(BC)

    ; Return if nullbyte
    OR 0
    JR Z, TermPrintStr_EXIT
    ; Print Char
    CALL TermPrintChar

    ; INC BC and JP start of loop
    INC BC
    jp TermPrintStr_LOOP
TermPrintStr_EXIT:
    POP BC
    POP AF
    RET

TermPrintChar:
    push AF
    push BC
    push DE
    push HL
    ; check if special char
    cp 0x7F ; Backspace
    jp z, TermPrintCharSPDelete
    cp 13
    jp z, TermPrintCharSPCR
    cp 10
    jp z, TermPrintCharSPLF
    ; a has char
    call TmsChrOut
    ld a,(TermCurrentX)
    inc a
    cp 39
    jp z,TermPrintCharWrap; if line end
    jp nc,TermPrintCharWrap; if line end
    ld (TermCurrentX), a
    call TermSetCursor
    jp TermPrintCharDone
TermPrintCharWrap:
    xor a
    ld (TermCurrentX), a
    ld a,(TermCurrentY)
    cp 23   ; if already last line
    jp z, TermPrintCharSPLFScroll
    inc a
    ld (TermCurrentY),a
    call TermSetCursor
    jp TermPrintCharDone
TermPrintCharSPDelete:
    ld a,(TermCurrentX)
    and a
    jp z, TermPrintCharSPDeleteWrap ; if at pos X 0 then go previous line
    dec a   ; else decrement X
    ld (TermCurrentX), a
    call TermSetCursor
    ld      a, " "     
    call    TmsChrOut
    call TermSetCursor
    jp TermPrintCharDone
TermPrintCharSPDeleteWrap:
    ld a,39
    ld (TermCurrentX), a
    ld a,(TermCurrentY)
    dec a
    ld (TermCurrentY),a
    call TermSetCursor
    ld      a, " "     
    call    TmsChrOut
    call TermSetCursor
    jp TermPrintCharDone

TermPrintCharSPLF:  ; linefeed
    ld a,(TermCurrentY)
    cp 23   ; if already last line
    jp z, TermPrintCharSPLFScroll
    inc a
    ld (TermCurrentY),a
    call TermSetCursor
    jp TermPrintCharDone
TermPrintCharSPLFScroll:
    call TermScroll
    ld a,23
    ld (TermCurrentY),a
    call TermSetCursor
    jp TermPrintCharDone
TermPrintCharSPCR:  ; linefeed
    xor a
    ld (TermCurrentX),a
    call TermSetCursor
    jp TermPrintCharDone

TermPrintCharDone:
    pop HL
    pop DE
    pop BC
    pop AF
    ret

TermScroll:
    ld bc, 0x3800    ; load start of text-buffer into pointer

TermScrollLine:
    ld h,b
    ld l,c

    ld de, 40   ; line width
    add hl, de

    ld a,l          ; setup VDP address
    out (0x81),a
    ld a,h
    and 3fh
    out (0x81),a
    call VDPWait
    in a, (0x80)    ; read byte
    push af

    ld a,c          ; setup VDP address
    out (0x81),a
    ld a,b
    and 3fh         
    or 40h          ; write bit
    out (0x81),a
    pop af
    call VDPWait
    out (0x80),a

    inc bc

    ld h,b
    ld l,c
    ld de, VDPTextBufferEnd   ; buffer end
    sbc hl, de

    jp nz, TermScrollLine

    ; clear last line
    ld bc,920
    ld a,c          ; setup VDP address
    out (0x81),a
    ld a,b
    and 3fh         
    or 40h          ; write bit
    out (0x81),a
    ld c,40
TermScrollClearLine:
    ld bc, 0x3800 + 920
TermScrollClearLineLoop:

    ld a,c          ; setup VDP addressh
    out (0x81),a
    ld a,b
    and 3fh         
    or 40h          ; write bit
    out (0x81),a
    ld a, ' '
    out (0x80),a
    call VDPWait

    inc bc
    ld h,b
    ld l,c
    ld de, 0x3800 + 960   ; buffer end
    sbc hl, de

    jp nz, TermScrollClearLineLoop
    ret

VDPWait:
    nop
    nop
    nop
    nop
    ret

VDPTextBufferStart  equ 0
VDPTextBufferEnd    equ 0x3800 + 920

TitleMessage:    
    db '  Z8C Mk IV - A Z80 Homebrew Computer   '
    db '  ROM Ver. 1.0 by Dennis Gunia (2022)   '
    db '         48k RAM - 4MHz Z80 CPU         ',0

TmsSubs:
    .include "tms.s"                       ; TMS graphics routines
;        .include "utility.s"                   ; BDOS utility routines
    .include "font1.s"
    

TermClear:
    ld bc, 0x3800
TermClearLoop:
    ld a,c          ; setup VDP addressh
    out (0x81),a
    ld a,b
    and 3fh         
    or 40h          ; write bit
    out (0x81),a
    ld a, ' '
    out (0x80),a
    call VDPWait
    inc bc

    ld h,b
    ld l,c
    ld de, 0x3800 + 960   ; buffer end
    sbc hl, de
    jp nz, TermClearLoop

    xor a
    ld (TermCurrentX), a
    ld (TermCurrentY), a

    call TermSetCursor

    ret