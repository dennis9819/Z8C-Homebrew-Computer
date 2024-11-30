.include "extern_symbols.s" ;include monitor symbols.

CS_VT82C42_DATA .EQU 0xF0
CS_VT82C42_CTRL .EQU 0xF1

    org 0x6000  
    ;VT82C42

keyboard_init:
    ld a, 0xA7                   ;Disable Mouse
    out (CS_VT82C42_CTRL), A
    ld a, 0xAD                   ;Disable Keyboard
    out (CS_VT82C42_CTRL), A
_keyboard_init_flush_buffer:
    in a,(CS_VT82C42_DATA)      ;Read buffer
    in a,(CS_VT82C42_CTRL)      ;Read status byte
    bit 0,a                     ;Test if buffer is empty
    jr nz, _keyboard_init_flush_buffer
    ;buffer is now flushed. Now set the Controller Configuration Byte

    ld a, 0x60              ;next byte is command byte register write
    ld b, 11111100b         ;Disable bits 0,1,6 (disable IRQ and Translation)
    call keyb_cmd_wr

    ;Perform Controller Self Test 
    ld a, 0xAA
    call keyb_cmd_rd
    cp 0x55
    jr nz, _keyboard_init_failed
    
    ;Perform Interface Tests 
    ld a, 0xAB
    call keyb_cmd_rd
    or a
    jr nz, _keyboard_init_failed

    ;Enable Devices
    ld a,0xAE
    out (CS_VT82C42_CTRL), A
    call keyb_wait_ibf_empty

    ;Test if device is present Devices 
    ld a,0xEE      
    out (CS_VT82C42_DATA), A    ;Send echo to keboard (0xEE command)
    call keyb_wait_ibf_empty
    call keyb_wait_obf
    in a,(CS_VT82C42_DATA)
    cp a, 0xEE
    jr z, _keyboard_init_okay    ; 0xFC -> Success. Init done!
    ;Else device error
    ld hl, [STR_keyboard_init_failed]
    call print_str
    ret
_keyboard_init_failed:
    LD HL, [STR_keyboard_init_err]
    CALL print_str
    RET
_keyboard_init_dev_missing:
    LD HL, [STR_keyboard_init_missing]
    CALL print_str
    RET
_keyboard_init_okay:
    LD HL, [STR_keyboard_init_okay]
    CALL print_str
    RET

; a contains command
; b conatins data
keyb_cmd_wr:    
    out (CS_VT82C42_CTRL),a ;write command byte
    ld a, b
    out (CS_VT82C42_DATA),a
    ret

; a contains command
; a returns data
keyb_cmd_rd:
    out (CS_VT82C42_CTRL),a ;write command byte
_keyb_cmd_rd_l1:
    in a, (CS_VT82C42_CTRL) ;read status
    call print_a_hex
    rra
    jr nc, _keyb_cmd_rd_l1   ;wait until OBF is set (data avail)
    in a, (CS_VT82C42_DATA)
    ret

keyb_wait_ibf_empty:
    in a, (CS_VT82C42_CTRL) ;read status
    rra
    rra
    jr c, keyb_wait_ibf_empty  ;if IBF, wait
    ret

keyb_wait_obf:
    in a, (CS_VT82C42_CTRL) ;read status
    rra
    jr nc, keyb_wait_obf  ;if IBF, wait
    ret

;Status message strings
STR_keyboard_init_okay:
    .BYTE "PS/2 Keyboard initialized.",0
STR_keyboard_init_err:
    .BYTE "PS/2 Controller error! System HALT!",0
STR_keyboard_init_failed:
    .BYTE "PS/2 Keyboard error! System HALT!",0
STR_keyboard_init_missing:
    .BYTE "PS/2 no keyboard found!",0