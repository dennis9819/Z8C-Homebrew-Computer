;----------------------------------------------------------------
;Keyboard interface driver for Z8C 
;Controller used: vt82c42
;Datasheet: http://www.s100computers.com/My%20System%20Pages/MSDOS%20Board/vt82c42%20PC%20Keyboard%20conrtroller.pdf
;by Dennis Gunia (04/2022)
;----------------------------------------------------------------

;IO Ports
CS_VT82C42_DATA .EQU F0h
CS_VT82C42_CTRL .EQU F1h

keyboard_init_io:   ;Subroutine for initializing keyboard controller
    LD A, A7h                   ;Disable Mouse
    OUT (CS_VT82C42_CTRL), A
    LD A, ADh                   ;Disable Keyboard
    OUT (CS_VT82C42_CTRL), A
_keyboard_init_flush_buffer:
    IN A,(CS_VT82C42_DATA)      ;Read buffer
    IN A,(CS_VT82C42_CTRL)      ;Read status byte
    BIT 0,A                     ;Test if buffer is empty
    JR Z, _keyboard_init_flush_buffer
    ;buffer is now flushed. Now set the Controller Configuration Byte 
    IN A,(CS_VT82C42_CTRL)      ;Disable bits 0,1,6 (disablee IRQ and Translation)
    AND 10111100
    OUT (CS_VT82C42_CTRL),A
    ;Perform Controller Self Test 
    LD A, AAh
    OUT (CS_VT82C42_CTRL), A
    NOP
    IN A,(CS_VT82C42_CTRL)  ;Check results
    CP 55h
    JR NZ, keyboard_init_failed
    ;Perform Interface Tests 
    LD A, ABh
    OUT (CS_VT82C42_CTRL), A
    NOP
    IN A,(CS_VT82C42_CTRL)  ;Check results
    CP 00h
    JR NZ, _keyboard_init_failed
    ;Enable Devices
    LD A,AEh
    OUT (CS_VT82C42_CTRL), A
    ;Reset Devices 
    LD A,FFh
    OUT (CS_VT82C42_DATA), A    ;Send reset to keboard (0xFF command)
    NOP
    IN A,(CS_VT82C42_CTRL)      ;Read status byte
    BIT 0,A                     ;Test if buffer is empty -> no keyboard found
    JP NZ, _keyboard_init_dev_missing
    CP A, 0xFC
    JR Z, _keyboard_init_okay    ; 0xFC -> Success. Init done!
    ;Else device error
    LD HL, [STR_keyboard_init_failed]
    CALL print_str
    HALT
_keyboard_init_failed:
    LD HL, [STR_keyboard_init_err]
    CALL print_str
    HALT
_keyboard_init_dev_missing:
    LD HL, [STR_keyboard_init_missing]
    CALL print_str
    HALT
_keyboard_init_okay:
    LD HL, [STR_keyboard_init_okay]
    CALL print_str
    RET

;Keyboard IO functions
keyboard_read:
    IN A,(CS_VT82C42_CTRL)      ;Read status byte
    BIT 0,A                     ;Test if buffer is empty
    RET NZ                      ;Return if empty
    IN A,(CS_VT82C42_DATA)
    RET                         ;Return with data in A

keyboard_write:
    PUSH AF
_keyboard_write_wait:
    IN A,(CS_VT82C42_CTRL)      ;Read status byte
    BIT 1,A                     ;Test if buffer is full
    JR Z, _keyboard_write_wait   ;Wait if input buffer is full
    POP AF
    OUT (CS_VT82C42_DATA), A
    RET
    

;Status message strings
STR_keyboard_init_okay:
    .BYTE "PS/2 Keyboard initialized.",0
STR_keyboard_init_err:
    .BYTE "PS/2 Controller error! System HALT!",0
STR_keyboard_init_failed:
    .BYTE "PS/2 Keyboard error! System HALT!",0
STR_keyboard_init_missing:
    .BYTE "PS/2 no keyboard found!",0