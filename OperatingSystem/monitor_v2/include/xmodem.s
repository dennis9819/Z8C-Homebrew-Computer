;-------------------------------------------------------------------------
; Z80 XMODEM implementation by Dennis Gunia
; 2022 - www,dennisgunia.de
;
; important doc: https://web.mit.edu/6.115/www/amulet/xmodem.htm
;                http://www.blunk-electronic.de/train-z/pdf/xymodem.pdf
;-------------------------------------------------------------------------

;Symbols
SYM_SOH equ 0x01
SYM_EOT equ 0x04
SYM_ACK equ 0x06
SYM_NAK equ 0x15
SYM_ETB equ 0x17
SYM_CAN equ 0x18
SYM_C   equ 0x43

;Memory locations
MEM_VAR_BLOCK equ 0x40FB    ;last block
MEM_VAR_TIMEA equ 0x40FC    ;timer var (mills)
MEM_VAR_TIMER equ 0x40FE    ;timer var (seconds)
MEM_INT_VEC_T equ 0x40FE    ;interrupt vector table

MEM_LOC_LOAD  equ 0x4400    ;load location for program

;XMODEM routine
xmodem_init:
    call A_RTS_OFF
    
    LD A,10100111b      ; Init CTC Channel 3
    OUT (CS_CTC_2),A
    LD A,14             ; 1028.57Hz ISR
    OUT (CS_CTC_2),A
    LD A,00h            ; Set CTC Ch3 Interrupt Vector
    OUT (CS_CTC_0),A
    ;load int vector to ram
    ld hl,xmodem_int        ;CTC Ch3 INT routine
    ld (0x4200 + 0x04),hl   
    ;reset timer vars
    ld hl,0x0000
    ld (MEM_VAR_TIMEA),hl
    ld (MEM_VAR_TIMER),hl

    ld a,0x42           ; Set interrupt vector register
    ld i,a
    im 2                ; Z80 Interrupt mode

    ld hl,MSG_START     ; Print start banner
    call print_str
    ei                  ; Enable interrupts
    ; init done. Continue with xmodem_await_conn
    call xmodem_wait

xmodem_await_conn:          ;Wait for initial connection
    ld a, SYM_C             ;Send C to notify sender, that we want CRC
    call xmodem_out         
    call xmodem_read_wait   ;Read with timeout
    jp c, xmodem_await_conn ;Carry flag set = timeout -> repeat
    ;else continue

xmodem_packet:              ;XmodemCRC packet start
    ;use 1st byte to decide further processing
    cp SYM_EOT              ;End of Transmission 
    jp z, xmodem_packet_EOT
    cp SYM_CAN              ;Cancel (Force receiver to start sending C's)
    jp z, xmodem_await_conn
    cp SYM_SOH              ;Start of 
    jp z, xmodem_packet_get

    jp xmodem_err           ;anything else is an error -> abort transmission

xmodem_packet_get:          ;if first byte == SYM_SOH -> receive block
    call xmodem_read_wait   ;get byte 2 => block ID
    jp c, xmodem_nak
    ld b,a
    ld (MEM_VAR_BLOCK), a   ;store block id to memory
    call xmodem_read_wait   ;get byte 3 => block ID complement 
    jp c, xmodem_nak
    add b
    cp 255                  ;both size infos should always sum to 255
    jp nz,xmodem_err        ;if not 255 then its an error

    ;calculate block start address in RAM
    ;multiply by 128
    ;use more efficient bit-wise operations
    dec a       ;a-1 to remove 1 sector offset
    ld a,b
    rra         ;shift 1 bit to the right
    and 0x7F
    ld h,a
    ld a,b
    dec a       ;a-1 to remove 1 sector offset
    rrca        ;shift bit0 to bit 7
    and 0x80    ;mask out all other bits
    ld l,a
    ;result:
    ;hl = 0aaaaaaa a000000

    ld de,MEM_LOC_LOAD  
    add hl,de               ;add calculated offset to base address
    ;hl now contains the true start address of this sector

    ld b,128    ;preload counter for data baytes
    ld c,0      ;packet length counter ( used for overflow error )

xmodem_packet_get_data: ;get 128 data bytes (loop)
    push hl                 ;push hl onto stack because xmodem_read_wait destroys hl
    call xmodem_read_wait   ;read byte or timeout
    jp c, xmodem_nak        ;if timeout -> nak and retry
    pop hl                  ;restore hl
    ld (hl), a              ;store received byte in memory
    inc hl                  ;increment pointer
    inc c                   ;increment packet length counter 
    dec b                   ;decerment data bytes remmaining
    jp nz, xmodem_packet_get_data   ;if bytes remaining, loop
    ;else continue with crc bytes

xmodem_packet_get_crc:      ;get 16-Bit CRC
    call xmodem_read_wait  
    jp c, xmodem_nak
    ld d,a
    inc c
    call xmodem_read_wait   
    jp c, xmodem_nak
    ld e,a 
    inc c
    ;de now contains CRC value
    ;check if c is not bigger than 130 byte (128 data + 2crc)
    ;TODO if so NACK

    ;de contains 16-bit CRC
    ;TODO if crc error NACK
    jp xmodem_ack         ;ack block -> then jump to start again

xmodem_packet_EOT:          ;End of transmission SUB. 
    ld a, SYM_ACK           ;Acknowledge EOT
    call xmodem_out
    jp xmodem_end           ;and end xmodem



xmodem_err:                 ;non recoverable error -> abort
    ld a, SYM_CAN
    call xmodem_out
    ld a, SYM_CAN
    call xmodem_out
    ld a, SYM_CAN
    call xmodem_out
    ld a, SYM_CAN
    call xmodem_out
    ld a, SYM_CAN
    call xmodem_out
    ld a, SYM_CAN
    call xmodem_out
    ld a, SYM_CAN
    call xmodem_out
    ld a, SYM_CAN
    call xmodem_out
    ld a, SYM_CAN
    call xmodem_out
    ld a, SYM_CAN
    call xmodem_out
    ld a, SYM_CAN

    ld hl, MSG_ERROR
    call print_str

    ;overflow to end
xmodem_end:
    di                      ;disable interrupts
    call print_newLine      ;print new line
    jp PROMPT_BEGIN         ;return ti prompt

;isr is used as counter for timeouts
xmodem_int:
    di                      ;setup ISR (disable further interrupts, exchange registers)
    ex AF,AF'
    exx

    ld hl,(MEM_VAR_TIMEA)   ;millis counter
    inc hl
    ld (MEM_VAR_TIMEA),hl

    ld de,1028              ;every 1028 millis counter
    sbc hl,de
    jp nz, xmodem_int_cont  ;if less than 1028 millis, loop

    ld hl,0                 ;reset millis
    ld (MEM_VAR_TIMEA),hl   
    ld hl,(MEM_VAR_TIMER)   ;incement seconds
    inc hl
    ld (MEM_VAR_TIMER),hl
xmodem_int_cont:            ;end isr
    ex AF,AF'               ;restore registers
    exx
    EI                      ;enable interrupts
    reti                    ;exit ISR





; A returns char
; Carry is set if timeout
xmodem_read_timeount    equ     3  ;3 seconds timeout
xmodem_read_wait:
    di
    ld hl,0
    ld (MEM_VAR_TIMEA),hl
    ld (MEM_VAR_TIMER),hl
    ei
    call A_RTS_ON
xmodem_read_wait_loop:
    ;check timeout
    ld hl,(MEM_VAR_TIMER)
    ld a, l
    cp xmodem_read_timeount
    jp z, xmodem_read_wait_timeout    ;if timeout retry

    ; if no timeout
    xor a               ; a = 0
    out (CS_SIO_A_C), a ; select reg 0
    in a, (CS_SIO_A_C)  ; read reg 0
    and	1               ; mask D0 (recieve char available)
    jp Z,xmodem_read_wait_loop  ; wait if no char
    
    ; if char avail
    in a, (CS_SIO_A_D)  ; read char 
    ;call debug_a_hex
    push af
    call A_RTS_OFF
    pop af
    scf
    ccf
    ret                 ; return
xmodem_read_wait_timeout:
    scf
    ret


xmodem_out:
    out (CS_SIO_A_D), a
    call xmodem_wait_out
    ret

xmodem_wait_out:
    sub a ;clear a, write into WR0: select RR0
    inc a ;select RR1
    out (CS_SIO_A_C),A
    in A,(CS_SIO_A_C) ;read RRx
    bit 0,A
    jr z,xmodem_wait_out
    ret

MSG_ERROR:
    db "Error: unexpected byte",13,10,0

MSG_START:
    db "Await xmodem connection",13,10,0


xmodem_ack:             ;ack routine. Only use when expecting transmission afterwards.
    ld a, SYM_ACK       ;send ACK
    call xmodem_out 
    call xmodem_read_wait   ;wait for response
    jp c, xmodem_ack        ;if timeout repeat
    jp xmodem_packet        ;if received, continue with new packet

xmodem_nak:             ;nak routine. Only use when expecting transmission afterwards.
    ld a, SYM_NAK           ;send NAK
    call xmodem_out         
    call xmodem_read_wait   ;wait for response
    jp c, xmodem_nak        ;if timeout repeat
    jp xmodem_packet        ;if received, continue with new packet

xmodem_wait:
    ld hl, 0xFF
    ld bc, 0x01
xmodem_wait_1:
    nop
    nop
    nop
    nop
    sbc hl,bc
    ret Z
    jr xmodem_wait_1