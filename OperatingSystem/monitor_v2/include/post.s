;Power-On self test
POST_START:
	call POST_CHECK_PIO
	call POST_CHECK_APU
    call POST_TEST_RTC
    call POST_CHECK_IDE_30
    call POST_CHECK_IDE_40
    ret

POST_CHECK_PIO:
	ld hl,[str_post_pio]
	call print_str
	in a,(CS_PIO_AC)	;test read from pio
	cp 0x00				;0x78 when not installed
	jp nz, _POST_CHECK_PIO_FAILED
	ld a, 0x00			;set present flag
	ld (var_pio_present),a
	ld hl,[str_post_ok]	
	call print_str
	ret
_POST_CHECK_PIO_FAILED:
	ld a, 0xFF
	ld (var_pio_present),a
	ld hl,[str_post_nd]
	call print_str
	ret

POST_CHECK_APU:
	ld hl,[str_post_apu]
	call print_str

	ld a, 0xFF
	out (CS_APU_DATA),a
	nop
	nop
	in a,(CS_APU_DATA)
	cp 0xFF
	jp nz, _POST_CHECK_APU_FAILED
	ld a, 0x00			;set present flag
	ld (var_apu_present),a
	ld hl,[str_post_ok]	
	call print_str
	ret
_POST_CHECK_APU_FAILED:
	ld a, 0xFF
	ld (var_apu_present),a
	ld hl,[str_post_nd]
	call print_str
	ret

POST_CHECK_IDE_30:
	ld hl,[str_post_ide_30]
	call print_str
	in a,(0x30)
	or a
	jp nz, _POST_CHECK_IDE_FAILED
	ld hl,[str_post_ok]	
	call print_str
	ret
POST_CHECK_IDE_40:
	ld hl,[str_post_ide_40]
	call print_str
	in a,(0x40)
	or a
	jp nz, _POST_CHECK_IDE_FAILED
	ld hl,[str_post_ok]	
	call print_str
	ret

_POST_CHECK_IDE_FAILED
	ld hl,[str_post_nd]
	call print_str
	ret

POST_TEST_RTC:
	ld a, 0x06	
	ld (var_scratch),a
	ld hl,[str_post_rtc]
	call print_str
	ld hl,[var_scratch]
	ld b, 1
	ld c, ADDR_RTC
	call iic_send_buffer
	or a
    jp nz, _POST_TEST_RTC_NOTFOUND
	ld hl,[var_scratch]
	ld b, 1
	ld c, ADDR_RTC
	call iic_receive_buffer
	ld a, (var_scratch)
 	or a
    jp z, _POST_TEST_RTC_INVALID
	ld hl,[str_post_ok]
	call print_str
	jp _OP_RTIME_NN
_POST_TEST_RTC_NOTFOUND:
	ld hl,[str_post_nd]
	call print_str
	ret
_POST_TEST_RTC_INVALID:
	ld hl,[str_post_rtc_iv]
	call print_str
	ret

str_post_ide_30:
    db 13,10,"Check Diskctrl.@030h... ",0
str_post_ide_40:
    db 13,10,"Check Diskctrl.@040h... ",0
str_post_pio:
    db 13,10,"Check IO-Controller ... ",0
str_post_apu:
    db 13,10,"Check AMD8911 APU   ... ",0
str_post_rtc:
    db 13,10,"Check DS1307 RTC    ... ",0
str_post_nd:
    db "not detected!",0
str_post_rtc_iv:
    db "not set. Check battery and run 'setdate'!",0
str_post_ok:
    db "ok! ",0