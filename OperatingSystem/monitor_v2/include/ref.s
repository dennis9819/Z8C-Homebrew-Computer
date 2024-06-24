;-------------------------------------
; BIOS Routines entry points
; Do not move in memory!!!!
;-------------------------------------
.org 0x0050
B_BEEP:
    jp beep

B_IICSEND:
    jp iic_send_buffer

B_IICRECV:
    jp iic_receive_buffer

B_PRINTCHAR:
    jp print_char

B_PRINTSTR:
    jp print_str

B_PRINTINLINE:
    jp PRINTINLINE

B_PRINTAHEX:
    jp print_a_hex

B_PRINTLN:
    jp print_newLine

B_READCHAR:
    jp read_char

B_KEYREAD:
    ret             ;placeholder -> not implemented
    db 0x00, 0x00

B_KEYREADASCII:
    ret             ;placeholder -> not implemented
    db 0x00, 0x00

B_KEYSEND:
    ret             ;placeholder -> not implemented
    db 0x00, 0x00

B_DSKSEL:
    jp ideif_drv_sel

B_FATOPEN:
    jp fat_openfile

B_FATREAD:
    jp fat_readfilesec

B_FATCD:
    jp fat_cd_single

B_FATCREATE:
    ret             ;placeholder -> not implemented
    db 0x00, 0x00

B_FATWRITE:
    ret             ;placeholder -> not implemented
    db 0x00, 0x00