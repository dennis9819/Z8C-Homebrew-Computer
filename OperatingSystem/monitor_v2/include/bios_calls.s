;----------------------------------------------------------------
; Terminal IN/OUT functions
;----------------------------------------------------------------
; prints out byte
; input:
; - a: byte to send
; modify:
; - af: destroy
bios_termout:
    jp print_char

; reads in byte
; input:
; modify:
; - a: read byte
; - f: destroy
bios_termin:
    jp read_char

; reads input status
; input:
; modify:
; - a: 0x00 if empty, 0xFF if byte is ready
; - f: destroy
bios_termsts:
    jp read_in_sts

;----------------------------------------------------------------
; I2C IN/OUT functions
;----------------------------------------------------------------
; sends I2C buffer to device
; input:
; - HL contains buffer location
; - B  defines amount of bytes to send
; - C  contains device address
; modify:
; - af: destroy
bios_iic_send:
    jp iic_send_buffer

; sends I2C buffer to device
; input:
; - HL contains buffer location
; - B  defines amount of bytes to receive
; - C  contains device address
; modify:
; - af: destroy
bios_iic_receive:
    jp iic_receive_buffer

;----------------------------------------------------------------
; disk functions
;----------------------------------------------------------------
; selects drive 0-3
; input:
; - a  contains drive number
; modify:
; - af: destroy
; - de: destroy
; - bc: destroy
; - hl: destroy
bios_disk_sel:
    jp ideif_drv_sel

; gets pointer to selcetd drive information
; input:
; modify:
; - af: destroy
; - ix: pointer to start of entry
bios_disk_get:
    jp ideif_get_drv_pointer


; reads from LBA address
; input:
; - a : sectro count
; - de: destination in memory
; - hl: pointer to sector value in memory (32 bit (lw))
; modify:
; - af: destroy
; - bc: destroy
; - de: destroy
; - hl: destroy
bios_disk_readlba:
    jp read_lba_sector

;----------------------------------------------------------------
; filesystem functions
;----------------------------------------------------------------
