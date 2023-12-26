;8645 Routines
; http://bitsavers.trailing-edge.com/components/motorola/_dataSheets/6845.pdf
CS_CRTC_ADDR	.EQU 80h
CS_CRTC_DATA	.EQU 81h

VAL_R0			.EQU 99		;Nr of Horizontal Characters Total.
VAL_R1			.EQU 80		;Nr of Horizontal Characters Displayed.
VAL_R2			.EQU 83		;Horizontal Sync Position.
VAL_R3			.EQU 6		;Sync width.
VAL_R4			.EQU 31		;Vertical Total.
VAL_R5			.EQU 13		;Vertical Total Adjustment.
VAL_R6			.EQU 25		;Nr of Vertical Characters Displayed.
VAL_R7			.EQU 29		;Vertical Sync Position (might need manual fine tuning).
VAL_R8			.EQU 0		;Interlace Mode.
VAL_R9			.EQU 15		;Max Scanline Address.
VAL_R10			.EQU 205	;Cursor Start Scan Line.
VAL_R11			.EQU 207	;Cursor Stop Scan Line.
VAL_R12			.EQU 0		;Start Address (High). Real start address is 0x0000.
VAL_R13			.EQU 0		;Start Address (Low). Real start address is 0x0000.
VAL_R14			.EQU 0		;Cursor Start Address (High). Cursor will be at position (0, 0).
VAL_R15			.EQU 0		;Cursor Start Address (Low). Cursor will be at position (0, 0).

; A contains DATA
; B contains REGISTER ID
CRTC_WRITE_REGISTER:
	PUSH AF
	LD A,B
	AND 1Fh
	OUT (CS_CRTC_ADDR), A
	POP AF
	OUT (CS_CRTC_DATA), A
	RET
	
; A returns DATA
; B contains REGISTER ID
CRTC_READ_REGISTER:
	LD A,B
	AND 1Fh
	OUT (CS_CRTC_ADDR), A
	IN A, (CS_CRTC_DATA)
	RET
	
CRTC_INIT_80COL:
	LD DE,[VAL_R0]
	LD B, 0
	
CRTC_INIT_80COL_LOOP:
	LD A, (DE)
	CALL CRTC_READ_REGISTER
	INC DE
	INC DE
	INC B
	LD A, B
	CP 16
	JR NC, CRTC_INIT_80COL_LOOP
	