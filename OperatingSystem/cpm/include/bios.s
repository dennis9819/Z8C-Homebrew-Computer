;	Z8C I/O Drivers for CP/M 2.2
;	(four drive single density version)
;
;	Version 2.2 February, 1980
;   Adapted by Dennis Gunia January, 2024
vers	equ	22	;version 2.2
;
;	Copyright (c) 1980
;	Digital Research
;	Box 579, Pacific Grove
;	California, 93950
;
;
true	equ	0ffffh	;value of "true"
false	equ	not true	;"false"
test	equ	false	;true if test bios
;
	if	test
bias	equ	03400h	;base of CCP in test system
	endif
	if	not test
bias	equ	0000h	;generate relocatable cp/m system
	endif
;
patch	equ	1600h
;
	org	patch
cpmb	equ	$-patch	;base of cpm console processor
bdos	equ	806h+cpmb	;basic dos (resident portion)
cpml	equ	$-cpmb	;length (in bytes) of cpm system
nsects	equ	cpml/128	;number of sectors to load
offset	equ	2	;number of disk tracks used by cp/m
cdisk	equ	0004h	;address of last logged disk on warm start
buff	equ	0080h	;default buffer address
retry	equ	10	;max retries on disk i/o before error
;
;	perform following functions
;	boot	cold start
;	wboot	warm start (save i/o byte)
;	(boot and wboot are the same for mds)
;	const	console status
;		reg-a = 00 if no character ready
;		reg-a = ff if character ready
;	conin	console character in (result in reg-a)
;	conout	console character out (char in reg-c)
;	list	list out (char in reg-c)
;	punch	punch out (char in reg-c)
;	reader	paper tape reader in (result to reg-a)
;	home	move to track 00
;
;	(the following calls set-up the io parameter block for the
;	mds, which is used to perform subsequent reads and writes)
;	seldsk	select disk given by reg-c (0,1,2...)
;	settrk	set track address (0,...76) for subsequent read/write
;	setsec	set sector address (1,...,26) for subsequent read/write
;	setdma	set subsequent dma address (initially 80h)
;
;	(read and write assume previous calls to set up the io parameters)
;	read	read track/sector to preset dma address
;	write	write track/sector from preset dma address
;
;	jump vector for indiviual routines
	jmp	boot
wboote:	jmp	wboot
	jmp	const
	jmp	conin
	jmp	conout
	jmp	list
	jmp	punch
	jmp	reader
	jmp	home
	jmp	seldsk
	jmp	settrk
	jmp	setsec
	jmp	setdma
	jmp	read
	jmp	write
	jmp	listst	;list status
	jmp	sectran

readf	equ	4h	;read function
writf	equ	6h	;write function
recal	equ	3h	;recalibrate drive
iordy	equ	4h	;i/o finished mask
cr	equ	0dh	;carriage return
lf	equ	0ah	;line feed
;
signon:	;signon message: xxk cp/m vers y.y
	db	cr,lf,lf
	if	test
	db	'32'	;32k example bios
	endif
	if	not test
	db	'00'	;memory size filled by relocator
	endif
	db	'k CP/M vers '
	db	vers/10+'0','.',vers mod 10+'0'
	db	cr,lf,0
;
boot:	;print signon message and go to ccp
;	(note: mds boot initialized iobyte at 0003h)
	lxi	sp,buff+80h
	lxi	h,signon
	call	prmsg	;print message
	xra	a	;clear accumulator
	sta	cdisk	;set initially to disk a
	jmp	gocpm	;go to cp/m