
;*** FL16.COM v1.33 for MSX

;*** ROM Loader for MegaflashROM mapped ASCII 16K

; Assembled with zasm cross assembler
; http://sourceforge.net/projects/zasm/


LF	equ	0ah
CR	equ	0dh
BDOS	equ	00005h
WRSLT	equ	00014h
CALSLT	equ	0001Ch
ENASLT	equ	00024h
FCB	equ	0005ch
DMA	equ	00080h
RAMAD1	equ	0f342h
RAMAD2	equ	0f343h
BUFTOP	equ	04000h
CHGET	equ	0009fh
MNROM	equ	0FCC1h	; Main-ROM Slot number & Secondary slot flags table
DRVINV	equ	0FB22H	; Installed Disk-ROM

	org	0100h

START:
	jp	Main

MESVER:
	db	CR,LF,"ROM Loader v1.33 for",CR,LF
	db	"ASCII 16K MegaflashROM by GDX",CR,LF
	db	"based on FLLOAD by K.Tsujikawa"
MESend:
	db	CR,LF,CR,LF,"$"
HlpMes:
	db	"Usage: FL16 filename.ext /Sxx /R",CR,LF
	db	"       FL16 /Sxx /E",CR,LF,CR,LF
	db	"(xx: MegaFlashRom slot address)",CR,LF,"$"
	db	CR,"(^_^)/~",CR,LF,1ah

DosErr:
	db	"File reading error!",CR,LF,"$"
FlsEra:
	db	"Flash erasing...$"
FlsEok:
	db	"OK",CR,LF,CR,LF,"$"
FlsErr:
	db	"Flash writing error!",CR,LF,"$"
DonMes:
	db	"Load complete. Thank you.",CR,LF,"$"
AM29F0xx:
	db	"29F040 found in Slot $"
NO_FLSH:
	db	"MegaFlashRom not found!",CR,LF,"$"
WarnMess:
	db	"You have selected a slot that",CR,LF
	db	"contains a DISK-ROM!",CR,LF,"$"
ConfirmMess:
	db	"Do you want to erase it? (Y/N)",CR,LF,"$"
CancelMess:
	db	"Canceled.",CR,LF,"$"

Main:
	ld	de,MESVER
	ld	c,9
	call	BDOS		; Print MESVER message (FL16 info)

; *** Auto-detection routine

	ld	b,3		; B=Primary Slot
BCLM:
	ld	c,0		; C=Secondary Slot
BCLMI:
	push	bc
	call	AutoSeek
	pop	bc
	inc	c
	ld	a,c
	cp	4
	jr	nz,BCLMI	; Jump if Secondary Slot < 4
	dec	b
	jp	p,BCLM		; Jump if Primary Slot < 0
	
NO_FND:
	ld	de,NO_FLSH	; Pointer to NO_FLSH message
	jp	Done

AutoSeek:
	ld	a,b
	xor	3		; Reverse the bits to reverse the search order (0 to 3)
	ld	hl,MNROM
	ld	d,0
	ld	e,a
	add	hl,de
	bit	7,(hl)
	jr	z,primSlt	; Jump if slot is not expanded
	or	(hl)		; Set flag for secondary slot
	sla	c
	sla	c
	or	c		; Add secondary slot value to format FxxxSSPP
primSlt:
	ld	(ERMSlt),a
; ---
	ld	b,a		; Keep actual slot value

	bit	7,a
	jr	nz,SecSlt	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSlt:
	ld	c,a
	ld	a,(DRVINV)	; A = slot value of main Rom-disk
	bit	7,a
	jr	nz,SecSlt1	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSlt1:
	cp	c
	ret	z		; Return if Disk-Rom Slot
	ld	a,(DRVINV+2)	; A = slot value of second Rom-disk
	bit	7,a
	jr	nz,SecSlt2	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSlt2:
	cp	c
	ret	z		; Return if Disk-Rom Slot
	ld	a,(DRVINV+4)	; A = slot value of third Rom-disk
	bit	7,a
	jr	nz,SecSlt3	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSlt3:
	cp	c
	ret	z		; Return if Disk-Rom Slot
	ld	a,(DRVINV+6)	; A = slot value of fourth Rom-disk
	bit	7,a
	jr	nz,SecSlt4	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSlt4:
	cp	c
	ret	z		; Return if Disk-Rom Slot

	ld	a,b		; Restore actual slot value
; ---
	ld	h,40h
	call	ENASLT		; Select a Slot in Bank 1 (4000h ~ 7FFFh)

	di
	ld	a,0aah
	ld	(4555h),a	; Autoselect
	ld	a,055h
	ld	(42aah),a	; Mode
	ld	a,090h
	ld	(4555h),a	; ON
	
	ld	b,16
	ld	hl,4000h
RDID_BCL:
	ld	a,(hl)		; (HL) = Manufacturer ID
;	ld	(MAN_ID),a
;	cp	01h		; Manufacturer ID (01h=AMD)
;	jr	z,SKIPmxicID
;	cp	0C2h		; Manufacturer ID (0C2h=MXIC)
;	ret	nz

;	ld	a,04Dh
;	ld	(AM29F0xx),a	; 'M'
;	ld	a,058h
;	ld	(AM29F0xx+1),a	; 'X'
;SKIPmxicID:

	inc	hl
	ld	a,(hl)

	cp	0D5h		; Device ID for AM29F080B
	ex	AF,AF'
	ld	a,038h
	ld	(AM29F0xx+4),a
	ld	a,030h
	ld	(AM29F0xx+5),a
	ex	AF,AF'
	jr	z,ID_OK

	cp	0A4h		; Device ID for AM29F040B
	ex	AF,AF'
	ld	a,034h
	ld	(AM29F0xx+4),a
	ld	a,030h
	ld	(AM29F0xx+5),a
	ex	AF,AF'
	jr	z,ID_OK

	cp	077h		; Device for AM29F004B (Top Boot Block)
	ex	AF,AF'
	ld	a,030h
	ld	(AM29F0xx+4),a
	ld	a,034h
	ld	(AM29F0xx+5),a
	ex	AF,AF'
	jr	z,ID_OK
	cp	07Bh		; Device for AM29F004B (Bottom Boot Block)
	jr	z,ID_OK

	cp	0B0h		; Device for AM29F002 (Top Boot Block)
	ex	AF,AF'
	ld	a,030h
	ld	(AM29F0xx+4),a
	ld	a,032h
	ld	(AM29F0xx+5),a
	ex	AF,AF'
	jr	z,ID_OK
	cp	034h		; Device for AM29F002 (Bottom Boot Block)
	jr	z,ID_OK

	cp	020h		; Device ID for AM29F010
	ex	AF,AF'
	ld	a,031h
	ld	(AM29F0xx+4),a
	ld	a,030h
	ld	(AM29F0xx+5),a
	ex	AF,AF'
	jr	z,ID_OK

	cp	0ADh		; Device ID for AM29F016
	ex	AF,AF'
	ld	a,031h
	ld	(AM29F0xx+4),a
	ld	a,036h
	ld	(AM29F0xx+5),a
	ex	AF,AF'
	jr	z,ID_OK
	ret
ID_OK:
;	ld	(DEV_ID),a

	ld	a,(hl)
	inc	hl
;	ld	(SEC_PROT),a
;	cp	01h		; Sector Protection. (01h=protected, 00h=unprotected)
;	ret	nz

	inc	hl
	inc	hl
	djnz	RDID_BCL
	
	ld	(hl),0f0h	; AM29F0xx ID reading mode OFF

	ei
	pop	hl		; Remove RET address in stack
	pop	hl		; Remove BC value in stack
	
FLH_FND:
	ld	a,(RAMAD1)
	ld	h,40h
	call	ENASLT		; Select Main-RAM in MSX"s Bank 1

	ld	de,AM29F0xx	; Pointer to AM29F0xx message
	ld	c,9
	call	BDOS		; Print AM29F0xx message
	
	ld	a,(ERMSlt)
	and	3
	add	a,30h
	ld	e,a
	ld	c,2
	call	BDOS		; Print primary Slot number

	ld	e,02Dh
	ld	c,2
	call	BDOS		; Print "-" character

	ld	a,(ERMSlt)
	and	0Ch
	srl	a
	srl	a
	add	a,30h
	ld	e,a
	ld	c,2
	call	BDOS		; Print secondary Slot number	

	ld	de,MESend
	ld	c,9
	call	BDOS		; Print 2x CR & LF character

; *** End of Auto-detection routine

Parameters:
	ld	hl,DMA
	ld	b,(HL)
	inc	b
	dec	b
	jp	z,Done		; Jump if no parameter

; Check parameter /S

	ld	c,053h		; 'S' character
	call	SeekParameter
	cp	255
	jp	z,Done		; Jump if syntax error
	or	a
	jr	z,No_S
	call	GetNum		; Get the slot number from parameter
	ld	a,e
	ld	(ERMSlt),a	; ERMSlt = Slot number from option S
	
	ld	a,(ERMSlt)	; A = Slot number from option S
	call	CheckSLT	; check if Megaflash is insered in /Sxx Slot
No_S:
	ld	a,(ERMSlt)
	or	a
	ld	de,NO_FLSH	; Pointer to NO_FLSH message
	jp	z,Done		; Jump if Flash Rom not found

; Check parameter /R
	
	ld	hl,DMA
	ld	b,(HL)
	ld	c,052h		; 'R' character
	call	SeekParameter
	cp	255
	jp	z,Done		; Jump if syntax error
	ld	(ParameterR),a

; Check parameter /E

	ld	hl,DMA
	ld	b,(HL)
	ld	c,045h		; 'E' character
	call	SeekParameter
	cp	255
	jp	z,Done		; Jump if syntax error
	or	a
	ld	(FLerase),a
	jp	nz,SKIP		; Jump if /E found
	jp	PreFCB
	
; Seek Parameter Routine
; In: B = Length of parameters zone, C = Character, HL = Pointer address
; Out: A = 0 if Parameter not found or 255 if syntax error, DE = HlpMes if syntax error
; Modify AF, BC, HL

SeekParameter:
	inc	hl
	ld	a,(hl)
	cp	02Fh		; Seek '/' character
	jr	nz,ParamBCL
	inc	hl
	ld	a,(hl)
	and	0dfh
	cp	c		; Compare found character with the input character
	ret	z
	call	SyntaxCheck
	cp	255
	ret	z
ParamBCL:
	djnz	SeekParameter
	xor	a
	ret
SyntaxCheck:
	push	hl
	push	bc
	cp	045h		; 'E' character
	jr	z,SyntaxOK
	cp	052h		; 'R' character
	jr	z,SyntaxOK
	cp	053h		; 'S' character
	jr	z,SyntaxOK
	ld	de,HlpMes
	ld	a,255		; Syntax error
SyntaxOK:
	pop	bc
	pop	hl
	ret

PreFCB:
; ----
	ld	a,(ERMSlt)	; A = Slot number from option S
	bit	7,a
	jr	nz,SecSltP	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSltP:
	ld	e,a
	ld	a,(DRVINV)
	bit	7,a
	jr	nz,SecSltP1	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSltP1:
	cp	e
	call	z,Warning	; Return if actual slot is same as Disk-Rom 1 Slot

	ld	a,(DRVINV+2)
	bit	7,a
	jr	nz,SecSltP2	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSltP2:
	cp	e
	call	z,Warning	; Return if actual slot is same as Disk-Rom 2 Slot

	ld	a,(DRVINV+4)
	bit	7,a
	jr	nz,SecSltP3	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSltP3:
	cp	e
	call	z,Warning	; Return if actual slot is same as Disk-Rom 3 Slot

	ld	a,(DRVINV+6)
	bit	7,a
	jr	nz,SecSltP4	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSltP4:
	cp	e
	call	z,Warning	; Return if actual slot is same as Disk-Rom 4 Slot
	ld	a,(OverWR)
	cp	'Y'
	jr	z,NoCancel
	ld	de,CancelMess
	jp	Done
	
Warning:
	push	de
	ld	de,WarnMess
	ld	c,9
	call	BDOS		; Print WarnMess message
	pop	de
	ld	a,'N'
	ld	(OverWR),a
	ret
	
NoCancel:
; ----	
	ld	bc,24		; Prepare the FCB
	ld	de,FCB+13
	ld	hl,FCB+12
	ld	(hl),b
	ldir			; Initialize the second half with zero

	ld	c,0fh
	ld	de,FCB
	call	BDOS		; Open file
	ld	hl,2
	ld	(FCB+14),hl	; Records size = 2 byte
	or	a
	ld	de,DosErr
	jp	nz,Done

	ld	c,1ah
	ld	de,BUFTOP
	call	BDOS		; Set disk transfer address (buffer start at 4000H)

; Flash Erase

SKIP:
; ----
	ld	a,(ERMSlt)	; A = Slot number from option S
	bit	7,a
	jr	nz,SecSltPa	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSltPa:
	ld	e,a
	ld	a,(DRVINV)
	bit	7,a
	jr	nz,SecSltP1a	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSltP1a:
	cp	e
	call	z,Confirm	; Return if actual slot is same as Disk-Rom 1 Slot

	ld	a,(DRVINV+2)
	bit	7,a
	jr	nz,SecSltP2a	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSltP2a:
	cp	e
	call	z,Confirm	; Return if actual slot is same as Disk-Rom 2 Slot

	ld	a,(DRVINV+4)
	bit	7,a
	jr	nz,SecSltP3a	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSltP3a:
	cp	e
	call	z,Confirm	; Return if actual slot is same as Disk-Rom 3 Slot

	ld	a,(DRVINV+6)
	bit	7,a
	jr	nz,SecSltP4a	; Jump if Secondary Slot
	and	3		; Keep primary slot bits
SecSltP4a:
	cp	e
	call	z,Confirm	; Return if actual slot is same as Disk-Rom 4 Slot
	ld	a,(OverWR)
	cp	'Y'
	jr	z,OverWrite
	ld	de,CancelMess
	jp	Done

Confirm:
	push	de
	ld	de,WarnMess
	ld	c,9
	call	BDOS		; Print WarnMess message

	ld	de,ConfirmMess
	ld	c,9
	call	BDOS		; Print WarnMess message
	pop	de
WaitKey:
	ld	ix,CHGET
	ld	iy,(MNROM)
	call	CALSLT		; Execute the ROM
	and	0dfh
	cp	'Y'
	ld	(OverWR),a
	ret	z
	cp	'N'
	ld	(OverWR),a
	ret	z
	jr	WaitKey

OverWrite:
; ----	
	ld	de,FlsEra	; Pointer to message FLASH-ROM erase start
	ld	c,9
	call	BDOS		; Print FlsEra message

	ld	a,(ERMSlt)
	ld	h,80h
	call	ENASLT		; Select Flashrom at Bank 8000h~BFFFh

	di
	ld	a,0aah
	ld	(8555h),a	; Flashrom...
	ld	a,055h
	ld	(82aah),a	;
	ld	a,080h
	ld	(8555h),a	; ... erase ...
	ld	a,0aah
	ld	(8555h),a	;
	ld	a,055h
	ld	(82aah),a	;
	ld	a,010h
	ld	(8555h),a	; ... command

	ld	a,0ffh
	ld	de,8000h
	call	CHECK
	jp	c,Done		; Jump if Erase fail

	ei
	ld	de,FlsEok	; Pointer to Erase OK message
	ld	a,(FLerase)
	cp	045h
	jp	z,Done		; Jump if Erase option used

	ld	c,9
	call	BDOS		; Print FlsEok message

_8kL01:
	ld	a,(RAMAD1)
	ld	h,40h
	call	ENASLT		; Select Main-RAM at bank 4000h~7FFFh

	ld	c,27h
	ld	de,FCB
	ld	hl,2000h	; Number of records to load
	call	BDOS		; Read a block from file

	ld	a,h
	or	l
	ld	de,DonMes
	jp	z,Done		; Jump if any record is loaded

	ld	de,DosErr
	cp	20h
	jp	z,NO_8K		; Jump if loaded records number is 2000h
	cp	10h
	jp	nz,Done		; Jump if records number is not 1000h

	ld	hl,CarRace
	call	MEGpatch	; Apply patch for page 0 of 8KB Rom

	ld	hl,Galaxian
	call	MEGpatch	; Apply patch for page 0 of 8KB Rom

	ld	bc,02000h	; Length
	ld	de,06000h	; Destination
	ld	hl,BUFTOP	; Source
	ldir			; Make a mirror
	jp	z,FLashPage	; Jump if records number is 1000h

NO_8K:
	ld	hl,Anty_P1
	call	MEGpatch		; Apply patch

	ld	hl,AthleticBall
	call	MEGpatch		; Apply patch

	ld	hl,CrossBlaim
	call	MEGpatch		; Apply patch

	ld	hl,HarryFoxYuki
	call	MEGpatch		; Apply patch

	ld	hl,Nausicaa
	call	MEGpatch		; Apply patch 

	ld	hl,Robocop
	call	MEGpatch			; Apply patch 

	ld	hl,QBert
	call	MEGpatch		; Apply patch 

	ld	hl,SupLodeRunner
	call	MEGpatch		; Apply patch 
	ld	hl,SupLodeRunner_P1
	call	MEGpatch		; Apply patch 
	ld	hl,SupLodeRunner_P2
	call	MEGpatch		; Apply patch 
	ld	hl,SupLodeRunner_P3
	call	MEGpatch		; Apply patch 
	ld	hl,SupLodeRunner_P4
	call	MEGpatch		; Apply patch 
	ld	hl,SupLodeRunner_P5
	call	MEGpatch		; Apply patch 
	ld	hl,SupLodeRunner_P6
	call	MEGpatch		; Apply patch 
	ld	hl,SupLodeRunner_P7
	call	MEGpatch		; Apply patch 

	ld	hl,SuperSuwanggi
	call	MEGpatch		; Apply patch 
	ld	hl,SuperSuwanggi_P1
	call	MEGpatch		; Apply patch 
	ld	hl,SuperSuwanggi_P3
	call	MEGpatch		; Apply patch 

; patch for 32KB Roms Header ---

	ld	a,(PreBnk)
	or	a
	jr	nz,FLashPage	; Jump if current page is not 0 
	ld	hl,FCB+16
	or	(hl)
	inc	hl
	or	(hl)
	inc	hl
	or	(hl)
	inc	hl
	or	(hl)
	cp	80h
	jr	nz,FLashPage	; Jump if not 32KB rom
	ld	hl,(04002h)
	bit	7,h
	jr	nz,FLashPage	; Jump if run address > 7FFFH
	ld	(0400Eh),hl
	ld	hl,0400Dh
	ld	(hl),0c3h
	dec	hl
	ld	(hl),070h
	dec	hl
	ld	(hl),0
	dec	hl
	ld	(hl),032h
	dec	hl
	ld	(hl),1
	dec	hl
	ld	(hl),03Eh
	ld	(04002h),hl
; ---

FLashPage:
	ld	a,(ERMSlt)
	ld	h,40h
	call	ENASLT		; Select Flashrom at Bank 4000h~7FFFh

	ld	a,(PreBnk)
	ld	(7000h),a	; Select Flashrom page at Bank 8000h~BFFFh

	ld	a,(RAMAD1)
	ld	h,40h
	call	ENASLT		; Select Main-RAM at bank 4000h~7FFFh

	ld	hl,8555h
	ld	de,82aah

	exx
	ld	bc,4000h	; Length
	ld	de,8000h	; Destination
	ld	hl,4000h	; Source
LOOP:
 	di
	exx
	ld	(hl),0aah
	ld	a,055h
	ld	(de),a
	ld	(hl),0a0h
 	exx
	ld	a,(hl)
	ld	(de),a		; Write a byte to flashrom

	call	CHECK		; Check this byte
	jp	c,Done
NEXT:
	inc	hl
	inc	de
	dec	bc
	ld	a,b
	or	c
	jr	nz,LOOP

	ei
	ld	a,(PreBnk)
	inc	a
	ld	(PreBnk),a	; Increments Rom mapper page
	dec	a

	and	0fh
	cp	10
	jr	c,_8kR01
	add	a,7		; add	a,'A'-'0'-10
_8kR01:
	add	a,030h		; add	a,'0'
	ld	e,a
	ld	c,2
	call	BDOS		; Print current mapper page number

	ld	a,(PreBnk)
	and	0fh
	jp	nz,_8kL01
	ld	e,CR
	ld	c,2
	call	BDOS		; Print CR character
	ld	e,LF
	ld	c,2
	call	BDOS		; Print LF character
	jp	_8kL01

CHECK:
	push	bc
	ld	c,a
CHK_L1:
	ld	a,(de)
	xor	c
	jp	p,CHK_R1	; Jump if readed bit 7 = written bit 7
	xor	c
	and	020h
	jr	z,CHK_L1	; Jump if readed bit 5 = 1
	ld	a,(de)
	xor	c
	jp	p,CHK_R1	; Jump if readed bit 7 = written bit 7
	ld	de,FlsErr
	scf
CHK_R1:
	pop	bc
	ret


; ~~~ Routine that check if Megaflash is insered in /Sxx Slot

CheckSLT:
	ld	(ERMSlt),a
	ld	h,40h
	call	ENASLT		; Select a Slot in Bank 1 (4000h ~ 7FFFh)

	di
	ld	a,0aah
	ld	(4555h),a	; Autoselect
	ld	a,055h
	ld	(42aah),a	; Mode
	ld	a,090h
	ld	(4555h),a	; ON
	
	ld	b,16
	ld	hl,4001h
	ld	a,(hl)
	inc	hl		; (HL) = Sector Protection
	inc	hl
	inc	hl		; (HL) = manufacturer ID
	inc	hl		; (HL) = Device ID
	ld	(DEV_ID),a
	cp	0D5h		; Device ID for AM29F080B
	jr	z,RDID_BCL2
	cp	0A4h		; Device ID for AM29F040B
	jr	z,RDID_BCL2
	cp	077h		; Device for AM29F004B (Top Boot Block)
	jr	z,RDID_BCL2
	cp	07Bh		; Device for AM29F004B (Bottom Boot Block)
	jr	z,RDID_BCL2
	cp	0B0h		; Device for AM29F002 (Top Boot Block)
	jr	z,RDID_BCL2
	cp	034h		; Device for AM29F002 (Bottom Boot Block)
	jr	z,RDID_BCL2
	cp	020h		; Device ID for AM29F010
	jr	z,RDID_BCL2
	cp	0ADh		; Device ID for AM29F016
	jr	z,RDID_BCL2
	jr	NO_FLH2

RDID_BCL2:
	ld	a,(DEV_ID)
	cp	(hl)		; Device ID for AM29F080B
	jr	z,ID_OK2
NO_FLH2:
	pop	hl		; Remove RET address in stack
	pop	hl		; Remove BC value in stack

	ld	de,NO_FLSH	; Pointer to NO_FLSH message
	jp	Done

ID_OK2:
	inc	hl		; (HL) = Sector Protection
	inc	hl
	inc	hl		; (HL) = manufacturer ID
	inc	hl		; (HL) = Device ID
	djnz	RDID_BCL2
	
	ld	(hl),0f0h	; AM29F0xx ID reading mode OFF
	ei
	
FLH_FND2:
	ld	a,(RAMAD1)
	ld	h,40h
	call	ENASLT		; Select Main-RAM in MSX"s Bank 1
	ret

; ~~~ End of routine that check if Megaflash is insered in /Sxx Slot

Done:
	ld	a,(ParameterR)
	cp	052h		; Seek 'R' character
	jr	z,ResetRoutine
	jp	NoReset

ResetRoutine:
	ld	a,(ERMSlt)	; Megaflashrom slot
	ld	hl,6000h	; Page selection address for scc mapper
	ld	e,0		; Page number
	call	0014h		; Select page 0 of Megaflashrom

	ld	a,(ERMSlt)	; Megaflashrom slot
	ld	hl,7000h	; Page selection address for scc mapper
	ld	e,0		; Page number
	call	0014h		; Select page 1 of Megaflashrom

	ld	hl,ResetMSX
	ld	de,0C000h
	ld	bc,0020h
	ldir 			; Copy ResetMSX Routine to 0C000h
	jp	0C000h		; jump to ResetMSX routine
ResetMSX:
	ld	a,(0FCC1h)	; A = Main-rom slot
	ld	h,00h		; HL = address start of slot bank
	call	0024h		; Select Main-rom
	ld	a,(0FCC1h)	; Main-rom slot
	ld	h,40h		; HL = address start of slot bank
	call	0024h		; Select Main-rom
	call	006fh		; Screen 1
	jp	0000h		; msx reset
NoReset:
	ei
	push	de

	ld	a,(RAMAD1)
	ld	h,40h
	call	ENASLT		; Select Main-RAM at bank 4000h~7FFFh
	ld	a,(RAMAD2)
	ld	h,80h
	call	ENASLT		; Select Main-RAM at bank 8000h~BFFFh

	ld	e,CR
	ld	c,2
	call	BDOS		; Print CR character
	ld	e,LF
	ld	c,2
	call	BDOS		; Print LF character

	pop	de
	ld	c,9
	call	BDOS		; Print final message
	rst	0

GetNum:
	ld	de,0
GetL01:
	inc	hl
	ld	a,(hl)
	sub	030h		; sub	'0'
	ret	c
	cp	10
	jr	c,GetR01
	and	0dfh
	sub	7		; sub	'A'-'0'-10
	ret	c
	cp	16
	ccf
	ret	c
GetR01:
	sla	e
	rl	d
	sla	e
	rl	d
	sla	e
	rl	d
	sla	e
	rl	d
	add	a,e
	ld	e,a
	jr	GetL01

; Routine to patch Megaroms

MEGpatch:
	ld	a,(PreBnk)
	cp	(hl)		; compare page number
	ret	nz		; Back if page number differs
	inc	hl
	ld	a,(hl)
	ld	(patchID),a	; Change patch ID
	inc	hl
	ld	b,(hl)
	push	hl
VERdata:
	inc	hl
	ld	c,(hl)		; C = Original value
	inc	hl
	inc	hl
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	ex	de,hl		; hl = address to apply patch
	ld	a,c
	cp	(hl)
	jr	nz,NOpatch	; Jump if a data differs
	ex	de,hl
	djnz	VERdata
	pop	hl
	ld	b,(hl)
BCLpatch:
	inc	hl
	inc	hl
	ld	c,(hl)		; B = New value
	inc	hl
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	ex	de,hl		; hl = address to apply patch
	ld	(hl),c		; change select page address
	ex	de,hl
	djnz	BCLpatch
	ld	a,(patchID)
	ld	(CURRpatchID),a	; Write current patch ID 
	ret
NOpatch:
	pop	hl
	ret
	
ERMSlt:
	db	0
RAMtyp:
	db	0
PreBnk:
	db	0
FLerase:
	db	0
MAN_ID:
	db	0
DEV_ID:
	db	0
;SEC_PROT:
;	db	0
patchID:
	db	0
;FileSize:
;	dw	0
CURRpatchID:
	db	0
ParameterR:
	db	0
OverWR:
	db	"Y"

; Patch data

;.db page number,patch ID,number of patch
;.db original value, new value,address FSB,address MSB, etc...
; (address of data = 4000h ~ 7FFFh)

; --------
Anty_P1:
	db	1,0,8
	db	03Eh,03Eh,030h,4Ah, 007h,007h,031h,4Ah, 00Eh,00Eh,032h,4Ah, 0F8h,0B8h,033h,4Ah
	db	0D3h,0D3h,034h,4Ah, 0A0h,0A0h,035h,4Ah, 008h,008h,036h,4Ah, 079h,079h,037h,4Ah
; --------
AthleticBall:
	db	0,0,18
	db	0F0h,0B0h,056h,4Eh, 0F8h,0B8h,072h,4Eh, 0F0h,0B0h,080h,4Eh, 0FCh,0BCh,08Eh,4Eh
	db	0F8h,0B8h,09Ch,4Eh, 0FEh,0BEh,0A5h,52h, 0FEh,0BEh,088h,5Bh, 0F0h,0B0h,096h,5Bh
	db	0FEh,0BEh,0A4h,5Bh, 0FEh,0BEh,0B2h,5Bh, 0FEh,0BEh,0C0h,5Bh, 0FEh,0BEh,0CEh,5Bh
	db	0FEh,0BEh,0DCh,5Bh, 0FEh,0BEh,0EAh,5Bh, 0FEh,0BEh,0F8h,5Bh, 0FEh,0BEh,006h,5Ch
	db	0FEh,0BEh,014h,5Ch, 0FEh,0BEh,022h,5Ch
; --------
CarRace:
	db	0,0,33
	db	0FAh,0BAh,066h,4Eh, 0DEh,09Eh,005h,4Fh, 0FAh,0BAh,014h,4Fh, 016h,000h,046h,52h
	db	053h,058h,047h,52h, 0FFh,0E5h,000h,58h, 0FFh,03Ah,001h,58h, 0FFh,069h,002h,58h
	db	0FFh,0E0h,003h,58h, 0FFh,0FEh,004h,98h, 0FFh,041h,005h,58h, 0FFh,02Ah,006h,58h
	db	0FFh,063h,007h,58h, 0FFh,0E0h,008h,98h, 0FFh,028h,009h,58h, 0FFh,00Ah,00Ah,58h
	db	0FFh,0FEh,00Bh,58h, 0FFh,042h,00Ch,98h, 0FFh,02Ah,00Dh,58h, 0FFh,065h,00Eh,58h
	db	0FFh,0E0h,00Fh,58h, 0FFh,028h,010h,98h, 0FFh,003h,011h,58h, 0FFh,02Ah,012h,58h
	db	0FFh,067h,013h,58h, 0FFh,0E0h,014h,98h, 0FFh,07Eh,015h,58h, 0FFh,0E6h,016h,58h
	db	0FFh,03Fh,017h,58h, 0FFh,0F6h,018h,98h, 0FFh,080h,019h,58h, 0FFh,0E1h,01Ah,58h
	db	0FFh,0C9h,01Bh,58h
; --------
CrossBlaim:
	db	0,0,18
	db	045h,000h,036h,40h, 040h,070h,037h,40h, 045h,000h,041h,40h, 040h,070h,042h,40h
	db	0FFh,000h,065h,40h, 0BFh,070h,066h,40h, 03Eh,000h,099h,40h, 0CDh,000h,09Ah,40h
	db	032h,000h,09Bh,40h, 035h,000h,09Ch,40h, 040h,000h,09Dh,40h, 021h,000h,09Eh,40h
	db	040h,000h,09Fh,40h, 040h,000h,0A0h,40h, 077h,000h,0A1h,40h, 0BEh,000h,0A2h,40h
	db	028h,000h,0A3h,40h, 020h,000h,0A4h,40h
; --------
Galaxian:
	db	0,0,8
	db	031h,031h,010h,40h, 000h,0FDh,011h,40h, 000h,0FFh,012h,40h, 0F3h,0F3h,013h,40h
	db	031h,031h,044h,40h, 000h,0FDh,045h,40h, 000h,0FFh,046h,40h, 0CDh,0CDh,047h,40h
; --------
HarryFoxYuki:
	db	0,0,43
	db	000h,00Ah,002h,40h, 000h,040h,003h,40h, 000h,03Eh,00Ah,40h, 000h,001h,00Bh,40h
	db	000h,032h,00Ch,40h, 000h,070h,00Eh,40h, 000h,0C9h,00Fh,40h, 021h,026h,0CBh,6Fh
	db	000h,040h,0CCh,6Fh, 040h,0CDh,0CDh,6Fh, 0CDh,024h,0CEh,6Fh, 024h,000h,0CFh,6Fh
	db	000h,0CDh,0D0h,6Fh, 0CDh,0CCh,0D1h,6Fh, 0CCh,0B6h,0D2h,6Fh, 0B6h,026h,0D3h,6Fh
	db	021h,080h,0D4h,6Fh, 000h,0CDh,0D5h,6Fh, 080h,024h,0D6h,6Fh, 0CDh,000h,0D7h,6Fh
	db	024h,03Eh,0D8h,6Fh, 000h,002h,0D9h,6Fh, 03Eh,032h,0DAh,6Fh, 001h,000h,0DBh,6Fh
	db	032h,060h,0DCh,6Fh, 000h,03Ch,0DDh,6Fh, 060h,032h,0DEh,6Fh, 032h,000h,0DFh,6Fh
	db	000h,070h,0E0h,6Fh, 070h,0E1h,0E1h,6Fh, 0E1h,001h,0E2h,6Fh, 001h,023h,0E3h,6Fh
	db	023h,028h,0E4h,6Fh, 028h,011h,0E5h,6Fh, 011h,000h,0E6h,6Fh, 000h,0C5h,0E7h,6Fh
	db	0C5h,0EDh,0E8h,6Fh, 0EDh,0B0h,0E9h,6Fh, 0B0h,0AFh,0EAh,6Fh, 0AFh,032h,0EBh,6Fh
	db	032h,000h,0ECh,6Fh, 000h,060h,0EDh,6Fh, 060h,03Ch,0EEh,6Fh
; --------
Nausicaa:
	db	0,0,16
	db	0FFh,0BFh,013h,45h, 0E9h,0A9h,023h,45h, 0E0h,0A0h,0D7h,4Bh, 0E8h,0A8h,077h,4Dh
	db	0E8h,0A8h,06Eh,4Eh, 0FFh,0BFh,013h,55h, 0E9h,0A9h,023h,55h, 0E0h,0A0h,0D7h,5Bh
	db	0E8h,0A8h,077h,5Dh, 0E8h,0A8h,06Eh,5Eh, 0E1h,0A1h,071h,6Ah, 0E0h,0A0h,098h,6Ah
	db	0F8h,0B8h,0F8h,6Fh, 0E8h,0A8h,013h,71h, 0E8h,0A8h,033h,72h, 0E0h,0A0h,09Fh,74h
; --------
Robocop:
	db	0,0,9
	db	060h,000h,03Dh,40h, 002h,001h,06Dh,79h, 080h,070h,070h,79h, 0A0h,000h,075h,79h
	db	004h,002h,081h,79h, 080h,070h,084h,79h, 0A0h,000h,089h,79h, 006h,003h,095h,79h
	db	080h,070h,098h,79h
	; --------
QBert:
	db	0,0,8
	db	043h,043h,010h,40h, 044h,044h,011h,40h, 03Eh,03Eh,07Eh,40h, 001h,001h,07Fh,40h
	db	032h,032h,080h,40h, 060h,000h,082h,40h, 080h,000h,086h,40h, 0A0h,000h,08Ah,40h
; --------
SupLodeRunner:
	db	0,0,85
	db	000h,08Bh,002h,40h, 000h,080h,003h,40h, 000h,0F5h,078h,40h, 000h,0E5h,079h,40h
	db	000h,0D5h,07Ah,40h, 000h,0C5h,07Bh,40h, 000h,05Fh,07Ch,40h, 000h,03Ah,07Dh,40h
	db	000h,01Eh,07Eh,40h, 000h,0F9h,07Fh,40h, 000h,021h,080h,40h, 000h,070h,082h,40h
	db	000h,0CDh,083h,40h, 000h,014h,084h,40h, 000h,0C1h,086h,40h, 000h,0D1h,087h,40h
	db	000h,0E1h,088h,40h, 000h,0F1h,089h,40h, 000h,0C9h,08Ah,40h, 000h,079h,08Bh,40h
	db	000h,032h,08Ch,40h, 000h,01Eh,08Dh,40h, 000h,0F9h,08Eh,40h, 000h,0EDh,08Fh,40h
	db	000h,073h,090h,40h, 000h,0C0h,092h,40h, 000h,021h,093h,40h, 000h,00Ah,094h,40h
	db	000h,039h,096h,40h, 000h,0F9h,097h,40h, 000h,0E3h,098h,40h, 000h,07Ch,099h,40h
	db	000h,0FEh,09Ah,40h, 000h,0FCh,09Bh,40h, 000h,020h,09Ch,40h, 000h,015h,09Dh,40h
	db	000h,02Bh,09Eh,40h, 000h,02Bh,09Fh,40h, 000h,07Dh,0A0h,40h, 000h,023h,0A1h,40h
	db	000h,0E6h,0A2h,40h, 000h,003h,0A3h,40h, 000h,0FEh,0A4h,40h, 000h,001h,0A5h,40h
	db	000h,0CBh,0A6h,40h, 000h,07Eh,0A7h,40h, 000h,028h,0A8h,40h, 000h,002h,0A9h,40h
	db	000h,0CBh,0AAh,40h, 000h,0BEh,0ABh,40h, 000h,023h,0ACh,40h, 000h,0E3h,0ADh,40h
	db	000h,0EDh,0AEh,40h, 000h,07Bh,0AFh,40h, 000h,0C0h,0B1h,40h, 000h,0C9h,0B2h,40h
	db	000h,0CDh,0B3h,40h, 000h,0A6h,0B4h,40h, 000h,080h,0B5h,40h, 000h,0C3h,0B6h,40h
	db	000h,081h,0B8h,40h, 032h,0CDh,01Ch,41h, 000h,078h,01Dh,41h, 000h,080h,01Eh,41h
	db	032h,0CDh,09Ah,63h, 000h,078h,09Bh,63h, 000h,080h,09Ch,63h, 032h,0CDh,0A4h,63h
	db	000h,078h,0A5h,63h, 000h,080h,0A6h,63h, 032h,0CDh,0AFh,63h, 000h,078h,0B0h,63h
	db	000h,080h,0B1h,63h, 032h,0CDh,0B9h,63h, 000h,078h,0BAh,63h, 000h,080h,0BBh,63h
	db	032h,0CDh,0CCh,63h, 000h,078h,0CDh,63h, 000h,080h,0CEh,63h, 032h,0CDh,0D6h,63h
	db	000h,078h,0D7h,63h, 000h,080h,0D8h,63h, 032h,0CDh,0E2h,63h, 000h,078h,0E3h,63h
	db	000h,080h,0E4h,63h
SupLodeRunner_P1:
	db	1,0,63
	db	000h,08Fh,002h,40h, 000h,080h,003h,40h, 000h,0F5h,078h,40h, 000h,0E5h,079h,40h
	db	000h,0D5h,07Ah,40h, 000h,0C5h,07Bh,40h, 000h,05Fh,07Ch,40h, 000h,03Ah,07Dh,40h
	db	000h,01Eh,07Eh,40h, 000h,0F9h,07Fh,40h, 000h,021h,080h,40h, 000h,070h,082h,40h
	db	000h,0CDh,083h,40h, 000h,014h,084h,40h, 000h,0C1h,086h,40h, 000h,0D1h,087h,40h
	db	000h,0E1h,088h,40h, 000h,0F1h,089h,40h, 000h,0C9h,08Ah,40h, 000h,0EDh,08Fh,40h
	db	000h,073h,090h,40h, 000h,0C0h,092h,40h, 000h,021h,093h,40h, 000h,00Ah,094h,40h
	db	000h,039h,096h,40h, 000h,0F9h,097h,40h, 000h,0E3h,098h,40h, 000h,07Ch,099h,40h
	db	000h,0FEh,09Ah,40h, 000h,0FCh,09Bh,40h, 000h,020h,09Ch,40h, 000h,015h,09Dh,40h
	db	000h,02Bh,09Eh,40h, 000h,02Bh,09Fh,40h, 000h,07Dh,0A0h,40h, 000h,023h,0A1h,40h
	db	000h,0E6h,0A2h,40h, 000h,003h,0A3h,40h, 000h,0FEh,0A4h,40h, 000h,001h,0A5h,40h
	db	000h,0CBh,0A6h,40h, 000h,07Eh,0A7h,40h, 000h,028h,0A8h,40h, 000h,002h,0A9h,40h
	db	000h,0CBh,0AAh,40h, 000h,0BEh,0ABh,40h, 000h,023h,0ACh,40h, 000h,0E3h,0ADh,40h
	db	000h,0EDh,0AEh,40h, 000h,07Bh,0AFh,40h, 000h,0C0h,0B1h,40h, 000h,0C9h,0B2h,40h
	db	000h,0CDh,0B3h,40h, 000h,0A6h,0B4h,40h, 000h,080h,0B5h,40h, 000h,0C3h,0B6h,40h
	db	000h,081h,0B8h,40h, 032h,0CDh,02Eh,41h, 000h,078h,02Fh,41h, 000h,080h,030h,41h
	db	032h,0CDh,0E5h,76h, 000h,078h,0E6h,76h, 000h,080h,0E7h,76h
SupLodeRunner_P2:
	db	2,0,78
	db	000h,08Fh,002h,40h, 000h,080h,003h,40h, 000h,0F5h,078h,40h, 000h,0E5h,079h,40h
	db	000h,0D5h,07Ah,40h, 000h,0C5h,07Bh,40h, 000h,05Fh,07Ch,40h, 000h,03Ah,07Dh,40h
	db	000h,01Eh,07Eh,40h, 000h,0F9h,07Fh,40h, 000h,021h,080h,40h, 000h,070h,082h,40h
	db	000h,0CDh,083h,40h, 000h,014h,084h,40h, 000h,0C1h,086h,40h, 000h,0D1h,087h,40h
	db	000h,0E1h,088h,40h, 000h,0F1h,089h,40h, 000h,0C9h,08Ah,40h, 000h,0EDh,08Fh,40h
	db	000h,073h,090h,40h, 000h,0C0h,092h,40h, 000h,021h,093h,40h, 000h,00Ah,094h,40h
	db	000h,039h,096h,40h, 000h,0F9h,097h,40h, 000h,0E3h,098h,40h, 000h,07Ch,099h,40h
	db	000h,0FEh,09Ah,40h, 000h,0FCh,09Bh,40h, 000h,020h,09Ch,40h, 000h,015h,09Dh,40h
	db	000h,02Bh,09Eh,40h, 000h,02Bh,09Fh,40h, 000h,07Dh,0A0h,40h, 000h,023h,0A1h,40h
	db	000h,0E6h,0A2h,40h, 000h,003h,0A3h,40h, 000h,0FEh,0A4h,40h, 000h,001h,0A5h,40h
	db	000h,0CBh,0A6h,40h, 000h,07Eh,0A7h,40h, 000h,028h,0A8h,40h, 000h,002h,0A9h,40h
	db	000h,0CBh,0AAh,40h, 000h,0BEh,0ABh,40h, 000h,023h,0ACh,40h, 000h,0E3h,0ADh,40h
	db	000h,0EDh,0AEh,40h, 000h,07Bh,0AFh,40h, 000h,0C0h,0B1h,40h, 000h,0C9h,0B2h,40h
	db	000h,0CDh,0B3h,40h, 000h,0A6h,0B4h,40h, 000h,080h,0B5h,40h, 000h,0C3h,0B6h,40h
	db	000h,081h,0B8h,40h, 032h,0CDh,09Ah,63h, 000h,078h,09Bh,63h, 000h,080h,09Ch,63h
	db	032h,0CDh,0A4h,63h, 000h,078h,0A5h,63h, 000h,080h,0A6h,63h, 032h,0CDh,0AFh,63h
	db	000h,078h,0B0h,63h, 000h,080h,0B1h,63h, 032h,0CDh,0B9h,63h, 000h,078h,0BAh,63h
	db	000h,080h,0BBh,63h, 032h,0CDh,0CCh,63h, 000h,078h,0CDh,63h, 000h,080h,0CEh,63h
	db	032h,0CDh,0D6h,63h, 000h,078h,0D7h,63h, 000h,080h,0D8h,63h, 032h,0CDh,0E2h,63h
	db	000h,078h,0E3h,63h, 000h,080h,0E4h,63h
SupLodeRunner_P3:
	db	3,0,60
	db	000h,08Fh,002h,40h, 000h,080h,003h,40h, 000h,0F5h,078h,40h, 000h,0E5h,079h,40h
	db	000h,0D5h,07Ah,40h, 000h,0C5h,07Bh,40h, 000h,05Fh,07Ch,40h, 000h,03Ah,07Dh,40h
	db	000h,01Eh,07Eh,40h, 000h,0F9h,07Fh,40h, 000h,021h,080h,40h, 000h,070h,082h,40h
	db	000h,0CDh,083h,40h, 000h,014h,084h,40h, 000h,0C1h,086h,40h, 000h,0D1h,087h,40h
	db	000h,0E1h,088h,40h, 000h,0F1h,089h,40h, 000h,0C9h,08Ah,40h, 000h,0EDh,08Fh,40h
	db	000h,073h,090h,40h, 000h,0C0h,092h,40h, 000h,021h,093h,40h, 000h,00Ah,094h,40h
	db	000h,039h,096h,40h, 000h,0F9h,097h,40h, 000h,0E3h,098h,40h, 000h,07Ch,099h,40h
	db	000h,0FEh,09Ah,40h, 000h,0FCh,09Bh,40h, 000h,020h,09Ch,40h, 000h,015h,09Dh,40h
	db	000h,02Bh,09Eh,40h, 000h,02Bh,09Fh,40h, 000h,07Dh,0A0h,40h, 000h,023h,0A1h,40h
	db	000h,0E6h,0A2h,40h, 000h,003h,0A3h,40h, 000h,0FEh,0A4h,40h, 000h,001h,0A5h,40h
	db	000h,0CBh,0A6h,40h, 000h,07Eh,0A7h,40h, 000h,028h,0A8h,40h, 000h,002h,0A9h,40h
	db	000h,0CBh,0AAh,40h, 000h,0BEh,0ABh,40h, 000h,023h,0ACh,40h, 000h,0E3h,0ADh,40h
	db	000h,0EDh,0AEh,40h, 000h,07Bh,0AFh,40h, 000h,0C0h,0B1h,40h, 000h,0C9h,0B2h,40h
	db	000h,0CDh,0B3h,40h, 000h,0A6h,0B4h,40h, 000h,080h,0B5h,40h, 000h,0C3h,0B6h,40h
	db	000h,081h,0B8h,40h, 032h,0CDh,02Eh,41h, 000h,078h,02Fh,41h, 000h,080h,030h,41h
SupLodeRunner_P4:
	db	4,0,60
	db	000h,08Fh,002h,40h, 000h,080h,003h,40h, 000h,0F5h,078h,40h, 000h,0E5h,079h,40h
	db	000h,0D5h,07Ah,40h, 000h,0C5h,07Bh,40h, 000h,05Fh,07Ch,40h, 000h,03Ah,07Dh,40h
	db	000h,01Eh,07Eh,40h, 000h,0F9h,07Fh,40h, 000h,021h,080h,40h, 000h,070h,082h,40h
	db	000h,0CDh,083h,40h, 000h,014h,084h,40h, 000h,0C1h,086h,40h, 000h,0D1h,087h,40h
	db	000h,0E1h,088h,40h, 000h,0F1h,089h,40h, 000h,0C9h,08Ah,40h, 000h,0EDh,08Fh,40h
	db	000h,073h,090h,40h, 000h,0C0h,092h,40h, 000h,021h,093h,40h, 000h,00Ah,094h,40h
	db	000h,039h,096h,40h, 000h,0F9h,097h,40h, 000h,0E3h,098h,40h, 000h,07Ch,099h,40h
	db	000h,0FEh,09Ah,40h, 000h,0FCh,09Bh,40h, 000h,020h,09Ch,40h, 000h,015h,09Dh,40h
	db	000h,02Bh,09Eh,40h, 000h,02Bh,09Fh,40h, 000h,07Dh,0A0h,40h, 000h,023h,0A1h,40h
	db	000h,0E6h,0A2h,40h, 000h,003h,0A3h,40h, 000h,0FEh,0A4h,40h, 000h,001h,0A5h,40h
	db	000h,0CBh,0A6h,40h, 000h,07Eh,0A7h,40h, 000h,028h,0A8h,40h, 000h,002h,0A9h,40h
	db	000h,0CBh,0AAh,40h, 000h,0BEh,0ABh,40h, 000h,023h,0ACh,40h, 000h,0E3h,0ADh,40h
	db	000h,0EDh,0AEh,40h, 000h,07Bh,0AFh,40h, 000h,0C0h,0B1h,40h, 000h,0C9h,0B2h,40h
	db	000h,0CDh,0B3h,40h, 000h,0A6h,0B4h,40h, 000h,080h,0B5h,40h, 000h,0C3h,0B6h,40h
	db	000h,081h,0B8h,40h, 032h,0CDh,02Eh,41h, 000h,078h,02Fh,41h, 000h,080h,030h,41h
SupLodeRunner_P5:
	db	5,0,60
	db	000h,08Fh,002h,40h, 000h,080h,003h,40h, 000h,0F5h,078h,40h, 000h,0E5h,079h,40h
	db	000h,0D5h,07Ah,40h, 000h,0C5h,07Bh,40h, 000h,05Fh,07Ch,40h, 000h,03Ah,07Dh,40h
	db	000h,01Eh,07Eh,40h, 000h,0F9h,07Fh,40h, 000h,021h,080h,40h, 000h,070h,082h,40h
	db	000h,0CDh,083h,40h, 000h,014h,084h,40h, 000h,0C1h,086h,40h, 000h,0D1h,087h,40h
	db	000h,0E1h,088h,40h, 000h,0F1h,089h,40h, 000h,0C9h,08Ah,40h, 000h,0EDh,08Fh,40h
	db	000h,073h,090h,40h, 000h,0C0h,092h,40h, 000h,021h,093h,40h, 000h,00Ah,094h,40h
	db	000h,039h,096h,40h, 000h,0F9h,097h,40h, 000h,0E3h,098h,40h, 000h,07Ch,099h,40h
	db	000h,0FEh,09Ah,40h, 000h,0FCh,09Bh,40h, 000h,020h,09Ch,40h, 000h,015h,09Dh,40h
	db	000h,02Bh,09Eh,40h, 000h,02Bh,09Fh,40h, 000h,07Dh,0A0h,40h, 000h,023h,0A1h,40h
	db	000h,0E6h,0A2h,40h, 000h,003h,0A3h,40h, 000h,0FEh,0A4h,40h, 000h,001h,0A5h,40h
	db	000h,0CBh,0A6h,40h, 000h,07Eh,0A7h,40h, 000h,028h,0A8h,40h, 000h,002h,0A9h,40h
	db	000h,0CBh,0AAh,40h, 000h,0BEh,0ABh,40h, 000h,023h,0ACh,40h, 000h,0E3h,0ADh,40h
	db	000h,0EDh,0AEh,40h, 000h,07Bh,0AFh,40h, 000h,0C0h,0B1h,40h, 000h,0C9h,0B2h,40h
	db	000h,0CDh,0B3h,40h, 000h,0A6h,0B4h,40h, 000h,080h,0B5h,40h, 000h,0C3h,0B6h,40h
	db	000h,081h,0B8h,40h, 032h,0CDh,02Eh,41h, 000h,078h,02Fh,41h, 000h,080h,030h,41h
SupLodeRunner_P6:
	db	6,0,63
	db	000h,08Fh,002h,40h, 000h,080h,003h,40h, 000h,0F5h,078h,40h, 000h,0E5h,079h,40h
	db	000h,0D5h,07Ah,40h, 000h,0C5h,07Bh,40h, 000h,05Fh,07Ch,40h, 000h,03Ah,07Dh,40h
	db	000h,01Eh,07Eh,40h, 000h,0F9h,07Fh,40h, 000h,021h,080h,40h, 000h,070h,082h,40h
	db	000h,0CDh,083h,40h, 000h,014h,084h,40h, 000h,0C1h,086h,40h, 000h,0D1h,087h,40h
	db	000h,0E1h,088h,40h, 000h,0F1h,089h,40h, 000h,0C9h,08Ah,40h, 000h,0EDh,08Fh,40h
	db	000h,073h,090h,40h, 000h,0C0h,092h,40h, 000h,021h,093h,40h, 000h,00Ah,094h,40h
	db	000h,039h,096h,40h, 000h,0F9h,097h,40h, 000h,0E3h,098h,40h, 000h,07Ch,099h,40h
	db	000h,0FEh,09Ah,40h, 000h,0FCh,09Bh,40h, 000h,020h,09Ch,40h, 000h,015h,09Dh,40h
	db	000h,02Bh,09Eh,40h, 000h,02Bh,09Fh,40h, 000h,07Dh,0A0h,40h, 000h,023h,0A1h,40h
	db	000h,0E6h,0A2h,40h, 000h,003h,0A3h,40h, 000h,0FEh,0A4h,40h, 000h,001h,0A5h,40h
	db	000h,0CBh,0A6h,40h, 000h,07Eh,0A7h,40h, 000h,028h,0A8h,40h, 000h,002h,0A9h,40h
	db	000h,0CBh,0AAh,40h, 000h,0BEh,0ABh,40h, 000h,023h,0ACh,40h, 000h,0E3h,0ADh,40h
	db	000h,0EDh,0AEh,40h, 000h,07Bh,0AFh,40h, 000h,0C0h,0B1h,40h, 000h,0C9h,0B2h,40h
	db	000h,0CDh,0B3h,40h, 000h,0A6h,0B4h,40h, 000h,080h,0B5h,40h, 000h,0C3h,0B6h,40h
	db	000h,081h,0B8h,40h, 032h,0CDh,012h,41h, 000h,078h,013h,41h, 000h,080h,014h,41h
	db	032h,0CDh,0E5h,76h, 000h,078h,0E6h,76h, 000h,080h,0E7h,76h
SupLodeRunner_P7:
	db	7,0,78
	db	000h,08Fh,002h,40h, 000h,080h,003h,40h, 000h,0F5h,078h,40h, 000h,0E5h,079h,40h
	db	000h,0D5h,07Ah,40h, 000h,0C5h,07Bh,40h, 000h,05Fh,07Ch,40h, 000h,03Ah,07Dh,40h
	db	000h,01Eh,07Eh,40h, 000h,0F9h,07Fh,40h, 000h,021h,080h,40h, 000h,070h,082h,40h
	db	000h,0CDh,083h,40h, 000h,014h,084h,40h, 000h,0C1h,086h,40h, 000h,0D1h,087h,40h
	db	000h,0E1h,088h,40h, 000h,0F1h,089h,40h, 000h,0C9h,08Ah,40h, 000h,0EDh,08Fh,40h
	db	000h,073h,090h,40h, 000h,0C0h,092h,40h, 000h,021h,093h,40h, 000h,00Ah,094h,40h
	db	000h,039h,096h,40h, 000h,0F9h,097h,40h, 000h,0E3h,098h,40h, 000h,07Ch,099h,40h
	db	000h,0FEh,09Ah,40h, 000h,0FCh,09Bh,40h, 000h,020h,09Ch,40h, 000h,015h,09Dh,40h
	db	000h,02Bh,09Eh,40h, 000h,02Bh,09Fh,40h, 000h,07Dh,0A0h,40h, 000h,023h,0A1h,40h
	db	000h,0E6h,0A2h,40h, 000h,003h,0A3h,40h, 000h,0FEh,0A4h,40h, 000h,001h,0A5h,40h
	db	000h,0CBh,0A6h,40h, 000h,07Eh,0A7h,40h, 000h,028h,0A8h,40h, 000h,002h,0A9h,40h
	db	000h,0CBh,0AAh,40h, 000h,0BEh,0ABh,40h, 000h,023h,0ACh,40h, 000h,0E3h,0ADh,40h
	db	000h,0EDh,0AEh,40h, 000h,07Bh,0AFh,40h, 000h,0C0h,0B1h,40h, 000h,0C9h,0B2h,40h
	db	000h,0CDh,0B3h,40h, 000h,0A6h,0B4h,40h, 000h,080h,0B5h,40h, 000h,0C3h,0B6h,40h
	db	000h,081h,0B8h,40h, 032h,0CDh,078h,76h, 000h,078h,079h,76h, 000h,080h,07Ah,76h
	db	032h,0CDh,085h,76h, 000h,078h,086h,76h, 000h,080h,087h,76h, 032h,0CDh,098h,76h 
	db	000h,078h,099h,76h, 000h,080h,09Ah,76h, 032h,0CDh,0A2h,76h, 000h,078h,0A3h,76h 
	db	000h,080h,0A4h,76h, 032h,0CDh,0FFh,76h, 000h,078h,000h,77h, 000h,080h,001h,77h 
	db	032h,0CDh,09Eh,77h, 000h,078h,09Fh,77h, 000h,080h,0A0h,77h, 032h,0CDh,0A7h,77h
	db	000h,078h,0A8h,77h, 000h,080h,0A9h,77h
; --------
SuperSuwanggi:
	db	0,0,9
	db	002h,001h,012h,46h, 080h,070h,015h,46h, 004h,002h,01Dh,46h, 080h,070h,020h,46h
	db	006h,003h,028h,46h, 080h,070h,02Bh,46h, 002h,001h,033h,46h, 080h,070h,036h,46h
	db	080h,070h,03Fh,46h
SuperSuwanggi_P1:
	db	1,0,9
	db	0CDh,0C3h,0F2h,61h, 0CDh,0C3h,03Ah,62h, 0CDh,0C3h,081h,62h, 0CDh,0C3h,0EBh,62h
	db	0CDh,0C3h,0D6h,64h, 0CDh,0C3h,034h,66h, 0F8h,0B8h,001h,6Ch, 0F8h,0B8h,0A1h,6Dh
	db	0F8h,0B8h,0EEh,7Fh
SuperSuwanggi_P3:
	db	3,0,4
	db	01Eh,01Eh,0D6h,7Bh, 0F1h,0B1h,0D7h,7Bh, 0CDh,0CDh,0D8h,7Bh, 093h,093h,0D9h,7Bh
; --------
