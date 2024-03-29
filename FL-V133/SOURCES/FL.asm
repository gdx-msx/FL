
;*** FL.COM v1.33 for MSX

;*** ROM Loader for MegaflashROM mapped Konami SCC

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
BUFTOP	equ	08000h
CHGET	equ	0009fh
MNROM	equ	0FCC1h	; Main-ROM Slot number & Secondary slot flags table
DRVINV	equ	0FB22H	; Installed Disk-ROM

	org	0100h

START:
	jp	Main

MESVER:
	db	CR,LF,"ROM Loader v1.33 for",CR,LF
	db	"SCC MegaFlashROM by GDX",CR,LF
	db	"based on FLLOAD by K.Tsujikawa"
MESend:
	db	CR,LF,CR,LF,"$"
HlpMes:
	db	"Usage: FL filename.ext /Sxx /R",CR,LF
	db	"       FL /Sxx /E",CR,LF,CR,LF
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
	call	BDOS		; Print MESVER message (FL info)

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
	jr	z,No_S		; Jump if S not found
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
	jp	z,Done		; Jump if Megaflashrom not detected
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
	ld	hl,1
	ld	(FCB+14),hl	; Record size = 1 byte
	or	a
	ld	de,DosErr
	jp	nz,Done

	ld	c,1ah
	ld	de,BUFTOP
	call	BDOS		; Set disk transfer address (buffer start at 8000H)

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
	ld	h,40h
	call	ENASLT		; Select Flashrom at bank 4000h~7FFFh

	di
	ld	a,0aah
	ld	(4555h),a	; Flashrom...
	ld	a,055h
	ld	(42aah),a	;
	ld	a,080h
	ld	(4555h),a	; ... erase ...
	ld	a,0aah
	ld	(4555h),a	;
	ld	a,055h
	ld	(42aah),a	;
	ld	a,010h
	ld	(4555h),a	; ... command

	ld	a,0ffh
	ld	de,4000h
	call	CHECK
	jp	c,Done		; Jump if Erase fail

	ei
	ld	de,FlsEok	; Pointer to Erase OK message
	ld	a,(FLerase)
	cp	045h
	jp	z,Done		; Jump if Erase option used

	ld	c,9
	call	BDOS		; Print FlsEok message

	ld	a,(RAMAD1)
	ld	h,40h
	call	ENASLT		; Select Main-RAM at bank 4000h~7FFFh

_8kL01:
	ld	c,27h
	ld	de,FCB
	ld	hl,2000h	; Number of records to read
	call	BDOS		; Read a block from file

	push	hl
	ld	hl,FCB+16
	xor	a
	or	(hl)
	inc	hl
	or	(hl)
	inc	hl
	or	(hl)
	inc	hl
	or	(hl)
	ld	(FileSize),a	; Get a simple value to test file size 
	pop	hl

	ld	a,h
	or	l
	ld	de,DonMes
	jp	nz,CONTloading	; Jump if record is readed

	ld	a,(PreBnk)
	cp	1
	jr	z,MakeMirror
	cp	2
	jr	z,MakeMirror
	cp	3
	jp	z,FLashPage	; FLash Page 1 again to page 3
	jp	Done		; Jump if any record is readed

MakeMirror:
	ld	a,(FileSize)
	cp	20h
	jp	z,FLashPage	; Jump if 8KB rom

Make16KMirror:
	ld	a,(ERMSlt)
	ld	h,40h
	call	ENASLT		; Select Flashrom at bank 4000h~7FFFh

	ld	a,(PreBnk)
	ld	(5000h),a	; Select Flashrom page at Bank 4000h~7FFFh for SCC mapper

	ld	hl,4555h
	ld	de,42aah

	exx
	ld	bc,2000h	; Length
	ld	de,4000h	; Destination
	ld	hl,0A000h	; Source
	jp	Loop

CONTloading:
	ld	a,h
	cp	20h
	ld	de,DosErr
	jp	nz,Done		; Jump if readed records number is not 2000h

	ld	a,(PreBnk)
	or	a
	jr	nz,SKIP_PatchRomHeader

	ld	a,(BUFTOP)
	cp	41h
	jr	nz,SKIP_PatchRomHeader
	ld	a,(BUFTOP+1)
	cp	42h
	jr	nz,SKIP_PatchRomHeader

	ld	a,(FileSize)
	cp	40h
	jr	nz,SKIP_PatchRomHeader

	ld	hl,AthleticBall
	call	MEGpatch	; Apply patch for page 0 of 16KB Rom

	ld	hl,CrazyBuggy
	call	MEGpatch	; Apply patch for page 0 of 16KB Rom

	ld	hl,Nausicaa
	call	MEGpatch	; Apply patch for page 0 of 16KB Rom

	ld	hl,Tatica
	call	MEGpatch	; Apply patch for page 0 of 16KB Rom

	ld	hl,BUFTOP
	ld	de,0A000h
	ld	bc,2000h
	ldir			; backup of page 0 for 8KB Rom

PatchHead:
	ld	hl,(BUFTOP+2)
	ld	a,h
	cp	80h
	jr	c,SKIP_PatchRomHeader	; Jump Start address is > 7FFFh

	ld	hl,BUFTOP+8h
	ld	(hl),0			; Remove pointer for Basic
	inc	hl
	ld	(hl),0			; Remove pointer for Basic
SKIP_PatchRomHeader:

	ld	hl,Akumajou
	call	MEGpatch		; Apply patch
	ld	hl,Akumajou_P1
	call	MEGpatch		; Apply patch 

	ld	hl,Akumajou_a
	call	MEGpatch		; Apply patch 
	ld	hl,Akumajou_a_P1
	call	MEGpatch		; Apply patch 

	ld	hl,Akumajou_a2
	call	MEGpatch		; Apply patch 
	ld	hl,Akumajou_a2_P1
	call	MEGpatch		; Apply patch 

	ld	hl,Akumajou_a3
	call	MEGpatch		; Apply patch 
	ld	hl,Akumajou_a3_P1
	call	MEGpatch		; Apply patch 

	ld	hl,Aleste
	call	MEGpatch		; Apply patch 
	ld	hl,Aleste_P1
	call	MEGpatch		; Apply patch 

	ld	hl,Anty_P1
	call	MEGpatch		; Apply patch 

	ld	hl,BubbleBobble
	call	MEGpatch		; Apply patch
	ld	hl,BubbleBobble_P1
	call	MEGpatch		; Apply patch
	ld	hl,BubbleBobble_P4
	call	MEGpatch		; Apply patch
	ld	hl,BubbleBobble_P5
	call	MEGpatch		; Apply patch
	ld	hl,BubbleBobble_P9
	call	MEGpatch		; Apply patch

	ld	hl,CarRace
	call	MEGpatch		; Apply patch 

	ld	hl,CrazyBuggy_P1
	call	MEGpatch		; Apply patch 

	ld	hl,Famboxing
	call	MEGpatch		; Apply patch 

	ld	hl,Firebird
	call	MEGpatch		; Apply patch 

	ld	hl,Galaxian
	call	MEGpatch		; Apply patch

	ld	hl,Game80zemina
	call	MEGpatch		; Apply patch 

	ld	hl,Garakuta
	call	MEGpatch		; Apply patch 
	ld	a,(CURRpatchID)
	cp	8			; condition used when a page patch is small
	jr	nz,SKIPgarakuta		; jump game if Graduis page 0 is not patched
	ld	hl,Garakuta_P1
	call	MEGpatch		; Apply patch 
SKIPgarakuta:

	ld	hl,Goemon
	call	MEGpatch		; Apply patch 
	ld	hl,Goemon_P1
	call	MEGpatch		; Apply patch 
	ld	hl,Goemon_P2
	call	MEGpatch		; Apply patch 

	ld	hl,GoonR
	call	MEGpatch		; Apply patch 
	ld	a,(CURRpatchID)
	cp	7			; condition used when a page patch is small
	jr	nz,SKIPGoonR		; jump game if Graduis page 0 is not patched
	ld	hl,GoonR_P3
	call	MEGpatch		; Apply patch 
	ld	hl,GoonR_P4
	call	MEGpatch		; Apply patch 
SKIPGoonR:

	ld	hl,Graduis
	call	MEGpatch		; Apply patch 
	ld	a,(CURRpatchID)
	cp	1			; condition used when a page patch is small
	jr	nz,SKIPgrad		; jump game if Graduis page 0 is not patched
	ld	hl,Graduis_P1
	call	MEGpatch		; Apply patch 
	ld	hl,Graduis_P2
	call	MEGpatch		; Apply patch 
SKIPgrad:

	ld	hl,Graduis_a
	call	MEGpatch		; Apply patch 
	ld	a,(CURRpatchID)
	cp	2			; condition used when a page patch is small
	jr	nz,SKIPgrad_a		; jump if Graduis alt. page 0 is not patched
	ld	hl,Graduis_a_P1
	call	MEGpatch		; Apply patch 
	ld	hl,Graduis_a_P2
	call	MEGpatch		; Apply patch 
SKIPgrad_a:

	ld	hl,KingKong2
	call	MEGpatch		; Apply patch 
	ld	a,(CURRpatchID)
	cp	3			; condition used when a page patch is small
	jr	nz,SKIPkkong2		; jump if Graduis alt. page 0 is not patched
	ld	hl,KingKong2_P1
	call	MEGpatch		; Apply patch 
	ld	hl,KingKong2_P2
	call	MEGpatch		; Apply patch 
	ld	hl,KingKong2_P6
	call	MEGpatch		; Apply patch 
SKIPkkong2:

	ld	hl,Labyrinth
	call	MEGpatch		; Apply patch 

	ld	hl,Manb2
	call	MEGpatch		; Apply patch 

;	ld	hl,Manb2v2
;	call	MEGpatch		; Apply patch 

	ld	hl,MetalGear
	call	MEGpatch		; Apply patch 

	ld	hl,PengAdv_Part1
	call	MEGpatch		; Apply patch 
	ld	hl,PengAdv_Part2
	call	MEGpatch		; Apply patch 
	ld	hl,PengAdv_P1
	call	MEGpatch		; Apply patch 

	ld	hl,MonMon_Daewoo
	call	MEGpatch		; Apply patch 
	ld	a,(CURRpatchID)
	cp	4			; condition used when a page patch is small
	jr	nz,SKIPmonmon_D		; jump if Monmon Kaibutsu published by Daewoo page 0 is not patched
	ld	hl,MonMon_Daewoo_P2
	call	MEGpatch		; Apply patch 
	ld	hl,MonMon_Daewoo_P3
	call	MEGpatch		; Apply patch 
SKIPmonmon_D:

	ld	hl,MSXDOS22
	call	MEGpatch		; Apply patch 
	ld	a,(CURRpatchID)
	cp	9			; condition used when a page patch is small
	jr	nz,SKIPmsxdos22		; jump if Monmon Kaibutsu published by Daewoo page 0 is not patched
	ld	hl,MSXDOS22_P1
	call	MEGpatch		; Apply patch 
	ld	hl,MSXDOS22_P2
	call	MEGpatch		; Apply patch 
	ld	hl,MSXDOS22_P3
	call	MEGpatch		; Apply patch 
SKIPmsxdos22:

	ld	hl,MSXDOS22NL
	call	MEGpatch		; Apply patch 
	ld	a,(CURRpatchID)
	cp	10			; condition used when a page patch is small
	jr	nz,SKIPmsxdos22NL	; jump if Monmon Kaibutsu published by Daewoo page 0 is not patched
	ld	hl,MSXDOS22NL_P1
	call	MEGpatch		; Apply patch 
	ld	hl,MSXDOS22NL_P2
	call	MEGpatch		; Apply patch 
SKIPmsxdos22NL:

	ld	hl,Nausicaa_P1
	call	MEGpatch		; Apply patch 

	ld	hl,Robocop
	call	MEGpatch			; Apply patch 
	ld	a,(CURRpatchID)
	cp	14			; condition used when a page patch is small
	jr	nz,SKIPRobocop		; jump if Robocop page 0 is not patched
	ld	hl,Robocop_P1
	call	MEGpatch		; Apply patch 
SKIPRobocop:

	ld	hl,Shalom
	call	MEGpatch		; Apply patch 
	ld	a,(CURRpatchID)
	cp	5			; condition used when a page patch is small
	jr	nz,SKIPshalom		; jump if Shalom page 0 is not patched
	ld	hl,Shalom_P1
	call	MEGpatch		; Apply patch 
SKIPshalom:

	ld	hl,SupLodeRunner
	call	MEGpatch		; Apply patch 
	ld	a,(CURRpatchID)
	cp	6			; condition used when a page patch is small
	jr	nz,SkipSupLodeR		; jump if Super Lode Runner page 0 is not patched
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
	ld	hl,SupLodeRunner_P8
	call	MEGpatch		; Apply patch 
	ld	hl,SupLodeRunner_PA
	call	MEGpatch		; Apply patch 
	ld	hl,SupLodeRunner_PC
	call	MEGpatch		; Apply patch 
	ld	hl,SupLodeRunner_PD
	call	MEGpatch		; Apply patch 
	ld	hl,SupLodeRunner_PE
	call	MEGpatch		; Apply patch 
	ld	hl,SupLodeRunner_PF
	call	MEGpatch		; Apply patch 
SkipSupLodeR:

	ld	hl,SuperRunner
	call	MEGpatch		; Apply patch 
	ld	hl,SuperRunner_PE
	call	MEGpatch		; Apply patch 

	ld	hl,Tatica_P1
	call	MEGpatch		; Apply patch

	ld	hl,TheFairyLand
	call	MEGpatch		; Apply patch

	ld	hl,TheFairyLand_P1
	call	MEGpatch		; Apply patch

	ld	hl,TheMoG
	call	MEGpatch		; Apply patch 

	ld	hl,USAS
	call	MEGpatch		; Apply patch 

	ld	hl,USAS_a
	call	MEGpatch		; Apply patch 

	ld	hl,ZanacEX
	call	MEGpatch		; Apply patch 
	ld	a,(CURRpatchID)
	cp	12			; condition used when a page patch is small
	jr	nz,SkipZanacEX		; jump if Zanac EX page 0 is not patched
	ld	hl,ZanacEX_P1
	call	MEGpatch		; Apply patch 
	ld	hl,ZanacEX_P3
	call	MEGpatch		; Apply patch 
	ld	hl,ZanacEX_P5
	call	MEGpatch		; Apply patch 
	ld	hl,ZanacEX_P7
	call	MEGpatch		; Apply patch 
	ld	hl,ZanacEX_P9
	call	MEGpatch		; Apply patch 
	ld	hl,ZanacEX_PB
	call	MEGpatch		; Apply patch 
	ld	hl,ZanacEX_PD
	call	MEGpatch		; Apply patch 
	ld	hl,ZanacEX_PE
	call	MEGpatch		; Apply patch 
	ld	hl,ZanacEX_PF
	call	MEGpatch		; Apply patch 
SkipZanacEX:

	ld	hl,ZombHunt
	call	MEGpatch		; Apply patch 
	ld	hl,ZombHunt_P1
	call	MEGpatch		; Apply patch 

FLashPage:	
	ld	a,(ERMSlt)
	ld	h,40h
	call	ENASLT		; Select Flashrom at bank 4000h~7FFFh

	ld	a,(PreBnk)
	ld	(5000h),a	; Select Flashrom page at Bank 4000h~7FFFh for SCC mapper

	ld	hl,4555h
	ld	de,42aah

	exx
	ld	bc,2000h	; Length
	ld	de,4000h	; Destination
	ld	hl,BUFTOP	; Source
Loop:
 	di
	exx
	ld	(hl),0aah
	ld	a,055h
	ld	(de),a
	ld	(hl),0a0h
 	exx
	ld	a,(hl)
	ld	(de),a		; Write a byte to flashrom

	ex	AF,AF'
	ld	a,(PreBnk)
	ld	(5000h),a	; Select flashrom page 4000h~7FFFh for SCC mapper 
	ex	AF,AF'

	call	CHECK		; Check this byte
	jp	c,Done

	inc	hl
	inc	de
	dec	bc
	ld	a,b
	or	c
	jr	nz,Loop

	ei
	ld	a,(RAMAD1)
	ld	h,40h
	call	ENASLT		; Select Main-RAM at bank 4000h~7FFFh

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
	ld	hl,5000h	; Page selection address for scc mapper
	ld	e,0		; Page number
	call	0014h		; Select page 0 of Megaflashrom

	ld	a,(ERMSlt)	; Megaflashrom slot
	ld	hl,7000h	; Page selection address for scc mapper
	ld	e,1		; Page number
	call	0014h		; Select page 1 of Megaflashrom

	ld	a,(ERMSlt)	; Megaflashrom slot
	ld	hl,9000h	; Page selection address for scc mapper
	ld	e,2		; Page number
	call	0014h		; Select page 2 of Megaflashrom

	ld	a,(ERMSlt)	; Megaflashrom slot
	ld	hl,0B000h	; Page selection address for scc mapper
	ld	e,3		; Page number
	call	0014h		; Select page 3 of Megaflashrom

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
	rst	0		; msx reset
NoReset:
	ei
	push	de
	ld	a,(RAMAD1)
	ld	h,40h
	call	ENASLT		; Select Main-RAM at bank 4000h~7FFFh
	ld	a,(RAMAD2)
	ld	h,80h
	call	ENASLT		; Select Main-RAM at bank 8000h~BFFFh
	pop	de

	ld	c,9
	call	BDOS		; Print final message

;	ld	de,HlpMes	; Pointer to Help message
;	ld	c,9
;	call	BDOS		; Print final message

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
	ld	b,(hl)		; B = Number of patch
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
CURRpatchID:
	db	0
FileSize:
	db	0
ParameterR:
	db	0
OverWR:
	db	"Y"

; Patch data

; db page number,patch ID,number of patch
; db original value, new value,address FSB,address MSB, etc...
; (address of data = 8000h ~ 9FFFh)

Akumajou:
	db	0,0,32
	db	060h,070h,0BEh,8Fh, 060h,070h,065h,90h, 060h,070h,042h,91h, 060h,070h,03Fh,93h
	db	060h,070h,06Ah,93h, 060h,070h,0A6h,93h, 080h,090h,035h,80h, 080h,090h,044h,80h
	db	080h,090h,0C4h,8Fh, 080h,090h,06Ah,90h, 080h,090h,0A8h,90h, 080h,090h,047h,91h
	db	080h,090h,044h,93h, 080h,090h,058h,93h, 080h,090h,070h,93h, 080h,090h,082h,93h
	db	080h,090h,094h,93h, 080h,090h,0ACh,93h, 080h,090h,0D3h,9Eh, 080h,090h,0EEh,9Eh
	db	0A0h,0B0h,03Ah,80h, 0A0h,0B0h,04Ah,80h, 0A0h,0B0h,0CAh,8Fh, 0A0h,0B0h,070h,90h
	db	0A0h,0B0h,0B2h,90h, 0A0h,0B0h,04Dh,91h, 0A0h,0B0h,04Ah,93h, 0A0h,0B0h,05Eh,93h
	db	0A0h,0B0h,076h,93h, 0A0h,0B0h,088h,93h, 0A0h,0B0h,09Ah,93h, 0A0h,0B0h,0B2h,93h
Akumajou_P1:
	db	1,0,7
	db	080h,090h,059h,81h, 080h,090h,07Bh,81h, 080h,090h,052h,87h, 0A0h,0B0h,053h,84h
	db	0A0h,0B0h,091h,84h, 0A0h,0B0h,0C7h,84h, 0A0h,0B0h,048h,87h
; --------
Akumajou_a:
	db	0,0,32
	db	060h,070h,0BEh,8Fh, 060h,070h,065h,90h, 060h,070h,03Ah,91h, 060h,070h,037h,93h
	db	060h,070h,062h,93h, 060h,070h,09Eh,93h, 080h,090h,035h,80h, 080h,090h,044h,80h
	db	080h,090h,0C4h,8Fh, 080h,090h,06Ah,90h, 080h,090h,0A8h,90h, 080h,090h,03Fh,91h
	db	080h,090h,03Ch,93h, 080h,090h,050h,93h, 080h,090h,068h,93h, 080h,090h,07Ah,93h
	db	080h,090h,08Ch,93h, 080h,090h,0A4h,93h, 080h,090h,0CCh,9Eh, 080h,090h,0E7h,9Eh
	db	0A0h,0B0h,03Ah,80h, 0A0h,0B0h,04Ah,80h, 0A0h,0B0h,0CAh,8Fh, 0A0h,0B0h,070h,90h
	db	0A0h,0B0h,0B2h,90h, 0A0h,0B0h,045h,91h, 0A0h,0B0h,042h,93h, 0A0h,0B0h,056h,93h
	db	0A0h,0B0h,06Eh,93h, 0A0h,0B0h,080h,93h, 0A0h,0B0h,092h,93h, 0A0h,0B0h,0AAh,93h
Akumajou_a_P1:
	db	1,0,7
	db	080h,090h,052h,81h, 080h,090h,074h,81h, 080h,090h,038h,87h, 0A0h,0B0h,049h,84h
	db	0A0h,0B0h,087h,84h, 0A0h,0B0h,0BDh,84h, 0A0h,0B0h,02Eh,87h
; --------
Akumajou_a2:
	db	0,0,32
	db	060h,070h,0C5h,8Fh, 060h,070h,06Ch,90h, 060h,070h,049h,91h, 060h,070h,046h,93h
	db	060h,070h,071h,93h, 060h,070h,0ADh,93h, 080h,090h,035h,80h, 080h,090h,044h,80h
	db	080h,090h,0CBh,8Fh, 080h,090h,071h,90h, 080h,090h,0AFh,90h, 080h,090h,04Eh,91h
	db	080h,090h,04Bh,93h, 080h,090h,05Fh,93h, 080h,090h,077h,93h, 080h,090h,089h,93h
	db	080h,090h,09Bh,93h, 080h,090h,0B3h,93h, 080h,090h,0DAh,9Eh, 080h,090h,0F5h,9Eh
	db	0A0h,0B0h,03Ah,80h, 0A0h,0B0h,04Ah,80h, 0A0h,0B0h,0D1h,8Fh, 0A0h,0B0h,077h,90h
	db	0A0h,0B0h,0B9h,90h, 0A0h,0B0h,054h,91h, 0A0h,0B0h,051h,93h, 0A0h,0B0h,065h,93h
	db	0A0h,0B0h,07Dh,93h, 0A0h,0B0h,08Fh,93h, 0A0h,0B0h,0A1h,93h, 0A0h,0B0h,0B9h,93h
Akumajou_a2_P1:
	db	1,0,7
	db	080h,090h,060h,81h, 080h,090h,082h,81h, 080h,090h,059h,87h, 0A0h,0B0h,05Ah,84h
	db	0A0h,0B0h,098h,84h, 0A0h,0B0h,0CEh,84h, 0A0h,0B0h,04Fh,87h
; --------
Akumajou_a3:
	db	0,0,32
	db	060h,070h,0C4h,8Fh, 060h,070h,06Bh,90h, 060h,070h,048h,91h, 060h,070h,05Ah,93h
	db	060h,070h,085h,93h, 060h,070h,0C1h,93h, 080h,090h,035h,80h, 080h,090h,044h,80h
	db	080h,090h,0CAh,8Fh, 080h,090h,070h,90h, 080h,090h,0AEh,90h, 080h,090h,04Dh,91h
	db	080h,090h,05Fh,93h, 080h,090h,073h,93h, 080h,090h,08Bh,93h, 080h,090h,09Dh,93h
	db	080h,090h,0AFh,93h, 080h,090h,0C7h,93h, 080h,090h,0EEh,9Eh, 080h,090h,009h,9Fh
	db	0A0h,0B0h,03Ah,80h, 0A0h,0B0h,04Ah,80h, 0A0h,0B0h,0D0h,8Fh, 0A0h,0B0h,076h,90h
	db	0A0h,0B0h,0B8h,90h, 0A0h,0B0h,053h,91h, 0A0h,0B0h,065h,93h, 0A0h,0B0h,079h,93h
	db	0A0h,0B0h,091h,93h, 0A0h,0B0h,0A3h,93h, 0A0h,0B0h,0B5h,93h, 0A0h,0B0h,0CDh,93h
Akumajou_a3_P1:
	db	1,0,7
	db	080h,090h,074h,81h, 080h,090h,096h,81h, 080h,090h,071h,87h, 0A0h,0B0h,072h,84h
	db	0A0h,0B0h,0B0h,84h, 0A0h,0B0h,0E6h,84h, 0A0h,0B0h,067h,87h
; --------
Aleste:
	db	0,0,8
	db	032h,0CDh,000h,8Bh, 000h,0C9h,001h,8Bh, 070h,074h,002h,8Bh, 032h,0CDh,06Eh,98h
	db	000h,0C9h,06Fh,98h, 070h,074h,070h,98h, 02Ah,02Ah,01Eh,9Bh, 05Eh,05Eh,01Fh,9Bh
Aleste_P1:
	db	1,0,16
	db	0FBh,0FBh,0C5h,94h, 078h,078h,0C6h,94h, 0C1h,0C1h,0C7h,94h, 0C9h,0C9h,0C8h,94h
	db	0FFh,0F5h,0C9h,94h, 0FFh,0CBh,0CAh,94h, 0FFh,027h,0CBh,94h, 0FFh,032h,0CCh,94h
	db	0FFh,000h,0CDh,94h, 0FFh,090h,0CEh,94h, 0FFh,03Ch,0CFh,94h, 0FFh,032h,0D0h,94h
	db	0FFh,000h,0D1h,94h, 0FFh,0B0h,0D2h,94h, 0FFh,0F1h,0D3h,94h, 0FFh,0C9h,0D4h,94h
; --------
Anty_P1:
	db	1,0,8
	db	03Eh,03Eh,030h,8Ah, 007h,007h,031h,8Ah, 00Eh,00Eh,032h,8Ah, 0F8h,0B8h,033h,8Ah
	db	0D3h,0D3h,034h,8Ah, 0A0h,0A0h,035h,8Ah, 008h,008h,036h,8Ah, 079h,079h,037h,8Ah
; --------
AthleticBall:
	db	0,0,18
	db	0F0h,0B0h,056h,8Eh, 0F8h,0B8h,072h,8Eh, 0F0h,0B0h,080h,8Eh, 0FCh,0BCh,08Eh,8Eh
	db	0F8h,0B8h,09Ch,8Eh, 0FEh,0BEh,0A5h,92h, 0FEh,0BEh,088h,9Bh, 0F0h,0B0h,096h,9Bh
	db	0FEh,0BEh,0A4h,9Bh, 0FEh,0BEh,0B2h,9Bh, 0FEh,0BEh,0C0h,9Bh, 0FEh,0BEh,0CEh,9Bh
	db	0FEh,0BEh,0DCh,9Bh, 0FEh,0BEh,0EAh,9Bh, 0FEh,0BEh,0F8h,9Bh, 0FEh,0BEh,006h,9Ch
	db	0FEh,0BEh,014h,9Ch, 0FEh,0BEh,022h,9Ch
; --------
BubbleBobble:
	db	0,0,129
	db	032h,021h,062h,80h, 032h,077h,065h,80h, 09Bh,023h,066h,80h, 0FDh,077h,067h,80h
	db	032h,023h,068h,80h, 09Ch,077h,069h,80h, 0FDh,03Eh,06Ah,80h, 03Eh,004h,06Bh,80h
	db	004h,032h,06Ch,80h, 032h,000h,06Dh,80h, 0F8h,070h,06Eh,80h, 06Fh,032h,06Fh,80h
	db	032h,088h,070h,80h, 088h,0E4h,071h,80h, 0E4h,021h,072h,80h, 021h,000h,073h,80h
	db	000h,0C0h,074h,80h, 0C0h,001h,075h,80h, 001h,07Eh,076h,80h, 07Eh,033h,077h,80h
	db	033h,0CDh,078h,80h, 0CDh,025h,079h,80h, 025h,068h,07Ah,80h, 068h,03Eh,07Bh,80h
	db	03Eh,00Dh,07Ch,80h, 00Dh,032h,07Dh,80h, 032h,000h,07Eh,80h, 0F8h,0B0h,07Fh,80h
	db	07Fh,032h,080h,80h, 032h,08Ah,081h,80h, 08Ah,0E4h,082h,80h, 0E4h,0CDh,083h,80h
	db	0CDh,02Eh,084h,80h, 02Eh,0AFh,085h,80h, 0AFh,03Eh,086h,80h, 03Eh,009h,087h,80h
	db	009h,032h,088h,80h, 032h,000h,089h,80h, 0F8h,090h,08Ah,80h, 077h,032h,08Bh,80h
	db	032h,089h,08Ch,80h, 089h,0E4h,08Dh,80h, 0E4h,021h,08Eh,80h, 021h,000h,08Fh,80h
	db	000h,001h,091h,80h, 001h,0FFh,092h,80h, 0FFh,0AFh,094h,80h, 03Eh,0CDh,095h,80h
	db	000h,06Bh,096h,80h, 0CDh,001h,097h,80h, 06Bh,03Eh,098h,80h, 03Eh,032h,09Ah,80h
	db	001h,000h,09Bh,80h, 032h,0B0h,09Ch,80h, 0F8h,032h,09Dh,80h, 07Fh,08Ah,09Eh,80h
	db	032h,0E4h,09Fh,80h, 08Ah,0CDh,0A0h,80h, 0E4h,0DEh,0A1h,80h, 03Eh,0BFh,0A2h,80h
	db	002h,000h,0A3h,80h, 0F8h,000h,0A5h,80h, 077h,090h,0A6h,80h, 0F8h,000h,0F5h,80h
	db	07Fh,0B0h,0F6h,80h, 0F8h,000h,009h,81h, 077h,090h,00Ah,81h, 0F8h,000h,011h,81h
	db	07Fh,0B0h,012h,81h, 0F8h,000h,05Eh,81h, 07Fh,0B0h,05Fh,81h, 0F8h,000h,06Ah,81h
	db	07Fh,0B0h,06Bh,81h, 0F8h,000h,073h,81h, 07Fh,0B0h,074h,81h, 0F8h,000h,07Eh,81h
	db	07Fh,0B0h,07Fh,81h, 0F8h,000h,0A7h,81h, 07Fh,0B0h,0A8h,81h, 0F8h,000h,0AFh,81h
	db	077h,090h,0B0h,81h, 0F8h,000h,0CFh,81h, 07Fh,0B0h,0D0h,81h, 0F8h,000h,0D7h,81h
	db	077h,090h,0D8h,81h, 0F8h,000h,0EBh,82h, 077h,090h,0ECh,82h, 0F8h,000h,0F0h,82h
	db	07Fh,0B0h,0F1h,82h, 0F8h,000h,0FDh,82h, 077h,090h,0FEh,82h, 0F8h,000h,003h,83h
	db	07Fh,0B0h,004h,83h, 0F8h,000h,05Dh,84h, 07Fh,0B0h,05Eh,84h, 0F8h,000h,068h,84h
	db	07Fh,0B0h,069h,84h, 0F8h,000h,02Ah,91h, 077h,090h,02Bh,91h, 0F8h,000h,0AAh,91h
	db	06Fh,070h,0ABh,91h, 0F8h,000h,0B2h,91h, 077h,090h,0B3h,91h, 0F8h,000h,0BAh,91h
	db	07Fh,0B0h,0BBh,91h, 0F8h,000h,0C5h,91h, 06Fh,070h,0C6h,91h, 0F8h,000h,0CDh,91h
	db	077h,090h,0CEh,91h, 0F8h,000h,0D5h,91h, 07Fh,0B0h,0D6h,91h, 0F8h,000h,009h,92h
	db	07Fh,0B0h,00Ah,92h, 0F8h,000h,014h,92h, 07Fh,0B0h,015h,92h, 0F8h,000h,0B6h,93h
	db	07Fh,0B0h,0B7h,93h, 0F8h,000h,0C1h,93h, 07Fh,0B0h,0C2h,93h, 0F8h,000h,012h,95h
	db	07Fh,0B0h,013h,95h, 0F8h,000h,01Dh,95h, 07Fh,0B0h,01Eh,95h, 0F8h,000h,02Bh,95h
	db	07Fh,0B0h,02Ch,95h, 0F8h,000h,036h,95h, 07Fh,0B0h,037h,95h, 0F8h,000h,0CFh,95h
	db	077h,090h,0D0h,95h
BubbleBobble_P1:
	db	1,0,36
	db	080h,080h,0D9h,9Fh, 0DDh,0DDh,0DAh,9Fh, 077h,077h,0DBh,9Fh, 00Ch,00Ch,0DCh,9Fh
	db	0C9h,0C9h,0DDh,9Fh, 0FFh,03Eh,0DEh,9Fh, 0FFh,005h,0DFh,9Fh, 0FFh,0CDh,0E0h,9Fh
	db	0FFh,041h,0E1h,9Fh, 0FFh,001h,0E2h,9Fh, 0FFh,0E6h,0E3h,9Fh, 0FFh,002h,0E4h,9Fh
	db	0FFh,0C0h,0E5h,9Fh, 0FFh,03Ah,0E6h,9Fh, 0FFh,02Dh,0E7h,9Fh, 0FFh,000h,0E8h,9Fh
	db	0FFh,0FEh,0E9h,9Fh, 0FFh,003h,0EAh,9Fh, 0FFh,028h,0EBh,9Fh, 0FFh,008h,0ECh,9Fh
	db	0FFh,0DBh,0EDh,9Fh, 0FFh,041h,0EEh,9Fh, 0FFh,0E6h,0EFh,9Fh, 0FFh,0FEh,0F0h,9Fh
	db	0FFh,0D3h,0F1h,9Fh, 0FFh,041h,0F2h,9Fh, 0FFh,018h,0F3h,9Fh, 0FFh,005h,0F4h,9Fh
	db	0FFh,03Eh,0F5h,9Fh, 0FFh,082h,0F6h,9Fh, 0FFh,0CDh,0F7h,9Fh, 0FFh,080h,0F8h,9Fh
	db	0FFh,001h,0F9h,9Fh, 0FFh,03Eh,0FAh,9Fh, 0FFh,002h,0FBh,9Fh, 0FFh,0C9h,0FCh,9Fh
BubbleBobble_P4:
	db	4,0,74
	db	0F8h,000h,044h,81h, 077h,090h,045h,81h, 0F8h,000h,04Ch,81h, 07Fh,0B0h,04Dh,81h
	db	0F8h,000h,06Dh,81h, 077h,090h,06Eh,81h, 0F8h,000h,075h,81h, 07Fh,0B0h,076h,81h
	db	0F8h,000h,094h,81h, 077h,090h,095h,81h, 032h,000h,09Eh,81h, 0F8h,090h,09Fh,81h
	db	0F8h,000h,028h,82h, 077h,090h,029h,82h, 0F8h,000h,030h,82h, 07Fh,0B0h,031h,82h
	db	0F8h,000h,024h,83h, 077h,090h,025h,83h, 0F8h,000h,02Ch,83h, 07Fh,0B0h,02Dh,83h
	db	0F8h,000h,0BBh,83h, 077h,090h,0BCh,83h, 0F8h,000h,0C3h,83h, 07Fh,0B0h,0C4h,83h
	db	0F8h,000h,0D7h,84h, 077h,090h,0D8h,84h, 0F8h,000h,0E2h,84h, 077h,090h,0E3h,84h
	db	0F8h,000h,024h,85h, 077h,090h,025h,85h, 0F8h,000h,06Ah,85h, 077h,090h,06Bh,85h
	db	0F8h,000h,06Eh,87h, 07Fh,0B0h,06Fh,87h, 0F8h,000h,079h,87h, 07Fh,0B0h,07Ah,87h
	db	0F8h,000h,05Eh,8Ah, 07Fh,0B0h,05Fh,8Ah, 0F8h,000h,079h,8Ah, 07Fh,0B0h,07Ah,8Ah
	db	0F8h,000h,0D4h,8Ah, 07Fh,0B0h,0D5h,8Ah, 0F8h,000h,057h,8Bh, 07Fh,0B0h,058h,8Bh
	db	0F8h,000h,064h,8Bh, 07Fh,0B0h,065h,8Bh, 0F8h,000h,06Eh,8Ch, 07Fh,0B0h,06Fh,8Ch
	db	0F8h,000h,079h,8Ch, 07Fh,0B0h,07Ah,8Ch, 0F8h,000h,0F8h,8Ch, 07Fh,0B0h,0F9h,8Ch
	db	0F8h,000h,003h,8Dh, 07Fh,0B0h,004h,8Dh, 0F8h,000h,056h,8Eh, 07Fh,0B0h,057h,8Eh
	db	0F8h,000h,072h,8Eh, 07Fh,0B0h,073h,8Eh, 0F8h,000h,01Bh,90h, 07Fh,0B0h,01Ch,90h
	db	0F8h,000h,026h,90h, 07Fh,0B0h,027h,90h, 0F8h,000h,098h,9Ah, 07Fh,0B0h,099h,9Ah
	db	0F8h,000h,0A3h,9Ah, 07Fh,0B0h,0A4h,9Ah, 0F8h,000h,0C2h,9Ah, 07Fh,0B0h,0C3h,9Ah
	db	0F8h,000h,0CDh,9Ah, 07Fh,0B0h,0CEh,9Ah, 0F8h,000h,0E5h,9Ch, 077h,090h,0E6h,9Ch
	db	0F8h,000h,0FFh,9Ch, 077h,090h,000h,9Dh
BubbleBobble_P5:
	db	5,0,14
	db	0F8h,000h,052h,97h, 077h,090h,053h,97h, 0F8h,000h,059h,98h, 077h,090h,05Ah,98h
	db	0F8h,000h,068h,98h, 077h,090h,069h,98h, 0F8h,000h,0C7h,98h, 077h,090h,0C8h,98h
	db	0F8h,000h,0CDh,9Eh, 077h,090h,0CEh,9Eh, 0F8h,000h,0D6h,9Eh, 077h,090h,0D7h,9Eh
	db	0F8h,000h,00Ch,9Fh, 077h,090h,00Dh,9Fh
BubbleBobble_P9:
	db	9,0,170
	db	0F8h,000h,0FEh,86h, 07Fh,0B0h,0FFh,86h, 0F8h,000h,009h,87h, 07Fh,0B0h,00Ah,87h
	db	0F8h,000h,029h,87h, 07Fh,0B0h,02Ah,87h, 0F8h,000h,040h,87h, 07Fh,0B0h,041h,87h
	db	0F8h,000h,08Bh,87h, 07Fh,0B0h,08Ch,87h, 0F8h,000h,096h,87h, 07Fh,0B0h,097h,87h
	db	0F8h,000h,0C8h,87h, 07Fh,0B0h,0C9h,87h, 0F8h,000h,0D3h,87h, 07Fh,0B0h,0D4h,87h
	db	0F8h,000h,00Fh,88h, 07Fh,0B0h,010h,88h, 0F8h,000h,01Ah,88h, 07Fh,0B0h,01Bh,88h
	db	0F8h,000h,0DCh,88h, 07Fh,0B0h,0DDh,88h, 0F8h,000h,0EAh,88h, 07Fh,0B0h,0EBh,88h
	db	0F8h,000h,00Ch,89h, 07Fh,0B0h,00Dh,89h, 0F8h,000h,017h,89h, 07Fh,0B0h,018h,89h
	db	0F8h,000h,064h,89h, 07Fh,0B0h,065h,89h, 0F8h,000h,06Fh,89h, 07Fh,0B0h,070h,89h
	db	0F8h,000h,0DBh,95h, 07Fh,0B0h,0DCh,95h, 0F8h,000h,0E6h,95h, 07Fh,0B0h,0E7h,95h
	db	0F8h,000h,060h,96h, 07Fh,0B0h,061h,96h, 0F8h,000h,06Bh,96h, 07Fh,0B0h,06Ch,96h
	db	0F8h,000h,074h,96h, 07Fh,0B0h,075h,96h, 0F8h,000h,080h,96h, 07Fh,0B0h,081h,96h
	db	0F8h,000h,037h,97h, 07Fh,0B0h,038h,97h, 0F8h,000h,042h,97h, 07Fh,0B0h,043h,97h
	db	0F8h,000h,085h,98h, 07Fh,0B0h,086h,98h, 0F8h,000h,093h,98h, 07Fh,0B0h,094h,98h
	db	0F8h,000h,09Ch,98h, 07Fh,0B0h,09Dh,98h, 0F8h,000h,0AAh,98h, 07Fh,0B0h,0ABh,98h
	db	0F8h,000h,0B3h,98h, 07Fh,0B0h,0B4h,98h, 0F8h,000h,0BEh,98h, 07Fh,0B0h,0BFh,98h
	db	0F8h,000h,0C7h,98h, 07Fh,0B0h,0C8h,98h, 0F8h,000h,0D2h,98h, 07Fh,0B0h,0D3h,98h
	db	0F8h,000h,0DBh,98h, 07Fh,0B0h,0DCh,98h, 0F8h,000h,0E6h,98h, 07Fh,0B0h,0E7h,98h
	db	0F8h,000h,0EFh,98h, 07Fh,0B0h,0F0h,98h, 0F8h,000h,0FAh,98h, 07Fh,0B0h,0FBh,98h
	db	0F8h,000h,015h,99h, 07Fh,0B0h,016h,99h, 0F8h,000h,020h,99h, 07Fh,0B0h,021h,99h
	db	0F8h,000h,065h,99h, 07Fh,0B0h,066h,99h, 0F8h,000h,070h,99h, 07Fh,0B0h,071h,99h
	db	0F8h,000h,079h,99h, 07Fh,0B0h,07Ah,99h, 0F8h,000h,084h,99h, 07Fh,0B0h,085h,99h
	db	0F8h,000h,090h,99h, 07Fh,0B0h,091h,99h, 0F8h,000h,099h,99h, 07Fh,0B0h,09Ah,99h
	db	0F8h,000h,0A4h,99h, 07Fh,0B0h,0A5h,99h, 0F8h,000h,0D1h,99h, 07Fh,0B0h,0D2h,99h
	db	0F8h,000h,0DDh,99h, 07Fh,0B0h,0DEh,99h, 0F8h,000h,0E9h,99h, 07Fh,0B0h,0EAh,99h
	db	0F8h,000h,078h,9Bh, 07Fh,0B0h,079h,9Bh, 0F8h,000h,083h,9Bh, 07Fh,0B0h,084h,9Bh
	db	0F8h,000h,0B5h,9Bh, 07Fh,0B0h,0B6h,9Bh, 0F8h,000h,0C0h,9Bh, 07Fh,0B0h,0C1h,9Bh
	db	0F8h,000h,017h,9Ch, 07Fh,0B0h,018h,9Ch, 0F8h,000h,026h,9Ch, 07Fh,0B0h,027h,9Ch
	db	0F8h,000h,050h,9Ch, 07Fh,0B0h,051h,9Ch, 0F8h,000h,05Ch,9Ch, 07Fh,0B0h,05Dh,9Ch
	db	0F8h,000h,075h,9Ch, 07Fh,0B0h,076h,9Ch, 0F8h,000h,080h,9Ch, 07Fh,0B0h,081h,9Ch
	db	0F8h,000h,014h,9Dh, 07Fh,0B0h,015h,9Dh, 0F8h,000h,01Fh,9Dh, 07Fh,0B0h,020h,9Dh
	db	0F8h,000h,097h,9Dh, 07Fh,0B0h,098h,9Dh, 0F8h,000h,0A2h,9Dh, 07Fh,0B0h,0A3h,9Dh
	db	0F8h,000h,0C2h,9Dh, 07Fh,0B0h,0C3h,9Dh, 0F8h,000h,0CDh,9Dh, 07Fh,0B0h,0CEh,9Dh
	db	0F8h,000h,0DFh,9Dh, 07Fh,0B0h,0E0h,9Dh, 0F8h,000h,0EAh,9Dh, 07Fh,0B0h,0EBh,9Dh
	db	0F8h,000h,009h,9Eh, 07Fh,0B0h,00Ah,9Eh, 0F8h,000h,014h,9Eh, 07Fh,0B0h,015h,9Eh
	db	0F8h,000h,033h,9Eh, 07Fh,0B0h,034h,9Eh, 0F8h,000h,03Eh,9Eh, 07Fh,0B0h,03Fh,9Eh
	db	0F8h,000h,09Ah,9Eh, 07Fh,0B0h,09Bh,9Eh, 0F8h,000h,0BEh,9Eh, 07Fh,0B0h,0BFh,9Eh
	db	0F8h,000h,0DDh,9Eh, 07Fh,0B0h,0DEh,9Eh, 0F8h,000h,0E8h,9Eh, 07Fh,0B0h,0E9h,9Eh
	db	0F8h,000h,006h,9Fh, 07Fh,0B0h,007h,9Fh, 0F8h,000h,011h,9Fh, 07Fh,0B0h,012h,9Fh
	db	0F8h,000h,01Ah,9Fh, 07Fh,0B0h,01Bh,9Fh, 0F8h,000h,025h,9Fh, 07Fh,0B0h,026h,9Fh
	db	0F8h,000h,09Dh,9Fh, 07Fh,0B0h,09Eh,9Fh, 0F8h,000h,0A8h,9Fh, 07Fh,0B0h,0A9h,9Fh
	db	0F8h,000h,0B1h,9Fh, 07Fh,0B0h,0B2h,9Fh, 0F8h,000h,0C1h,9Fh, 07Fh,0B0h,0C2h,9Fh
	db	0F8h,000h,0CAh,9Fh, 07Fh,0B0h,0CBh,9Fh, 0F8h,000h,0D8h,9Fh, 07Fh,0B0h,0D9h,9Fh
	db	0F8h,000h,0EDh,9Fh, 07Fh,0B0h,0EEh,9Fh
; --------
CarRace:
	db	0,0,33
	db	0FAh,0BAh,066h,8Eh, 0DEh,09Eh,005h,8Fh, 0FAh,0BAh,014h,8Fh, 016h,000h,046h,92h
	db	053h,058h,047h,92h, 0FFh,0E5h,000h,98h, 0FFh,03Ah,001h,98h, 0FFh,069h,002h,98h
	db	0FFh,0E0h,003h,98h, 0FFh,0FEh,004h,98h, 0FFh,041h,005h,98h, 0FFh,02Ah,006h,98h
	db	0FFh,063h,007h,98h, 0FFh,0E0h,008h,98h, 0FFh,028h,009h,98h, 0FFh,00Ah,00Ah,98h
	db	0FFh,0FEh,00Bh,98h, 0FFh,042h,00Ch,98h, 0FFh,02Ah,00Dh,98h, 0FFh,065h,00Eh,98h
	db	0FFh,0E0h,00Fh,98h, 0FFh,028h,010h,98h, 0FFh,003h,011h,98h, 0FFh,02Ah,012h,98h
	db	0FFh,067h,013h,98h, 0FFh,0E0h,014h,98h, 0FFh,07Eh,015h,98h, 0FFh,0E6h,016h,98h
	db	0FFh,03Fh,017h,98h, 0FFh,0F6h,018h,98h, 0FFh,080h,019h,98h, 0FFh,0E1h,01Ah,98h
	db	0FFh,0C9h,01Bh,98h
; --------
CrazyBuggy:
	db	0,13,8
	db	0C5h,076h,07Eh,81h, 076h,03Eh,07Fh,81h, 0AFh,001h,080h,81h, 0C1h,000h,086h,81h
	db	020h,0B7h,03Bh,83h, 005h,020h,03Ch,83h, 03Eh,004h,03Dh,83h, 001h,03Ch,03Eh,83h
CrazyBuggy_P1:
	db	1,13,12
	db	093h,0F4h,08Dh,93h, 000h,07Fh,08Eh,93h, 000h,0FEh,0F4h,9Fh, 000h,007h,0F5h,9Fh
	db	000h,0C2h,0F6h,9Fh, 000h,093h,0F7h,9Fh, 000h,0CBh,0F9h,9Fh, 000h,0FBh,0FAh,9Fh
	db	000h,0CBh,0FBh,9Fh, 000h,0B3h,0FCh,9Fh, 000h,0C3h,0FDh,9Fh, 000h,093h,0FEh,9Fh
; --------
Famboxing:
	db	0,0,15
	db	040h,050h,050h,80h, 060h,070h,057h,80h, 060h,070h,0D7h,80h, 060h,070h,0E5h,80h
	db	060h,070h,029h,81h, 060h,070h,040h,81h, 080h,090h,05Eh,80h, 080h,090h,0DEh,80h
	db	080h,090h,0ECh,80h, 080h,090h,02Dh,81h, 080h,090h,046h,81h, 0A0h,0B0h,065h,80h
	db	0A0h,0B0h,0F3h,80h, 0A0h,0B0h,031h,81h, 0A0h,0B0h,04Ch,81h
; --------
Firebird:
	db	0,0,21
	db	060h,070h,05Dh,80h, 060h,070h,088h,80h, 060h,070h,0F2h,93h, 060h,070h,00Fh,94h
	db	080h,090h,062h,80h, 080h,090h,072h,80h, 080h,090h,082h,80h, 080h,090h,0D7h,81h
	db	080h,090h,0EDh,81h, 080h,090h,0F7h,93h, 080h,090h,015h,94h, 080h,090h,043h,94h
	db	0A0h,0B0h,067h,80h, 0A0h,0B0h,06Dh,80h, 0A0h,0B0h,07Ch,80h, 0A0h,0B0h,020h,81h
	db	0A0h,0B0h,0DFh,81h, 0A0h,0B0h,0F6h,81h, 0A0h,0B0h,0FFh,93h, 0A0h,0B0h,01Bh,94h
	db	0A0h,0B0h,03Ah,94h
; --------
Galaxian:
	db	0,0,8
	db	031h,031h,010h,80h, 000h,0FDh,011h,80h, 000h,0FFh,012h,80h, 0F3h,0F3h,013h,80h
	db	031h,031h,044h,80h, 000h,0FDh,045h,80h, 000h,0FFh,046h,80h, 0CDh,0CDh,047h,80h
; --------
Game80zemina:
	db	0,0,32
	db	040h,050h,036h,90h, 040h,050h,07Fh,94h, 040h,050h,0A1h,94h, 040h,050h,04Eh,9Fh
	db	001h,000h,039h,90h, 040h,070h,03Ah,90h, 001h,000h,082h,94h, 040h,070h,083h,94h
	db	001h,000h,051h,9Fh, 040h,070h,052h,9Fh, 002h,000h,066h,91h, 040h,090h,067h,91h
	db	002h,000h,086h,94h, 040h,090h,087h,94h, 002h,000h,093h,94h, 040h,090h,094h,94h
	db	002h,000h,0A4h,94h, 040h,090h,0A5h,94h, 003h,000h,06Bh,91h, 040h,0B0h,06Ch,91h
	db	003h,000h,0C0h,92h, 040h,0B0h,0C1h,92h, 003h,000h,0DAh,92h, 040h,0B0h,0DBh,92h
	db	003h,000h,0E4h,92h, 040h,0B0h,0E5h,92h, 003h,000h,0EEh,92h, 040h,0B0h,0EFh,92h
	db	003h,000h,08Ah,94h, 040h,0B0h,08Bh,94h, 003h,000h,097h,94h, 040h,0B0h,098h,94h
; --------
Garakuta:
	db	0,8,59
	db	010h,012h,002h,80h, 000h,032h,00Ah,80h, 000h,090h,00Ch,80h, 000h,03Ch,00Dh,80h
	db	000h,032h,00Eh,80h, 0F3h,0B0h,010h,80h, 021h,0C9h,011h,80h, 0DAh,03Eh,012h,80h
	db	0FEh,001h,013h,80h, 03Eh,032h,014h,80h, 0F7h,000h,015h,80h, 077h,070h,016h,80h
	db	023h,021h,017h,80h, 0CDh,0DAh,018h,80h, 03Fh,0FEh,019h,80h, 040h,036h,01Ah,80h
	db	077h,0F7h,01Bh,80h, 011h,071h,01Dh,80h, 025h,023h,01Eh,80h, 040h,036h,01Fh,80h
	db	073h,025h,020h,80h, 072h,036h,022h,80h, 0FBh,040h,023h,80h, 002h,004h,085h,80h
	db	032h,0CDh,086h,80h, 000h,00Ah,087h,80h, 070h,040h,088h,80h, 001h,002h,08Fh,80h
	db	032h,0CDh,090h,80h, 000h,00Ah,091h,80h, 070h,040h,092h,80h, 001h,002h,084h,81h
	db	032h,0CDh,085h,81h, 000h,00Ah,086h,81h, 070h,040h,087h,81h, 002h,004h,008h,82h
	db	032h,0CDh,009h,82h, 000h,00Ah,00Ah,82h, 070h,040h,00Bh,82h, 001h,002h,075h,82h
	db	032h,0CDh,076h,82h, 000h,00Ah,077h,82h, 070h,040h,078h,82h, 002h,004h,093h,82h
	db	032h,0CDh,094h,82h, 000h,00Ah,095h,82h, 070h,040h,096h,82h, 002h,004h,0B0h,82h
	db	032h,0CDh,0B1h,82h, 000h,00Ah,0B2h,82h, 070h,040h,0B3h,82h, 002h,004h,0CDh,82h
	db	032h,0CDh,0CEh,82h, 000h,00Ah,0CFh,82h, 070h,040h,0D0h,82h, 002h,004h,0EAh,82h
	db	032h,0CDh,0EBh,82h, 000h,00Ah,0ECh,82h, 070h,040h,0EDh,82h
Garakuta_P1:
	db	1,8,4
	db	003h,006h,047h,87h, 032h,0CDh,048h,87h, 000h,00Ah,049h,87h, 070h,040h,04Ah,87h
; --------
Goemon:
	db	0,0,28
	db	060h,070h,051h,80h, 060h,070h,063h,80h, 060h,070h,00Fh,82h, 060h,070h,028h,82h
	db	060h,070h,040h,82h, 060h,070h,058h,82h, 060h,070h,070h,82h, 060h,070h,0EDh,8Fh
	db	080h,090h,055h,80h, 080h,090h,069h,80h, 080h,090h,014h,82h, 080h,090h,02Eh,82h
	db	080h,090h,046h,82h, 080h,090h,05Eh,82h, 080h,090h,076h,82h, 080h,090h,0F7h,8Fh
	db	080h,090h,01Fh,94h, 0A0h,0B0h,059h,80h, 0A0h,0B0h,06Fh,80h, 0A0h,0B0h,01Ah,82h
	db	0A0h,0B0h,034h,82h, 0A0h,0B0h,04Ch,82h, 0A0h,0B0h,064h,82h, 0A0h,0B0h,07Ch,82h
	db	0A0h,0B0h,001h,90h, 0A0h,0B0h,029h,94h, 0A0h,0B0h,076h,99h, 0A0h,0B0h,0D2h,9Eh
Goemon_P1:
	db	1,0,15
	db	080h,090h,047h,9Eh, 0A0h,0B0h,099h,82h, 0A0h,0B0h,041h,85h, 0A0h,0B0h,03Fh,86h
	db	0A0h,0B0h,077h,86h, 0A0h,0B0h,098h,86h, 0A0h,0B0h,060h,87h, 0A0h,0B0h,04Bh,88h
	db	0A0h,0B0h,0D6h,89h, 0A0h,0B0h,0DDh,8Ch, 0A0h,0B0h,085h,99h, 0A0h,0B0h,0A2h,99h
	db	0A0h,0B0h,08Ah,9Ah, 0A0h,0B0h,0A3h,9Dh, 0A0h,0B0h,0EEh,9Dh

Goemon_P2:
	db	2,0,6
	db	0A0h,0B0h,0F5h,81h, 0A0h,0B0h,0F7h,87h, 0A0h,0B0h,0C5h,90h, 0A0h,0B0h,01Fh,96h
	db	0A0h,0B0h,057h,97h, 0A0h,0B0h,001h,9Ah
; --------
GoonR:
	db	0,7,21
	db	030h,038h,0C0h,84h, 010h,0A0h,021h,91h, 010h,0A0h,0C3h,91h, 011h,0A1h,0CBh,91h
	db	010h,0A0h,0EAh,91h, 011h,0A1h,0F8h,91h, 010h,0A0h,074h,95h, 010h,0A0h,08Fh,95h
	db	011h,0A1h,0C0h,95h, 010h,0A0h,0EEh,96h, 011h,0A1h,022h,97h, 010h,0A0h,07Bh,97h
	db	011h,0A1h,080h,97h, 010h,0A0h,084h,97h, 011h,0A1h,089h,97h, 010h,0A0h,08Dh,97h
	db	011h,0A1h,092h,97h, 010h,0A0h,096h,97h, 011h,0A1h,09Bh,97h, 010h,0A0h,09Fh,97h
	db	011h,0A1h,0A4h,97h
GoonR_P3:
	db	3,7,1
	db	021h,0C9h,01Dh,9Fh
GoonR_P4:
	db	4,7,2
	db	038h,030h,055h,82h, 038h,030h,059h,8Bh
; --------
Graduis:
	db	0,1,72
	db	060h,070h,07Dh,80h, 060h,070h,0CEh,80h, 060h,070h,036h,82h, 060h,070h,0A2h,82h
	db	060h,070h,030h,88h, 060h,070h,073h,88h, 060h,070h,076h,8Ah, 060h,070h,0A2h,8Ah
	db	060h,070h,0E0h,93h, 060h,070h,02Fh,94h, 080h,090h,030h,80h, 080h,090h,03Eh,80h
	db	080h,090h,082h,80h, 080h,090h,0D3h,80h, 080h,090h,03Bh,82h, 080h,090h,0A7h,82h
	db	080h,090h,056h,85h, 080h,090h,06Dh,85h, 080h,090h,012h,86h, 080h,090h,029h,86h
	db	080h,090h,035h,88h, 080h,090h,078h,88h, 080h,090h,0F3h,89h, 080h,090h,00Dh,8Ah
	db	080h,090h,07Bh,8Ah, 080h,090h,0A7h,8Ah, 080h,090h,019h,93h, 080h,090h,030h,93h
	db	080h,090h,0E5h,93h, 080h,090h,034h,94h, 080h,090h,036h,9Bh, 080h,090h,067h,9Bh
	db	080h,090h,07Fh,9Bh, 080h,090h,0BBh,9Bh, 080h,090h,000h,9Ch, 080h,090h,074h,9Ch
	db	080h,090h,0E3h,9Ch, 080h,090h,00Eh,9Dh, 080h,090h,086h,9Dh, 080h,090h,0B4h,9Dh
	db	0A0h,0B0h,034h,80h, 0A0h,0B0h,044h,80h, 0A0h,0B0h,088h,80h, 0A0h,0B0h,0D9h,80h
	db	0A0h,0B0h,041h,82h, 0A0h,0B0h,0ADh,82h, 0A0h,0B0h,060h,85h, 0A0h,0B0h,077h,85h
	db	0A0h,0B0h,01Ch,86h, 0A0h,0B0h,033h,86h, 0A0h,0B0h,03Bh,88h, 0A0h,0B0h,07Eh,88h
	db	0A0h,0B0h,0FDh,89h, 0A0h,0B0h,017h,8Ah, 0A0h,0B0h,081h,8Ah, 0A0h,0B0h,0ADh,8Ah
	db	0A0h,0B0h,00Ch,8Bh, 0A0h,0B0h,031h,8Bh, 0A0h,0B0h,023h,93h, 0A0h,0B0h,03Ah,93h
	db	0A0h,0B0h,0EBh,93h, 0A0h,0B0h,03Ah,94h, 0A0h,0B0h,040h,9Bh, 0A0h,0B0h,071h,9Bh
	db	0A0h,0B0h,089h,9Bh, 0A0h,0B0h,0C5h,9Bh, 0A0h,0B0h,00Ah,9Ch, 0A0h,0B0h,07Eh,9Ch
	db	0A0h,0B0h,0EDh,9Ch, 0A0h,0B0h,018h,9Dh, 0A0h,0B0h,090h,9Dh, 0A0h,0B0h,0BEh,9Dh
Graduis_P1:
	db	1,1,2
	db	0A0h,0B0h,0AEh,09Fh, 0A0h,0B0h,0C4h,09Fh
Graduis_P2:
	db	2,1,4
	db	0A0h,0B0h,09Dh,82h, 0A0h,0B0h,0AAh,82h, 0A0h,0B0h,0D9h,82h, 0A0h,0B0h,002h,83h
; --------
Graduis_a:
	db	0,2,72
	db	060h,070h,07Dh,80h, 060h,070h,0CEh,80h, 060h,070h,036h,82h, 060h,070h,0A2h,82h
	db	060h,070h,030h,88h, 060h,070h,073h,88h, 060h,070h,076h,8Ah, 060h,070h,0A2h,8Ah
	db	060h,070h,0C6h,93h, 060h,070h,015h,94h, 080h,090h,030h,80h, 080h,090h,03Eh,80h
	db	080h,090h,082h,80h, 080h,090h,0D3h,80h, 080h,090h,03Bh,82h, 080h,090h,0A7h,82h
	db	080h,090h,056h,85h, 080h,090h,06Dh,85h, 080h,090h,012h,86h, 080h,090h,029h,86h
	db	080h,090h,035h,88h, 080h,090h,078h,88h, 080h,090h,0F3h,89h, 080h,090h,00Dh,8Ah
	db	080h,090h,07Bh,8Ah, 080h,090h,0A7h,8Ah, 080h,090h,0FFh,92h, 080h,090h,016h,93h
	db	080h,090h,0CBh,93h, 080h,090h,01Ah,94h, 080h,090h,01Ch,9Bh, 080h,090h,04Dh,9Bh
	db	080h,090h,065h,9Bh, 080h,090h,0A1h,9Bh, 080h,090h,0E6h,9Bh, 080h,090h,05Ah,9Ch
	db	080h,090h,0C9h,9Ch, 080h,090h,0F4h,9Ch, 080h,090h,06Ch,9Dh, 080h,090h,09Ah,9Dh
	db	0A0h,0B0h,034h,80h, 0A0h,0B0h,044h,80h, 0A0h,0B0h,088h,80h, 0A0h,0B0h,0D9h,80h
	db	0A0h,0B0h,041h,82h, 0A0h,0B0h,0ADh,82h, 0A0h,0B0h,060h,85h, 0A0h,0B0h,077h,85h
	db	0A0h,0B0h,01Ch,86h, 0A0h,0B0h,033h,86h, 0A0h,0B0h,03Bh,88h, 0A0h,0B0h,07Eh,88h
	db	0A0h,0B0h,0FDh,89h, 0A0h,0B0h,017h,8Ah, 0A0h,0B0h,081h,8Ah, 0A0h,0B0h,0ADh,8Ah
	db	0A0h,0B0h,00Ch,8Bh, 0A0h,0B0h,031h,8Bh, 0A0h,0B0h,009h,93h, 0A0h,0B0h,020h,93h
	db	0A0h,0B0h,0D1h,93h, 0A0h,0B0h,020h,94h, 0A0h,0B0h,026h,9Bh, 0A0h,0B0h,057h,9Bh
	db	0A0h,0B0h,06Fh,9Bh, 0A0h,0B0h,0ABh,9Bh, 0A0h,0B0h,0F0h,9Bh, 0A0h,0B0h,064h,9Ch
	db	0A0h,0B0h,0D3h,9Ch, 0A0h,0B0h,0FEh,9Ch, 0A0h,0B0h,076h,9Dh, 0A0h,0B0h,0A4h,9Dh
Graduis_a_P1:
	db	1,2,2
	db	0A0h,0B0h,094h,9Fh, 0A0h,0B0h,0AAh,9Fh
Graduis_a_P2:
	db	2,2,4
	db	0A0h,0B0h,083h,82h, 0A0h,0B0h,090h,82h, 0A0h,0B0h,0BFh,82h, 0A0h,0B0h,0E8h,82h
; --------
KingKong2:
	db	0,3,14
	db	060h,070h,02Eh,82h, 060h,070h,03Bh,82h, 060h,070h,0CCh,82h, 060h,070h,09Dh,94h
	db	060h,070h,0B3h,94h, 080h,090h,032h,82h, 080h,090h,041h,82h, 080h,090h,0D2h,82h
	db	080h,090h,0A7h,94h, 080h,090h,0B7h,94h, 080h,090h,063h,9Fh, 0A0h,0B0h,022h,82h
	db	0A0h,0B0h,0D7h,82h, 0A0h,0B0h,0A2h,9Dh
KingKong2_P1:
	db	1,3,6
	db	080h,090h,004h,80h, 0A0h,0B0h,00Bh,80h, 0A0h,0B0h,07Ch,81h, 0A0h,0B0h,08Ch,81h
	db	0A0h,0B0h,0D7h,81h, 0A0h,0B0h,07Dh,82h
KingKong2_P2:
	db	2,3,1
	db	0A0h,0B0h,07Bh,81h
KingKong2_P6:
	db	6,3,1
	db	080h,090h,0C2h,99h
; --------
Labyrinth:
	db	0,0,14
	db	068h,070h,0C9h,81h, 068h,070h,006h,85h, 068h,070h,017h,85h, 068h,070h,024h,85h
	db	070h,090h,0CEh,81h, 070h,090h,00Bh,85h, 070h,090h,01Ch,85h, 070h,090h,02Ah,85h
	db	078h,0B0h,010h,85h, 078h,0B0h,030h,85h, 060h,050h,0F8h,97h, 068h,070h,0F9h,97h
	db	070h,090h,0FAh,97h, 078h,0B0h,0FBh,97h
; --------
Manb2:
	db	0,0,8
	db	010h,0A0h,0DAh,87h, 011h,0A1h,0DDh,87h, 012h,0A2h,0EEh,87h, 011h,0A1h,025h,88h
	db	0BFh,0BFh,0C0h,91h, 034h,034h,0C5h,91h, 0DBh,0DBh,0EDh,87h, 00Eh,00Eh,024h,88h
; --------
;Manb2v2:	; Patch not available
;	db	0,0,8
;	db	010h,0A0h,0DAh,87h, 011h,0A1h,0DDh,87h, 012h,0A2h,0EEh,87h, 011h,0A1h,025h,88h
;	db	010h,0A0h,0C0h,91h, 011h,0A1h,0C5h,91h, 0DBh,0DBh,0EDh,87h, 00Eh,00Eh,024h,88h
; --------
MetalGear:
	db	0,0,13
	db	060h,070h,0B4h,81h, 060h,070h,0C1h,81h, 060h,070h,058h,82h, 060h,070h,091h,82h
	db	060h,070h,0A7h,82h, 080h,090h,0B8h,81h, 080h,090h,0C7h,81h, 080h,090h,05Eh,82h
	db	080h,090h,09Bh,82h, 080h,090h,0ABh,82h, 0A0h,0B0h,0A5h,81h, 0A0h,0B0h,064h,82h
	db	0A0h,0B0h,02Eh,89h
; --------
MonMon_Daewoo:
	db	0,4,17
	db	040h,050h,047h,80h, 060h,070h,04Bh,80h, 060h,070h,085h,88h, 060h,070h,09Dh,88h
	db	060h,070h,0B7h,88h, 060h,070h,0D1h,88h, 060h,070h,0EEh,88h, 060h,070h,0C8h,8Ah
	db	060h,070h,018h,8Bh, 060h,070h,02Ah,8Bh, 060h,070h,04Dh,8Bh, 060h,070h,0E9h,8Bh
	db	060h,070h,0F0h,8Ch, 060h,070h,002h,8Dh, 060h,070h,093h,94h, 080h,090h,04Fh,80h
	db	0A0h,0B0h,053h,80h
MonMon_Daewoo_P2:
	db	2,4,2
	db	060h,070h,0D7h,9Dh, 060h,070h,0EBh,9Dh
MonMon_Daewoo_P3:
	db	3,4,7
	db	040h,050h,00Ch,9Fh, 060h,070h,0F4h,96h, 060h,070h,024h,97h, 060h,070h,02Dh,97h
	db	060h,070h,083h,9Ah, 060h,070h,044h,9Eh, 060h,070h,012h,9Fh
; --------
MSXDOS22:
	db	0,9,15
	db	032h,007h,092h,80h, 000h,032h,093h,80h, 060h,000h,094h,80h, 0C9h,050h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,070h,099h,80h, 000h,0C9h,09Ah,80h
	db	04Dh,04Dh,01Eh,81h, 053h,053h,01Fh,81h, 058h,058h,020h,81h, 02Dh,02Dh,021h,81h
	db	044h,044h,022h,81h, 04Fh,04Fh,023h,81h, 053h,053h,024h,81h
MSXDOS22_P1:
	db	1,9,8
	db	032h,007h,092h,80h, 000h,032h,093h,80h, 060h,000h,094h,80h, 0C9h,050h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,070h,099h,80h, 000h,0C9h,09Ah,80h
MSXDOS22_P2:
	db	2,9,8
	db	032h,007h,092h,80h, 000h,032h,093h,80h, 060h,000h,094h,80h, 0C9h,050h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,070h,099h,80h, 000h,0C9h,09Ah,80h
MSXDOS22_P3:
	db	3,9,8
	db	032h,007h,092h,80h, 000h,032h,093h,80h, 060h,000h,094h,80h, 0C9h,050h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,070h,099h,80h, 000h,0C9h,09Ah,80h
; --------
MSXDOS22NL:
	db	0,10,15
	db	032h,007h,092h,80h, 0FEh,032h,093h,80h, 07Fh,000h,094h,80h, 0C9h,050h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,070h,099h,80h, 000h,0C9h,09Ah,80h
	db	04Dh,04Dh,01Eh,81h, 053h,053h,01Fh,81h, 058h,058h,020h,81h, 02Dh,02Dh,021h,81h
	db	044h,044h,022h,81h, 04Fh,04Fh,023h,81h, 053h,053h,024h,81h
MSXDOS22NL_P1:
	db	1,10,8
	db	032h,007h,092h,80h, 0FEh,032h,093h,80h, 07Fh,000h,094h,80h, 0C9h,050h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,070h,099h,80h, 000h,0C9h,09Ah,80h
MSXDOS22NL_P2:
	db	2,10,8
	db	032h,007h,092h,80h, 0FEh,032h,093h,80h, 07Fh,000h,094h,80h, 0C9h,050h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,070h,099h,80h, 000h,0C9h,09Ah,80h
; --------
Nausicaa:
	db	0,0,10
	db	0FFh,0BFh,013h,85h, 0E9h,0A9h,023h,85h, 0E0h,0A0h,0D7h,8Bh, 0E8h,0A8h,077h,8Dh
	db	0E8h,0A8h,06Eh,8Eh, 0FFh,0BFh,013h,95h, 0E9h,0A9h,023h,95h, 0E0h,0A0h,0D7h,9Bh
	db	0E8h,0A8h,077h,9Dh, 0E8h,0A8h,06Eh,9Eh
Nausicaa_P1:
	db	1,0,8
	db	0E1h,0A1h,071h,8Ah, 0E0h,0A0h,098h,8Ah, 0F8h,0B8h,0F8h,8Fh, 0E8h,0A8h,013h,91h
	db	0E8h,0A8h,033h,92h, 0E0h,0A0h,09Fh,94h, 03Eh,03Eh,0A0h,94h, 007h,007h,0A1h,94h
; --------
PengAdv_Part1:
	db	0,0,180
	db	060h,070h,076h,80h, 060h,070h,0E5h,80h, 060h,070h,0D6h,81h, 060h,070h,013h,82h
	db	060h,070h,0D2h,85h, 060h,070h,009h,86h, 060h,070h,0EBh,87h, 060h,070h,00Ah,88h
	db	060h,070h,024h,88h, 060h,070h,08Ah,88h, 060h,070h,0A8h,88h, 060h,070h,0CCh,88h
	db	060h,070h,0EBh,88h, 060h,070h,005h,89h, 060h,070h,084h,89h, 060h,070h,09Eh,89h
	db	060h,070h,0DBh,89h, 060h,070h,005h,8Ah, 060h,070h,078h,8Ah, 060h,070h,0C8h,8Bh
	db	060h,070h,027h,8Ch, 060h,070h,041h,8Ch, 060h,070h,0C0h,8Ch, 060h,070h,0E0h,8Ch
	db	060h,070h,06Eh,8Dh, 060h,070h,08Eh,8Dh, 060h,070h,009h,8Eh, 060h,070h,029h,8Eh
	db	060h,070h,0A5h,8Eh, 060h,070h,0CEh,8Eh, 060h,070h,04Ah,8Fh, 060h,070h,06Ah,8Fh
	db	060h,070h,09Bh,8Fh, 060h,070h,0BFh,8Fh, 060h,070h,01Ch,90h, 060h,070h,03Ch,90h
	db	060h,070h,099h,90h, 060h,070h,0B9h,90h, 060h,070h,016h,91h, 060h,070h,036h,91h
	db	060h,070h,055h,91h, 060h,070h,081h,91h, 060h,070h,0BCh,91h, 060h,070h,0D6h,91h
	db	060h,070h,001h,92h, 060h,070h,01Bh,92h, 060h,070h,045h,92h, 060h,070h,05Fh,92h
	db	060h,070h,089h,92h, 060h,070h,0A3h,92h, 060h,070h,0F9h,92h, 060h,070h,013h,93h
	db	060h,070h,069h,93h, 060h,070h,083h,93h, 060h,070h,0BEh,93h, 060h,070h,0D8h,93h
	db	060h,070h,013h,94h, 060h,070h,02Dh,94h, 060h,070h,087h,94h, 060h,070h,0A1h,94h
	db	060h,070h,0FBh,94h, 060h,070h,015h,95h, 060h,070h,050h,95h, 060h,070h,06Ah,95h
	db	060h,070h,0E6h,95h, 060h,070h,000h,96h, 060h,070h,04Eh,96h, 060h,070h,070h,96h
	db	060h,070h,08Fh,96h, 060h,070h,0A9h,96h, 060h,070h,0C8h,96h, 060h,070h,0E2h,96h
	db	060h,070h,006h,97h, 060h,070h,020h,97h, 060h,070h,03Fh,97h, 060h,070h,059h,97h
	db	060h,070h,078h,97h, 060h,070h,092h,97h, 060h,070h,0B1h,97h, 060h,070h,0CBh,97h
	db	060h,070h,0EAh,97h, 060h,070h,004h,98h, 060h,070h,0A0h,98h, 060h,070h,0BAh,98h
	db	060h,070h,0DFh,98h, 060h,070h,0F9h,98h, 060h,070h,01Bh,99h, 060h,070h,035h,99h
	db	060h,070h,063h,99h, 060h,070h,07Dh,99h, 060h,070h,099h,99h, 060h,070h,0B8h,99h
	db	060h,070h,0D2h,99h, 060h,070h,0F1h,99h, 060h,070h,00Bh,9Ah, 060h,070h,02Ah,9Ah
	db	060h,070h,044h,9Ah, 060h,070h,063h,9Ah, 060h,070h,07Dh,9Ah, 060h,070h,09Ch,9Ah
	db	060h,070h,0B6h,9Ah, 060h,070h,0D5h,9Ah, 060h,070h,0EFh,9Ah, 060h,070h,00Eh,9Bh
	db	060h,070h,028h,9Bh, 060h,070h,047h,9Bh, 060h,070h,061h,9Bh, 060h,070h,080h,9Bh
	db	060h,070h,09Dh,9Bh, 060h,070h,0BFh,9Bh, 060h,070h,0DDh,9Bh, 060h,070h,0FFh,9Bh
	db	060h,070h,02Eh,9Ch, 060h,070h,05Ah,9Ch, 080h,090h,02Bh,80h, 080h,090h,039h,80h
	db	080h,090h,07Bh,80h, 080h,090h,0EAh,80h, 080h,090h,05Eh,81h, 080h,090h,075h,81h
	db	080h,090h,0DBh,81h, 080h,090h,018h,82h, 080h,090h,0D7h,85h, 080h,090h,00Eh,86h
	db	080h,090h,039h,87h, 080h,090h,08Eh,87h, 080h,090h,0A2h,87h, 080h,090h,0CDh,87h
	db	080h,090h,0F0h,87h, 080h,090h,00Fh,88h, 080h,090h,029h,88h, 080h,090h,08Fh,88h
	db	080h,090h,0ADh,88h, 080h,090h,0D1h,88h, 080h,090h,0F0h,88h, 080h,090h,00Ah,89h
	db	080h,090h,089h,89h, 080h,090h,0A3h,89h, 080h,090h,0E0h,89h, 080h,090h,00Ah,8Ah
	db	080h,090h,07Dh,8Ah, 080h,090h,0CDh,8Bh, 080h,090h,02Ch,8Ch, 080h,090h,046h,8Ch
	db	080h,090h,0C5h,8Ch, 080h,090h,0E5h,8Ch, 080h,090h,073h,8Dh, 080h,090h,093h,8Dh
	db	080h,090h,00Eh,8Eh, 080h,090h,02Eh,8Eh, 080h,090h,0AAh,8Eh, 080h,090h,0D3h,8Eh
	db	080h,090h,04Fh,8Fh, 080h,090h,06Fh,8Fh, 080h,090h,0A0h,8Fh, 080h,090h,0C4h,8Fh
	db	080h,090h,021h,90h, 080h,090h,041h,90h, 080h,090h,09Eh,90h, 080h,090h,0BEh,90h
	db	080h,090h,01Bh,91h, 080h,090h,03Bh,91h, 080h,090h,05Ah,91h, 080h,090h,086h,91h
	db	080h,090h,0C1h,91h, 080h,090h,0DBh,91h, 080h,090h,006h,92h, 080h,090h,020h,92h
	db	080h,090h,04Ah,92h, 080h,090h,064h,92h, 080h,090h,08Eh,92h, 080h,090h,0A8h,92h
	db	080h,090h,0FEh,92h, 080h,090h,018h,93h, 080h,090h,06Eh,93h, 080h,090h,088h,93h
	db	080h,090h,0C3h,93h, 080h,090h,0DDh,93h, 080h,090h,018h,94h, 080h,090h,032h,94h
PengAdv_Part2:
	db	0,0,184
	db	080h,090h,08Ch,94h, 080h,090h,0A6h,94h, 080h,090h,000h,95h, 080h,090h,01Ah,95h
	db	080h,090h,055h,95h, 080h,090h,06Fh,95h, 080h,090h,0EBh,95h, 080h,090h,005h,96h
	db	080h,090h,053h,96h, 080h,090h,075h,96h, 080h,090h,094h,96h, 080h,090h,0AEh,96h
	db	080h,090h,0CDh,96h, 080h,090h,0E7h,96h, 080h,090h,00Bh,97h, 080h,090h,025h,97h
	db	080h,090h,044h,97h, 080h,090h,05Eh,97h, 080h,090h,07Dh,97h, 080h,090h,097h,97h
	db	080h,090h,0B6h,97h, 080h,090h,0D0h,97h, 080h,090h,0EFh,97h, 080h,090h,009h,98h
	db	080h,090h,0A5h,98h, 080h,090h,0BFh,98h, 080h,090h,0E4h,98h, 080h,090h,0FEh,98h
	db	080h,090h,020h,99h, 080h,090h,03Ah,99h, 080h,090h,068h,99h, 080h,090h,082h,99h
	db	080h,090h,09Eh,99h, 080h,090h,0BDh,99h, 080h,090h,0D7h,99h, 080h,090h,0F6h,99h
	db	080h,090h,010h,9Ah, 080h,090h,02Fh,9Ah, 080h,090h,049h,9Ah, 080h,090h,068h,9Ah
	db	080h,090h,082h,9Ah, 080h,090h,0A1h,9Ah, 080h,090h,0BBh,9Ah, 080h,090h,0DAh,9Ah
	db	080h,090h,0F4h,9Ah, 080h,090h,013h,9Bh, 080h,090h,02Dh,9Bh, 080h,090h,04Ch,9Bh
	db	080h,090h,066h,9Bh, 080h,090h,085h,9Bh, 080h,090h,0A2h,9Bh, 080h,090h,0C4h,9Bh
	db	080h,090h,0E2h,9Bh, 080h,090h,004h,9Ch, 080h,090h,033h,9Ch, 080h,090h,05Fh,9Ch
	db	080h,090h,070h,9Ch, 080h,090h,0ACh,9Eh, 080h,090h,0F1h,9Eh, 0A0h,0B0h,02Fh,80h
	db	0A0h,0B0h,03Fh,80h, 0A0h,0B0h,081h,80h, 0A0h,0B0h,0F0h,80h, 0A0h,0B0h,066h,81h
	db	0A0h,0B0h,07Eh,81h, 0A0h,0B0h,0E1h,81h, 0A0h,0B0h,01Eh,82h, 0A0h,0B0h,0DDh,85h
	db	0A0h,0B0h,014h,86h, 0A0h,0B0h,043h,87h, 0A0h,0B0h,098h,87h, 0A0h,0B0h,0ACh,87h
	db	0A0h,0B0h,0D7h,87h, 0A0h,0B0h,0F6h,87h, 0A0h,0B0h,015h,88h, 0A0h,0B0h,02Fh,88h
	db	0A0h,0B0h,095h,88h, 0A0h,0B0h,0B3h,88h, 0A0h,0B0h,0D7h,88h, 0A0h,0B0h,0F6h,88h
	db	0A0h,0B0h,010h,89h, 0A0h,0B0h,08Fh,89h, 0A0h,0B0h,0A9h,89h, 0A0h,0B0h,0E6h,89h
	db	0A0h,0B0h,010h,8Ah, 0A0h,0B0h,083h,8Ah, 0A0h,0B0h,0D3h,8Bh, 0A0h,0B0h,032h,8Ch
	db	0A0h,0B0h,04Ch,8Ch, 0A0h,0B0h,0CBh,8Ch, 0A0h,0B0h,0EBh,8Ch, 0A0h,0B0h,079h,8Dh
	db	0A0h,0B0h,099h,8Dh, 0A0h,0B0h,014h,8Eh, 0A0h,0B0h,034h,8Eh, 0A0h,0B0h,0B0h,8Eh
	db	0A0h,0B0h,0D9h,8Eh, 0A0h,0B0h,055h,8Fh, 0A0h,0B0h,075h,8Fh, 0A0h,0B0h,0A6h,8Fh
	db	0A0h,0B0h,0CAh,8Fh, 0A0h,0B0h,027h,90h, 0A0h,0B0h,047h,90h, 0A0h,0B0h,0A4h,90h
	db	0A0h,0B0h,0C4h,90h, 0A0h,0B0h,021h,91h, 0A0h,0B0h,041h,91h, 0A0h,0B0h,060h,91h
	db	0A0h,0B0h,08Ch,91h, 0A0h,0B0h,0C7h,91h, 0A0h,0B0h,0E1h,91h, 0A0h,0B0h,00Ch,92h
	db	0A0h,0B0h,026h,92h, 0A0h,0B0h,050h,92h, 0A0h,0B0h,06Ah,92h, 0A0h,0B0h,094h,92h
	db	0A0h,0B0h,0AEh,92h, 0A0h,0B0h,004h,93h, 0A0h,0B0h,01Eh,93h, 0A0h,0B0h,074h,93h
	db	0A0h,0B0h,08Eh,93h, 0A0h,0B0h,0C9h,93h, 0A0h,0B0h,0E3h,93h, 0A0h,0B0h,01Eh,94h
	db	0A0h,0B0h,038h,94h, 0A0h,0B0h,092h,94h, 0A0h,0B0h,0ACh,94h, 0A0h,0B0h,006h,95h
	db	0A0h,0B0h,020h,95h, 0A0h,0B0h,05Bh,95h, 0A0h,0B0h,075h,95h, 0A0h,0B0h,0F1h,95h
	db	0A0h,0B0h,00Bh,96h, 0A0h,0B0h,059h,96h, 0A0h,0B0h,07Bh,96h, 0A0h,0B0h,09Ah,96h
	db	0A0h,0B0h,0B4h,96h, 0A0h,0B0h,0D3h,96h, 0A0h,0B0h,0EDh,96h, 0A0h,0B0h,011h,97h
	db	0A0h,0B0h,02Bh,97h, 0A0h,0B0h,04Ah,97h, 0A0h,0B0h,064h,97h, 0A0h,0B0h,083h,97h
	db	0A0h,0B0h,09Dh,97h, 0A0h,0B0h,0BCh,97h, 0A0h,0B0h,0D6h,97h, 0A0h,0B0h,0F5h,97h
	db	0A0h,0B0h,00Fh,98h, 0A0h,0B0h,0ABh,98h, 0A0h,0B0h,0C5h,98h, 0A0h,0B0h,0EAh,98h
	db	0A0h,0B0h,004h,99h, 0A0h,0B0h,026h,99h, 0A0h,0B0h,040h,99h, 0A0h,0B0h,06Eh,99h
	db	0A0h,0B0h,088h,99h, 0A0h,0B0h,0A4h,99h, 0A0h,0B0h,0C3h,99h, 0A0h,0B0h,0DDh,99h
	db	0A0h,0B0h,0FCh,99h, 0A0h,0B0h,016h,9Ah, 0A0h,0B0h,035h,9Ah, 0A0h,0B0h,04Fh,9Ah
	db	0A0h,0B0h,06Eh,9Ah, 0A0h,0B0h,088h,9Ah, 0A0h,0B0h,0A7h,9Ah, 0A0h,0B0h,0C1h,9Ah
	db	0A0h,0B0h,0E0h,9Ah, 0A0h,0B0h,0FAh,9Ah, 0A0h,0B0h,019h,9Bh, 0A0h,0B0h,033h,9Bh
	db	0A0h,0B0h,052h,9Bh, 0A0h,0B0h,06Ch,9Bh, 0A0h,0B0h,08Bh,9Bh, 0A0h,0B0h,0A8h,9Bh
	db	0A0h,0B0h,039h,9Ch, 0A0h,0B0h,0CAh,9Bh, 0A0h,0B0h,0E8h,9Bh, 0A0h,0B0h,00Ah,9Ch
	db	0A0h,0B0h,065h,9Ch, 0A0h,0B0h,07Ah,9Ch, 0A0h,0B0h,0B6h,9Eh, 0A0h,0B0h,0FBh,9Eh
PengAdv_P1:
	db	1,0,148
	db	080h,090h,02Fh,80h, 080h,090h,04Ch,80h, 080h,090h,061h,80h, 080h,090h,07Bh,80h
	db	080h,090h,090h,80h, 080h,090h,0ADh,80h, 080h,090h,0C2h,80h, 080h,090h,0DCh,80h
	db	080h,090h,0F1h,80h, 080h,090h,00Eh,81h, 080h,090h,023h,81h, 080h,090h,03Dh,81h
	db	080h,090h,052h,81h, 080h,090h,06Ch,81h, 080h,090h,081h,81h, 080h,090h,09Bh,81h
	db	080h,090h,0B0h,81h, 080h,090h,0CDh,81h, 080h,090h,004h,82h, 080h,090h,03Ch,82h
	db	080h,090h,051h,82h, 080h,090h,094h,82h, 080h,090h,0F4h,82h, 080h,090h,013h,83h
	db	080h,090h,028h,83h, 080h,090h,060h,83h, 080h,090h,075h,83h, 080h,090h,0ACh,83h
	db	080h,090h,0C1h,83h, 080h,090h,0EDh,83h, 080h,090h,002h,84h, 080h,090h,02Fh,84h
	db	080h,090h,044h,84h, 080h,090h,066h,84h, 080h,090h,03Eh,85h, 080h,090h,097h,85h
	db	080h,090h,09Ch,86h, 080h,090h,0F3h,86h, 080h,090h,027h,87h, 080h,090h,041h,87h
	db	080h,090h,089h,87h, 080h,090h,0B2h,87h, 080h,090h,042h,88h, 080h,090h,0E7h,88h
	db	080h,090h,048h,89h, 080h,090h,05Ch,89h, 080h,090h,0AFh,89h, 080h,090h,0E9h,8Ah
	db	080h,090h,031h,8Bh, 080h,090h,045h,8Bh, 080h,090h,082h,8Bh, 080h,090h,09Ch,8Bh
	db	080h,090h,0C8h,8Bh, 080h,090h,03Fh,8Ch, 080h,090h,061h,8Fh, 080h,090h,076h,8Fh
	db	080h,090h,063h,9Bh, 080h,090h,08Ah,9Bh, 080h,090h,0C9h,9Bh, 080h,090h,0ECh,9Bh
	db	080h,090h,006h,9Ch, 080h,090h,043h,9Ch, 080h,090h,08Fh,9Ch, 080h,090h,0A4h,9Ch
	db	080h,090h,0FDh,9Ch, 080h,090h,079h,9Dh, 080h,090h,0A1h,9Dh, 080h,090h,0B8h,9Dh
	db	080h,090h,0DAh,9Dh, 080h,090h,0FCh,9Dh, 080h,090h,016h,9Eh, 080h,090h,032h,9Eh
	db	080h,090h,016h,9Fh, 080h,090h,04Dh,9Fh, 0A0h,0B0h,039h,80h, 0A0h,0B0h,056h,80h
	db	0A0h,0B0h,06Bh,80h, 0A0h,0B0h,085h,80h, 0A0h,0B0h,09Ah,80h, 0A0h,0B0h,0B7h,80h
	db	0A0h,0B0h,0CCh,80h, 0A0h,0B0h,0E6h,80h, 0A0h,0B0h,0FBh,80h, 0A0h,0B0h,018h,81h
	db	0A0h,0B0h,02Dh,81h, 0A0h,0B0h,047h,81h, 0A0h,0B0h,05Ch,81h, 0A0h,0B0h,076h,81h
	db	0A0h,0B0h,08Bh,81h, 0A0h,0B0h,0A5h,81h, 0A0h,0B0h,0BAh,81h, 0A0h,0B0h,0D7h,81h
	db	0A0h,0B0h,00Eh,82h, 0A0h,0B0h,046h,82h, 0A0h,0B0h,05Bh,82h, 0A0h,0B0h,09Eh,82h
	db	0A0h,0B0h,0FEh,82h, 0A0h,0B0h,01Dh,83h, 0A0h,0B0h,032h,83h, 0A0h,0B0h,06Ah,83h
	db	0A0h,0B0h,07Fh,83h, 0A0h,0B0h,0B6h,83h, 0A0h,0B0h,0CBh,83h, 0A0h,0B0h,0F7h,83h
	db	0A0h,0B0h,00Ch,84h, 0A0h,0B0h,039h,84h, 0A0h,0B0h,04Eh,84h, 0A0h,0B0h,070h,84h
	db	0A0h,0B0h,048h,85h, 0A0h,0B0h,0A1h,85h, 0A0h,0B0h,0A6h,86h, 0A0h,0B0h,0FDh,86h
	db	0A0h,0B0h,031h,87h, 0A0h,0B0h,04Bh,87h, 0A0h,0B0h,093h,87h, 0A0h,0B0h,0BCh,87h
	db	0A0h,0B0h,04Ch,88h, 0A0h,0B0h,0F1h,88h, 0A0h,0B0h,052h,89h, 0A0h,0B0h,066h,89h
	db	0A0h,0B0h,0B9h,89h, 0A0h,0B0h,0F3h,8Ah, 0A0h,0B0h,03Bh,8Bh, 0A0h,0B0h,04Fh,8Bh
	db	0A0h,0B0h,08Ch,8Bh, 0A0h,0B0h,0A6h,8Bh, 0A0h,0B0h,0D2h,8Bh, 0A0h,0B0h,049h,8Ch
	db	0A0h,0B0h,06Bh,8Fh, 0A0h,0B0h,080h,8Fh, 0A0h,0B0h,06Dh,9Bh, 0A0h,0B0h,094h,9Bh
	db	0A0h,0B0h,0D3h,9Bh, 0A0h,0B0h,0F6h,9Bh, 0A0h,0B0h,010h,9Ch, 0A0h,0B0h,04Dh,9Ch
	db	0A0h,0B0h,099h,9Ch, 0A0h,0B0h,0AEh,9Ch, 0A0h,0B0h,007h,9Dh, 0A0h,0B0h,083h,9Dh
	db	0A0h,0B0h,0ABh,9Dh, 0A0h,0B0h,0C2h,9Dh, 0A0h,0B0h,0E4h,9Dh, 0A0h,0B0h,006h,9Eh
	db	0A0h,0B0h,020h,9Eh, 0A0h,0B0h,03Ch,9Eh, 0A0h,0B0h,020h,9Fh, 0A0h,0B0h,057h,9Fh
; --------
Robocop:
	db	0,14,4
	db	060h,070h,03Dh,80h, 032h,032h,040h,80h, 0AFh,0AFh,041h,80h, 0FCh,0FCh,042h,80h
Robocop_P1:	
	db	1,14,5
	db	080h,090h,070h,99h, 0A0h,0B0h,075h,99h, 080h,090h,084h,99h, 0A0h,0B0h,089h,99h
	db	080h,090h,098h,99h
; --------
Shalom:
	db	0,5,19
	db	040h,050h,0F0h,81h, 060h,070h,05Bh,80h, 060h,070h,06Ch,80h, 060h,070h,009h,81h
	db	060h,070h,070h,81h, 060h,070h,092h,81h, 060h,070h,0F7h,81h, 080h,090h,05Fh,80h
	db	080h,090h,072h,80h, 080h,090h,00Fh,81h, 080h,090h,077h,81h, 080h,090h,096h,81h
	db	080h,090h,0FEh,81h, 0A0h,0B0h,063h,80h, 0A0h,0B0h,078h,80h, 0A0h,0B0h,015h,81h
	db	0A0h,0B0h,081h,81h, 0A0h,0B0h,09Ah,81h, 0A0h,0B0h,005h,82h
Shalom_P1:
	db	0,5,2
	db	080h,090h,085h,80h, 080h,090h,0ADh,80h
; --------
SupLodeRunner:
	db	0,6,54
	db	000h,08Dh,002h,80h, 000h,080h,003h,80h, 000h,0F3h,080h,80h, 000h,0F5h,081h,80h
	db	000h,087h,082h,80h, 000h,032h,083h,80h, 000h,090h,085h,80h, 000h,03Ch,086h,80h
	db	000h,032h,087h,80h, 000h,0B0h,089h,80h, 000h,0F1h,08Ah,80h, 000h,0FBh,08Bh,80h
	db	000h,0C9h,08Ch,80h, 000h,0EDh,08Dh,80h, 000h,073h,08Eh,80h, 000h,0C0h,090h,80h
	db	000h,021h,091h,80h, 000h,00Ah,092h,80h, 000h,039h,094h,80h, 000h,0F9h,095h,80h
	db	000h,0E3h,096h,80h, 000h,07Ch,097h,80h, 000h,0FEh,098h,80h, 000h,0FCh,099h,80h
	db	000h,020h,09Ah,80h, 000h,015h,09Bh,80h, 000h,02Bh,09Ch,80h, 000h,02Bh,09Dh,80h
	db	000h,07Dh,09Eh,80h, 000h,023h,09Fh,80h, 000h,0E6h,0A0h,80h, 000h,003h,0A1h,80h
	db	000h,0FEh,0A2h,80h, 000h,001h,0A3h,80h, 000h,0CBh,0A4h,80h, 000h,07Eh,0A5h,80h
	db	000h,028h,0A6h,80h, 000h,002h,0A7h,80h, 000h,0CBh,0A8h,80h, 000h,0BEh,0A9h,80h
	db	000h,023h,0AAh,80h, 000h,0E3h,0ABh,80h, 000h,0EDh,0ACh,80h, 000h,07Bh,0ADh,80h
	db	000h,0C0h,0AFh,80h, 000h,0C9h,0B0h,80h, 000h,0CDh,0B1h,80h, 000h,0ABh,0B2h,80h
	db	000h,080h,0B3h,80h, 000h,0C3h,0B4h,80h, 000h,081h,0B6h,80h, 032h,0CDh,01Ch,81h
	db	000h,080h,01Dh,81h, 000h,080h,01Eh,81h
SupLodeRunner_P1:
	db	1,6,21
	db	032h,0CDh,09Ah,83h, 000h,080h,09Bh,83h, 000h,080h,09Ch,83h, 032h,0CDh,0A4h,83h
	db	000h,080h,0A5h,83h, 000h,080h,0A6h,83h, 032h,0CDh,0AFh,83h, 000h,080h,0B0h,83h
	db	000h,080h,0B1h,83h, 032h,0CDh,0B9h,83h, 000h,080h,0BAh,83h, 000h,080h,0BBh,83h
	db	032h,0CDh,0CCh,83h, 000h,080h,0CDh,83h, 000h,080h,0CEh,83h, 032h,0CDh,0D6h,83h
	db	000h,080h,0D7h,83h, 000h,080h,0D8h,83h, 032h,0CDh,0E2h,83h, 000h,080h,0E3h,83h
	db	000h,080h,0E4h,83h
SupLodeRunner_P2:
	db	2,6,54
	db	000h,08Dh,002h,80h, 000h,080h,003h,80h, 000h,0F3h,080h,80h, 000h,0F5h,081h,80h
	db	000h,087h,082h,80h, 000h,032h,083h,80h, 000h,090h,085h,80h, 000h,03Ch,086h,80h
	db	000h,032h,087h,80h, 000h,0B0h,089h,80h, 000h,0F1h,08Ah,80h, 000h,0FBh,08Bh,80h
	db	000h,0C9h,08Ch,80h, 000h,0EDh,08Dh,80h, 000h,073h,08Eh,80h, 000h,0C0h,090h,80h
	db	000h,021h,091h,80h, 000h,00Ah,092h,80h, 000h,039h,094h,80h, 000h,0F9h,095h,80h
	db	000h,0E3h,096h,80h, 000h,07Ch,097h,80h, 000h,0FEh,098h,80h, 000h,0FCh,099h,80h
	db	000h,020h,09Ah,80h, 000h,015h,09Bh,80h, 000h,02Bh,09Ch,80h, 000h,02Bh,09Dh,80h
	db	000h,07Dh,09Eh,80h, 000h,023h,09Fh,80h, 000h,0E6h,0A0h,80h, 000h,003h,0A1h,80h
	db	000h,0FEh,0A2h,80h, 000h,001h,0A3h,80h, 000h,0CBh,0A4h,80h, 000h,07Eh,0A5h,80h
	db	000h,028h,0A6h,80h, 000h,002h,0A7h,80h, 000h,0CBh,0A8h,80h, 000h,0BEh,0A9h,80h
	db	000h,023h,0AAh,80h, 000h,0E3h,0ABh,80h, 000h,0EDh,0ACh,80h, 000h,07Bh,0ADh,80h
	db	000h,0C0h,0AFh,80h, 000h,0C9h,0B0h,80h, 000h,0CDh,0B1h,80h, 000h,0ABh,0B2h,80h
	db	000h,080h,0B3h,80h, 000h,0C3h,0B4h,80h, 000h,081h,0B6h,80h, 032h,0CDh,02Eh,81h
	db	000h,080h,02Fh,81h, 000h,080h,030h,81h
SupLodeRunner_P3:
	db	3,6,3
	db	032h,0CDh,0E5h,96h, 000h,080h,0E6h,96h, 000h,080h,0E7h,96h
SupLodeRunner_P4:
	db	4,6,51
	db	000h,08Dh,002h,80h, 000h,080h,003h,80h, 000h,0F3h,080h,80h, 000h,0F5h,081h,80h
	db	000h,087h,082h,80h, 000h,032h,083h,80h, 000h,090h,085h,80h, 000h,03Ch,086h,80h
	db	000h,032h,087h,80h, 000h,0B0h,089h,80h, 000h,0F1h,08Ah,80h, 000h,0FBh,08Bh,80h
	db	000h,0C9h,08Ch,80h, 000h,0EDh,08Dh,80h, 000h,073h,08Eh,80h, 000h,0C0h,090h,80h
	db	000h,021h,091h,80h, 000h,00Ah,092h,80h, 000h,039h,094h,80h, 000h,0F9h,095h,80h
	db	000h,0E3h,096h,80h, 000h,07Ch,097h,80h, 000h,0FEh,098h,80h, 000h,0FCh,099h,80h
	db	000h,020h,09Ah,80h, 000h,015h,09Bh,80h, 000h,02Bh,09Ch,80h, 000h,02Bh,09Dh,80h
	db	000h,07Dh,09Eh,80h, 000h,023h,09Fh,80h, 000h,0E6h,0A0h,80h, 000h,003h,0A1h,80h
	db	000h,0FEh,0A2h,80h, 000h,001h,0A3h,80h, 000h,0CBh,0A4h,80h, 000h,07Eh,0A5h,80h
	db	000h,028h,0A6h,80h, 000h,002h,0A7h,80h, 000h,0CBh,0A8h,80h, 000h,0BEh,0A9h,80h
	db	000h,023h,0AAh,80h, 000h,0E3h,0ABh,80h, 000h,0EDh,0ACh,80h, 000h,07Bh,0ADh,80h
	db	000h,0C0h,0AFh,80h, 000h,0C9h,0B0h,80h, 000h,0CDh,0B1h,80h, 000h,0ABh,0B2h,80h
	db	000h,080h,0B3h,80h, 000h,0C3h,0B4h,80h, 000h,081h,0B6h,80h
SupLodeRunner_P5:
	db	5,6,21
	db	032h,0CDh,09Ah,83h, 000h,080h,09Bh,83h, 000h,080h,09Ch,83h, 032h,0CDh,0A4h,83h
	db	000h,080h,0A5h,83h, 000h,080h,0A6h,83h, 032h,0CDh,0AFh,83h, 000h,080h,0B0h,83h
	db	000h,080h,0B1h,83h, 032h,0CDh,0B9h,83h, 000h,080h,0BAh,83h, 000h,080h,0BBh,83h
	db	032h,0CDh,0CCh,83h, 000h,080h,0CDh,83h, 000h,080h,0CEh,83h, 032h,0CDh,0D6h,83h
	db	000h,080h,0D7h,83h, 000h,080h,0D8h,83h, 032h,0CDh,0E2h,83h, 000h,080h,0E3h,83h
	db	000h,080h,0E4h,83h
SupLodeRunner_P6:
	db	6,6,54
	db	000h,08Dh,002h,80h, 000h,080h,003h,80h, 000h,0F3h,080h,80h, 000h,0F5h,081h,80h
	db	000h,087h,082h,80h, 000h,032h,083h,80h, 000h,090h,085h,80h, 000h,03Ch,086h,80h
	db	000h,032h,087h,80h, 000h,0B0h,089h,80h, 000h,0F1h,08Ah,80h, 000h,0FBh,08Bh,80h
	db	000h,0C9h,08Ch,80h, 000h,0EDh,08Dh,80h, 000h,073h,08Eh,80h, 000h,0C0h,090h,80h
	db	000h,021h,091h,80h, 000h,00Ah,092h,80h, 000h,039h,094h,80h, 000h,0F9h,095h,80h
	db	000h,0E3h,096h,80h, 000h,07Ch,097h,80h, 000h,0FEh,098h,80h, 000h,0FCh,099h,80h
	db	000h,020h,09Ah,80h, 000h,015h,09Bh,80h, 000h,02Bh,09Ch,80h, 000h,02Bh,09Dh,80h
	db	000h,07Dh,09Eh,80h, 000h,023h,09Fh,80h, 000h,0E6h,0A0h,80h, 000h,003h,0A1h,80h
	db	000h,0FEh,0A2h,80h, 000h,001h,0A3h,80h, 000h,0CBh,0A4h,80h, 000h,07Eh,0A5h,80h
	db	000h,028h,0A6h,80h, 000h,002h,0A7h,80h, 000h,0CBh,0A8h,80h, 000h,0BEh,0A9h,80h
	db	000h,023h,0AAh,80h, 000h,0E3h,0ABh,80h, 000h,0EDh,0ACh,80h, 000h,07Bh,0ADh,80h
	db	000h,0C0h,0AFh,80h, 000h,0C9h,0B0h,80h, 000h,0CDh,0B1h,80h, 000h,0ABh,0B2h,80h
	db	000h,080h,0B3h,80h, 000h,0C3h,0B4h,80h, 000h,081h,0B6h,80h, 032h,0CDh,02Eh,81h
	db	000h,080h,02Fh,81h, 000h,080h,030h,81h
SupLodeRunner_P8:
	db	8,6,54
	db	000h,08Dh,002h,80h, 000h,080h,003h,80h, 000h,0F3h,080h,80h, 000h,0F5h,081h,80h
	db	000h,087h,082h,80h, 000h,032h,083h,80h, 000h,090h,085h,80h, 000h,03Ch,086h,80h
	db	000h,032h,087h,80h, 000h,0B0h,089h,80h, 000h,0F1h,08Ah,80h, 000h,0FBh,08Bh,80h
	db	000h,0C9h,08Ch,80h, 000h,0EDh,08Dh,80h, 000h,073h,08Eh,80h, 000h,0C0h,090h,80h
	db	000h,021h,091h,80h, 000h,00Ah,092h,80h, 000h,039h,094h,80h, 000h,0F9h,095h,80h
	db	000h,0E3h,096h,80h, 000h,07Ch,097h,80h, 000h,0FEh,098h,80h, 000h,0FCh,099h,80h
	db	000h,020h,09Ah,80h, 000h,015h,09Bh,80h, 000h,02Bh,09Ch,80h, 000h,02Bh,09Dh,80h
	db	000h,07Dh,09Eh,80h, 000h,023h,09Fh,80h, 000h,0E6h,0A0h,80h, 000h,003h,0A1h,80h
	db	000h,0FEh,0A2h,80h, 000h,001h,0A3h,80h, 000h,0CBh,0A4h,80h, 000h,07Eh,0A5h,80h
	db	000h,028h,0A6h,80h, 000h,002h,0A7h,80h, 000h,0CBh,0A8h,80h, 000h,0BEh,0A9h,80h
	db	000h,023h,0AAh,80h, 000h,0E3h,0ABh,80h, 000h,0EDh,0ACh,80h, 000h,07Bh,0ADh,80h
	db	000h,0C0h,0AFh,80h, 000h,0C9h,0B0h,80h, 000h,0CDh,0B1h,80h, 000h,0ABh,0B2h,80h
	db	000h,080h,0B3h,80h, 000h,0C3h,0B4h,80h, 000h,081h,0B6h,80h, 032h,0CDh,02Eh,81h
	db	000h,080h,02Fh,81h, 000h,080h,030h,81h
SupLodeRunner_PA:
	db	10,6,54
	db	000h,08Dh,002h,80h, 000h,080h,003h,80h, 000h,0F3h,080h,80h, 000h,0F5h,081h,80h
	db	000h,087h,082h,80h, 000h,032h,083h,80h, 000h,090h,085h,80h, 000h,03Ch,086h,80h
	db	000h,032h,087h,80h, 000h,0B0h,089h,80h, 000h,0F1h,08Ah,80h, 000h,0FBh,08Bh,80h
	db	000h,0C9h,08Ch,80h, 000h,0EDh,08Dh,80h, 000h,073h,08Eh,80h, 000h,0C0h,090h,80h
	db	000h,021h,091h,80h, 000h,00Ah,092h,80h, 000h,039h,094h,80h, 000h,0F9h,095h,80h
	db	000h,0E3h,096h,80h, 000h,07Ch,097h,80h, 000h,0FEh,098h,80h, 000h,0FCh,099h,80h
	db	000h,020h,09Ah,80h, 000h,015h,09Bh,80h, 000h,02Bh,09Ch,80h, 000h,02Bh,09Dh,80h
	db	000h,07Dh,09Eh,80h, 000h,023h,09Fh,80h, 000h,0E6h,0A0h,80h, 000h,003h,0A1h,80h
	db	000h,0FEh,0A2h,80h, 000h,001h,0A3h,80h, 000h,0CBh,0A4h,80h, 000h,07Eh,0A5h,80h
	db	000h,028h,0A6h,80h, 000h,002h,0A7h,80h, 000h,0CBh,0A8h,80h, 000h,0BEh,0A9h,80h
	db	000h,023h,0AAh,80h, 000h,0E3h,0ABh,80h, 000h,0EDh,0ACh,80h, 000h,07Bh,0ADh,80h
	db	000h,0C0h,0AFh,80h, 000h,0C9h,0B0h,80h, 000h,0CDh,0B1h,80h, 000h,0ABh,0B2h,80h
	db	000h,080h,0B3h,80h, 000h,0C3h,0B4h,80h, 000h,081h,0B6h,80h, 032h,0CDh,02Eh,81h
	db	000h,080h,02Fh,81h, 000h,080h,030h,81h
SupLodeRunner_PC:
	db	12,6,54
	db	000h,08Dh,002h,80h, 000h,080h,003h,80h, 000h,0F3h,080h,80h, 000h,0F5h,081h,80h
	db	000h,087h,082h,80h, 000h,032h,083h,80h, 000h,090h,085h,80h, 000h,03Ch,086h,80h
	db	000h,032h,087h,80h, 000h,0B0h,089h,80h, 000h,0F1h,08Ah,80h, 000h,0FBh,08Bh,80h
	db	000h,0C9h,08Ch,80h, 000h,0EDh,08Dh,80h, 000h,073h,08Eh,80h, 000h,0C0h,090h,80h
	db	000h,021h,091h,80h, 000h,00Ah,092h,80h, 000h,039h,094h,80h, 000h,0F9h,095h,80h
	db	000h,0E3h,096h,80h, 000h,07Ch,097h,80h, 000h,0FEh,098h,80h, 000h,0FCh,099h,80h
	db	000h,020h,09Ah,80h, 000h,015h,09Bh,80h, 000h,02Bh,09Ch,80h, 000h,02Bh,09Dh,80h
	db	000h,07Dh,09Eh,80h, 000h,023h,09Fh,80h, 000h,0E6h,0A0h,80h, 000h,003h,0A1h,80h
	db	000h,0FEh,0A2h,80h, 000h,001h,0A3h,80h, 000h,0CBh,0A4h,80h, 000h,07Eh,0A5h,80h
	db	000h,028h,0A6h,80h, 000h,002h,0A7h,80h, 000h,0CBh,0A8h,80h, 000h,0BEh,0A9h,80h
	db	000h,023h,0AAh,80h, 000h,0E3h,0ABh,80h, 000h,0EDh,0ACh,80h, 000h,07Bh,0ADh,80h
	db	000h,0C0h,0AFh,80h, 000h,0C9h,0B0h,80h, 000h,0CDh,0B1h,80h, 000h,0ABh,0B2h,80h
	db	000h,080h,0B3h,80h, 000h,0C3h,0B4h,80h, 000h,081h,0B6h,80h, 032h,0CDh,012h,81h
	db	000h,080h,013h,81h, 000h,080h,014h,81h
SupLodeRunner_PD:
	db	13,6,3
	db	032h,0CDh,0E5h,96h, 000h,080h,0E6h,96h, 000h,080h,0E7h,96h
SupLodeRunner_PE:
	db	14,6,51
	db	000h,08Dh,002h,80h, 000h,080h,003h,80h, 000h,0F3h,080h,80h, 000h,0F5h,081h,80h
	db	000h,087h,082h,80h, 000h,032h,083h,80h, 000h,090h,085h,80h, 000h,03Ch,086h,80h
	db	000h,032h,087h,80h, 000h,0B0h,089h,80h, 000h,0F1h,08Ah,80h, 000h,0FBh,08Bh,80h
	db	000h,0C9h,08Ch,80h, 000h,0EDh,08Dh,80h, 000h,073h,08Eh,80h, 000h,0C0h,090h,80h
	db	000h,021h,091h,80h, 000h,00Ah,092h,80h, 000h,039h,094h,80h, 000h,0F9h,095h,80h
	db	000h,0E3h,096h,80h, 000h,07Ch,097h,80h, 000h,0FEh,098h,80h, 000h,0FCh,099h,80h
	db	000h,020h,09Ah,80h, 000h,015h,09Bh,80h, 000h,02Bh,09Ch,80h, 000h,02Bh,09Dh,80h
	db	000h,07Dh,09Eh,80h, 000h,023h,09Fh,80h, 000h,0E6h,0A0h,80h, 000h,003h,0A1h,80h
	db	000h,0FEh,0A2h,80h, 000h,001h,0A3h,80h, 000h,0CBh,0A4h,80h, 000h,07Eh,0A5h,80h
	db	000h,028h,0A6h,80h, 000h,002h,0A7h,80h, 000h,0CBh,0A8h,80h, 000h,0BEh,0A9h,80h
	db	000h,023h,0AAh,80h, 000h,0E3h,0ABh,80h, 000h,0EDh,0ACh,80h, 000h,07Bh,0ADh,80h
	db	000h,0C0h,0AFh,80h, 000h,0C9h,0B0h,80h, 000h,0CDh,0B1h,80h, 000h,0ABh,0B2h,80h
	db	000h,080h,0B3h,80h, 000h,0C3h,0B4h,80h, 000h,081h,0B6h,80h
SupLodeRunner_PF:
	db	15,6,21
	db	032h,0CDh,078h,96h, 000h,080h,079h,96h, 000h,080h,07Ah,96h, 032h,0CDh,085h,96h
	db	000h,080h,086h,96h, 000h,080h,087h,96h, 032h,0CDh,098h,96h, 000h,080h,099h,96h
	db	000h,080h,09Ah,96h, 032h,0CDh,0A2h,96h, 000h,080h,0A3h,96h, 000h,080h,0A4h,96h
	db	032h,0CDh,0FFh,96h, 000h,080h,000h,97h, 000h,080h,001h,97h, 032h,0CDh,09Eh,97h
	db	000h,080h,09Fh,97h, 000h,080h,0A0h,97h, 032h,0CDh,0A7h,97h, 000h,080h,0A8h,97h
	db	000h,080h,0A9h,97h
; --------
SuperRunner:
	db	0,0,8
	db	068h,070h,059h,80h, 070h,090h,05Dh,80h, 078h,0B0h,061h,80h, 078h,0B0h,0C1h,81h
	db	070h,090h,0CAh,81h, 078h,0B0h,0CEh,81h, 078h,0B0h,0D2h,81h, 070h,090h,0D7h,81h
SuperRunner_PE:
	db	14,0,8
	db	068h,070h,03Fh,98h, 070h,090h,043h,98h, 078h,0B0h,047h,98h, 078h,0B0h,0A7h,99h
	db	070h,090h,0B0h,99h, 078h,0B0h,0B4h,99h, 078h,0B0h,0B8h,99h, 070h,090h,0BDh,99h
; --------
Tatica:
	db	0,11,6
	db	0E5h,0E5h,09Ah,97h, 070h,000h,09Bh,97h, 0BEh,0BEh,09Ch,97h, 0C8h,0C8h,09Dh,97h
	db	0CDh,0CDh,09Eh,97h, 006h,006h,09Fh,97h
Tatica_P1:
	db	1,11,4
	db	077h,000h,0E2h,91h, 077h,000h,0CDh,9Ah, 0EDh,000h,0BEh,9Eh, 0B0h,000h,0BFh,9Eh
; --------
TheFairyLand:
	db	0,0,29
	db	060h,050h,0A6h,85h, 068h,070h,0AAh,85h, 068h,070h,003h,88h, 068h,070h,00Ch,88h
	db	068h,070h,029h,88h, 068h,070h,031h,88h, 070h,090h,0AEh,85h, 078h,0B0h,0EFh,83h
	db	078h,0B0h,0F7h,83h, 078h,0B0h,0B2h,85h, 078h,0B0h,063h,86h, 078h,0B0h,06Bh,86h
	db	070h,090h,0ACh,93h, 070h,090h,0CCh,93h, 070h,090h,0C3h,97h, 070h,090h,05Ah,98h
	db	070h,090h,057h,9Ch, 070h,090h,00Dh,9Dh, 070h,090h,07Dh,9Dh, 070h,090h,0DBh,9Dh
	db	078h,0B0h,005h,94h, 078h,0B0h,013h,94h, 078h,0B0h,0C7h,97h, 078h,0B0h,020h,98h
	db	078h,0B0h,05Eh,98h, 078h,0B0h,05Bh,9Ch, 078h,0B0h,011h,9Dh, 078h,0B0h,082h,9Dh
	db	078h,0B0h,0DFh,9Dh
TheFairyLand_P1:
	db	1,0,24
	db	070h,090h,0F9h,86h, 070h,090h,024h,87h, 070h,090h,08Dh,89h, 070h,090h,0AAh,89h
	db	070h,090h,0C6h,8Bh, 070h,090h,0FBh,8Bh, 070h,090h,051h,95h, 070h,090h,089h,95h
	db	078h,0B0h,0BAh,81h, 078h,0B0h,0E4h,81h, 078h,0B0h,05Dh,82h, 078h,0B0h,077h,82h
	db	078h,0B0h,081h,82h, 078h,0B0h,089h,82h, 078h,0B0h,0FEh,86h, 078h,0B0h,085h,89h
	db	078h,0B0h,0AEh,89h, 078h,0B0h,028h,87h, 078h,0B0h,02Bh,8Fh, 078h,0B0h,033h,8Fh
	db	078h,0B0h,0C5h,91h, 078h,0B0h,0CDh,91h, 078h,0B0h,0DCh,95h, 078h,0B0h,0EAh,95h
; --------
TheMoG:
	db	0,0,32
	db	060h,070h,005h,81h, 060h,070h,017h,81h, 060h,070h,071h,81h, 060h,070h,08Ah,81h
	db	060h,070h,0A3h,81h, 060h,070h,0BCh,81h, 060h,070h,0D5h,81h, 060h,070h,0EEh,81h
	db	060h,070h,0F9h,81h, 060h,070h,049h,8Eh, 060h,070h,05Bh,8Eh, 080h,090h,009h,81h
	db	080h,090h,01Dh,81h, 080h,090h,078h,81h, 080h,090h,091h,81h, 080h,090h,0AAh,81h
	db	080h,090h,0C3h,81h, 080h,090h,0DCh,81h, 080h,090h,004h,82h, 080h,090h,00Fh,82h
	db	080h,090h,04Dh,8Eh, 080h,090h,064h,8Eh, 0A0h,0B0h,07Fh,81h, 0A0h,0B0h,098h,81h
	db	0A0h,0B0h,0B1h,81h, 0A0h,0B0h,0CAh,81h, 0A0h,0B0h,0E3h,81h, 0A0h,0B0h,01Ah,82h
	db	0A0h,0B0h,025h,82h, 0A0h,0B0h,030h,82h, 0A0h,0B0h,03Bh,82h, 0A0h,0B0h,046h,82h
; --------
USAS:
	db	0,0,16
	db	060h,070h,04Dh,80h, 060h,070h,05Fh,80h, 060h,070h,06Dh,94h, 060h,070h,095h,94h
	db	060h,070h,0ECh,96h, 080h,090h,051h,80h, 080h,090h,065h,80h, 080h,090h,072h,94h
	db	080h,090h,09Bh,94h, 080h,090h,0F6h,96h, 0A0h,0B0h,055h,80h, 0A0h,0B0h,06Bh,80h
	db	0A0h,0B0h,078h,94h, 0A0h,0B0h,0A1h,94h, 0A0h,0B0h,0BBh,94h, 0A0h,0B0h,000h,97h
; --------
USAS_a:
	db	0,0,16
	db	060h,070h,02Dh,80h, 060h,070h,03Fh,80h, 060h,070h,04Eh,94h, 060h,070h,076h,94h
	db	060h,070h,0CFh,96h, 080h,090h,031h,80h, 080h,090h,045h,80h, 080h,090h,053h,94h
	db	080h,090h,07Ch,94h, 080h,090h,0D9h,96h, 0A0h,0B0h,035h,80h, 0A0h,0B0h,04Bh,80h
	db	0A0h,0B0h,059h,94h, 0A0h,0B0h,082h,94h, 0A0h,0B0h,09Ch,94h, 0A0h,0B0h,0E3h,96h
; --------
ZanacEX:
	db	0,12,8
	db	038h,038h,00Bh,80h, 036h,036h,00Ch,80h, 030h,030h,00Dh,80h, 031h,031h,00Eh,80h
	db	033h,033h,00Fh,80h, 032h,0CDh,03Fh,80h, 000h,0E0h,040h,80h, 060h,07Fh,041h,80h
ZanacEX_P1:
	db	1,12,22
	db	0FFh,0F5h,0E0h,9Fh, 0FFh,007h,0E1h,9Fh, 0FFh,032h,0E2h,9Fh, 0FFh,000h,0E3h,9Fh
	db	0FFh,050h,0E4h,9Fh, 0FFh,03Ch,0E5h,9Fh, 0FFh,032h,0E6h,9Fh, 0FFh,000h,0E7h,9Fh
	db	0FFh,070h,0E8h,9Fh, 0FFh,0F1h,0E9h,9Fh, 0FFh,0C9h,0EAh,9Fh, 0FFh,0F5h,0F0h,9Fh
	db	0FFh,007h,0F1h,9Fh, 0FFh,032h,0F2h,9Fh, 0FFh,000h,0F3h,9Fh, 0FFh,090h,0F4h,9Fh
	db	0FFh,03Ch,0F5h,9Fh, 0FFh,032h,0F6h,9Fh, 0FFh,000h,0F7h,9Fh, 0FFh,0B0h,0F8h,9Fh
	db	0FFh,0F1h,0F9h,9Fh, 0FFh,0C9h,0FAh,9Fh
ZanacEX_P3:
	db	3,12,22
	db	0FFh,0F5h,0E0h,9Fh, 0FFh,007h,0E1h,9Fh, 0FFh,032h,0E2h,9Fh, 0FFh,000h,0E3h,9Fh
	db	0FFh,050h,0E4h,9Fh, 0FFh,03Ch,0E5h,9Fh, 0FFh,032h,0E6h,9Fh, 0FFh,000h,0E7h,9Fh
	db	0FFh,070h,0E8h,9Fh, 0FFh,0F1h,0E9h,9Fh, 0FFh,0C9h,0EAh,9Fh, 0FFh,0F5h,0F0h,9Fh
	db	0FFh,007h,0F1h,9Fh, 0FFh,032h,0F2h,9Fh, 0FFh,000h,0F3h,9Fh, 0FFh,090h,0F4h,9Fh
	db	0FFh,03Ch,0F5h,9Fh, 0FFh,032h,0F6h,9Fh, 0FFh,000h,0F7h,9Fh, 0FFh,0B0h,0F8h,9Fh
	db	0FFh,0F1h,0F9h,9Fh, 0FFh,0C9h,0FAh,9Fh
ZanacEX_P5:
	db	5,12,22
	db	0FFh,0F5h,0E0h,9Fh, 0FFh,007h,0E1h,9Fh, 0FFh,032h,0E2h,9Fh, 0FFh,000h,0E3h,9Fh
	db	0FFh,050h,0E4h,9Fh, 0FFh,03Ch,0E5h,9Fh, 0FFh,032h,0E6h,9Fh, 0FFh,000h,0E7h,9Fh
	db	0FFh,070h,0E8h,9Fh, 0FFh,0F1h,0E9h,9Fh, 0FFh,0C9h,0EAh,9Fh, 0FFh,0F5h,0F0h,9Fh
	db	0FFh,007h,0F1h,9Fh, 0FFh,032h,0F2h,9Fh, 0FFh,000h,0F3h,9Fh, 0FFh,090h,0F4h,9Fh
	db	0FFh,03Ch,0F5h,9Fh, 0FFh,032h,0F6h,9Fh, 0FFh,000h,0F7h,9Fh, 0FFh,0B0h,0F8h,9Fh
	db	0FFh,0F1h,0F9h,9Fh, 0FFh,0C9h,0FAh,9Fh
ZanacEX_P7:
	db	7,12,22
	db	0FFh,0F5h,0E0h,9Fh, 0FFh,007h,0E1h,9Fh, 0FFh,032h,0E2h,9Fh, 0FFh,000h,0E3h,9Fh
	db	0FFh,050h,0E4h,9Fh, 0FFh,03Ch,0E5h,9Fh, 0FFh,032h,0E6h,9Fh, 0FFh,000h,0E7h,9Fh
	db	0FFh,070h,0E8h,9Fh, 0FFh,0F1h,0E9h,9Fh, 0FFh,0C9h,0EAh,9Fh, 0FFh,0F5h,0F0h,9Fh
	db	0FFh,007h,0F1h,9Fh, 0FFh,032h,0F2h,9Fh, 0FFh,000h,0F3h,9Fh, 0FFh,090h,0F4h,9Fh
	db	0FFh,03Ch,0F5h,9Fh, 0FFh,032h,0F6h,9Fh, 0FFh,000h,0F7h,9Fh, 0FFh,0B0h,0F8h,9Fh
	db	0FFh,0F1h,0F9h,9Fh, 0FFh,0C9h,0FAh,9Fh
ZanacEX_P9:
	db	9,12,22
	db	0FFh,0F5h,0E0h,9Fh, 0FFh,007h,0E1h,9Fh, 0FFh,032h,0E2h,9Fh, 0FFh,000h,0E3h,9Fh
	db	0FFh,050h,0E4h,9Fh, 0FFh,03Ch,0E5h,9Fh, 0FFh,032h,0E6h,9Fh, 0FFh,000h,0E7h,9Fh
	db	0FFh,070h,0E8h,9Fh, 0FFh,0F1h,0E9h,9Fh, 0FFh,0C9h,0EAh,9Fh, 0FFh,0F5h,0F0h,9Fh
	db	0FFh,007h,0F1h,9Fh, 0FFh,032h,0F2h,9Fh, 0FFh,000h,0F3h,9Fh, 0FFh,090h,0F4h,9Fh
	db	0FFh,03Ch,0F5h,9Fh, 0FFh,032h,0F6h,9Fh, 0FFh,000h,0F7h,9Fh, 0FFh,0B0h,0F8h,9Fh
	db	0FFh,0F1h,0F9h,9Fh, 0FFh,0C9h,0FAh,9Fh
ZanacEX_PB:
	db	11,12,22
	db	0FFh,0F5h,0E0h,9Fh, 0FFh,007h,0E1h,9Fh, 0FFh,032h,0E2h,9Fh, 0FFh,000h,0E3h,9Fh
	db	0FFh,050h,0E4h,9Fh, 0FFh,03Ch,0E5h,9Fh, 0FFh,032h,0E6h,9Fh, 0FFh,000h,0E7h,9Fh
	db	0FFh,070h,0E8h,9Fh, 0FFh,0F1h,0E9h,9Fh, 0FFh,0C9h,0EAh,9Fh, 0FFh,0F5h,0F0h,9Fh
	db	0FFh,007h,0F1h,9Fh, 0FFh,032h,0F2h,9Fh, 0FFh,000h,0F3h,9Fh, 0FFh,090h,0F4h,9Fh
	db	0FFh,03Ch,0F5h,9Fh, 0FFh,032h,0F6h,9Fh, 0FFh,000h,0F7h,9Fh, 0FFh,0B0h,0F8h,9Fh
	db	0FFh,0F1h,0F9h,9Fh, 0FFh,0C9h,0FAh,9Fh
ZanacEX_PD:
	db	13,12,22
	db	0FFh,0F5h,0E0h,9Fh, 0FFh,007h,0E1h,9Fh, 0FFh,032h,0E2h,9Fh, 0FFh,000h,0E3h,9Fh
	db	0FFh,050h,0E4h,9Fh, 0FFh,03Ch,0E5h,9Fh, 0FFh,032h,0E6h,9Fh, 0FFh,000h,0E7h,9Fh
	db	0FFh,070h,0E8h,9Fh, 0FFh,0F1h,0E9h,9Fh, 0FFh,0C9h,0EAh,9Fh, 0FFh,0F5h,0F0h,9Fh
	db	0FFh,007h,0F1h,9Fh, 0FFh,032h,0F2h,9Fh, 0FFh,000h,0F3h,9Fh, 0FFh,090h,0F4h,9Fh
	db	0FFh,03Ch,0F5h,9Fh, 0FFh,032h,0F6h,9Fh, 0FFh,000h,0F7h,9Fh, 0FFh,0B0h,0F8h,9Fh
	db	0FFh,0F1h,0F9h,9Fh, 0FFh,0C9h,0FAh,9Fh
ZanacEX_PE:
	db	14,12,9
	db	032h,0CDh,0CCh,86h, 000h,0F0h,0CDh,86h, 070h,07Fh,0CEh,86h, 032h,0CDh,0D8h,86h
	db	000h,0F0h,0D9h,86h, 070h,07Fh,0DAh,86h, 032h,0CDh,0E8h,86h, 000h,0F0h,0E9h,86h
	db	070h,07Fh,0EAh,86h
ZanacEX_PF:
	db	15,12,22
	db	0FFh,0F5h,0E0h,9Fh, 0FFh,007h,0E1h,9Fh, 0FFh,032h,0E2h,9Fh, 0FFh,000h,0E3h,9Fh
	db	0FFh,050h,0E4h,9Fh, 0FFh,03Ch,0E5h,9Fh, 0FFh,032h,0E6h,9Fh, 0FFh,000h,0E7h,9Fh
	db	0FFh,070h,0E8h,9Fh, 0FFh,0F1h,0E9h,9Fh, 0FFh,0C9h,0EAh,9Fh, 0FFh,0F5h,0F0h,9Fh
	db	0FFh,007h,0F1h,9Fh, 0FFh,032h,0F2h,9Fh, 0FFh,000h,0F3h,9Fh, 0FFh,090h,0F4h,9Fh
	db	0FFh,03Ch,0F5h,9Fh, 0FFh,032h,0F6h,9Fh, 0FFh,000h,0F7h,9Fh, 0FFh,0B0h,0F8h,9Fh
	db	0FFh,0F1h,0F9h,9Fh, 0FFh,0C9h,0FAh,9Fh
; --------
ZombHunt:
	db	0,0,18
	db	0AFh,0CDh,010h,80h, 032h,000h,011h,80h, 000h,060h,012h,80h, 064h,0AFh,013h,80h
	db	03Eh,032h,014h,80h, 001h,000h,015h,80h, 032h,050h,016h,80h, 000h,03Ch,017h,80h
	db	06Ah,032h,018h,80h, 03Eh,000h,019h,80h, 002h,070h,01Ah,80h, 032h,03Ch,01Bh,80h
	db	000h,032h,01Ch,80h, 074h,000h,01Dh,80h, 03EH,090h,01Eh,80h, 003h,03Ch,01Fh,80h
	db	07Ah,0B0h,022h,80h, 000h,021h,027h,80h
ZombHunt_P1:
	db	1,0,26
	db	0CDh,0C3h,01Eh,80h, 07Ah,0B0h,040h,80h, 07Ah,0B0h,050h,80h, 07Ah,0B0h,008h,82h
	db	07Ah,0B0h,019h,82h, 07Ah,0B0h,03Ah,82h, 07Ah,0B0h,072h,82h, 07Ah,0B0h,079h,82h
	db	07Ah,0B0h,0B1h,82h, 07Ah,0B0h,0B7h,82h, 07Ah,0B0h,010h,83h, 07Ah,0B0h,017h,83h
	db	07Ah,0B0h,045h,83h, 07Ah,0B0h,067h,83h, 07Ah,0B0h,0CBh,83h, 07Ah,0B0h,0D1h,83h
	db	07Ah,0B0h,0F4h,83h, 07Ah,0B0h,03Ch,84h, 07Ah,0B0h,0AEh,84h, 07Ah,0B0h,0BBh,84h
	db	07Ah,0B0h,0EFh,84h, 07Ah,0B0h,011h,85h, 07Ah,0B0h,047h,85h, 07Ah,0B0h,06Dh,85h
	db	07Ah,0B0h,074h,85h, 07Ah,0B0h,0E4h,85h
; --------
;, 060h,070h,0h,h
;, 080h,090h,0h,h
;, 0A0h,0B0h,0h,h
