
;*** FL8.COM v1.33 for MSX

;*** ROM Loader for MegaflashROM mapped ASCII 8K

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
	db	"ASCII 8K MegaFlashROM by GDX",CR,LF
	db	"based on FLLOAD by K.Tsujikawa"
MESend:
	db	CR,LF,CR,LF,"$"
HlpMes:
	db	"Usage: FL8 filename.ext /Sxx /R /A",CR,LF
	db	"       FL8 /Sxx /E",CR,LF,CR,LF
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
	call	BDOS		; Print MESVER message (FL8 info)

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

; Check parameter /A
	
	ld	hl,DMA
	ld	b,(HL)
	ld	c,041h		; 'A' character
	call	SeekParameter
	cp	255
	jp	z,Done		; Jump if syntax error
	ld	(ParameterA),a

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
	cp	041h		; 'A' character
	jr	z,SyntaxOK
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
	ld	a,(ParameterA)
	cp	041h
	jr	z,NoErase	; Jump if option /A used 
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
NoErase:
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
	cp	4
	jr	z,TestSize
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
	ld	(6000h),a	; Select Flashrom page at Bank 4000h~5FFFh for ASCII mapper
	ld	(5000h),a	; Select Flashrom page at Bank 4000h~5FFFh for SCC mapper

	ld	hl,4555h
	ld	de,42aah

	exx
	ld	bc,2000h	; Length
	ld	de,4000h	; Destination
	ld	hl,0A000h	; Source
	jp	Loop

TestSize:
	ld	a,(FileSize)
	cp	80h
	jr	z,Patch4P4	; Jump if 32KB rom
	cp	40h
	jr	z,Patch4P4	; Jump if 16KB rom
	jp	Done

Patch4P4:
	ld	hl,BUFTOP
	xor	a
	ld	(hl),a
	ld	de,BUFTOP+1
	ld	bc,50h
	ldir			; clean before insert the patch
	
	ld	hl,CopyINIpages
	ld	de,BUFTOP+10h
	ld	bc,023h
	ldir			; Add patch for 16/32K Rom into page 4

	ld	hl,(HeaderADRS)
	ex	de,hl
	ld	(hl),e
	inc	hl
	ld	(hl),d		; Put ROM start addresse 

	ld	a,(ERMSlt)
	ld	h,40h
	call	ENASLT		; Select Flashrom at bank 4000h~7FFFh

	ld	a,(PreBnk)
	ld	(6000h),a	; Select Flashrom page at Bank 4000h~5FFFh for ASCII mapper
	ld	(5000h),a	; Select Flashrom page at Bank 4000h~5FFFh for SCC mapper

	ld	hl,4555h
	ld	de,42aah

	exx
	ld	bc,36h		; Length
	ld	de,4000h	; Destination
	ld	hl,BUFTOP	; Source

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

	ld	hl,CarRace
	call	MEGpatch	; Apply patch for page 0 of 8KB Rom

	ld	hl,Galaxian
	call	MEGpatch	; Apply patch for page 0 of 8KB Rom

	ld	a,(FileSize)
	cp	80h
	jr	z,PatchHead
	cp	40h
	jr	nz,SKIP_PatchRomHeader

	ld	hl,BUFTOP
	ld	de,0A000h
	ld	bc,2000h
	ldir			; backup of page 0 for 16KB Rom

PatchHead:
	ld	hl,(BUFTOP+2)
	ld	a,h
	and	40h
	jr	nz,KeepJP
	ld	a,0c9h
	ld	(HeaderJump),a
KeepJP:
	ld	(HeaderADRS),hl	; Address start to JP of INIpages

	ld	hl,BUFTOP+8h
	ld	(hl),0		; Remove pointer for Basic
	inc	hl
	ld	(hl),0		; Remove pointer for Basic

	ld	hl,400Bh
	ld	(BUFTOP+2),hl	; Address start = patch into Header
	ld	hl,NewHeader
	ld	de,BUFTOP+11
	ld	bc,05h
	ldir			; Patch 32KB Rom header

SKIP_PatchRomHeader:
	ld	hl,QBert
	call	MEGpatch		; Apply patch

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

	ld	hl,Anty_P1
	call	MEGpatch		; Apply patch 

	ld	hl,AthleticBall
	call	MEGpatch		; Apply patch 

	ld	hl,Famboxing
	call	MEGpatch		; Apply patch 

	ld	hl,Firebird
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

	ld	hl,Robocop
	call	MEGpatch			; Apply patch 
	ld	a,(CURRpatchID)
	cp	11			; condition used when a page patch is small
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

	ld	hl,TheMoG
	call	MEGpatch	; Apply patch 

	ld	hl,USAS
	call	MEGpatch	; Apply patch 

	ld	hl,USAS_a
	call	MEGpatch	; Apply patch 

FLashPage:	
	ld	a,(ERMSlt)
	ld	h,40h
	call	ENASLT		; Select Flashrom at bank 4000h~7FFFh

	ld	a,(PreBnk)
	ld	(6000h),a	; Select Flashrom page at Bank 4000h~5FFFh for ASCII mapper
	ld	(5000h),a	; Select Flashrom page at Bank 4000h~5FFFh for SCC mapper

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

NEXT:
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
	
	ld	(hl),0f0h		; AM29F0xx ID reading mode OFF
	ei
	
FLH_FND2:
	ld	a,(RAMAD1)
	ld	h,40h
	call	ENASLT			; Select Main-RAM in MSX"s Bank 1
	ret

; ~~~ End of routine that check if Megaflash is insered in /Sxx Slot


Done:
	ld	a,(ParameterR)
	cp	052h		; Seek 'R' character
	jr	z,ResetRoutine
	jp	NoReset

ResetRoutine:
	ld	a,(ERMSlt)	; Megaflashrom slot
	ld	hl,6000h	; Page selection address for ASC8 mapper
	ld	e,0		; Page number
	call	0014h		; Select page 0 of Megaflashrom

	ld	a,(ERMSlt)	; Megaflashrom slot
	ld	hl,6800h	; Page selection address for ASC8 mapper
	ld	e,0		; Page number
	call	0014h		; Select page 1 of Megaflashrom

	ld	a,(ERMSlt)	; Megaflashrom slot
	ld	hl,7000h	; Page selection address for ASC8 mapper
	ld	e,0		; Page number
	call	0014h		; Select page 2 of Megaflashrom

	ld	a,(ERMSlt)	; Megaflashrom slot
	ld	hl,7800h	; Page selection address for ASC8 mapper
	ld	e,0		; Page number
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

; Patch for 16/32KB ROM Header & added page 4

NewHeader:
	ld	a,4
	ld	(06000h),a; length = 5
CopyINIpages:
	push	hl
	push	bc
	ld	hl,04022h; Start of INIpages routine in ROM
	ld	de,0C000h
	ld	bc,014H
	ldir
	pop	bc
	pop	hl
	jp	0C000h	; CopyINIpages length = 12h
INIpages:
	xor	a
	ld	(06000h),a
	inc	a
	ld	(06800h),a
	inc	a
	ld	(07000h),a
	inc	a
	ld	(07800h),a
HeaderJump:
	jp	4010h	; INIpages length = 13h


; Initialisation routine of pages for 32KB ROM (length = 11h)


; Initialisation routine of pages for 16KB ROM (length = 14h)

INIpages16:
	ld	a,1
	ld	(06800h),a
	ld	(07800h),a
	push	hl
	ld	hl,(0C012h)
	ld	a,(hl)
	inc	hl
	or	(hl)
	pop	hl
	ret	z
adrs16:
	jp	4010h

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
HeaderADRS:
	dw	0
ParameterR:
	db	0
ParameterA:
	db	0
OverWR:
	db	"Y"

; Patch data

;.db page number,patch ID,number of patch
;.db original value, new value,address FSB,address MSB, etc...
; (address of data = 8000h ~ 9FFFh)

Akumajou:
	db	0,0,32
	db	060h,068h,0BEh,8Fh, 060h,068h,065h,90h, 060h,068h,042h,91h, 060h,068h,03Fh,93h
	db	060h,068h,06Ah,93h, 060h,068h,0A6h,93h, 080h,070h,035h,80h, 080h,070h,044h,80h
	db	080h,070h,0C4h,8Fh, 080h,070h,06Ah,90h, 080h,070h,0A8h,90h, 080h,070h,047h,91h
	db	080h,070h,044h,93h, 080h,070h,058h,93h, 080h,070h,070h,93h, 080h,070h,082h,93h
	db	080h,070h,094h,93h, 080h,070h,0ACh,93h, 080h,070h,0D3h,9Eh, 080h,070h,0EEh,9Eh
	db	0A0h,078h,03Ah,80h, 0A0h,078h,04Ah,80h, 0A0h,078h,0CAh,8Fh, 0A0h,078h,070h,90h
	db	0A0h,078h,0B2h,90h, 0A0h,078h,04Dh,91h, 0A0h,078h,04Ah,93h, 0A0h,078h,05Eh,93h
	db	0A0h,078h,076h,93h, 0A0h,078h,088h,93h, 0A0h,078h,09Ah,93h, 0A0h,078h,0B2h,93h
Akumajou_P1:
	db	1,0,7
	db	080h,070h,059h,81h, 080h,070h,07Bh,81h, 080h,070h,052h,87h, 0A0h,078h,053h,84h
	db	0A0h,078h,091h,84h, 0A0h,078h,0C7h,84h, 0A0h,078h,048h,87h
; --------
Akumajou_a:
	db	0,0,32
	db	060h,068h,0BEh,8Fh, 060h,068h,065h,90h, 060h,068h,03Ah,91h, 060h,068h,037h,93h
	db	060h,068h,062h,93h, 060h,068h,09Eh,93h, 080h,070h,035h,80h, 080h,070h,044h,80h
	db	080h,070h,0C4h,8Fh, 080h,070h,06Ah,90h, 080h,070h,0A8h,90h, 080h,070h,03Fh,91h
	db	080h,070h,03Ch,93h, 080h,070h,050h,93h, 080h,070h,068h,93h, 080h,070h,07Ah,93h
	db	080h,070h,08Ch,93h, 080h,070h,0A4h,93h, 080h,070h,0CCh,9Eh, 080h,070h,0E7h,9Eh
	db	0A0h,078h,03Ah,80h, 0A0h,078h,04Ah,80h, 0A0h,078h,0CAh,8Fh, 0A0h,078h,070h,90h
	db	0A0h,078h,0B2h,90h, 0A0h,078h,045h,91h, 0A0h,078h,042h,93h, 0A0h,078h,056h,93h
	db	0A0h,078h,06Eh,93h, 0A0h,078h,080h,93h, 0A0h,078h,092h,93h, 0A0h,078h,0AAh,93h
Akumajou_a_P1:
	db	1,0,7
	db	080h,070h,052h,81h, 080h,070h,074h,81h, 080h,070h,038h,87h, 0A0h,078h,049h,84h
	db	0A0h,078h,087h,84h, 0A0h,078h,0BDh,84h, 0A0h,078h,02Eh,87h
; --------
Akumajou_a2:
	db	0,0,32
	db	060h,068h,0C5h,8Fh, 060h,068h,06Ch,90h, 060h,068h,049h,91h, 060h,068h,046h,93h
	db	060h,068h,071h,93h, 060h,068h,0ADh,93h, 080h,070h,035h,80h, 080h,070h,044h,80h
	db	080h,070h,0CBh,8Fh, 080h,070h,071h,90h, 080h,070h,0AFh,90h, 080h,070h,04Eh,91h
	db	080h,070h,04Bh,93h, 080h,070h,05Fh,93h, 080h,070h,077h,93h, 080h,070h,089h,93h
	db	080h,070h,09Bh,93h, 080h,070h,0B3h,93h, 080h,070h,0DAh,9Eh, 080h,070h,0F5h,9Eh
	db	0A0h,078h,03Ah,80h, 0A0h,078h,04Ah,80h, 0A0h,078h,0D1h,8Fh, 0A0h,078h,077h,90h
	db	0A0h,078h,0B9h,90h, 0A0h,078h,054h,91h, 0A0h,078h,051h,93h, 0A0h,078h,065h,93h
	db	0A0h,078h,07Dh,93h, 0A0h,078h,08Fh,93h, 0A0h,078h,0A1h,93h, 0A0h,078h,0B9h,93h
Akumajou_a2_P1:
	db	1,0,7
	db	080h,070h,060h,81h, 080h,070h,082h,81h, 080h,070h,059h,87h, 0A0h,078h,05Ah,84h
	db	0A0h,078h,098h,84h, 0A0h,078h,0CEh,84h, 0A0h,078h,04Fh,87h
; --------
Akumajou_a3:
	db	0,0,32
	db	060h,068h,0C4h,8Fh, 060h,068h,06Bh,90h, 060h,068h,048h,91h, 060h,068h,05Ah,93h
	db	060h,068h,085h,93h, 060h,068h,0C1h,93h, 080h,070h,035h,80h, 080h,070h,044h,80h
	db	080h,070h,0CAh,8Fh, 080h,070h,070h,90h, 080h,070h,0AEh,90h, 080h,070h,04Dh,91h
	db	080h,070h,05Fh,93h, 080h,070h,073h,93h, 080h,070h,08Bh,93h, 080h,070h,09Dh,93h
	db	080h,070h,0AFh,93h, 080h,070h,0C7h,93h, 080h,070h,0EEh,9Eh, 080h,070h,009h,9Fh
	db	0A0h,078h,03Ah,80h, 0A0h,078h,04Ah,80h, 0A0h,078h,0D0h,8Fh, 0A0h,078h,076h,90h
	db	0A0h,078h,0B8h,90h, 0A0h,078h,053h,91h, 0A0h,078h,065h,93h, 0A0h,078h,079h,93h
	db	0A0h,078h,091h,93h, 0A0h,078h,0A3h,93h, 0A0h,078h,0B5h,93h, 0A0h,078h,0CDh,93h
Akumajou_a3_P1:
	db	1,0,7
	db	080h,070h,074h,81h, 080h,070h,096h,81h, 080h,070h,071h,87h, 0A0h,078h,072h,84h
	db	0A0h,078h,0B0h,84h, 0A0h,078h,0E6h,84h, 0A0h,078h,067h,87h
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
Famboxing:
	db	0,0,15
	db	040h,060h,050h,80h, 060h,068h,057h,80h, 060h,068h,0D7h,80h, 060h,068h,0E5h,80h
	db	060h,068h,029h,81h, 060h,068h,040h,81h, 080h,070h,05Eh,80h, 080h,070h,0DEh,80h
	db	080h,070h,0ECh,80h, 080h,070h,02Dh,81h, 080h,070h,046h,81h, 0A0h,078h,065h,80h
	db	0A0h,078h,0F3h,80h, 0A0h,078h,031h,81h, 0A0h,078h,04Ch,81h
; --------
Firebird:
	db	0,0,21
	db	060h,068h,05Dh,80h, 060h,068h,088h,80h, 060h,068h,0F2h,93h, 060h,068h,00Fh,94h
	db	080h,070h,062h,80h, 080h,070h,072h,80h, 080h,070h,082h,80h, 080h,070h,0D7h,81h
	db	080h,070h,0EDh,81h, 080h,070h,0F7h,93h, 080h,070h,015h,94h, 080h,070h,043h,94h
	db	0A0h,078h,067h,80h, 0A0h,078h,06Dh,80h, 0A0h,078h,07Ch,80h, 0A0h,078h,020h,81h
	db	0A0h,078h,0DFh,81h, 0A0h,078h,0F6h,81h, 0A0h,078h,0FFh,93h, 0A0h,078h,01Bh,94h
	db	0A0h,078h,03Ah,94h
; --------
Galaxian:
	db	0,0,8
	db	031h,031h,010h,80h, 000h,0FDh,011h,80h, 000h,0FFh,012h,80h, 0F3h,0F3h,013h,80h
	db	031h,031h,044h,80h, 000h,0FDh,045h,80h, 000h,0FFh,046h,80h, 0CDh,0CDh,047h,80h
; --------
Game80zemina:
	db	0,0,32
	db	040h,060h,036h,90h, 040h,060h,07Fh,94h, 040h,060h,0A1h,94h, 040h,060h,04Eh,9Fh
	db	001h,000h,039h,90h, 040h,068h,03Ah,90h, 001h,000h,082h,94h, 040h,068h,083h,94h
	db	001h,000h,051h,9Fh, 040h,068h,052h,9Fh, 002h,000h,066h,91h, 040h,070h,067h,91h
	db	002h,000h,086h,94h, 040h,070h,087h,94h, 002h,000h,093h,94h, 040h,070h,094h,94h
	db	002h,000h,0A4h,94h, 040h,070h,0A5h,94h, 003h,000h,06Bh,91h, 040h,078h,06Ch,91h
	db	003h,000h,0C0h,92h, 040h,078h,0C1h,92h, 003h,000h,0DAh,92h, 040h,078h,0DBh,92h
	db	003h,000h,0E4h,92h, 040h,078h,0E5h,92h, 003h,000h,0EEh,92h, 040h,078h,0EFh,92h
	db	003h,000h,08Ah,94h, 040h,078h,08Bh,94h, 003h,000h,097h,94h, 040h,078h,098h,94h
; --------
Garakuta:
	db	0,8,59
	db	010h,012h,002h,80h, 000h,032h,00Ah,80h, 000h,070h,00Ch,80h, 000h,03Ch,00Dh,80h
	db	000h,032h,00Eh,80h, 0F3h,078h,010h,80h, 021h,0C9h,011h,80h, 0DAh,03Eh,012h,80h
	db	0FEh,001h,013h,80h, 03Eh,032h,014h,80h, 0F7h,000h,015h,80h, 077h,068h,016h,80h
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
	db	060h,068h,051h,80h, 060h,068h,063h,80h, 060h,068h,00Fh,82h, 060h,068h,028h,82h
	db	060h,068h,040h,82h, 060h,068h,058h,82h, 060h,068h,070h,82h, 060h,068h,0EDh,8Fh
	db	080h,070h,055h,80h, 080h,070h,069h,80h, 080h,070h,014h,82h, 080h,070h,02Eh,82h
	db	080h,070h,046h,82h, 080h,070h,05Eh,82h, 080h,070h,076h,82h, 080h,070h,0F7h,8Fh
	db	080h,070h,01Fh,94h, 0A0h,078h,059h,80h, 0A0h,078h,06Fh,80h, 0A0h,078h,01Ah,82h
	db	0A0h,078h,034h,82h, 0A0h,078h,04Ch,82h, 0A0h,078h,064h,82h, 0A0h,078h,07Ch,82h
	db	0A0h,078h,001h,90h, 0A0h,078h,029h,94h, 0A0h,078h,076h,99h, 0A0h,078h,0D2h,9Eh
Goemon_P1:
	db	1,0,15
	db	080h,070h,047h,9Eh, 0A0h,078h,099h,82h, 0A0h,078h,041h,85h, 0A0h,078h,03Fh,86h
	db	0A0h,078h,077h,86h, 0A0h,078h,098h,86h, 0A0h,078h,060h,87h, 0A0h,078h,04Bh,88h
	db	0A0h,078h,0D6h,89h, 0A0h,078h,0DDh,8Ch, 0A0h,078h,085h,99h, 0A0h,078h,0A2h,99h
	db	0A0h,078h,08Ah,9Ah, 0A0h,078h,0A3h,9Dh, 0A0h,078h,0EEh,9Dh

Goemon_P2:
	db	2,0,6
	db	0A0h,078h,0F5h,81h, 0A0h,078h,0F7h,87h, 0A0h,078h,0C5h,90h, 0A0h,078h,01Fh,96h
	db	0A0h,078h,057h,97h, 0A0h,078h,001h,9Ah
; --------
Graduis:
	db	0,1,72
	db	060h,068h,07Dh,80h, 060h,068h,0CEh,80h, 060h,068h,036h,82h, 060h,068h,0A2h,82h
	db	060h,068h,030h,88h, 060h,068h,073h,88h, 060h,068h,076h,8Ah, 060h,068h,0A2h,8Ah
	db	060h,068h,0E0h,93h, 060h,068h,02Fh,94h, 080h,070h,030h,80h, 080h,070h,03Eh,80h
	db	080h,070h,082h,80h, 080h,070h,0D3h,80h, 080h,070h,03Bh,82h, 080h,070h,0A7h,82h
	db	080h,070h,056h,85h, 080h,070h,06Dh,85h, 080h,070h,012h,86h, 080h,070h,029h,86h
	db	080h,070h,035h,88h, 080h,070h,078h,88h, 080h,070h,0F3h,89h, 080h,070h,00Dh,8Ah
	db	080h,070h,07Bh,8Ah, 080h,070h,0A7h,8Ah, 080h,070h,019h,93h, 080h,070h,030h,93h
	db	080h,070h,0E5h,93h, 080h,070h,034h,94h, 080h,070h,036h,9Bh, 080h,070h,067h,9Bh
	db	080h,070h,07Fh,9Bh, 080h,070h,0BBh,9Bh, 080h,070h,000h,9Ch, 080h,070h,074h,9Ch
	db	080h,070h,0E3h,9Ch, 080h,070h,00Eh,9Dh, 080h,070h,086h,9Dh, 080h,070h,0B4h,9Dh
	db	0A0h,078h,034h,80h, 0A0h,078h,044h,80h, 0A0h,078h,088h,80h, 0A0h,078h,0D9h,80h
	db	0A0h,078h,041h,82h, 0A0h,078h,0ADh,82h, 0A0h,078h,060h,85h, 0A0h,078h,077h,85h
	db	0A0h,078h,01Ch,86h, 0A0h,078h,033h,86h, 0A0h,078h,03Bh,88h, 0A0h,078h,07Eh,88h
	db	0A0h,078h,0FDh,89h, 0A0h,078h,017h,8Ah, 0A0h,078h,081h,8Ah, 0A0h,078h,0ADh,8Ah
	db	0A0h,078h,00Ch,8Bh, 0A0h,078h,031h,8Bh, 0A0h,078h,023h,93h, 0A0h,078h,03Ah,93h
	db	0A0h,078h,0EBh,93h, 0A0h,078h,03Ah,94h, 0A0h,078h,040h,9Bh, 0A0h,078h,071h,9Bh
	db	0A0h,078h,089h,9Bh, 0A0h,078h,0C5h,9Bh, 0A0h,078h,00Ah,9Ch, 0A0h,078h,07Eh,9Ch
	db	0A0h,078h,0EDh,9Ch, 0A0h,078h,018h,9Dh, 0A0h,078h,090h,9Dh, 0A0h,078h,0BEh,9Dh
Graduis_P1:
	db	1,1,2
	db	0A0h,078h,0AEh,09Fh, 0A0h,078h,0C4h,09Fh
Graduis_P2:
	db	2,1,4
	db	0A0h,078h,09Dh,82h, 0A0h,078h,0AAh,82h, 0A0h,078h,0D9h,82h, 0A0h,078h,002h,83h
; --------
Graduis_a:
	db	0,2,72
	db	060h,068h,07Dh,80h, 060h,068h,0CEh,80h, 060h,068h,036h,82h, 060h,068h,0A2h,82h
	db	060h,068h,030h,88h, 060h,068h,073h,88h, 060h,068h,076h,8Ah, 060h,068h,0A2h,8Ah
	db	060h,068h,0C6h,93h, 060h,068h,015h,94h, 080h,070h,030h,80h, 080h,070h,03Eh,80h
	db	080h,070h,082h,80h, 080h,070h,0D3h,80h, 080h,070h,03Bh,82h, 080h,070h,0A7h,82h
	db	080h,070h,056h,85h, 080h,070h,06Dh,85h, 080h,070h,012h,86h, 080h,070h,029h,86h
	db	080h,070h,035h,88h, 080h,070h,078h,88h, 080h,070h,0F3h,89h, 080h,070h,00Dh,8Ah
	db	080h,070h,07Bh,8Ah, 080h,070h,0A7h,8Ah, 080h,070h,0FFh,92h, 080h,070h,016h,93h
	db	080h,070h,0CBh,93h, 080h,070h,01Ah,94h, 080h,070h,01Ch,9Bh, 080h,070h,04Dh,9Bh
	db	080h,070h,065h,9Bh, 080h,070h,0A1h,9Bh, 080h,070h,0E6h,9Bh, 080h,070h,05Ah,9Ch
	db	080h,070h,0C9h,9Ch, 080h,070h,0F4h,9Ch, 080h,070h,06Ch,9Dh, 080h,070h,09Ah,9Dh
	db	0A0h,078h,034h,80h, 0A0h,078h,044h,80h, 0A0h,078h,088h,80h, 0A0h,078h,0D9h,80h
	db	0A0h,078h,041h,82h, 0A0h,078h,0ADh,82h, 0A0h,078h,060h,85h, 0A0h,078h,077h,85h
	db	0A0h,078h,01Ch,86h, 0A0h,078h,033h,86h, 0A0h,078h,03Bh,88h, 0A0h,078h,07Eh,88h
	db	0A0h,078h,0FDh,89h, 0A0h,078h,017h,8Ah, 0A0h,078h,081h,8Ah, 0A0h,078h,0ADh,8Ah
	db	0A0h,078h,00Ch,8Bh, 0A0h,078h,031h,8Bh, 0A0h,078h,009h,93h, 0A0h,078h,020h,93h
	db	0A0h,078h,0D1h,93h, 0A0h,078h,020h,94h, 0A0h,078h,026h,9Bh, 0A0h,078h,057h,9Bh
	db	0A0h,078h,06Fh,9Bh, 0A0h,078h,0ABh,9Bh, 0A0h,078h,0F0h,9Bh, 0A0h,078h,064h,9Ch
	db	0A0h,078h,0D3h,9Ch, 0A0h,078h,0FEh,9Ch, 0A0h,078h,076h,9Dh, 0A0h,078h,0A4h,9Dh
Graduis_a_P1:
	db	1,2,2
	db	0A0h,078h,094h,9Fh, 0A0h,078h,0AAh,9Fh
Graduis_a_P2:
	db	2,2,4
	db	0A0h,078h,083h,82h, 0A0h,078h,090h,82h, 0A0h,078h,0BFh,82h, 0A0h,078h,0E8h,82h
; --------
KingKong2:
	db	0,3,14
	db	060h,068h,02Eh,82h, 060h,068h,03Bh,82h, 060h,068h,0CCh,82h, 060h,068h,09Dh,94h
	db	060h,068h,0B3h,94h, 080h,070h,032h,82h, 080h,070h,041h,82h, 080h,070h,0D2h,82h
	db	080h,070h,0A7h,94h, 080h,070h,0B7h,94h, 080h,070h,063h,9Fh, 0A0h,078h,022h,82h
	db	0A0h,078h,0D7h,82h, 0A0h,078h,0A2h,9Dh
KingKong2_P1:
	db	1,3,6
	db	080h,070h,004h,80h, 0A0h,078h,00Bh,80h, 0A0h,078h,07Ch,81h, 0A0h,078h,08Ch,81h
	db	0A0h,078h,0D7h,81h, 0A0h,078h,07Dh,82h
KingKong2_P2:
	db	2,3,1
	db	0A0h,078h,07Bh,81h
KingKong2_P6:
	db	6,3,1
	db	080h,070h,0C2h,99h
; --------
MetalGear:
	db	0,0,13
	db	060h,068h,0B4h,81h, 060h,068h,0C1h,81h, 060h,068h,058h,82h, 060h,068h,091h,82h
	db	060h,068h,0A7h,82h, 080h,070h,0B8h,81h, 080h,070h,0C7h,81h, 080h,070h,05Eh,82h
	db	080h,070h,09Bh,82h, 080h,070h,0ABh,82h, 0A0h,078h,0A5h,81h, 0A0h,078h,064h,82h
	db	0A0h,078h,02Eh,89h
; --------
MonMon_Daewoo:
	db	0,4,17
	db	040h,060h,047h,80h, 060h,068h,04Bh,80h, 060h,068h,085h,88h, 060h,068h,09Dh,88h
	db	060h,068h,0B7h,88h, 060h,068h,0D1h,88h, 060h,068h,0EEh,88h, 060h,068h,0C8h,8Ah
	db	060h,068h,018h,8Bh, 060h,068h,02Ah,8Bh, 060h,068h,04Dh,8Bh, 060h,068h,0E9h,8Bh
	db	060h,068h,0F0h,8Ch, 060h,068h,002h,8Dh, 060h,068h,093h,94h, 080h,070h,04Fh,80h
	db	0A0h,078h,053h,80h
MonMon_Daewoo_P2:
	db	2,4,2
	db	060h,068h,0D7h,9Dh, 060h,068h,0EBh,9Dh
MonMon_Daewoo_P3:
	db	3,4,7
	db	040h,060h,00Ch,9Fh, 060h,068h,0F4h,96h, 060h,068h,024h,97h, 060h,068h,02Dh,97h
	db	060h,068h,083h,9Ah, 060h,068h,044h,9Eh, 060h,068h,012h,9Fh
; --------
MSXDOS22:
	db	0,9,15
	db	032h,007h,092h,80h, 000h,032h,093h,80h, 060h,000h,094h,80h, 0C9h,060h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,068h,099h,80h, 000h,0C9h,09Ah,80h
	db	04Dh,04Dh,01Eh,81h, 053h,053h,01Fh,81h, 058h,058h,020h,81h, 02Dh,02Dh,021h,81h
	db	044h,044h,022h,81h, 04Fh,04Fh,023h,81h, 053h,053h,024h,81h
MSXDOS22_P1:
	db	1,9,8
	db	032h,007h,092h,80h, 000h,032h,093h,80h, 060h,000h,094h,80h, 0C9h,060h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,068h,099h,80h, 000h,0C9h,09Ah,80h
MSXDOS22_P2:
	db	2,9,8
	db	032h,007h,092h,80h, 000h,032h,093h,80h, 060h,000h,094h,80h, 0C9h,060h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,068h,099h,80h, 000h,0C9h,09Ah,80h
MSXDOS22_P3:
	db	3,9,8
	db	032h,007h,092h,80h, 000h,032h,093h,80h, 060h,000h,094h,80h, 0C9h,060h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,068h,099h,80h, 000h,0C9h,09Ah,80h
; --------
MSXDOS22NL:
	db	0,10,15
	db	032h,007h,092h,80h, 0FEh,032h,093h,80h, 07Fh,000h,094h,80h, 0C9h,060h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,068h,099h,80h, 000h,0C9h,09Ah,80h
	db	04Dh,04Dh,01Eh,81h, 053h,053h,01Fh,81h, 058h,058h,020h,81h, 02Dh,02Dh,021h,81h
	db	044h,044h,022h,81h, 04Fh,04Fh,023h,81h, 053h,053h,024h,81h
MSXDOS22NL_P1:
	db	1,10,8
	db	032h,007h,092h,80h, 0FEh,032h,093h,80h, 07Fh,000h,094h,80h, 0C9h,060h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,068h,099h,80h, 000h,0C9h,09Ah,80h
MSXDOS22NL_P2:
	db	2,10,8
	db	032h,007h,092h,80h, 0FEh,032h,093h,80h, 07Fh,000h,094h,80h, 0C9h,060h,095h,80h
	db	000h,03Ch,096h,80h, 000h,032h,097h,80h, 000h,068h,099h,80h, 000h,0C9h,09Ah,80h
; --------
PengAdv_Part1:
	db	0,0,180
	db	060h,068h,076h,80h, 060h,068h,0E5h,80h, 060h,068h,0D6h,81h, 060h,068h,013h,82h
	db	060h,068h,0D2h,85h, 060h,068h,009h,86h, 060h,068h,0EBh,87h, 060h,068h,00Ah,88h
	db	060h,068h,024h,88h, 060h,068h,08Ah,88h, 060h,068h,0A8h,88h, 060h,068h,0CCh,88h
	db	060h,068h,0EBh,88h, 060h,068h,005h,89h, 060h,068h,084h,89h, 060h,068h,09Eh,89h
	db	060h,068h,0DBh,89h, 060h,068h,005h,8Ah, 060h,068h,078h,8Ah, 060h,068h,0C8h,8Bh
	db	060h,068h,027h,8Ch, 060h,068h,041h,8Ch, 060h,068h,0C0h,8Ch, 060h,068h,0E0h,8Ch
	db	060h,068h,06Eh,8Dh, 060h,068h,08Eh,8Dh, 060h,068h,009h,8Eh, 060h,068h,029h,8Eh
	db	060h,068h,0A5h,8Eh, 060h,068h,0CEh,8Eh, 060h,068h,04Ah,8Fh, 060h,068h,06Ah,8Fh
	db	060h,068h,09Bh,8Fh, 060h,068h,0BFh,8Fh, 060h,068h,01Ch,90h, 060h,068h,03Ch,90h
	db	060h,068h,099h,90h, 060h,068h,0B9h,90h, 060h,068h,016h,91h, 060h,068h,036h,91h
	db	060h,068h,055h,91h, 060h,068h,081h,91h, 060h,068h,0BCh,91h, 060h,068h,0D6h,91h
	db	060h,068h,001h,92h, 060h,068h,01Bh,92h, 060h,068h,045h,92h, 060h,068h,05Fh,92h
	db	060h,068h,089h,92h, 060h,068h,0A3h,92h, 060h,068h,0F9h,92h, 060h,068h,013h,93h
	db	060h,068h,069h,93h, 060h,068h,083h,93h, 060h,068h,0BEh,93h, 060h,068h,0D8h,93h
	db	060h,068h,013h,94h, 060h,068h,02Dh,94h, 060h,068h,087h,94h, 060h,068h,0A1h,94h
	db	060h,068h,0FBh,94h, 060h,068h,015h,95h, 060h,068h,050h,95h, 060h,068h,06Ah,95h
	db	060h,068h,0E6h,95h, 060h,068h,000h,96h, 060h,068h,04Eh,96h, 060h,068h,070h,96h
	db	060h,068h,08Fh,96h, 060h,068h,0A9h,96h, 060h,068h,0C8h,96h, 060h,068h,0E2h,96h
	db	060h,068h,006h,97h, 060h,068h,020h,97h, 060h,068h,03Fh,97h, 060h,068h,059h,97h
	db	060h,068h,078h,97h, 060h,068h,092h,97h, 060h,068h,0B1h,97h, 060h,068h,0CBh,97h
	db	060h,068h,0EAh,97h, 060h,068h,004h,98h, 060h,068h,0A0h,98h, 060h,068h,0BAh,98h
	db	060h,068h,0DFh,98h, 060h,068h,0F9h,98h, 060h,068h,01Bh,99h, 060h,068h,035h,99h
	db	060h,068h,063h,99h, 060h,068h,07Dh,99h, 060h,068h,099h,99h, 060h,068h,0B8h,99h
	db	060h,068h,0D2h,99h, 060h,068h,0F1h,99h, 060h,068h,00Bh,9Ah, 060h,068h,02Ah,9Ah
	db	060h,068h,044h,9Ah, 060h,068h,063h,9Ah, 060h,068h,07Dh,9Ah, 060h,068h,09Ch,9Ah
	db	060h,068h,0B6h,9Ah, 060h,068h,0D5h,9Ah, 060h,068h,0EFh,9Ah, 060h,068h,00Eh,9Bh
	db	060h,068h,028h,9Bh, 060h,068h,047h,9Bh, 060h,068h,061h,9Bh, 060h,068h,080h,9Bh
	db	060h,068h,09Dh,9Bh, 060h,068h,0BFh,9Bh, 060h,068h,0DDh,9Bh, 060h,068h,0FFh,9Bh
	db	060h,068h,02Eh,9Ch, 060h,068h,05Ah,9Ch, 080h,070h,02Bh,80h, 080h,070h,039h,80h
	db	080h,070h,07Bh,80h, 080h,070h,0EAh,80h, 080h,070h,05Eh,81h, 080h,070h,075h,81h
	db	080h,070h,0DBh,81h, 080h,070h,018h,82h, 080h,070h,0D7h,85h, 080h,070h,00Eh,86h
	db	080h,070h,039h,87h, 080h,070h,08Eh,87h, 080h,070h,0A2h,87h, 080h,070h,0CDh,87h
	db	080h,070h,0F0h,87h, 080h,070h,00Fh,88h, 080h,070h,029h,88h, 080h,070h,08Fh,88h
	db	080h,070h,0ADh,88h, 080h,070h,0D1h,88h, 080h,070h,0F0h,88h, 080h,070h,00Ah,89h
	db	080h,070h,089h,89h, 080h,070h,0A3h,89h, 080h,070h,0E0h,89h, 080h,070h,00Ah,8Ah
	db	080h,070h,07Dh,8Ah, 080h,070h,0CDh,8Bh, 080h,070h,02Ch,8Ch, 080h,070h,046h,8Ch
	db	080h,070h,0C5h,8Ch, 080h,070h,0E5h,8Ch, 080h,070h,073h,8Dh, 080h,070h,093h,8Dh
	db	080h,070h,00Eh,8Eh, 080h,070h,02Eh,8Eh, 080h,070h,0AAh,8Eh, 080h,070h,0D3h,8Eh
	db	080h,070h,04Fh,8Fh, 080h,070h,06Fh,8Fh, 080h,070h,0A0h,8Fh, 080h,070h,0C4h,8Fh
	db	080h,070h,021h,90h, 080h,070h,041h,90h, 080h,070h,09Eh,90h, 080h,070h,0BEh,90h
	db	080h,070h,01Bh,91h, 080h,070h,03Bh,91h, 080h,070h,05Ah,91h, 080h,070h,086h,91h
	db	080h,070h,0C1h,91h, 080h,070h,0DBh,91h, 080h,070h,006h,92h, 080h,070h,020h,92h
	db	080h,070h,04Ah,92h, 080h,070h,064h,92h, 080h,070h,08Eh,92h, 080h,070h,0A8h,92h
	db	080h,070h,0FEh,92h, 080h,070h,018h,93h, 080h,070h,06Eh,93h, 080h,070h,088h,93h
	db	080h,070h,0C3h,93h, 080h,070h,0DDh,93h, 080h,070h,018h,94h, 080h,070h,032h,94h
PengAdv_Part2:
	db	0,0,184
	db	080h,070h,08Ch,94h, 080h,070h,0A6h,94h, 080h,070h,000h,95h, 080h,070h,01Ah,95h
	db	080h,070h,055h,95h, 080h,070h,06Fh,95h, 080h,070h,0EBh,95h, 080h,070h,005h,96h
	db	080h,070h,053h,96h, 080h,070h,075h,96h, 080h,070h,094h,96h, 080h,070h,0AEh,96h
	db	080h,070h,0CDh,96h, 080h,070h,0E7h,96h, 080h,070h,00Bh,97h, 080h,070h,025h,97h
	db	080h,070h,044h,97h, 080h,070h,05Eh,97h, 080h,070h,07Dh,97h, 080h,070h,097h,97h
	db	080h,070h,0B6h,97h, 080h,070h,0D0h,97h, 080h,070h,0EFh,97h, 080h,070h,009h,98h
	db	080h,070h,0A5h,98h, 080h,070h,0BFh,98h, 080h,070h,0E4h,98h, 080h,070h,0FEh,98h
	db	080h,070h,020h,99h, 080h,070h,03Ah,99h, 080h,070h,068h,99h, 080h,070h,082h,99h
	db	080h,070h,09Eh,99h, 080h,070h,0BDh,99h, 080h,070h,0D7h,99h, 080h,070h,0F6h,99h
	db	080h,070h,010h,9Ah, 080h,070h,02Fh,9Ah, 080h,070h,049h,9Ah, 080h,070h,068h,9Ah
	db	080h,070h,082h,9Ah, 080h,070h,0A1h,9Ah, 080h,070h,0BBh,9Ah, 080h,070h,0DAh,9Ah
	db	080h,070h,0F4h,9Ah, 080h,070h,013h,9Bh, 080h,070h,02Dh,9Bh, 080h,070h,04Ch,9Bh
	db	080h,070h,066h,9Bh, 080h,070h,085h,9Bh, 080h,070h,0A2h,9Bh, 080h,070h,0C4h,9Bh
	db	080h,070h,0E2h,9Bh, 080h,070h,004h,9Ch, 080h,070h,033h,9Ch, 080h,070h,05Fh,9Ch
	db	080h,070h,070h,9Ch, 080h,070h,0ACh,9Eh, 080h,070h,0F1h,9Eh, 0A0h,078h,02Fh,80h
	db	0A0h,078h,03Fh,80h, 0A0h,078h,081h,80h, 0A0h,078h,0F0h,80h, 0A0h,078h,066h,81h
	db	0A0h,078h,07Eh,81h, 0A0h,078h,0E1h,81h, 0A0h,078h,01Eh,82h, 0A0h,078h,0DDh,85h
	db	0A0h,078h,014h,86h, 0A0h,078h,043h,87h, 0A0h,078h,098h,87h, 0A0h,078h,0ACh,87h
	db	0A0h,078h,0D7h,87h, 0A0h,078h,0F6h,87h, 0A0h,078h,015h,88h, 0A0h,078h,02Fh,88h
	db	0A0h,078h,095h,88h, 0A0h,078h,0B3h,88h, 0A0h,078h,0D7h,88h, 0A0h,078h,0F6h,88h
	db	0A0h,078h,010h,89h, 0A0h,078h,08Fh,89h, 0A0h,078h,0A9h,89h, 0A0h,078h,0E6h,89h
	db	0A0h,078h,010h,8Ah, 0A0h,078h,083h,8Ah, 0A0h,078h,0D3h,8Bh, 0A0h,078h,032h,8Ch
	db	0A0h,078h,04Ch,8Ch, 0A0h,078h,0CBh,8Ch, 0A0h,078h,0EBh,8Ch, 0A0h,078h,079h,8Dh
	db	0A0h,078h,099h,8Dh, 0A0h,078h,014h,8Eh, 0A0h,078h,034h,8Eh, 0A0h,078h,0B0h,8Eh
	db	0A0h,078h,0D9h,8Eh, 0A0h,078h,055h,8Fh, 0A0h,078h,075h,8Fh, 0A0h,078h,0A6h,8Fh
	db	0A0h,078h,0CAh,8Fh, 0A0h,078h,027h,90h, 0A0h,078h,047h,90h, 0A0h,078h,0A4h,90h
	db	0A0h,078h,0C4h,90h, 0A0h,078h,021h,91h, 0A0h,078h,041h,91h, 0A0h,078h,060h,91h
	db	0A0h,078h,08Ch,91h, 0A0h,078h,0C7h,91h, 0A0h,078h,0E1h,91h, 0A0h,078h,00Ch,92h
	db	0A0h,078h,026h,92h, 0A0h,078h,050h,92h, 0A0h,078h,06Ah,92h, 0A0h,078h,094h,92h
	db	0A0h,078h,0AEh,92h, 0A0h,078h,004h,93h, 0A0h,078h,01Eh,93h, 0A0h,078h,074h,93h
	db	0A0h,078h,08Eh,93h, 0A0h,078h,0C9h,93h, 0A0h,078h,0E3h,93h, 0A0h,078h,01Eh,94h
	db	0A0h,078h,038h,94h, 0A0h,078h,092h,94h, 0A0h,078h,0ACh,94h, 0A0h,078h,006h,95h
	db	0A0h,078h,020h,95h, 0A0h,078h,05Bh,95h, 0A0h,078h,075h,95h, 0A0h,078h,0F1h,95h
	db	0A0h,078h,00Bh,96h, 0A0h,078h,059h,96h, 0A0h,078h,07Bh,96h, 0A0h,078h,09Ah,96h
	db	0A0h,078h,0B4h,96h, 0A0h,078h,0D3h,96h, 0A0h,078h,0EDh,96h, 0A0h,078h,011h,97h
	db	0A0h,078h,02Bh,97h, 0A0h,078h,04Ah,97h, 0A0h,078h,064h,97h, 0A0h,078h,083h,97h
	db	0A0h,078h,09Dh,97h, 0A0h,078h,0BCh,97h, 0A0h,078h,0D6h,97h, 0A0h,078h,0F5h,97h
	db	0A0h,078h,00Fh,98h, 0A0h,078h,0ABh,98h, 0A0h,078h,0C5h,98h, 0A0h,078h,0EAh,98h
	db	0A0h,078h,004h,99h, 0A0h,078h,026h,99h, 0A0h,078h,040h,99h, 0A0h,078h,06Eh,99h
	db	0A0h,078h,088h,99h, 0A0h,078h,0A4h,99h, 0A0h,078h,0C3h,99h, 0A0h,078h,0DDh,99h
	db	0A0h,078h,0FCh,99h, 0A0h,078h,016h,9Ah, 0A0h,078h,035h,9Ah, 0A0h,078h,04Fh,9Ah
	db	0A0h,078h,06Eh,9Ah, 0A0h,078h,088h,9Ah, 0A0h,078h,0A7h,9Ah, 0A0h,078h,0C1h,9Ah
	db	0A0h,078h,0E0h,9Ah, 0A0h,078h,0FAh,9Ah, 0A0h,078h,019h,9Bh, 0A0h,078h,033h,9Bh
	db	0A0h,078h,052h,9Bh, 0A0h,078h,06Ch,9Bh, 0A0h,078h,08Bh,9Bh, 0A0h,078h,0A8h,9Bh
	db	0A0h,078h,039h,9Ch, 0A0h,078h,0CAh,9Bh, 0A0h,078h,0E8h,9Bh, 0A0h,078h,00Ah,9Ch
	db	0A0h,078h,065h,9Ch, 0A0h,078h,07Ah,9Ch, 0A0h,078h,0B6h,9Eh, 0A0h,078h,0FBh,9Eh
PengAdv_P1:
	db	1,0,148
	db	080h,070h,02Fh,80h, 080h,070h,04Ch,80h, 080h,070h,061h,80h, 080h,070h,07Bh,80h
	db	080h,070h,090h,80h, 080h,070h,0ADh,80h, 080h,070h,0C2h,80h, 080h,070h,0DCh,80h
	db	080h,070h,0F1h,80h, 080h,070h,00Eh,81h, 080h,070h,023h,81h, 080h,070h,03Dh,81h
	db	080h,070h,052h,81h, 080h,070h,06Ch,81h, 080h,070h,081h,81h, 080h,070h,09Bh,81h
	db	080h,070h,0B0h,81h, 080h,070h,0CDh,81h, 080h,070h,004h,82h, 080h,070h,03Ch,82h
	db	080h,070h,051h,82h, 080h,070h,094h,82h, 080h,070h,0F4h,82h, 080h,070h,013h,83h
	db	080h,070h,028h,83h, 080h,070h,060h,83h, 080h,070h,075h,83h, 080h,070h,0ACh,83h
	db	080h,070h,0C1h,83h, 080h,070h,0EDh,83h, 080h,070h,002h,84h, 080h,070h,02Fh,84h
	db	080h,070h,044h,84h, 080h,070h,066h,84h, 080h,070h,03Eh,85h, 080h,070h,097h,85h
	db	080h,070h,09Ch,86h, 080h,070h,0F3h,86h, 080h,070h,027h,87h, 080h,070h,041h,87h
	db	080h,070h,089h,87h, 080h,070h,0B2h,87h, 080h,070h,042h,88h, 080h,070h,0E7h,88h
	db	080h,070h,048h,89h, 080h,070h,05Ch,89h, 080h,070h,0AFh,89h, 080h,070h,0E9h,8Ah
	db	080h,070h,031h,8Bh, 080h,070h,045h,8Bh, 080h,070h,082h,8Bh, 080h,070h,09Ch,8Bh
	db	080h,070h,0C8h,8Bh, 080h,070h,03Fh,8Ch, 080h,070h,061h,8Fh, 080h,070h,076h,8Fh
	db	080h,070h,063h,9Bh, 080h,070h,08Ah,9Bh, 080h,070h,0C9h,9Bh, 080h,070h,0ECh,9Bh
	db	080h,070h,006h,9Ch, 080h,070h,043h,9Ch, 080h,070h,08Fh,9Ch, 080h,070h,0A4h,9Ch
	db	080h,070h,0FDh,9Ch, 080h,070h,079h,9Dh, 080h,070h,0A1h,9Dh, 080h,070h,0B8h,9Dh
	db	080h,070h,0DAh,9Dh, 080h,070h,0FCh,9Dh, 080h,070h,016h,9Eh, 080h,070h,032h,9Eh
	db	080h,070h,016h,9Fh, 080h,070h,04Dh,9Fh, 0A0h,078h,039h,80h, 0A0h,078h,056h,80h
	db	0A0h,078h,06Bh,80h, 0A0h,078h,085h,80h, 0A0h,078h,09Ah,80h, 0A0h,078h,0B7h,80h
	db	0A0h,078h,0CCh,80h, 0A0h,078h,0E6h,80h, 0A0h,078h,0FBh,80h, 0A0h,078h,018h,81h
	db	0A0h,078h,02Dh,81h, 0A0h,078h,047h,81h, 0A0h,078h,05Ch,81h, 0A0h,078h,076h,81h
	db	0A0h,078h,08Bh,81h, 0A0h,078h,0A5h,81h, 0A0h,078h,0BAh,81h, 0A0h,078h,0D7h,81h
	db	0A0h,078h,00Eh,82h, 0A0h,078h,046h,82h, 0A0h,078h,05Bh,82h, 0A0h,078h,09Eh,82h
	db	0A0h,078h,0FEh,82h, 0A0h,078h,01Dh,83h, 0A0h,078h,032h,83h, 0A0h,078h,06Ah,83h
	db	0A0h,078h,07Fh,83h, 0A0h,078h,0B6h,83h, 0A0h,078h,0CBh,83h, 0A0h,078h,0F7h,83h
	db	0A0h,078h,00Ch,84h, 0A0h,078h,039h,84h, 0A0h,078h,04Eh,84h, 0A0h,078h,070h,84h
	db	0A0h,078h,048h,85h, 0A0h,078h,0A1h,85h, 0A0h,078h,0A6h,86h, 0A0h,078h,0FDh,86h
	db	0A0h,078h,031h,87h, 0A0h,078h,04Bh,87h, 0A0h,078h,093h,87h, 0A0h,078h,0BCh,87h
	db	0A0h,078h,04Ch,88h, 0A0h,078h,0F1h,88h, 0A0h,078h,052h,89h, 0A0h,078h,066h,89h
	db	0A0h,078h,0B9h,89h, 0A0h,078h,0F3h,8Ah, 0A0h,078h,03Bh,8Bh, 0A0h,078h,04Fh,8Bh
	db	0A0h,078h,08Ch,8Bh, 0A0h,078h,0A6h,8Bh, 0A0h,078h,0D2h,8Bh, 0A0h,078h,049h,8Ch
	db	0A0h,078h,06Bh,8Fh, 0A0h,078h,080h,8Fh, 0A0h,078h,06Dh,9Bh, 0A0h,078h,094h,9Bh
	db	0A0h,078h,0D3h,9Bh, 0A0h,078h,0F6h,9Bh, 0A0h,078h,010h,9Ch, 0A0h,078h,04Dh,9Ch
	db	0A0h,078h,099h,9Ch, 0A0h,078h,0AEh,9Ch, 0A0h,078h,007h,9Dh, 0A0h,078h,083h,9Dh
	db	0A0h,078h,0ABh,9Dh, 0A0h,078h,0C2h,9Dh, 0A0h,078h,0E4h,9Dh, 0A0h,078h,006h,9Eh
	db	0A0h,078h,020h,9Eh, 0A0h,078h,03Ch,9Eh, 0A0h,078h,020h,9Fh, 0A0h,078h,057h,9Fh
; --------
QBert:
	db	0,0,8
	db	043h,043h,010h,80h, 044h,044h,011h,80h, 03Eh,03Eh,07Eh,80h, 001h,001h,07Fh,80h
	db	032h,032h,080h,80h, 060h,000h,082h,80h, 080h,000h,086h,80h, 0A0h,000h,08Ah,80h
; --------
Robocop:
	db	0,11,4
	db	060h,068h,03Dh,80h, 032h,032h,040h,80h, 0AFh,0AFh,041h,80h, 0FCh,0FCh,042h,80h
Robocop_P1:	
	db	1,11,5
	db	080h,070h,070h,99h, 0A0h,078h,075h,99h, 080h,070h,084h,99h, 0A0h,078h,089h,99h
	db	080h,070h,098h,99h
; --------
Shalom:
	db	0,5,19
	db	040h,060h,0F0h,81h, 060h,068h,05Bh,80h, 060h,068h,06Ch,80h, 060h,068h,009h,81h
	db	060h,068h,070h,81h, 060h,068h,092h,81h, 060h,068h,0F7h,81h, 080h,070h,05Fh,80h
	db	080h,070h,072h,80h, 080h,070h,00Fh,81h, 080h,070h,077h,81h, 080h,070h,096h,81h
	db	080h,070h,0FEh,81h, 0A0h,078h,063h,80h, 0A0h,078h,078h,80h, 0A0h,078h,015h,81h
	db	0A0h,078h,081h,81h, 0A0h,078h,09Ah,81h, 0A0h,078h,005h,82h
Shalom_P1:
	db	0,5,2
	db	080h,070h,085h,80h, 080h,070h,0ADh,80h
; --------
SupLodeRunner:
	db	0,6,79
	db	000h,086h,002h,80h, 000h,080h,003h,80h, 000h,0AFh,068h,80h, 000h,0F5h,069h,80h
	db	000h,0E5h,06Ah,80h, 000h,0D5h,06Bh,80h, 000h,0C5h,06Ch,80h, 000h,087h,06Dh,80h
	db	000h,05Fh,06Eh,80h, 000h,021h,06Fh,80h, 000h,070h,071h,80h, 000h,03Ah,072h,80h
	db	000h,01Eh,073h,80h, 000h,0F9h,074h,80h, 000h,0CDh,075h,80h, 000h,014h,076h,80h
	db	000h,01Ch,078h,80h, 000h,026h,079h,80h, 000h,078h,07Ah,80h, 000h,03Ah,07Bh,80h
	db	000h,01Eh,07Ch,80h, 000h,0F9h,07Dh,80h, 000h,0CDh,07Eh,80h, 000h,014h,07Fh,80h
	db	000h,0C1h,081h,80h, 000h,0D1h,082h,80h, 000h,0E1h,083h,80h, 000h,0F1h,084h,80h
	db	000h,0C9h,085h,80h, 000h,079h,086h,80h, 000h,032h,087h,80h, 000h,01Eh,088h,80h
	db	000h,0F9h,089h,80h, 000h,0D5h,08Ah,80h, 000h,0CDh,08Bh,80h, 000h,068h,08Ch,80h
	db	000h,080h,08Dh,80h, 000h,0D1h,08Eh,80h, 000h,0EDh,08Fh,80h, 000h,073h,090h,80h
	db	000h,0C0h,092h,80h, 000h,021h,093h,80h, 000h,00Ah,094h,80h, 000h,039h,096h,80h
	db	000h,0F9h,097h,80h, 000h,0E3h,098h,80h, 000h,07Ch,099h,80h, 000h,0FEh,09Ah,80h
	db	000h,0FCh,09Bh,80h, 000h,020h,09Ch,80h, 000h,015h,09Dh,80h, 000h,02Bh,09Eh,80h
	db	000h,02Bh,09Fh,80h, 000h,07Dh,0A0h,80h, 000h,023h,0A1h,80h, 000h,0E6h,0A2h,80h
	db	000h,003h,0A3h,80h, 000h,0FEh,0A4h,80h, 000h,001h,0A5h,80h, 000h,0CBh,0A6h,80h
	db	000h,07Eh,0A7h,80h, 000h,028h,0A8h,80h, 000h,002h,0A9h,80h, 000h,0CBh,0AAh,80h
	db	000h,0BEh,0ABh,80h, 000h,023h,0ACh,80h, 000h,0E3h,0ADh,80h, 000h,0EDh,0AEh,80h
	db	000h,07Bh,0AFh,80h, 000h,0C0h,0B1h,80h, 000h,0C9h,0B2h,80h, 000h,0CDh,0B3h,80h
	db	000h,0A6h,0B4h,80h, 000h,080h,0B5h,80h, 000h,0C3h,0B6h,80h, 000h,081h,0B8h,80h
	db	032h,0CDh,01Ch,81h, 000h,069h,01Dh,81h, 000h,080h,01Eh,81h
SupLodeRunner_P1:
	db	1,6,21
	db	032h,0CDh,09Ah,83h, 000h,069h,09Bh,83h, 000h,080h,09Ch,83h, 032h,0CDh,0A4h,83h
	db	000h,069h,0A5h,83h, 000h,080h,0A6h,83h, 032h,0CDh,0AFh,83h, 000h,069h,0B0h,83h
	db	000h,080h,0B1h,83h, 032h,0CDh,0B9h,83h, 000h,069h,0BAh,83h, 000h,080h,0BBh,83h
	db	032h,0CDh,0CCh,83h, 000h,069h,0CDh,83h, 000h,080h,0CEh,83h, 032h,0CDh,0D6h,83h
	db	000h,069h,0D7h,83h, 000h,080h,0D8h,83h, 032h,0CDh,0E2h,83h, 000h,069h,0E3h,83h
	db	000h,080h,0E4h,83h
SupLodeRunner_P2:
	db	2,6,69
	db	000h,08Fh,002h,80h, 000h,080h,003h,80h, 000h,0F5h,069h,80h, 000h,0E5h,06Ah,80h
	db	000h,0D5h,06Bh,80h, 000h,0C5h,06Ch,80h, 000h,087h,06Dh,80h, 000h,05Fh,06Eh,80h
	db	000h,021h,06Fh,80h, 000h,070h,071h,80h, 000h,03Ah,072h,80h, 000h,01Eh,073h,80h
	db	000h,0F9h,074h,80h, 000h,0CDh,075h,80h, 000h,014h,076h,80h, 000h,01Ch,078h,80h
	db	000h,026h,079h,80h, 000h,078h,07Ah,80h, 000h,03Ah,07Bh,80h, 000h,01Eh,07Ch,80h
	db	000h,0F9h,07Dh,80h, 000h,0CDh,07Eh,80h, 000h,014h,07Fh,80h, 000h,0C1h,081h,80h
	db	000h,0D1h,082h,80h, 000h,0E1h,083h,80h, 000h,0F1h,084h,80h, 000h,0C9h,085h,80h
	db	000h,0EDh,08Fh,80h, 000h,073h,090h,80h, 000h,0C0h,092h,80h, 000h,021h,093h,80h
	db	000h,00Ah,094h,80h, 000h,039h,096h,80h, 000h,0F9h,097h,80h, 000h,0E3h,098h,80h
	db	000h,07Ch,099h,80h, 000h,0FEh,09Ah,80h, 000h,0FCh,09Bh,80h, 000h,020h,09Ch,80h
	db	000h,015h,09Dh,80h, 000h,02Bh,09Eh,80h, 000h,02Bh,09Fh,80h, 000h,07Dh,0A0h,80h
	db	000h,023h,0A1h,80h, 000h,0E6h,0A2h,80h, 000h,003h,0A3h,80h, 000h,0FEh,0A4h,80h
	db	000h,001h,0A5h,80h, 000h,0CBh,0A6h,80h, 000h,07Eh,0A7h,80h, 000h,028h,0A8h,80h
	db	000h,002h,0A9h,80h, 000h,0CBh,0AAh,80h, 000h,0BEh,0ABh,80h, 000h,023h,0ACh,80h
	db	000h,0E3h,0ADh,80h, 000h,0EDh,0AEh,80h, 000h,07Bh,0AFh,80h, 000h,0C0h,0B1h,80h
	db	000h,0C9h,0B2h,80h, 000h,0CDh,0B3h,80h, 000h,0A6h,0B4h,80h, 000h,080h,0B5h,80h
	db	000h,0C3h,0B6h,80h, 000h,081h,0B8h,80h, 032h,0CDh,02Eh,81h, 000h,069h,02Fh,81h
	db	000h,080h,030h,81h
SupLodeRunner_P3:
	db	3,6,3
	db	032h,0CDh,0E5h,96h, 000h,069h,0E6h,96h, 000h,080h,0E7h,96h
SupLodeRunner_P4:
	db	4,6,66
	db	000h,08Fh,002h,80h, 000h,080h,003h,80h, 000h,0F5h,069h,80h, 000h,0E5h,06Ah,80h
	db	000h,0D5h,06Bh,80h, 000h,0C5h,06Ch,80h, 000h,087h,06Dh,80h, 000h,05Fh,06Eh,80h
	db	000h,021h,06Fh,80h, 000h,070h,071h,80h, 000h,03Ah,072h,80h, 000h,01Eh,073h,80h
	db	000h,0F9h,074h,80h, 000h,0CDh,075h,80h, 000h,014h,076h,80h, 000h,01Ch,078h,80h
	db	000h,026h,079h,80h, 000h,078h,07Ah,80h, 000h,03Ah,07Bh,80h, 000h,01Eh,07Ch,80h
	db	000h,0F9h,07Dh,80h, 000h,0CDh,07Eh,80h, 000h,014h,07Fh,80h, 000h,0C1h,081h,80h
	db	000h,0D1h,082h,80h, 000h,0E1h,083h,80h, 000h,0F1h,084h,80h, 000h,0C9h,085h,80h
	db	000h,0EDh,08Fh,80h, 000h,073h,090h,80h, 000h,0C0h,092h,80h, 000h,021h,093h,80h
	db	000h,00Ah,094h,80h, 000h,039h,096h,80h, 000h,0F9h,097h,80h, 000h,0E3h,098h,80h
	db	000h,07Ch,099h,80h, 000h,0FEh,09Ah,80h, 000h,0FCh,09Bh,80h, 000h,020h,09Ch,80h
	db	000h,015h,09Dh,80h, 000h,02Bh,09Eh,80h, 000h,02Bh,09Fh,80h, 000h,07Dh,0A0h,80h
	db	000h,023h,0A1h,80h, 000h,0E6h,0A2h,80h, 000h,003h,0A3h,80h, 000h,0FEh,0A4h,80h
	db	000h,001h,0A5h,80h, 000h,0CBh,0A6h,80h, 000h,07Eh,0A7h,80h, 000h,028h,0A8h,80h
	db	000h,002h,0A9h,80h, 000h,0CBh,0AAh,80h, 000h,0BEh,0ABh,80h, 000h,023h,0ACh,80h
	db	000h,0E3h,0ADh,80h, 000h,0EDh,0AEh,80h, 000h,07Bh,0AFh,80h, 000h,0C0h,0B1h,80h
	db	000h,0C9h,0B2h,80h, 000h,0CDh,0B3h,80h, 000h,0A6h,0B4h,80h, 000h,080h,0B5h,80h
	db	000h,0C3h,0B6h,80h, 000h,081h,0B8h,80h
SupLodeRunner_P5:
	db	5,6,21
	db	032h,0CDh,09Ah,83h, 000h,069h,09Bh,83h, 000h,080h,09Ch,83h, 032h,0CDh,0A4h,83h
	db	000h,069h,0A5h,83h, 000h,080h,0A6h,83h, 032h,0CDh,0AFh,83h, 000h,069h,0B0h,83h
	db	000h,080h,0B1h,83h, 032h,0CDh,0B9h,83h, 000h,069h,0BAh,83h, 000h,080h,0BBh,83h
	db	032h,0CDh,0CCh,83h, 000h,069h,0CDh,83h, 000h,080h,0CEh,83h, 032h,0CDh,0D6h,83h
	db	000h,069h,0D7h,83h, 000h,080h,0D8h,83h, 032h,0CDh,0E2h,83h, 000h,069h,0E3h,83h
	db	000h,080h,0E4h,83h
SupLodeRunner_P6:
	db	6,6,69
	db	000h,08Fh,002h,80h, 000h,080h,003h,80h, 000h,0F5h,069h,80h, 000h,0E5h,06Ah,80h
	db	000h,0D5h,06Bh,80h, 000h,0C5h,06Ch,80h, 000h,087h,06Dh,80h, 000h,05Fh,06Eh,80h
	db	000h,021h,06Fh,80h, 000h,070h,071h,80h, 000h,03Ah,072h,80h, 000h,01Eh,073h,80h
	db	000h,0F9h,074h,80h, 000h,0CDh,075h,80h, 000h,014h,076h,80h, 000h,01Ch,078h,80h
	db	000h,026h,079h,80h, 000h,078h,07Ah,80h, 000h,03Ah,07Bh,80h, 000h,01Eh,07Ch,80h
	db	000h,0F9h,07Dh,80h, 000h,0CDh,07Eh,80h, 000h,014h,07Fh,80h, 000h,0C1h,081h,80h
	db	000h,0D1h,082h,80h, 000h,0E1h,083h,80h, 000h,0F1h,084h,80h, 000h,0C9h,085h,80h
	db	000h,0EDh,08Fh,80h, 000h,073h,090h,80h, 000h,0C0h,092h,80h, 000h,021h,093h,80h
	db	000h,00Ah,094h,80h, 000h,039h,096h,80h, 000h,0F9h,097h,80h, 000h,0E3h,098h,80h
	db	000h,07Ch,099h,80h, 000h,0FEh,09Ah,80h, 000h,0FCh,09Bh,80h, 000h,020h,09Ch,80h
	db	000h,015h,09Dh,80h, 000h,02Bh,09Eh,80h, 000h,02Bh,09Fh,80h, 000h,07Dh,0A0h,80h
	db	000h,023h,0A1h,80h, 000h,0E6h,0A2h,80h, 000h,003h,0A3h,80h, 000h,0FEh,0A4h,80h
	db	000h,001h,0A5h,80h, 000h,0CBh,0A6h,80h, 000h,07Eh,0A7h,80h, 000h,028h,0A8h,80h
	db	000h,002h,0A9h,80h, 000h,0CBh,0AAh,80h, 000h,0BEh,0ABh,80h, 000h,023h,0ACh,80h
	db	000h,0E3h,0ADh,80h, 000h,0EDh,0AEh,80h, 000h,07Bh,0AFh,80h, 000h,0C0h,0B1h,80h
	db	000h,0C9h,0B2h,80h, 000h,0CDh,0B3h,80h, 000h,0A6h,0B4h,80h, 000h,080h,0B5h,80h
	db	000h,0C3h,0B6h,80h, 000h,081h,0B8h,80h, 032h,0CDh,02Eh,81h, 000h,069h,02Fh,81h
	db	000h,080h,030h,81h
SupLodeRunner_P8:
	db	8,6,69
	db	000h,08Fh,002h,80h, 000h,080h,003h,80h, 000h,0F5h,069h,80h, 000h,0E5h,06Ah,80h
	db	000h,0D5h,06Bh,80h, 000h,0C5h,06Ch,80h, 000h,087h,06Dh,80h, 000h,05Fh,06Eh,80h
	db	000h,021h,06Fh,80h, 000h,070h,071h,80h, 000h,03Ah,072h,80h, 000h,01Eh,073h,80h
	db	000h,0F9h,074h,80h, 000h,0CDh,075h,80h, 000h,014h,076h,80h, 000h,01Ch,078h,80h
	db	000h,026h,079h,80h, 000h,078h,07Ah,80h, 000h,03Ah,07Bh,80h, 000h,01Eh,07Ch,80h
	db	000h,0F9h,07Dh,80h, 000h,0CDh,07Eh,80h, 000h,014h,07Fh,80h, 000h,0C1h,081h,80h
	db	000h,0D1h,082h,80h, 000h,0E1h,083h,80h, 000h,0F1h,084h,80h, 000h,0C9h,085h,80h
	db	000h,0EDh,08Fh,80h, 000h,073h,090h,80h, 000h,0C0h,092h,80h, 000h,021h,093h,80h
	db	000h,00Ah,094h,80h, 000h,039h,096h,80h, 000h,0F9h,097h,80h, 000h,0E3h,098h,80h
	db	000h,07Ch,099h,80h, 000h,0FEh,09Ah,80h, 000h,0FCh,09Bh,80h, 000h,020h,09Ch,80h
	db	000h,015h,09Dh,80h, 000h,02Bh,09Eh,80h, 000h,02Bh,09Fh,80h, 000h,07Dh,0A0h,80h
	db	000h,023h,0A1h,80h, 000h,0E6h,0A2h,80h, 000h,003h,0A3h,80h, 000h,0FEh,0A4h,80h
	db	000h,001h,0A5h,80h, 000h,0CBh,0A6h,80h, 000h,07Eh,0A7h,80h, 000h,028h,0A8h,80h
	db	000h,002h,0A9h,80h, 000h,0CBh,0AAh,80h, 000h,0BEh,0ABh,80h, 000h,023h,0ACh,80h
	db	000h,0E3h,0ADh,80h, 000h,0EDh,0AEh,80h, 000h,07Bh,0AFh,80h, 000h,0C0h,0B1h,80h
	db	000h,0C9h,0B2h,80h, 000h,0CDh,0B3h,80h, 000h,0A6h,0B4h,80h, 000h,080h,0B5h,80h
	db	000h,0C3h,0B6h,80h, 000h,081h,0B8h,80h, 032h,0CDh,02Eh,81h, 000h,069h,02Fh,81h
	db	000h,080h,030h,81h
SupLodeRunner_PA:
	db	10,6,69
	db	000h,08Fh,002h,80h, 000h,080h,003h,80h, 000h,0F5h,069h,80h, 000h,0E5h,06Ah,80h
	db	000h,0D5h,06Bh,80h, 000h,0C5h,06Ch,80h, 000h,087h,06Dh,80h, 000h,05Fh,06Eh,80h
	db	000h,021h,06Fh,80h, 000h,070h,071h,80h, 000h,03Ah,072h,80h, 000h,01Eh,073h,80h
	db	000h,0F9h,074h,80h, 000h,0CDh,075h,80h, 000h,014h,076h,80h, 000h,01Ch,078h,80h
	db	000h,026h,079h,80h, 000h,078h,07Ah,80h, 000h,03Ah,07Bh,80h, 000h,01Eh,07Ch,80h
	db	000h,0F9h,07Dh,80h, 000h,0CDh,07Eh,80h, 000h,014h,07Fh,80h, 000h,0C1h,081h,80h
	db	000h,0D1h,082h,80h, 000h,0E1h,083h,80h, 000h,0F1h,084h,80h, 000h,0C9h,085h,80h
	db	000h,0EDh,08Fh,80h, 000h,073h,090h,80h, 000h,0C0h,092h,80h, 000h,021h,093h,80h
	db	000h,00Ah,094h,80h, 000h,039h,096h,80h, 000h,0F9h,097h,80h, 000h,0E3h,098h,80h
	db	000h,07Ch,099h,80h, 000h,0FEh,09Ah,80h, 000h,0FCh,09Bh,80h, 000h,020h,09Ch,80h
	db	000h,015h,09Dh,80h, 000h,02Bh,09Eh,80h, 000h,02Bh,09Fh,80h, 000h,07Dh,0A0h,80h
	db	000h,023h,0A1h,80h, 000h,0E6h,0A2h,80h, 000h,003h,0A3h,80h, 000h,0FEh,0A4h,80h
	db	000h,001h,0A5h,80h, 000h,0CBh,0A6h,80h, 000h,07Eh,0A7h,80h, 000h,028h,0A8h,80h
	db	000h,002h,0A9h,80h, 000h,0CBh,0AAh,80h, 000h,0BEh,0ABh,80h, 000h,023h,0ACh,80h
	db	000h,0E3h,0ADh,80h, 000h,0EDh,0AEh,80h, 000h,07Bh,0AFh,80h, 000h,0C0h,0B1h,80h
	db	000h,0C9h,0B2h,80h, 000h,0CDh,0B3h,80h, 000h,0A6h,0B4h,80h, 000h,080h,0B5h,80h
	db	000h,0C3h,0B6h,80h, 000h,081h,0B8h,80h, 032h,0CDh,02Eh,81h, 000h,069h,02Fh,81h
	db	000h,080h,030h,81h
SupLodeRunner_PC:
	db	12,6,69
	db	000h,08Fh,002h,80h, 000h,080h,003h,80h, 000h,0F5h,069h,80h, 000h,0E5h,06Ah,80h
	db	000h,0D5h,06Bh,80h, 000h,0C5h,06Ch,80h, 000h,087h,06Dh,80h, 000h,05Fh,06Eh,80h
	db	000h,021h,06Fh,80h, 000h,070h,071h,80h, 000h,03Ah,072h,80h, 000h,01Eh,073h,80h
	db	000h,0F9h,074h,80h, 000h,0CDh,075h,80h, 000h,014h,076h,80h, 000h,01Ch,078h,80h
	db	000h,026h,079h,80h, 000h,078h,07Ah,80h, 000h,03Ah,07Bh,80h, 000h,01Eh,07Ch,80h
	db	000h,0F9h,07Dh,80h, 000h,0CDh,07Eh,80h, 000h,014h,07Fh,80h, 000h,0C1h,081h,80h
	db	000h,0D1h,082h,80h, 000h,0E1h,083h,80h, 000h,0F1h,084h,80h, 000h,0C9h,085h,80h
	db	000h,0EDh,08Fh,80h, 000h,073h,090h,80h, 000h,0C0h,092h,80h, 000h,021h,093h,80h
	db	000h,00Ah,094h,80h, 000h,039h,096h,80h, 000h,0F9h,097h,80h, 000h,0E3h,098h,80h
	db	000h,07Ch,099h,80h, 000h,0FEh,09Ah,80h, 000h,0FCh,09Bh,80h, 000h,020h,09Ch,80h
	db	000h,015h,09Dh,80h, 000h,02Bh,09Eh,80h, 000h,02Bh,09Fh,80h, 000h,07Dh,0A0h,80h
	db	000h,023h,0A1h,80h, 000h,0E6h,0A2h,80h, 000h,003h,0A3h,80h, 000h,0FEh,0A4h,80h
	db	000h,001h,0A5h,80h, 000h,0CBh,0A6h,80h, 000h,07Eh,0A7h,80h, 000h,028h,0A8h,80h
	db	000h,002h,0A9h,80h, 000h,0CBh,0AAh,80h, 000h,0BEh,0ABh,80h, 000h,023h,0ACh,80h
	db	000h,0E3h,0ADh,80h, 000h,0EDh,0AEh,80h, 000h,07Bh,0AFh,80h, 000h,0C0h,0B1h,80h
	db	000h,0C9h,0B2h,80h, 000h,0CDh,0B3h,80h, 000h,0A6h,0B4h,80h, 000h,080h,0B5h,80h
	db	000h,0C3h,0B6h,80h, 000h,081h,0B8h,80h, 032h,0CDh,012h,81h, 000h,069h,013h,81h
	db	000h,080h,014h,81h
SupLodeRunner_PD:
	db	13,6,3
	db	032h,0CDh,0E5h,96h, 000h,069h,0E6h,96h, 000h,080h,0E7h,96h
SupLodeRunner_PE:
	db	14,6,66
	db	000h,08Fh,002h,80h, 000h,080h,003h,80h, 000h,0F5h,069h,80h, 000h,0E5h,06Ah,80h
	db	000h,0D5h,06Bh,80h, 000h,0C5h,06Ch,80h, 000h,087h,06Dh,80h, 000h,05Fh,06Eh,80h
	db	000h,021h,06Fh,80h, 000h,070h,071h,80h, 000h,03Ah,072h,80h, 000h,01Eh,073h,80h
	db	000h,0F9h,074h,80h, 000h,0CDh,075h,80h, 000h,014h,076h,80h, 000h,01Ch,078h,80h
	db	000h,026h,079h,80h, 000h,078h,07Ah,80h, 000h,03Ah,07Bh,80h, 000h,01Eh,07Ch,80h
	db	000h,0F9h,07Dh,80h, 000h,0CDh,07Eh,80h, 000h,014h,07Fh,80h, 000h,0C1h,081h,80h
	db	000h,0D1h,082h,80h, 000h,0E1h,083h,80h, 000h,0F1h,084h,80h, 000h,0C9h,085h,80h
	db	000h,0EDh,08Fh,80h, 000h,073h,090h,80h, 000h,0C0h,092h,80h, 000h,021h,093h,80h
	db	000h,00Ah,094h,80h, 000h,039h,096h,80h, 000h,0F9h,097h,80h, 000h,0E3h,098h,80h
	db	000h,07Ch,099h,80h, 000h,0FEh,09Ah,80h, 000h,0FCh,09Bh,80h, 000h,020h,09Ch,80h
	db	000h,015h,09Dh,80h, 000h,02Bh,09Eh,80h, 000h,02Bh,09Fh,80h, 000h,07Dh,0A0h,80h
	db	000h,023h,0A1h,80h, 000h,0E6h,0A2h,80h, 000h,003h,0A3h,80h, 000h,0FEh,0A4h,80h
	db	000h,001h,0A5h,80h, 000h,0CBh,0A6h,80h, 000h,07Eh,0A7h,80h, 000h,028h,0A8h,80h
	db	000h,002h,0A9h,80h, 000h,0CBh,0AAh,80h, 000h,0BEh,0ABh,80h, 000h,023h,0ACh,80h
	db	000h,0E3h,0ADh,80h, 000h,0EDh,0AEh,80h, 000h,07Bh,0AFh,80h, 000h,0C0h,0B1h,80h
	db	000h,0C9h,0B2h,80h, 000h,0CDh,0B3h,80h, 000h,0A6h,0B4h,80h, 000h,080h,0B5h,80h
	db	000h,0C3h,0B6h,80h, 000h,081h,0B8h,80h
SupLodeRunner_PF:
	db	15,6,21
	db	032h,0CDh,078h,96h, 000h,069h,079h,96h, 000h,080h,07Ah,96h, 032h,0CDh,085h,96h
	db	000h,069h,086h,96h, 000h,080h,087h,96h, 032h,0CDh,098h,96h, 000h,069h,099h,96h
	db	000h,080h,09Ah,96h, 032h,0CDh,0A2h,96h, 000h,069h,0A3h,96h, 000h,080h,0A4h,96h
	db	032h,0CDh,0FFh,96h, 000h,069h,000h,97h, 000h,080h,001h,97h, 032h,0CDh,09Eh,97h
	db	000h,069h,09Fh,97h, 000h,080h,0A0h,97h, 032h,0CDh,0A7h,97h, 000h,069h,0A8h,97h
	db	000h,080h,0A9h,97h
; --------
TheMoG:
	db	0,0,32
	db	060h,068h,005h,81h, 060h,068h,017h,81h, 060h,068h,071h,81h, 060h,068h,08Ah,81h
	db	060h,068h,0A3h,81h, 060h,068h,0BCh,81h, 060h,068h,0D5h,81h, 060h,068h,0EEh,81h
	db	060h,068h,0F9h,81h, 060h,068h,049h,8Eh, 060h,068h,05Bh,8Eh, 080h,070h,009h,81h
	db	080h,070h,01Dh,81h, 080h,070h,078h,81h, 080h,070h,091h,81h, 080h,070h,0AAh,81h
	db	080h,070h,0C3h,81h, 080h,070h,0DCh,81h, 080h,070h,004h,82h, 080h,070h,00Fh,82h
	db	080h,070h,04Dh,8Eh, 080h,070h,064h,8Eh, 0A0h,078h,07Fh,81h, 0A0h,078h,098h,81h
	db	0A0h,078h,0B1h,81h, 0A0h,078h,0CAh,81h, 0A0h,078h,0E3h,81h, 0A0h,078h,01Ah,82h
	db	0A0h,078h,025h,82h, 0A0h,078h,030h,82h, 0A0h,078h,03Bh,82h, 0A0h,078h,046h,82h
; --------
USAS:
	db	0,0,16
	db	060h,068h,04Dh,80h, 060h,068h,05Fh,80h, 060h,068h,06Dh,94h, 060h,068h,095h,94h
	db	060h,068h,0ECh,96h, 080h,070h,051h,80h, 080h,070h,065h,80h, 080h,070h,072h,94h
	db	080h,070h,09Bh,94h, 080h,070h,0F6h,96h, 0A0h,078h,055h,80h, 0A0h,078h,06Bh,80h
	db	0A0h,078h,078h,94h, 0A0h,078h,0A1h,94h, 0A0h,078h,0BBh,94h, 0A0h,078h,000h,97h
; --------
USAS_a:
	db	0,0,16
	db	060h,068h,02Dh,80h, 060h,068h,03Fh,80h, 060h,068h,04Eh,94h, 060h,068h,076h,94h
	db	060h,068h,0CFh,96h, 080h,070h,031h,80h, 080h,070h,045h,80h, 080h,070h,053h,94h
	db	080h,070h,07Ch,94h, 080h,070h,0D9h,96h, 0A0h,078h,035h,80h, 0A0h,078h,04Bh,80h
	db	0A0h,078h,059h,94h, 0A0h,078h,082h,94h, 0A0h,078h,09Ch,94h, 0A0h,078h,0E3h,96h
; --------
;, 060h,070h,0h,h
;, 080h,090h,0h,h
;, 0A0h,0B0h,0h,h