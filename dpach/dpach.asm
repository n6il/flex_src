	TTL	DPACH Disk Patcher

* COPYRIGHT (C) 1978-2022 BY
* EVENSON CONSULTING SYSTEMS
* 1410 OAKDALE ROAD
* CANTON, GA. 30114
*
* VERSION 2 - UPDATED TO DISPLAY '.' FOR DEL ($7F) SINCE NOT ALL
* TERMINALS DISPLAY A PLACEHOLDER FOR DEL

* FLEX ADDRESSES

BASE	EQU	$A000		Uncomment for 6800 machines
*BASE	EQU	$C000		Uncomment for 6809 machines

IO	EQU	$8004		Uncomment for SWTPC 6800
*IO	EQU	$F000		Uncomment for Altair 680
*IO	EQU	$E004		Uncomment for SWTPC 6809

FPSFLG	EQU	BASE+$0C09
ESCRTN	EQU	BASE+$0C16

FLEX	EQU	BASE+$0D03
CHROUT	EQU	BASE+$0D18
PCRLF	EQU	BASE+$0D24
OUTHEX	EQU	BASE+$0D3C
OUTADR	EQU	BASE+$0D45

FMS	EQU	BASE+$1406

* I/O equates

CONSTAT	EQU	IO
CONDATA	EQU	IO+1

* Code Start

	ORG	$100

START	BRA	START2
	FCB	2		Version 2
START2	JMP	START1

* TEXT STRINGS

FMAINT	FCB	$1B,'*
STATLN	FCC	'DPACH (disk sector read-write utility) '
	FCC	'Drive ['
ADRVNO	FCC	'0] Track ['
ATRKNO	FCC	'00] Sector ['
ASECNO	FCC	'00]' 
	FCB	4

HPOS	FCB	$1b,'=,0,0,4
APOS	FCB	$1b,'=,0,0,4

CRMSG	FCB	$d,$1b,'T,4

GOTO0	FCB	$1b,$3D,32,32,$1b,'T,4
GOTO1	FCB	$1b,$3D,33,32,$1b,'T,4

INQUE	FCB	$1b,$3D,52,32,$1b,'T
	FCC	'Enter command (R)ead,(W)rite,(H)ex,(A)scii,(P)aint screen'
	FCC	',(Q)uit' 
	FCB	$d,$a,$1b,'T
	FCC	'              (D)rive,(T)rack,(S)ector,(+) or (-) :> '
	FCB	$1b,'T,4

DRVM	FCB	$1b,$3D,54,32
	FCC	'Drive? '
	FCB	$1b,'T,4

TRKM	FCB	$1b,$3D,54,32
	FCC	'Track? '
	FCB	$1b,'T,4

SECTM	FCB	$1b,$3D,54,32
	FCC	'Sector? '
	FCB	$1b,'T,4

FERRM	FCB	$1b,$3D,54,32
	FCC	'ERROR - COMMAND ABORTED'
	FCB	$1b,'T,4

BYTEM	FCB	$1b,'=,54,32
	FCC	'Enter offset :> '
	FCB	$1b,'T,4

CLR22	FCB	$1b,'=,54,32,$1b,'T,4

* MEMORY ASSIGNMENTS

TBUFF	RMB	2
CRFLAG	RMB	1
OCNT	RMB	1
VSCTNO	RMB	1
DRVNO	FCB	0
TRK	FCB	0
SECT	FCB	0
BUFPTR	RMB	2

OFFSET	RMB	1
ROW	RMB	1
HEXCOL	RMB	1
ASCCOL	RMB	1
PSFLG	RMB	1

* Actual code start

START1	LDX	#FMAINT		point to signon message
	JSR	TXOUT		print to console
	LDX	#START		point to start of program as esc entry
	STX	ESCRTN		set up escape key entry
	LDAA	FPSFLG		get current value of FLEX pause flag
	STAA	PSFLG		save it
	LDAA	#$00		set new value = off
	STAA	FPSFLG
	BRA	BEGIN

* Loop point when repainting the buffer to the screen is required

MLOOP	JSR	PRINT

* Loop point when repaint not necessary

BEGIN	LDX	#CLR22
	JSR	TXOUT
	LDX	#INQUE
	JSR	TXOUT
	JSR	CHARIN
	JSR	UCASE

	CMPA	#'R		READ A SECTOR?
	BNE	MCHK2		no -

	JSR	READ		yes - do read
	JMP	MLOOP		loop on command input

MCHK2	CMPA	#'H		CHANGE DATA?
	BNE	MCHK3		no -

	JSR	CHANGE		yes - do modify memory
	JMP	BEGIN		loop on command input

MCHK3	CMPA	#'Q		RETURN TO FLEX?
	BNE	MCHK4		no - 

	JSR	PCRLF
	LDAA	PSFLG
	STAA	FPSFLG
	JMP	FLEX		yes - return to flex

MCHK4	CMPA	#'W		WRITE A SECTOR?
	BNE	MCHK5		no - 

	JSR	WRITE		yes - do write sector
	JMP	BEGIN		loop on command input

MCHK5	CMPA	#'+		increment to next sector?
	BNE	MCHK6		no -

	LDAA	SECT		
	INCA
	STAA	SECT
	JSR	DOSTAT
	JMP	BEGIN		loop on command input

MCHK6	CMPA	#'-		decrement sector number?
	BNE	MCHK7		no - 

	LDAA	SECT
	DECA
	STAA	SECT
	JSR	DOSTAT
	JMP	BEGIN

MCHK7	CMPA	#'D		log different drive?
	BNE	MCHK8		no - 

	JSR	GETDRV
	JSR	DOSTAT
	JMP	BEGIN

MCHK8	CMPA	#'T		Track change?
	BNE	MCHK9		no - 

	JSR	GETTRK
	JSR	DOSTAT
	JMP	BEGIN

MCHK9	CMPA	#'S		Sector change?
	BNE	MCHKA		no - 

	JSR	GETSCT
	JSR	DOSTAT
	JMP	BEGIN

MCHKA	CMPA	#'A
	BNE	MCHKB

	JSR	ASCIIC
	JMP	BEGIN

MCHKB	CMPA	#'P		Paint buffer
	BNE	MERR

	JMP	MLOOP

MERR	JMP	BEGIN

* SECTOR READ ROUTINE

READ	LDX	#FCB		IR = ADR OF FILE CNTRL BLK
	LDAA	DRVNO		SET DRIVE #
	STAA	3,X
	LDAA	TRK		SET TRACK #
	STAA	30,X
	LDAA	SECT		SET SECTOR #
	STAA	31,X
	LDAA	#9		SET FMS CODE TO SECTOR READ
	STAA	0,X
	JSR	FMS		READ THE SECTOR
	BEQ	RDXIT		JUMP IF ALL IS WELL

	JSR	FERR		PRINT ERROR MESSAGE
RDXIT	RTS

* ROUTINE TO GET DRIVE, TRACK AND SECTOR NUMBERS
*
*   NOTE: IF A CARRIAGE RETURN IS ENTERED FOR ANY
*         INPUT THE OLD NUMBERS ARE PRESERVED

GETDRV	LDX	#DRVM		PRINT DRIVE PROMPT
	JSR	TXOUT
	CLR	CRFLAG		CLEAR CARRIAGE RETURN FLAG
	CLRB			BR=0
	JSR	GET1HX		GET DRIVE #
	TST	CRFLAG		CARR RETN ENTERED?
	BNE	GDXIT		.JUMP IF YES

	CMPA	#4		VALID DRIVE #?
	BHS	GETDRV		.JUMP IF NOT
	STAA	DRVNO		SAVE WHEN VALID
	JSR	DOSTAT		redisplay status line
GDXIT	RTS

GETTRK	LDX	#TRKM		GET CORRECT TRACK #
	JSR	TXOUT
	CLR	CRFLAG
	JSR	GETHEX		GET 2 HEX DIGITS
	TST	CRFLAG
	BNE	GTXIT

	STAA	TRK
	JSR	DOSTAT
GTXIT	RTS

GETSCT	LDX	#SECTM		GET A VALID SECTOR NUMBER
	JSR	TXOUT
	CLR	CRFLAG
	JSR	GETHEX
	TST	CRFLAG
	BNE	GSXIT

	STAA	SECT
	JSR	DOSTAT
GSXIT	RTS

* GET 1 HEX BYTE OF INPUT

GETHEX	JSR	CHARIN		GET A CHAR
	JSR	UCASE
	CMPA	#$0D		WAS IT A CARR RETN?
	BNE	CNVRT1		.JUMP IF NOT
	INC	CRFLAG
	BRA	GOTHEX

CNVRT1	JSR	C2HEX		CONVERT INPUT DIGIT TO HEX
	ASLA			SHIFT IT TO THE
	ASLA			.HI-ORD NIBBLE
	ASLA
	ASLA
	TAB			SAVE IN BR FOR A WHILE
GET1HX	JSR	CHARIN		GET NEXT CHAR
	JSR	UCASE
	CMPA	#$0D		.(ENTRY POINT FOR 1 DIGIT)
	BEQ	CRTN
	CMPA	#$20
	BNE	CNVRT2
	JSR	OUTS
	LSRB
	LSRB
	LSRB
	LSRB
	TBA
	BRA	GOTHEX
CRTN	INC	CRFLAG
	BRA	GOTHEX

CNVRT2	JSR	C2HEX
	ABA			COMBINE BOTH DIGITS
GOTHEX	RTS

* CONVERT FROM CHAR TO HEX

C2HEX	SUBA	#$30		SUBT ASCII NUMERIC BASE
	BGE	CONVR2		JUMP IF ZERO OR GREATER
BADCHR	LDAA	#$08		PRINT ERROR INDICATION
	JSR	CHROUT
	JSR	CHARIN		GET ANOTHER CHAR
	JSR	UCASE
	CMPA	#$0D		ANOTHER CARR RETN?
	BNE	C2HEX		.JUMP IF NOT
	INC	CRFLAG		SET CARR RETN FLAG
	BRA	C2XIT

CONVR2	CMPA	#$09		WAS IT 0-9?
	BLE	C2XIT		.JUMP IF YES
	SUBA	#$07		SUBT ASCII LETTER DELTA
	BLT	BADCHR		JUMP IF LESS THAN "A"
	CMPA	#$0F		WAS IT A-F?
	BGT	BADCHR		.JUMP IF NOT
C2XIT	RTS

* PRINT A FILE ERROR MESSAGE

FERR	JSR	PCRLF		RETN CARR
	LDX	#FERRM
	JSR	TXOUT
	TAB
	JSR	OUTHL		PRINT HI-ORD NIBBLE
	TBA			RESTORE AR VALUE
	JSR	OUTHR		PRINT LO-ORD NIBBLE
	RTS

* CHANGE BYTES IN THE SECTOR BUFFER AREA WITH HEX INPUT

CHANGE	LDX	#BYTEM		GET THE BYTE TO CHANGE
	JSR	TXOUT
	CLR	CRFLAG
	JSR	GETHEX		GET THE BYTE NUMBER
	TST	CRFLAG
	BEQ	C1
	JMP	CXIT

C1	STAA	OFFSET

PNTADR	LDX	#SBUFF
	LDAA	OFFSET
	DEX
	INCA

CINX	INX			SET IR TO THE BYTE NUMBER
	DECA
	BNE	CINX

	STX	BUFPTR
	JSR	CALCRC		set up cursor positioning strings
	LDX	#HPOS
	JSR	TXOUT
	JSR	CHARIN		GET AN ENTRY
	JSR	UCASE
	CMPA	#$0D		WAS IT A CARR RETN?
	BNE	P2
	JMP	CXIT

P2	CMPA	#$0C		WAS IT A CURSOR FORWARD?
	BEQ	HAGAIN

	CMPA	#$04		^D moves to right
	BEQ	HAGAIN

	CMPA	#$08		BS moves left
	BEQ	HPREV

	CMPA	#$13		^S moves left also
	BEQ	HPREV

	CMPA	#$18		^X moves down
	BEQ	HDOWN

	CMPA	#$05		^E moves up
	BEQ	HUP

	CLR	CRFLAG		SET UP FOR SECOND DIGIT
	JSR	CNVRT1		COMPLETE THE BYTE INPUT
	TST	CRFLAG
	BNE	CHANGE

	LDX	BUFPTR		get buffer pointer
	STAA	0,X		STORE BYTE IN SECTOR BUFF
	LDX	#APOS		position to Ascii for this byte
	JSR	TXOUT		do cursor position
	LDX	BUFPTR		get pointer back
	LDAA	0,X		get character
	ANDA	#$7F		print those above $80 also
	CMPA	#$7F		DEL does not display on some terminals
	BEQ	DSPDOT
	CMPA	#$20		see if printable
	BHS	P3		OK to display

DSPDOT	LDAA	#'.		substitute a period

P3	JSR	CHROUT

HAGAIN	LDAA	OFFSET
	INCA
	STAA	OFFSET
	JMP	PNTADR

HPREV	LDAA	OFFSET
	DECA
	STAA	OFFSET
	JMP	PNTADR

HDOWN	LDAA	OFFSET
	ADDA	#$10
	STAA	OFFSET
	JMP	PNTADR

HUP	LDAA	OFFSET
	SUBA	#$10
	STAA	OFFSET
	JMP	PNTADR

CXIT	RTS

* Change contents of sector buffer using ASCII input

ASCIIC	LDX	#BYTEM		GET THE BYTE TO CHANGE
	JSR	TXOUT
	CLR	CRFLAG
	JSR	GETHEX		GET THE BYTE NUMBER
	TST	CRFLAG
	BEQ	A1
	JMP	ACXIT

A1	STAA	OFFSET

APNTADR	LDX	#SBUFF
	LDAA	OFFSET
	DEX
	INCA

ACINX	INX			SET IR TO THE BYTE NUMBER
	DECA
	BNE	ACINX

	STX	BUFPTR
	JSR	CALCRC		set up cursor positioning strings
	LDX	#APOS
	JSR	TXOUT
	JSR	CHARIN		GET AN ENTRY
	CMPA	#$0D		WAS IT A CARR RETN?
	BNE	A2
	JMP	ACXIT

A2	CMPA	#$0C		WAS IT A CURSOR FORWARD?
	BEQ	AAGAIN

	CMPA	#$04		^D moves to right
	BEQ	AAGAIN

	CMPA	#$08		BS moves left
	BEQ	APREV

	CMPA	#$13		^S moves left also
	BEQ	APREV

	CMPA	#$18		^X moves down
	BEQ	ADOWN

	CMPA	#$05		^E moves up
	BEQ	AUP

	LDX	BUFPTR		get buffer pointer
	STAA	0,X		STORE BYTE IN SECTOR BUFF
	LDX	#HPOS		position to Hex for this byte
	JSR	TXOUT		do cursor position
	LDX	BUFPTR		get pointer back
	JSR	OUTHEX

AAGAIN	LDAA	OFFSET
	INCA
	STAA	OFFSET
	JMP	APNTADR

APREV	LDAA	OFFSET
	DECA
	STAA	OFFSET
	JMP	APNTADR

ADOWN	LDAA	OFFSET
	ADDA	#$10
	STAA	OFFSET
	JMP	APNTADR

AUP	LDAA	OFFSET
	SUBA	#$10
	STAA	OFFSET
	JMP	APNTADR

ACXIT	RTS

* Calculate row and column for offset

CALCRC	LDAA	OFFSET
	ASRA
	ASRA
	ASRA
	ASRA			get high nibble into low nibble
	ANDA	#$0F		strip off any junk
	ADDA	#$02		calc row
	STAA	ROW		save row
	ADDA	#$20		add in offset for cursor pos
	STAA	HPOS+2
	STAA	APOS+2
	LDAA	OFFSET
	ANDA	#$0F		only low nibble for column calc
	TAB			put in B for * 3
	ABA			A = A + B
	ABA			A = A + B (original A * 3)
	ADDA	#$04		offset to hex column
	STAA	HEXCOL
	ADDA	#$20
	STAA	HPOS+3
	LDAA	OFFSET		get offset again
	ANDA	#$0F
	ADDA	#54		offset to ascii stuff
	STAA	ASCCOL		save it
	ADDA	#$20
	STAA	APOS+3
	RTS


* WRITE A SECTOR TO DISK FROM THE SECTOR BUFFER AREA

WRITE	LDX	#FCB		POINT TO FILE CNTRL BLK
	LDAA	DRVNO		SET UP THE DRIVE NO
	STAA	3,X
	LDAA	TRK		SET UP THE TRACK NO
	STAA	30,X
	LDAA	SECT		SET UP THE SECTOR NO
	STAA	31,X
	LDAA	#$0A		INIT FMS TO WRITE SECTOR
	STAA	0,X
	JSR	FMS		WRITE THE SECTOR
	BEQ	WXIT		JUMP IF ALL OK

	JSR	FERR		PRINT ERROR MESSAGE
WXIT	RTS

* OUTPUT THE DATA IN THE SECTOR BUFFER IN
* HEX AND ASCII.

PRINT	LDX	#GOTO1
	JSR	TXOUT
	LDX	#SBUFF		POINT TO THE DATA

PLOOP1	STX	TBUFF		SAVE THE IR
	JSR	PCRLF		RETN THE CARR
	LDAA	#16		SET OUTPUT COUNT TO 16
	STAA	OCNT
	LDX	#TBUFF+1
	JSR	OUTHEX
	LDX	TBUFF
	JSR	OUTS
	JSR	OUTS
PLOOP2	JSR	OUTHEX		PRINT BYTE
	JSR	OUTS		PRINT A SPACE
	INX			POINT TO NEXT BYTE
	CPX	#SBEND		REACHED END OF SECTOR BUFF?
	BEQ	PLOOP3		.JUMP IF YES

	DEC	OCNT		SUBT 1 FROM THE OUTPUT COUNT
	BNE	PLOOP2		JUMP IF NOT ZERO

PLOOP3	LDX	TBUFF		WRITE 16 ASCII CHARACTERS
	JSR	OUTS		PRINT 2 SPACES
	JSR	OUTS
	LDAA	#16		SET OUTPUT COUNT
	STAA	OCNT

PLOOP4	LDAA	0,X		GET A CHAR
	ANDA	#$7F
	CMPA	#$7F		DEL DOES NOT DISPLAY ON SOME TERMINALS
	BEQ	NOTOK
	CMPA	#$20		WAS IT A SPACE OR GREATER?
	BHS	OUTCHR		.JUMP IF YES

NOTOK	LDAA	#'.		DOT = NOT PRINTABLE

OUTCHR	JSR	CHROUT		PRINT THE CHAR
	INX			POINT TO NEXT POS IN BUFF
	CPX	#SBEND		REACHED THE END?
	BEQ	PXIT		.JUMP IF YES

	DEC	OCNT		SUBT 1 FROM OUTPUT COUNT
	BNE	PLOOP4		JUMP IF NOT ZERO YET

	BRA	PLOOP1

PXIT	JSR	PCRLF		RETN CARR AGAIN
	RTS

* PRINT A STRING THRU FLEX, NO AUTO CARR RETN!

TLOOP	JSR	CHROUT		PRINT A CHAR
	INX			POINT TO NEXT CHAR
TXOUT	LDAA	0,X		GET THE CHAR
	CMPA	#$04		AND <EOT>?
	BNE	TLOOP		.JUMP IF NOT

	RTS

* PRINT A SPACE THRU FLEX

OUTS	PSHA			SAVE AR CONTENTS
	LDAA	#$20
	JSR	CHROUT
	PULA			RESTORE AR
	RTS

* OUTPUT BYTE IN "A"

OUTHL	ASRA
	ASRA
	ASRA
	ASRA
OUTHR	ANDA	#$0F
	ADDA	#$30
	CMPA	#$39
	BLS	OUTHLR
	ADDA	#$07
OUTHLR	JSR	CHROUT
	RTS

* input character with Upper case conversion

UCASE	ANDA	#$7F
	CMPA	#$41
	BLO	NOC		no conversio
	CMPA	#$7B
	BLO	DOC		do conversion
NOC	RTS

DOC	ANDA	#$DF		make upper case
	RTS

DOSTAT	JSR	CASCII
	LDX	#GOTO0
	JSR	TXOUT
	LDX	#STATLN
	JSR	TXOUT
	RTS

CASCII	LDAA	DRVNO
	ADDA	#$30
	STAA	ADRVNO
	LDX	#ASECNO
	LDAA	SECT
	JSR	CVTH
	LDX	#ATRKNO
	LDAA	TRK
	JSR	CVTH
	RTS

CVTH	PSHA			save contents of A
	ANDA	#$F0
	ASRA
	ASRA
	ASRA
	ASRA
	BSR	CVTH1
	INX
	PULA

CVTH1	ANDA	#$0F
	ADDA	#$30
	CMPA	#$3A
	BLO	CVTH2
	ADDA	#7
CVTH2	STAA	0,X
	RTS

* Input a character without echo if not printable ascii

CHARIN	PSHB			save B reg
CKSTAT	LDAB	CONSTAT		get console status
	ANDB	#$01		RDA?
	BEQ	CKSTAT		no - wait for it
	PULB
	LDAA	CONDATA		get data byte
	ANDA	#$7F		Strip parity if present
	CMPA	#$1F
	BHI	CIRET		not control character
	RTS			do not echo control characters

CIRET	JSR	CHROUT
	RTS

	ORG	$FC0

FCB	RMB	320
SBUFF	EQU	FCB+64		SECTOR BUFFER AREA
SBEND	EQU	*		END OF SECTOR BUFFER+1

	END	START
