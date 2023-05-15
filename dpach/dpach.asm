            TTL     DPACH Disk Patcher
*
* COPYRIGHT (C) 1978-2022 BY
* EVENSON CONSULTING SYSTEMS
* 1410 OAKDALE ROAD
* CANTON, GA. 30114
*
* External References
*
*   These are for 6800 FLEX
*
*       change $AC to $CC for 6809 FLEX
*       change $AD to $CD for 6809 FLEX
*       change $B4 to $D4 for 6809 FLEX
*
TTYPS      EQU      $AC09
ESCRET     EQU      $AC16
WARMS      EQU      $AD03
PUTCHR     EQU      $AD18
PCRLF      EQU      $AD24
ADDBX      EQU      $AD36
OUTHEX     EQU      $AD3C
FMSINT     EQU      $B400
FMSCLS     EQU      $B403
FMSCALL    EQU      $B406

ACIAPRT    EQU      $F000           Change this to your ACIA status port address

           ORG      $0100

START      BRA      MAIN

           FCB      $01
MAIN       JMP      BEGIN


* Strings used in the program

CLRSCRN    FCB      $1B,$2A
STATLN     FCC      'Dpach (disk sector read-write utility) Drive ['
DRVNUM     FCC      '0] Track ['
TRKNUM     FCC      '00] Sector ['
SECNUM     FCC      '00]'
           FCB      $04

* used to poistion the cursor

*     set line number at CP0Y
*     and column at CP0X
*
*     then print this string

* the first one is for positiong the HEX bytes

CURPS0     FCB      $1B,$3D                ESC= - this is the leadin for cursor positioning
CP0Y       FCB      $00                    this will get the line (Y) coordinate
CP0X       FCB      $00,$04                this will get the column (X) coordinate

* the second one is for positioning the ASCII bytes on the screen

CURPS1     FCB      $1B,$3D                ESC= - this is the leadin for cursor positioning
CP1Y       FCB      $00                    this will get the line (Y) coordinate
CP1X       FCB      $00,$04                this will get the column (X) coordinate
           FCB      $0D
           FCB      $1B
           FCC      'T'                     clear to end of line
           FCB      $04

* position to line 0 column 0 and clear to end of line

CLRLN0     FCB      $1B
           FCC      '=  '                   position to line 0, column 0
           FCB      $1B
           FCC      'T'                     clear to end of line
           FCB      $04

* position to line 1 column 0 and clear to end of line

CLRLN1     FCB      $1B
           FCC      '=! '                   position to line 1 column 0
           FCB      $1B
           FCC      'T'                     clear to end of line
           FCB      $04

* position to line 20 column 0 and clear to end of line
* then ask for command

PROMPT     FCB      $1B                     position to line 20 colimn 0
           FCC      '=4 '
           FCB      $1B
           FCC      'T'                     clear to end of line
           FCC      'Enter command (R)ead,(W)rite,(H)ex,(A)scii'
           FCB      $F1
           FCC      ',(P)aint screen,(Q)uit'
           FCB      $0D
           FCB      $0A
           FCB      $1B,$54                clear to end of line
           FCC      '              (D)rive,(T)rack,(S)ector,(+) or (-) :> '
           FCB      $1B,$54,$04

* position to line 22 column 0 ANDA sk for drive number then clear to end of line

PRMPTDRV   FCB      $1B,$3D,$36,$20         position to line 22 column 0
           FCC      'Drive? '
           FCB      $1B,$54,$04             clear to end of line

* position to line 22 column 0 ANDA sk for track number then clear to end of line

PRMPTTRK   FCB      $1B,$3D,$36,$20         position to line 22 column 0
           FCC      'Track? '
           FCB      $1B,$54,$04

* position to line 22 column 0 ANDA sk for sector number then clear to end of line

PRMPTSEC   FCB      $1B,$3D,$36,$20         position to line 22 column 0
           FCC      'Sector? '
           FCB      $1B,$54,$04             clear to end of line

* position to line 22 column 0 and show error message then clear to end of line

PRTERROR   FCB      $1B,$3D,$36,$20         position to line 22 column 0
           FCC      'ERROR - COMMANDA BORTED'
           FCB      $1B,$54,$04             clear to end of line

* position to line 22 column 0 ANDA sk for offset then clear to end of line

PRMPTOFF   FCB      $1B,$3D,$36,$20         position to line 22 column 0
           FCC      'Enter offset :> '
           FCB      $1B,$54,$04             clear to end of line

* position to line 22 column 0 then clear to end of line

CLRLN22    FCB      $1B,$3D,$36,$20,$1B,$54,$04     position to line 22 column 0 and clear to end of line

CURPTR     RMB      2       EQU      $0256
NOINPUT    RMB      1       EQU      $0258
COUNTER    RMB      1       EQU      $0259
           RMB      1

*          ORG      $025B

CURDRV     FCB      $00
CURTRK     FCB      $00
CURSEC     FCB      $00

BUFPTR     RMB      2       EQU      $025E
BUFFOFF    RMB      1       EQU      $0260
CURLINE    RMB      1       EQU      $0261
CURCOL     RMB      1       EQU      $0262
ASCCPOS    RMB      1       EQU      $0263
PSSAVE     RMB      1       EQU      $0264

*          ORG      $0265

BEGIN      LDX      #CLRSCRN
           JSR      PSTRING
           LDX      #START                 set start as the ESC RETUN location
           STX      ESCRET
           LDAA     TTYPS                  get TTY pause value
           STAA     PSSAVE                 save it
           LDAA     #$00                   turn it off
           STAA     TTYPS
           BRA      GETCMD

REPAINT    JSR      SHOWSEC                show the contents of the selected sector
GETCMD     LDX      #CLRLN22               clear line 22
           JSR      PSTRING
           LDX      #PROMPT                prompt for command
           JSR      PSTRING

           JSR      GETCHAR
           JSR      MKUCASE                make command upper case
           CMPA     #'R'                   'R' = read
           BNE      CHKH                   no - check for H
           JSR      CMDREAD
           JMP      REPAINT                repaint the current sector

CHKH       CMPA     #'H'                   'H' = modify Hex
           BNE      CHKQ                   no - check for Q
           JSR      HEXMOD
           JMP      GETCMD

CHKQ       CMPA     #'Q'                   'Q' = quit
           BNE      CHKW                   no - check for W
           JSR      PCRLF
           LDAA     PSSAVE
           STAA     TTYPS
           JMP      WARMS

CHKW       CMPA     #'W'                   'W' = write
           BNE      CHKPLUS                no - check for +
           JSR      WRITESEC
           JMP      GETCMD

CHKPLUS    CMPA     #$2B                   '+' = next sector
           BNE      CHKMINU                no - check for -
           LDAA     CURSEC
           INCA
           STAA     CURSEC
           JSR      SHOWSTAT
           JMP      GETCMD

CHKMINU    CMPA     #$2D                   '-' = previous sector
           BNE      CHKD                   no - check for D
           LDAA     CURSEC
           DECA
           STAA     CURSEC
           JSR      SHOWSTAT
           JMP      GETCMD

CHKD       CMPA     #'D'                   'D' = drive
           BNE      CHKT                   no - check for T
           JSR      GETDRV
           JSR      SHOWSTAT
           JMP      GETCMD

CHKT       CMPA     #'T'                   'T' = track
           BNE      CHKS                   no - check for S
           JSR      GETTRK
           JSR      SHOWSTAT
           JMP      GETCMD

CHKS       CMPA     #'S'                   'S' = sector
           BNE      CHKA                   no - check for A
           JSR      GETSEC
           JSR      SHOWSTAT
           JMP      GETCMD

CHKA       CMPA     #'A'                   'A' = modify ascii
           BNE      CHKP                   no - check for P
           JSR      ASCMOD
           JMP      GETCMD

CHKP       CMPA     #'P'                   'P' = paint screen
           BNE      BADCMD                 no - branch to jump to get command
           JMP      REPAINT                just go paint the screen and get command

BADCMD     JMP      GETCMD                 go back to get a command


* Read the sector selected

CMDREAD    LDX      #FCB                   point to FCB
           LDAA     CURDRV                 get current drive
           STAA     $03,X                  stuff current drive in FCB
           LDAA     CURTRK                 get the current track
           STAA     $1E,X                  stuff it in the FCB
           LDAA     CURSEC                 get the current sector
           STAA     $1F,X                  stuuf it in the FCB as well
           LDAA     #$09                   set read command
           STAA     $00,X                  stuff the command in the FCB
           JSR      FMSCALL                make the FMS call
           BEQ      CRNOERR                no error - just return
           JSR      ERROR                  report error
CRNOERR    RTS                             return to command loop

* SUBROUTINE: GETDRV
*
*   ENTRY:
*
*   EXIT:

GETDRV     LDX      #PRMPTDRV              point to prompt to enter drive number
           JSR      PSTRING                print string on console
           CLR      NOINPUT                clear no input flag
           CLRB
           JSR      GET1HEX                get a single hex digit
           TST      NOINPUT                was one input?
           BNE      GDNOINP                no - ignore command

           CMPA     #$04                   make sure it is a valid drive number
           BCC      GETDRV                 it is not - loop
           STAA     CURDRV                 save selected drive number
           JSR      SHOWSTAT               re-dosplay the status line
GDNOINP    RTS                             return

* SUBROUTINE: GETTRK
*
*   ENTRY:
*
*   EXIT:

GETTRK     LDX      #PRMPTTRK              point to prompt to ask for track
           JSR      PSTRING                print string on colsoe
           CLR      NOINPUT                clear no input flag
           JSR      GET2HEX                get two hex bytes
           TST      NOINPUT                was a track entered?
           BNE      GTNOINP                no - ignore command

           STAA     CURTRK                 yes - save the track selected
           JSR      SHOWSTAT               re-paint the satus line
GTNOINP    RTS                             return

* SUBROUTINE: GETSEC
*
*   ENTRY:
*
*   EXIT:

GETSEC     LDX      #PRMPTSEC              point to prompt to ask for sector
           JSR      PSTRING                print string on console
           CLR      NOINPUT                clear no input flag
           JSR      GET2HEX                get two hex bytes
           TST      NOINPUT                was a sector input?
           BNE      GSNOINP                no - ignore command

           STAA     CURSEC                 save selected sector
           JSR      SHOWSTAT
GSNOINP    RTS


* Get two hex bytes from the keyboard and return as hex in A

GET2HEX    JSR      GETCHAR                get a character
           JSR      MKUCASE                make it upper case
           CMPA     #$0D                   is the first character a <cr>
           BNE      MKAHEX                 no - proceed

           INC      NOINPUT                yes - set no input flag and
           BRA      G1HRTN                 return


* Make the ascii character in A the upper nibble of
* an 8 bit hex value and then get the lower nibble.

* If only one nibble is to be input, use space for
* the second nibble.

MKAHEX     JSR      MKHEXVAL               make a hex value from ascii char in A
           ASL A                           move it to the upper nibble
           ASL A
           ASL A
           ASL A
           TAB                             save it in B while we do the lower nibble

* SUBROUTINE: GET1HEX
*
*   ENTRY:
*
*   EXIT:

GET1HEX    JSR      GETCHAR                get the lower nibble as ascii character
           JSR      MKUCASE                make sure it is upper case
           CMPA     #$0D                   valid entry?
           BEQ      G1H01                  no - we need two hex digits

           CMPA     #$20                   is it a space?
           BNE      G1H02                  no - use it

           JSR      OUTSPC                 output a space
           LSR B                           only one digit entered - move back to lower nibble
           LSR B
           LSR B
           LSR B
           TBA
           BRA      G1HRTN                 and exit

G1H01      INC      NOINPUT                set no input flag for aller.
           BRA      G1HRTN                 and return

G1H02      JSR      MKHEXVAL               do lower nibble
           ABA                             and make it part of the reult to retun
G1HRTN     RTS                             and return the value in the A register

* Make a single hex value from ascii character in A

MKHEXVAL   SUBA     #$30                   see if >= '0'
           BGE      MHV02                  yes - check for <= '9'

MHV01      LDAA     #$08                   not valid input - back up the cursor and get character
           JSR      PUTCHR                 output backspace
           JSR      GETCHAR                get a character from the keyboard
           JSR      MKUCASE                make it upper case
           CMPA     #$0D                   see if <CR>
           BNE      MKHEXVAL               no - proceed - top of loop

           INC      NOINPUT
           BRA      MHVRTN                 return

MHV02      CMPA     #$09                   is the character a decimal number?
           BLE      MHVRTN                 yes - return

           SUBA     #$07                   See if it is hex 'A'
           BLT      MHV01                  no - try again
           CMPA     #$0F                   see if Hex A - F
           BGT      MHV01                  no - try again
MHVRTN     RTS                             yes - return


* This is called when we get an FMS error, so there will
* be information in A about the error.

ERROR      JSR      PCRLF                  output a CRLF
           LDX      #PRTERROR              point to the error message
           JSR      PSTRING                send it to the console
           TAB
           JSR      OUTHL                  output the upper nibble of A
           TBA
           JSR      OUTHR                  output the lower nibble of A
           RTS

* SUBROUTINE: HEXMOD
*
*   ENTRY:
*
*   EXIT:

HEXMOD     LDX      #PRMPTOFF              point to prompt string to get offset
           JSR      PSTRING                send string to the console
           CLR      NOINPUT                clear no input flag
           JSR      GET2HEX                get two hex bytes
           TST      NOINPUT                did user give is a valid offset?
           BEQ      HM01                   yes - proceed

           JMP      HMRTN                  no - jump to the return

HM01       STAA     BUFFOFF                save the offset for modifying the buffer
HM02       LDX      #FCB+64                L1000
           LDAA     BUFFOFF                get the current offset for modifying the buffer
           DEX                             get X ANDA  ready to increment to point X at offset
           INCA

HM03       INX
           DECA
           BNE      HM03
           STX      BUFPTR                 save current buffer pointer
           JSR      SETCURS                set the cursor positioning strings
           LDX      #CURPS0                position the cursor over the offset
           JSR      PSTRING                position the cursor
           JSR      GETCHAR                get input from the keybaord
           JSR      MKUCASE
           CMPA     #$0D
           BNE      HM04
           JMP      HMRTN

HM04       CMPA     #$0C
           BEQ      HM06
           CMPA     #$04                   right arrow
           BEQ      HM06
           CMPA     #$08                   back space - move back one
           BEQ      HM07
           CMPA     #$13                   left arrow
           BEQ      HM07
           CMPA     #$18                   down arrow
           BEQ      HM08
           CMPA     #$05                   up arrow
           BEQ      HM09

           CLR      NOINPUT
           JSR      MKAHEX
           TST      NOINPUT
           BNE      HEXMOD
           LDX      BUFPTR
           STAA     $00,X
           LDX      #CURPS1
           JSR      PSTRING
           LDX      BUFPTR
           LDAA     $00,X
           ANDA     #$7F
           CMPA     #$20
           BCC      HM05

           LDAA     #$2E
HM05       JSR      PUTCHR
HM06       LDAA     BUFFOFF                get the current offset for modifying the buffer
           INCA
           STAA     BUFFOFF                save the offset for modifying the buffer
           JMP      HM02

HM07       LDAA     BUFFOFF                get the current offset for modifying the buffer
           DECA
           STAA     BUFFOFF                save the offset for modifying the buffer
           JMP      HM02

HM08       LDAA     BUFFOFF                get the current offset for modifying the buffer
           ADDA     #$10
           STAA     BUFFOFF                save the offset for modifying the buffer
           JMP      HM02

HM09       LDAA     BUFFOFF                get the current offset for modifying the buffer
           SUBA     #$10
           STAA     BUFFOFF                save the offset for modifying the buffer
           JMP      HM02

HMRTN      RTS

* SUBROUTINE: ASCMOD
*
*   ENTRY:
*
*   EXIT:

ASCMOD     LDX      #PRMPTOFF              prompt for offset
           JSR      PSTRING                send prompt to console
           CLR      NOINPUT                clear no input flag
           JSR      GET2HEX                get buffer offset to modify
           TST      NOINPUT
           BEQ      AM01
           JMP      AMNOINP

AM01       STAA     BUFFOFF                save the offset for modifying the buffer
AM02       LDX      #FCB+64                L1000 point to data portion of FCB
           LDAA     BUFFOFF                get the current offset for modifying the buffer
           DEX                             get X ANDA  ready to increment to point X at offset
           INCA

AM03       INX                             get X pointing to the offset into the buffer according to A
           DECA
           BNE      AM03                   keep incrementing pointer

           STX      BUFPTR                 save current buffer pointer
           JSR      SETCURS                set the cursor positioning strings
           LDX      #CURPS1                position the cursor over the offset
           JSR      PSTRING                position the cursor
           JSR      GETCHAR                get character
           CMPA     #$0D                   input that will end ASCII modify
           BNE      AM04                   not ending - proceed
           JMP      AMNOINP                done - back to accepting command

* see if user is using cursor positioning keys like
* up/down/right/left arrows

AM04       CMPA     #$0C                   move forward one
           BEQ      AM05
           CMPA     #$04                   right arrow
           BEQ      AM05
           CMPA     #$08                   backspace - move back one
           BEQ      AM06
           CMPA     #$13                   left arrow
           BEQ      AM06
           CMPA     #$18                   down arrow
           BEQ      AM07
           CMPA     #$05                   up arrow
           BEQ      AM08

           LDX      BUFPTR
           STAA     $00,X
           LDX      #CURPS0
           JSR      PSTRING
           LDX      BUFPTR
           JSR      OUTHEX

AM05       LDAA     BUFFOFF                get the current offset for modifying the buffer
           INCA
           STAA     BUFFOFF                save the offset for modifying the buffer
           JMP      AM02

AM06       LDAA     BUFFOFF                get the current offset for modifying the buffer
           DECA
           STAA     BUFFOFF                save the offset for modifying the buffer
           JMP      AM02

AM07       LDAA     BUFFOFF                get the current offset for modifying the buffer
           ADDA     #$10
           STAA     BUFFOFF                save the offset for modifying the buffer
           JMP      AM02

AM08       LDAA     BUFFOFF                get the current offset for modifying the buffer
           SUBA     #$10
           STAA     BUFFOFF                save the offset for modifying the buffer
           JMP      AM02

AMNOINP    RTS


* This calculates the position on the screen of the
* character being pointed to by the buffer offset

SETCURS    LDAA     BUFFOFF                get the current offset for modifying the buffer
           ASRA                            get the upper nibble (which line) as A
           ASRA
           ASRA
           ASRA
           ANDA     #$0F                   make sure we just have lower 4 bits
           ADDA     #$02                   sector buffer content display starts at line 2
           STAA     CURLINE                save the hex value of the screen line number
           ADDA     #$20                   make it acceptable to the ESC= Y value
           STAA     CP0Y                   put this value in both cursor positioning strings
           STAA     CP1Y
           LDAA     BUFFOFF                get the current offset for modifying the buffer
           ANDA     #$0F                   now do the same for the column
           TAB
           ABA
           ABA
           ADDA     #$04                   we start the buffer contents display at column 4
           STAA     CURCOL                 save the hex value of the column position on the screen for the hex
           ADDA     #$20                   make it acceptable to the ESC= X value
           STAA     CP0X                   save it in the cursor position 0 string only (hex)
           LDAA     BUFFOFF                get the current offset for modifying the buffer
           ANDA     #$0F                   save the hex value of the column position on the screen
           ADDA     #$36                   the ascii starts at column 54 ($36)
           STAA     ASCCPOS                save the hex value of the column position on the screen for the ascii
           ADDA     #$20                   make it acceptable to the ESC= X value
           STAA     CP1X                   save it in the cursor position 0 string only (ascii)
           RTS

* SUBROUTINE: WRITESEC
*
*   ENTRY:
*
*   EXIT:

WRITESEC   LDX      #FCB                   point to the FMS FCB
           LDAA     CURDRV                 get the current frive selected
           STAA     $03,X                  put it in the FCB
           LDAA     CURTRK                 get the current track selected
           STAA     $1E,X                  put it in the FCB
           LDAA     CURSEC                 get the current sector selected
           STAA     $1F,X                  put it in the FCB
           LDAA     #$0A                   FMS write function
           STAA     $00,X                  put it in the FCB
           JSR      FMSCALL                do the FMS call to perform function
           BEQ      WRNOERR                no error - return
           JSR      ERROR                  report error
WRNOERR    RTS                             return


* Show the contents of the selected sector

SHOWSEC    LDX      #CLRLN1
           JSR      PSTRING                clear line 0
           LDX      #FCB+64                L1000 this is the FCB address + 64 (data buffer)
SS01       STX      CURPTR                 pointer to data
           JSR      PCRLF
           LDAA     #$10                   set to do 16 characters per line
           STAA     COUNTER                save column counter
           LDX      #CURPTR+1               point to the lower byte of the buffer address
           JSR      OUTHEX                 show as the first two characters of the line (buffer offset)
           LDX      CURPTR                 get the pointer back into X
           JSR      OUTSPC                 output 2 spaces between offset and first byte of the line
           JSR      OUTSPC
SS02       JSR      OUTHEX                 output the byte at offset in the buffer pointed to by X
           JSR      OUTSPC                 ANDA  space between bytes
           INX                             bump the pointer
           CPX      #FCB+320               L1100 end of data buffer?
           BEQ      SS03                   yes - show ascii for the last line

           DEC      COUNTER                decrement column counter
           BNE      SS02                   we have not done 16 bytes yet

* We have just output a line of 16 bytes of data from
* the buffer. Now show the ASCII data to the right of
* the data bytes

SS03       LDX      CURPTR                 get the pointer that the line starts at in the buffer
           JSR      OUTSPC                 let's have a couple of spaces between data bytes ANDA SCII
           JSR      OUTSPC
           LDAA     #$10                   set number of ASCII byte to output to 16
           STAA     COUNTER
SS04       LDAA     $00,X                  get the character
           ANDA     #$7F                   strip upper bit
           CMPA     #$20                   see if printable ASCII
           BCC      SS05                   yes - show it on the console
           LDAA     #$2E                   no - show a period instead of non-printable ASCII
SS05       JSR      PUTCHR                 send to the console
           INX                             bump pointer
           CPX      #FCB+320               L1100 are we done with the data buffer?
           BEQ      SSRTN                  yes - PCRLF and return
           DEC      COUNTER                no - decrement counter
           BNE      SS04                   we are still on the same line
           BRA      SS01                   go do the next line

SSRTN      JSR      PCRLF                  do a PCRLF
           RTS                             and return


* put character to the screen

PUTCHAR    JSR      PUTCHR                 put character to the screen
           INX                             bump pointer

* print $04 terminated string pointer to by X

PSTRING    LDAA     $00,X                  get character
           CMPA     #$04                   terminator?
           BNE      PUTCHAR                no - send to console
           RTS                             yes - return

* SUBROUTINE: OUTSPC
*
*   ENTRY:
*
*   EXIT:

OUTSPC     PSHA                             save the A register
           LDAA     #$20                    output a space
           JSR      PUTCHR                  send to console
           PULA                             restore the A register
           RTS                              return

* SUBROUTINE: OUTHL
*
*   ENTRY:
*
*   EXIT:

OUTHL      ASRA                             move upper nibble to lower nibble
           ASRA
           ASRA
           ASRA

* SUBROUTINE: OUTHR
*
*   ENTRY:  A lower nibble is byte to output as ASCII hex
*   EXIT:   A register is changed

OUTHR      ANDA     #$0F                    strip upper nibble
           ADDA     #$30                    make printable ASCII
           CMPA     #$39                    see if greater than 9
           BLS      OHRTN                   if not - just send to console
           ADDA     #$07                    make it valid hex
OHRTN      JSR      PUTCHR                  send character to console
           RTS                              and return

* SUBROUTINE: MKUCASE
*
*   ENTRY:
*
*   EXIT:

MKUCASE    ANDA     #$7F                   strip upper bit
           CMPA     #$41                   Check for ALPHA
           BCS      MKU01                  less than 'A' - return
           CMPA     #$7B                   less than or equal to 'Z'?
           BCS      MKU02                  yes - make it upper case
MKU01      RTS                             no - just return

MKU02      ANDA     #$DF                   make it upper case
           RTS

* SUBROUTINE: SHOWSTAT
*
*   ENTRY:
*
*   EXIT:

SHOWSTAT   JSR      SETDTS                 set drive, track and sector in status line
           LDX      #CLRLN0                point to string to clear status line
           JSR      PSTRING                position to and clear the status line
           LDX      #STATLN                point to the status line
           JSR      PSTRING                senf it to the console
           RTS                             return


* Set the current drive, track and sector in the status line on line 1

SETDTS     LDAA     CURDRV                 get current drive
           ADDA     #$30                   can only be 0 through 4 - no conversion required
           STAA     DRVNUM                 put it in the status line
           LDX      #SECNUM                point target at sector number in status line
           LDAA     CURSEC                 get current sector
           JSR      HEX2ASC                convert to ASCII at X
           LDX      #TRKNUM                point target at track number in status line
           LDAA     CURTRK                 get current track
           JSR      HEX2ASC                convert to ASCII at X
           RTS                             return


* store the byte in A as two ascii characters in the memory location pointed to by X

HEX2ASC    PSHA                            save the byte to output
           ANDA     #$F0                   concentrate on the upper 4 bits
           ASRA                            move them to the lower 4 bits
           ASRA
           ASRA
           ASRA
           BSR      H2A01                  convert lower 4 bits of A to ASCII character
           INX                             bump memory pointer for next ascii character store
           PULA                            get the value back to do the lower 4 bits

* SUBROUTINE: H2A01
*
*   ENTRY:
*
*   EXIT:

H2A01      ANDA     #$0F                   make sure we are dealing with only lower 4 bits
           ADDA     #$30                   Make ASCII if less than $0A
           CMPA     #$3A                   see if we need to add $07 to Make $3A = $41
           BCS      H2A02                  no - nibble is between 0 and 9
           ADDA     #$07                   make it true hex (A - F)

H2A02      STAA     $00,X                  store the ascii character
           RTS

* SUBROUTINE: GETCHAR
*
*   ENTRY:*
*   EXIT:   A register has character typed on the keyboard
*           all other regsiters except CCR preserved.

GETCHAR    PSHB
GC01       LDAB     ACIAPRT
           ANDB     #$01
           BEQ      GC01
           PULB
           LDAA     ACIAPRT+1
           ANDA     #$7F
           CMPA     #$1F
           BHI      GCRTN
           RTS

GCRTN      JSR      PUTCHR
           RTS

           ORG      $0FC0

FCB        RMB      320

           END      START
