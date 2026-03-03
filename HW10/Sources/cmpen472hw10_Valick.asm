;*******************************************************
;* CMPEN 472, HW8 Multi-Real Time Interrupt, MC9S12C128 Program
;* CodeWarrior Simulator/Debug edition, not for CSM-12C128 board
;*
;* Programmer: Jamin Valick
;*
;*Command gw : generate sawtooth wave, printing 0 through 255, repeated for total 2048 points
;*Command gt : generate triangle wave, printing 0 through 255, then 255 down to 0, repeated for total 2048 points
;*Command gq : generate square wave, printing 0 for 255 times, then print 255 for 255 times, then repeated for total 2048 points
;*Print one signal sample (an 8-bit unsigned integer) every 125usec (8000Hz sampling rate)
;*Capture the terminal output into a file, so the wave can be plotted with Excel or MATLAB. (Click for terminal setup. )
;*Show '> ' prompt and echo print user keystrokes until the Enter/Return key
;*In case of an invalid input format, print error message on the next line: 'Invalid input format'
;*10 minute digital clock (same as Homework 8)
;*Update the time display every one second
;*Time display: three 7-segment displays on PORTA and PORTB
;*Use Real Time Interrupt (RTI) feature to keep the time
;*Waveform display: on the terminal screen (ASCII text print, integer number)
;*
;* 
;*******************************************************
;*******************************************************

; export symbols - program starting point
            XDEF        Entry        ; export 'Entry' symbol
            ABSENTRY    Entry        ; for assembly entry point

; include derivative specific macros
PORTA       EQU         $0000
PORTB       EQU         $0001
DDRA        EQU         $0002
DDRB        EQU         $0003

SCIBDH      EQU         $00C8        ; Serial port (SCI) Baud Register H
SCIBDL      EQU         $00C9        ; Serial port (SCI) Baud Register L
SCICR2      EQU         $00CB        ; Serial port (SCI) Control Register 2
SCISR1      EQU         $00CC        ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00CF        ; Serial port (SCI) Data Register

CRGFLG      EQU         $0037        ; Clock and Reset Generator Flags
CRGINT      EQU         $0038        ; Clock and Reset Generator Interrupts
RTICTL      EQU         $003B        ; Real Time Interrupt Control

TIOS        EQU         $0040         ; Timer Input Capture (IC) or Output Compare (OC) select
TIE         EQU         $004C         ; Timer interrupt enable register
TCNTH       EQU         $0044         ; Timer free runing main counter
TSCR1       EQU         $0046         ; Timer system control 1
TSCR2       EQU         $004D         ; Timer system control 2
TFLG1       EQU         $004E         ; Timer interrupt flag 1
TC5H        EQU         $005A         ; Timer channel 2 register

CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character

DATAmax     equ         2048         ; Data count maximum, 2048 constant

;*******************************************************
; variable/data section
            ORG    $3000             ; RAMStart defined as $3000
                                     ; in MC9S12C128 chip

timeh       DS.B   1                 ; Hour
timem       DS.B   1                 ; Minute
times       DS.B   1                 ; Second
minute      DS.B   3                 ;buffer for minute
second1     DS.B   3                 ;buffer for first seond
second2     DS.B   3                 ;buffer for second second
digMin      DS.B   3                 ;buffer for minute display output
digSec1     DS.B   3                 ;buffer for first seond display output
digSec2     DS.B   3                 ;buffer for second second display output
setBool     DS.B   2                 ;bool to check if clock has been set

ctr2p5m     DS.W   1                 ; interrupt counter for 2.5 mSec. of time
ctr125u     DS.W   1                 ; 16bit interrupt counter for 125 uSec. of time

BUF         DS.B   6                 ; character buffer for a 16bit number in decimal ASCII
CTR         DS.B   1                 ; character buffer fill count

wave        DS.B   1                 ;indicator for wave type

buffer      DS.B   7                 ;buffer for message from terminal
buffCount   DC.B   $00,$00           ;counter for number of characters in buffer

;*******************************************************
; interrupt vector section
            ORG    $FFF0             ; RTI interrupt vector setup for the simulator
;            ORG    $3FF0            ; RTI interrupt vector setup for the CSM-12C128 board
            DC.W   rtiisr
            
            ORG    $FFE4             ; Timer channel 5 interrupt vector setup, on simulator
            DC.W    oc5isr
;*******************************************************
; code section

            ORG    $3100
Entry
            LDS    #Entry         ; initialize the stack pointer

            LDAA   #%11111111   ; Set PORTA and PORTB bit 0,1,2,3,4,5,6,7
            STAA   DDRA         ; all bits of PORTA as output
            STAA   PORTA        ; set all bits of PORTA, initialize
            STAA   DDRB         ; all bits of PORTB as output
            STAA   PORTB        ; set all bits of PORTB, initialize

            ldaa   #$0C         ; Enable SCI port Tx and Rx units
            staa   SCICR2       ; disable SCI interrupts

            ldd    #$0001       ; Set SCI Baud Register = $0001 => 1.5M baud at 24MHz (for simulation)
;            ldd    #$0002       ; Set SCI Baud Register = $0002 => 750K baud at 24MHz
;            ldd    #$000D       ; Set SCI Baud Register = $000D => 115200 baud at 24MHz
;            ldd    #$009C       ; Set SCI Baud Register = $009C => 9600 baud at 24MHz
            std    SCIBDH       ; SCI port baud rate change
            
            bset   RTICTL,%00011001 ; set RTI: dev=10*(2**10)=2.555msec for C128 board
                                    ;      4MHz quartz oscillator clock
            bset   CRGINT,%10000000 ; enable RTI interrupt
            bset   CRGFLG,%10000000 ; clear RTI IF (Interrupt Flag)

            ldx    #0
            stx    ctr2p5m          ; initialize interrupt counter with 0.
            cli                     ; enable interrupt, global
            
            ldx    #0
            stx    ctr125u          ; initialize interrupt counter with 0.
            cli                     ; enable interrupt, global
            
            jsr    printMenu
            ldy   #buffer           ;load address of instruction buffer

mainLoop    ldaa   setBool
            cmpa   #1
            bne    notSet           ;if clock hasn't been set skip addSecond
            jsr    addSecond        ; if 1 second is up, increse the clock 

notSet      jsr    getchar          ; type writer - check the key board
            tsta                    ;  if nothing typed, keep checking
            beq    mainLoop
                                    ;  otherwise - what is typed on key board
            jsr    putchar          ; is displayed on the terminal window
            cmpa   #CR
            beq    enter            ;if Enter/Return key is pressed
            staa   1,Y+             ;if not Enter add character to instrusction buffer
            inc    buffCount        ;add 1 to buffCount
            bra    mainLoop
            
            
enter       jsr   nextline
            
            ldaa  buffCount
            suba  #6                 ;check if character count is greater than 6
            bgt   formatJ            ;print error message if more than 6 characters
            
            ldy   #buffer            ;load address of instruction buffer for reading
            ldaa  1,Y+               ;pick up an ASCII character from buffer
            cmpa  #$71               ;see if first character is 'q'
            beq   typeWriterJ
            cmpa  #$73               ;see if first character is 's'
            beq   setTimeJ
            cmpa  #$67               ;see if first character is 'g'
            beq   Gs
            bra   formatJ            ;print error mesage if no first characters match
            
Gs          ldaa  1,Y+
            ldx   #wave
            cmpa  #$77               ;see if first character is 'w'
            bne   cont1
            ldab  #0       
            stab  X                  ;wave = 0
            bra   begin2048J
cont1       cmpa  #$74               ;see if first character is 't'
            bne   cont2
            ldab  #1       
            stab  X                  ;wave = 1
            bra   begin2048J         
cont2       cmpa  #$71               ;see if first character is 'q'
            bne   cont3
            ldab  #2       
            stab  X                  ;wave = 2
            bra   begin2048J         
cont3       bra   formatJ            ;print error mesage if no first characters match 

                                     
setTimeJ    jmp   setTime
typeWriterJ ldaa  1,Y+
            cmpa  #0
            bne   formatJ
            jmp   typeWriter
begin2048J  ldaa  1,Y+
            cmpa  #0
            bne   formatJ
            jmp   begin2048
formatJ     jmp   format

            
intrEnd     ldaa  #$0A
            jsr   putchar
            ldaa  #10                 ;clear 9 bytes of buffers
            ldx   #buffer
clearLoop   clr   X
            inx         
            deca
            beq   clearEnd
            bra   clearLoop
clearEnd    ldy   #buffer            ;reset address of instruction buffer
            jsr   mainLoop  
            
;subroutine section below
;***********Timer OC5 interrupt service routine***************
oc5isr
            ldd   #3000              ; 125usec with (24MHz/1 clock)
            addd  TC5H               ;    for next interrupt
            std   TC5H               ; 
            bset  TFLG1,%00100000    ; clear timer CH5 interrupt flag, not needed if fast clear enabled
           
            
            ldaa  wave
saw         cmpa  #0                 ;check if wave is sawtooth
            bne   tri
            ldd   ctr125u
            clra                     ;print ctr125u, only the last byte 
            bra   done
            
tri         cmpa  #1                 ;check if wave is triangle
            bne   squ 
            ldd   ctr125u
            ldx   #2
            tab  
            idiv
            tba
            bne   down               ;check if a register is odd or even
up          ldd   ctr125u            ;if even go up the triangle wave
            clra                     ;print ctr125u, only the last byte 
            bra   done
down        ldd   ctr125u
            clra                     
            ldaa  #255
            sba
            tab
            clra
            bra   done    
            
squ         ldd   ctr125u
            ldx   #2
            tab  
            idiv
            tba
            bne   one                ;check if a register is odd or even
zero        ldd   #0                 ;if even print 0
            bra   done
one         clra                     
            ldab  #255
            bra   done    

done        ldx   ctr125u
            inx                      ; update OC5 (125usec) interrupt counter
            stx   ctr125u
            jsr   pnum10             ;to make the file RxData3.txt with exactly 2048 data 

oc5done     RTI
;***********end of Timer OC5 interrupt service routine********

;**************************2048 loop**************************
begin2048   ldx     #0               ; Enter/Return key hit
            stx     ctr125u
            jsr     StartTimer5oc

            CLI                      ; Interrupt enable, for Timer OC5 interrupt start


loop2048
            ldaa    setBool
            cmpa    #1
            bne     notSet1          ;if clock hasn't been set skip addSecond
            jsr     addSecond        ; if 1 second is up, increse the clock 
            
notSet1     ldd     ctr125u
            cpd     #DATAmax         ; 2048 bytes will be sent, the receiver at Windows PC 
            bhs     loopTxON         ;   will only take 2048 bytes.
            bra     loop2048         ; set Terminal Cache Size to 10000 lines, update from 1000 lines

loopTxON
            LDAA    #%00000000
            STAA    TIE               ; disable OC5 interrupt

            jsr     nextline
            jsr     nextline

            ldx     #msg7            ; print '> Done!  Close Output file.'
            jsr     printmsg
            jsr     nextline

            ldx     #msg8            ; print '> Ready for next data transmission'
            jsr     printmsg
            jsr     nextline

            jmp     intrEnd

;************************2048 loop end************************

;***************StartTimer5oc************************
;* Program: Start the timer interrupt, timer channel 6 output compare
;* Input:   Constants - channel 5 output compare, 125usec at 24MHz
;* Output:  None, only the timer interrupt
;* Registers modified: D used and CCR modified
;* Algorithm:
;             initialize TIOS, TIE, TSCR1, TSCR2, TC2H, and TFLG1
;****************************************************
StartTimer5oc
            PSHD
            LDAA   #%00100000
            STAA   TIOS              ; set CH5 Output Compare
            STAA   TIE               ; set CH5 interrupt Enable
            LDAA   #%10000000        ; enable timer, Fast Flag Clear not set
            STAA   TSCR1
            LDAA   #%00000000        ; TOI Off, TCRE Off, TCLK = BCLK/1
            STAA   TSCR2             ;   not needed if started from reset

            LDD    #3000            ; 125usec with (24MHz/1 clock)
            ADDD   TCNTH            ;    for first interrupt
            STD    TC5H             ; 

            BSET   TFLG1,%00100000   ; initial Timer CH5 interrupt flag Clear, not needed if fast clear set
            LDAA   #%00100000
            STAA   TIE               ; set CH5 interrupt Enable
            PULD
            RTS
;***************end of StartTimer5oc*****************

;*******************pnum10***************************
;* Program: print a word (16bit) in decimal to SCI port
;* Input:   Register D contains a 16 bit number to print in decimal number
;* Output:  decimal number printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Keep divide number by 10 and keep the remainders
;     Then send it out to SCI port
;  Need memory location for counter CTR and buffer BUF(6 byte max)
;****************************************************
pnum10          pshd                   ;Save registers
                pshx
                pshy
                clr     CTR            ; clear character count of an 8 bit number

                ldy     #BUF
pnum10p1        ldx     #10
                idiv
                beq     pnum10p2
                stab    1,y+
                inc     CTR
                tfr     x,d
                bra     pnum10p1

pnum10p2        stab    1,y+
                inc     CTR                        
;--------------------------------------

pnum10p3        ldaa    #$30                
                adda    1,-y
                jsr     putchar
                dec     CTR
                bne     pnum10p3
                jsr     nextline
                puly
                pulx
                puld
                rts
;*****************end of pnum10********************

;***********RTI interrupt service routine***************
rtiisr      bset   CRGFLG,%10000000 ; clear RTI Interrupt Flag - for the next one
            ldx    ctr2p5m          ; every time the RTI occur, increase
            inx                     ;    the 16bit interrupt count
            stx    ctr2p5m
rtidone     RTI
;***********end of RTI interrupt service routine********

;*******************Set Time******************
setTime     ldx    #setBool         ;set bool is now true
            ldaa   #1
            staa   X
            ldaa   1,Y+
            cmpa   #$20             
            bne    timeFormatJ      ;see if second character is SPACE
            jsr    checkMin         ;check the minute digit and store in the buffer 
            ldaa   1,Y+
            cmpa   #$3A
            bne    timeFormatJ      ;see if fouth character is ":"
            jsr    checkSec1        ;check the sec1 digit and store in the buffer 
            jsr    checkSec2        ;check the sec2 digit and store in the buffer 
            
            jmp    intrEnd
;****************Set Time End*****************

timeFormatJ jmp    timeFormat

;*******************Add Second****************
addSecond   psha
            pshx

            ldx    ctr2p5m          ; check for 1.0 sec
;            cpx    #400             ; 2.5msec * 400 = 1.0 sec
            cpx    #80              ; 2.5msec * 400 = 1.0 sec
            blo    doneLED          ; NOT yet

            ldx    #0               ; 1.0sec is up,
            stx    ctr2p5m          ; clear counter to restart

            ldaa   minute           ;load time
            staa   PORTA            ;show the character on PORTA
            ldaa   second1
            ldab   #4
shiftLoop   lsla                    ;shift left 4 times so second2 can be included
            decb
            bne    shiftLoop
            adda   second2
            staa   PORTB            ;show the character on PORTB

            
            inc    second2          ;go up a second
            ldaa   second2
            cmpa   #10              ;check if digit is 10
            bne    doneLED          ;if not finish the function
            
            ldaa   #0
            ldx    #second2         ;if digit is 6 reset it to 0
            staa   X
            inc    second1          ;go up a 10s place second
            ldaa   second1
            cmpa   #6
            bne    doneLED
            
            ldaa   #0               ;set 10s place second to 0
            ldx    #second1
            staa   X
            inc    minute           ;go up a minute
            ldaa   minute
            cmpa   #10
            bne    doneLED
            
            ldaa   #0               ;set 10s place second to 0
            ldx    #minute
            staa   X
            
doneLED     pulx
            pula
            rts
;******************Add Second end**************

;***********check numbers**********************     
checkMin    ldaa  Y                  ;load number
            suba  #$30               ;subtract ascii '0'
            blt   timeFormat         ;print error if less than ascii '0'
            ldaa  Y                  ;reload number
            suba  #$39               ;subtract ascii '9'
            bgt   timeFormat         ;print error if greater than ascii '9'
            ldaa  1,Y+               ;reload number
            suba  #$30               ;subtract ascii '0'
            ldx   #minute            ;store the minute
            staa  X
            rts     
 
checkSec1   ldaa  Y                  ;load number
            suba  #$30               ;subtract ascii '0'
            blt   timeFormat         ;print error if less than ascii '0'
            ldaa  Y                  ;reload number
            suba  #$35               ;subtract ascii '5'
            bgt   timeFormat         ;print error if greater than ascii '5'
            ldaa  1,Y+               ;reload number
            suba  #$30               ;subtract ascii '0'            
            ldx   #second1           ;store the second
            staa  X
            rts     
            
checkSec2   ldaa  Y                  ;load number
            suba  #$30               ;subtract ascii '0'
            blt   timeFormat         ;print error if less than ascii '0'
            ldaa  Y                  ;reload number
            suba  #$39               ;subtract ascii '9'
            bgt   timeFormat         ;print error if greater than ascii '9'
            ldaa  1,Y+               ;reload number
            suba  #$30               ;subtract ascii '0'
            ldx   #second2           ;store the second
            staa  X
            rts                   
;***********check numbers end******************

;***********print menu*************************
printMenu   ldx   #msg3              ; print the third message
            jsr   printmsg
            jsr   nextline

            ldx   #msg4              ; print the fourth message
            jsr   printmsg
            jsr   nextline

            ldx   #msg5              ; print the fifth message
            jsr   printmsg
            jsr   nextline
            rts
;***********print menu ends********************

;***********error message**********************
format      ldx   #formatEr          ; print the error message
            jsr   printmsg
            jsr   nextline
            jmp   intrEnd
            
timeFormat  ldx   #timeEr            ; print the error message
            jsr   printmsg
            jsr   nextline
            jmp   intrEnd

;***********error message end******************

;***********typer writer loop******************
typeWriter  ldx   #msg2              ; print the 2nd message
            jsr   printmsg
            jsr   nextline
            
writerLoop  jsr   getchar            ; type writer - check the key board
            cmpa  #$00               ;  if nothing typed, keep checking
            beq   writerLoop
                                     ;  otherwise - what is typed on key board
            jsr   putchar            ; is displayed on the terminal window - echo print
            
            staa  PORTB              ; show the character on PORTB

            cmpa  #CR
            bne   writerLoop         ; if Enter/Return key is pressed, move the
            ldaa  #LF                ; cursor to next line
            jsr   putchar
            bra   writerLoop  
;***********typer writer loop ends*************   

;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL            equ     $00
printmsg        psha                   ;Save registers
                pshx
printmsgloop    ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
                cmpa    #NULL
                beq     printmsgdone   ;end of strint yet?
                bsr     putchar        ;if not, print character and do next
                bra     printmsgloop
printmsgdone    pulx 
                pula
                rts
;***********end of printmsg********************

;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar     brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
            staa  SCIDRL                      ; send a character
            rts
;***************end of putchar*****************

;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************

getchar     brclr SCISR1,#%00100000,getchar7
            ldaa  SCIDRL
            rts
getchar7    clra
            rts
;****************end of getchar**************** 

;****************nextline**********************
nextline    psha
            ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            pula
            rts
;****************end of nextline***************

msg1        DC.B        'Hello', $00
msg2        DC.B        'You may type below', $00
msg3        DC.B        'Welcome to the 10 minute dgital clock', $00
msg4        DC.B        'Please enter "s " and a time between  0:00 and 9:59 to set the clock,', $00
msg5        DC.B        '"gw" for sawtooth, "gt" for triangle, "gq" for square functions, or', $00
msg6        DC.B        '"q" to quit the clock,', $00
msg7        DC.B        'Done!  Close Output file.', $00
msg8        DC.B        'Ready for next data transmission.', $00
formatEr    DC.B        'Invalid command. ("s" to set time and "q" to quit)', $00
timeEr      DC.B        'Invalid time format. Correct example => 0:00 to 9:59', $00


            END               ; this is end of assembly source file
                              ; lines below are ignored - not assembled/compiled
