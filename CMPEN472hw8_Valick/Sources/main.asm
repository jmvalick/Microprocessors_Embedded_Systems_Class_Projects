;*******************************************************
;* CMPEN 472, HW8 Real Time Interrupt, MC9S12C128 Program
;* CodeWarrior Simulator/Debug edition, not for CSM-12C128 board
;*
;* Programmer: Jamin Valick
;*
;* 1 second LED1 blink, timer using Real Time Interrupt.
;* This program is a 1 second timer using 
;* a Real Time Interrupt service subroutine (RTIISR).  This program
;* displays the time on the 7 Segment Disply in Visualization Tool 
;* every 1 second.  
;* The 7 segment displys are connected to port B of
;* MC9S12C32 chip in CodeWarrior Debugger/Simulator.
;* Also on the Terminal component of the simulator,  
;* user may enter any key, it will be displayed on the screen - effectively
;* it is a typewriter.
;*
;* Please note the new feature of this program:
;* RTI vector, initialization of CRGFLG, CRGINT, RTICTL, registers for the
;* Real Time Interrupt.
;* We assumed 24MHz bus clock and 4MHz external resonator clock frequency.  
;* This program any user input (a typewriter). 
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

CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character

;*******************************************************
; variable/data section
            ORG    $3000             ; RAMStart defined as $3000
                                     ; in MC9S12C128 chip

timeh       DS.B   1                 ; Hour
timem       DS.B   1                 ; Minute
times       DS.B   1                 ; Second
ctr2p5m     DS.W   1                 ; interrupt counter for 2.5 mSec. of time
buffer      DS.B   7                 ;buffer for message from terminal
buffCount   DC.B   $00,$00           ;counter for number of characters in buffer
minute      DS.B   3                 ;buffer for minute
second1     DS.B   3                 ;buffer for first seond
second2     DS.B   3                 ;buffer for second second
digMin      DS.B   3                 ;buffer for minute display output
digSec1     DS.B   3                 ;buffer for first seond display output
digSec2     DS.B   3                 ;buffer for second second display output
setBool     DS.B   2                 ;bool to check if clock has been set



;*******************************************************
; interrupt vector section
            ORG    $FFF0             ; RTI interrupt vector setup for the simulator
;            ORG    $3FF0             ; RTI interrupt vector setup for the CSM-12C128 board
            DC.W   rtiisr

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
            
            jsr    printMenu
            ldy   #buffer           ;load address of instruction buffer

mainLoop    ldaa   setBool
            cmpa   #1
            bne    notSet           ;if clock hasn't been set skip addSecond
            jsr    addSecond        ; if 0.5 second is up, toggle the LED 

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
            bra   formatJ            ;print error mesage if no first characters match

setTimeJ    jmp   setTime
typeWriterJ ldaa  1,Y+
            cmpa  NULL
            bne   formatJ
            jmp   typeWriter
formatJ     jmp   format

            
intrEnd     ldaa  #$0A
            jsr   putchar
            ldaa  #9                 ;clear 9 bytes of buffers
            ldx   #buffer
clearLoop   clr   X
            inx         
            deca
            beq   clearEnd
            bra   clearLoop
clearEnd    ldy   #buffer            ;reset address of instruction buffer
            jsr   mainLoop  
;subroutine section below

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
msg4        DC.B        'Please enter "s " and a time between  0:00 and 9:59 to set the clock', $00
msg5        DC.B        'or "q" to quit the clock', $00
formatEr    DC.B        'Invalid command. ("s" to set time and "q" to quit)', $00
timeEr      DC.B        'Invalid time format. Correct example => 0:00 to 9:59', $00


            END               ; this is end of assembly source file
                              ; lines below are ignored - not assembled/compiled
