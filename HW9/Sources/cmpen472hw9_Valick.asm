***********************************************************************
*
* Title:          Homework 9
*
* Objective:      To combine homework 7 and 8 to make a calculator and digital clock
*
* Date:	          November 5, 2022
*
* Programmer:     Jamin Valick
*
* Program:        Simple SCI Serial Port I/O 
*                 elementary calculator
*                 digital clock
*
* Memory use:     RAM Locations from $3000 for data, 
*                 RAM Locations from $3100 for program
*
* Output:         hyper terminal
*                 7-segment display 
*                 
*
* Observation:    show calculation based on input
*                 show 10 minute clock based on time set
*
***********************************************************************
* Parameter Declearation Section
*
* Export Symbols
            XDEF        pstart       ; export 'pstart' symbol
            ABSENTRY    pstart       ; for assembly entry point
  
* Symbols and Macros
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

CR          EQU         $0d          ; carriage return, ASCII 'Return' key
LF          EQU         $0a          ; line feed, ASCII 'next line' character
S           EQU         $53          ; ASCII 'S'
W           EQU         $57          ; ASCII 'W'
dollar      EQU         $24          ; ASCII '$'
Q           EQU         $51          ; ASCII 'Q
U           EQU         $55          ; ASCII 'U
I           EQU         $49          ; ASCII 'I
T           EQU         $54          ; ASCII 'T'

***********************************************************************
* Data Section: address used [ $3000 to $30FF ] RAM memory
*
            ORG         $3000                ;Reserved RAM memory starting address 
buffCount   DC.B        $00,$00              ;counter for number of characters in buffer
buffer      DS.B        10                   ;buffer for message from terminal
num1_4      DS.B        5                    ;buffer for seperated characters for number 1
operator    DS.B        2                    ;buffer for operator
num2_4      DS.B        5                    ;buffer for seperated characters for number 2
num1        DS.B        3                    ;buffer for number 1
num2        DS.B        3                    ;buffer for number 2
answer_4    DS.B        5                    ;buffer for seperated characters for answer
answer      DS.B        3                    ;buffer for answer
numCount    DC.B        $00,$00              ;counter of digits
addressBuf  DC.B        3                    ;save addresses 

timeh       DS.B   1                 ; Hour
timem       DS.B   1                 ; Minute
times       DS.B   1                 ; Second
ctr2p5m     DS.W   1                 ; interrupt counter for 2.5 mSec. of time
minute      DS.B   3                 ;buffer for minute
second1     DS.B   3                 ;buffer for first seond
second2     DS.B   3                 ;buffer for second second
digMin      DS.B   3                 ;buffer for minute display output
digSec1     DS.B   3                 ;buffer for first seond display output
digSec2     DS.B   3                 ;buffer for second second display output
setBool     DS.B   2                 ;bool to check if clock has been set


; Each message ends with $00 (NULL ASCII character) for your program.
;
; There are 256 bytes from $3000 to $3100.  If you need more bytes for
; your messages, you can put more messages 'msg3' and 'msg4' at the end of 
; the program - before the last "END" line.
                                     ; Remaining data memory space for stack,
                                     ;   up to program memory start

;*******************************************************
; interrupt vector section
            ORG    $FFF0             ; RTI interrupt vector setup for the simulator
;            ORG    $3FF0             ; RTI interrupt vector setup for the CSM-12C128 board
            DC.W   rtiisr

;*******************************************************
;code section: address used [ $3100 to $3FFF ] RAM memory

            ORG        $3100         ; Program start address, in RAM
pstart      LDS        #$3100        ; initialize the stack pointer


            LDAA   #%11111111   ; Set PORTA and PORTB bit 0,1,2,3,4,5,6,7
            STAA   DDRA         ; all bits of PORTA as output
            STAA   PORTA        ; set all bits of PORTA, initialize
            STAA   DDRB         ; all bits of PORTB as output
            STAA   PORTB        ; set all bits of PORTB, initialize
            
            LDAA       #%00000000
            STAA       PORTB         ; clear all bits of PORTB
            STAA       PORTA         ; clear all bits of PORTA

            ldaa       #$0C          ; Enable SCI port Tx and Rx units
            staa       SCICR2        ; disable SCI interrupts

            ldd        #$0001        ; Set SCI Baud Register = $0001 => 1.5M baud at 24MHz (for simulation)
            std        SCIBDH        ; SCI port baud rate change
            
            bset   RTICTL,%00011001 ; set RTI: dev=10*(2**10)=2.555msec for C128 board
                                    ;      4MHz quartz oscillator clock
            bset   CRGINT,%10000000 ; enable RTI interrupt
            bset   CRGFLG,%10000000 ; clear RTI IF (Interrupt Flag)

            ldx    #0
            stx    ctr2p5m          ; initialize interrupt counter with 0.
            cli                     ; enable interrupt, global
            
            jsr   printMenu          ;print instruction menu
            ldy   #buffer            ;load address of instruction buffer
            
mainLoop    ldaa   setBool
            cmpa   #1
            bne    notSet           ;if clock hasn't been set skip addSecond
            jsr    addSecond        ; if 0.5 second is up, toggle the LED 
            
notSet      jsr   getchar            ; type writer - check the key board
            cmpa  #$00               ;  if nothing typed, keep checking
            beq   mainLoop
            
            cmpa  #CR
            beq   enter              ;if Enter/Return key is pressed
            staa  1,Y+               ;if not Enter add character to instrusction buffer
            inc   buffCount          ;add 1 to buffCount
            jsr   putchar            ;character is displayed on the terminal window - echo print
            bra   mainLoop
            
            
enter       jsr   nextline           ;move the cursor to next line
            
            ldaa  buffCount
            suba  #9                 ;check if character count is greater than 9
            bgt   formatJ            ;print error message if more than 16 characters

            ldy   #buffer            ;load address of instruction buffer for reading
            ldaa  Y                  ;pick up an ASCII character from buffer
            cmpa  #$71               ;see if first character is 'q'
            beq   typeWriterJ
            cmpa  #$73               ;see if first character is 's'
            beq   setTimeJ
            bra   startNum1          ;move to calculator if no first characters match
            
setTimeJ    iny
            jmp   setTime
typeWriterJ iny
            ldaa  1,Y+
            cmpa  NULL
            bne   formatJ
            jmp   typeWriter
            
            
startNum1   ldx   #num1_4
            jsr   checkNum1          ;get first number
            jsr   checkNum1 
            jsr   checkNum1 
            jsr   checkNum1
            jsr   checkNum1 
            
endNum1     ldx   #addressBuf
            dey
            sty   X
            ldx   #num1_4            ;reload address of seperated num1 buffer
            ldaa  #0
            ldab  1,X+               ;combine characters
            psha
            dec   numCount 
            
loop        ldaa  numCount
            beq   loopEnd
            pula
            ldy   #10
            emul
            addb  1,X+
            psha
            dec   numCount
            bra   loop
          
loopEnd     pula
            ldx   #num1               
            std   X                  ;store characters into num1 buffer 
              
            ldy   addressBuf
            iny 
            iny
            clr   numCount  
            bra   startNum2
            
formatJ     jmp   format
            
                  
startNum2   ldx   #num2_4
            jsr   checkNum2          ;get second number
            jsr   checkNum2 
            jsr   checkNum2 
            jsr   checkNum2 
            
            
endNum2     ldx   #num2_4            ;reload address of seperated num1 buffer
            ldaa  #0
            ldab  1,X+               ;combine characters
            psha
            dec   numCount 
            
loop2       ldaa  numCount
            beq   loopEnd2
            pula
            ldy   #10
            emul
            addb  1,X+
            psha
            dec   numCount
            bra   loop2
          
loopEnd2    pula
            ldx   #num2               
            std   X                  ;store characters into num2 buffer 
              
            ldy   addressBuf
            clr   numCount
            ldaa  #$3D
            jsr   putchar   
                                      
branchOp    ldaa  operator           ;branch to operator
            cmpa  #$2B          
            beq   add
            cmpa  #$2D
            beq   subtractJ
            cmpa  #$2A
            beq   multiplyJ
            cmpa  #$2F
            beq   divideJ

instrEnd    cpd   #0
            beq   printZero
            ldx   #answer
            std   X 
            ldy   #answer_4+3
            ldd   answer             ;load answer data
loopPrint   ldx   #$A
            idiv  
            beq   endLoop
            tba   
            stab  1,Y-
            tfr   X,D
            bra   loopPrint
endLoop     tba   
            stab  1,Y-
            
            ldab  #5
            ldy   #answer_4
zeroLoop    ldaa  Y
            bne   printLoop2
            decb  
            beq   printLoop2         ;move to print decimal
            iny
            bra   zeroLoop
            
printLoop2  decb  
            beq   endLoop2           ;move to print decimal
            ldaa  Y
            jsr   getAscii
            jsr   putchar
            iny
            bra   printLoop2

endLoop2    ldaa  #$0A
            jsr   putchar
            bra   endPrint
            
printZero   ldaa  #$30
            jsr   putchar
            ldaa  #$0A
            jsr   putchar            
            
endPrint    ldaa  #$0A
            jsr   putchar
            ldaa  #44                ;clear 43 bytes of buffers
            ldx   #buffCount
clearLoop   clr   X
            inx         
            deca
            beq   clearEnd
            bra   clearLoop
clearEnd    ldy   #buffer            ;reset address of instruction buffer
            jsr   mainLoop           

subtractJ   bra   subtract
divideJ     bra   divide
multiplyJ   bra   multiply
;subroutine section below
;*********************operations*************************
add         ldd   num2
            addd  num1
            pshd
            subd  #999
            bgt   overFlowJ
            puld
            jmp   instrEnd
            
subtract    ldd   num1
            subd  num2
            bmi   negNum
            jmp   instrEnd
negNum      ldaa  #$2D
            jsr   putchar
            ldd   num2
            subd  num1           
            jmp   instrEnd
            
multiply    ldd   num1
            ldy   num2
            emul  
            cpy   #0
            bne   overFlowJ
            pshd
            subd  #999
            bgt   overFlowJ
            puld
            jmp   instrEnd

divide      ldaa  num2
            cmpa  #0
            beq   overFlowJ
            ldd   num1
            ldx   num2
            idiv
            tfr   X,D
            jmp   instrEnd
                                            
;********************operations end*********************
overFlowJ   jmp   overFlow

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
            
            jmp   endPrint
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

;***********error message**********************
format      jsr   nextline
            ldx   #formatEr          ; print the error message
            jsr   printmsg
            jsr   nextline
            jmp   endPrint
            jsr   nextline
            
overFlow    jsr   nextline
            ldx   #overFlowEr        ; print the error message
            jsr   printmsg
            jsr   nextline
            jmp   endPrint
            jsr   nextline

timeFormat  jsr   nextline 
            ldx   #timeEr            ; print the error message
            jsr   printmsg
            jmp   endPrint
            jsr   nextline
;***********error message end******************

endNum2J    jmp   endNum2
 
;***********check numbers**********************
checkNum1   ldaa  Y                  ;load char
            cmpa  NULL               ;check if at end of data
            beq   format             ;finish writing if at end of data
checkOp     ldaa  Y                  ;load number
            suba  #$2A               ;subtract ascii '*'
            blt   notOp              ;print error if less than ascii '*'
            ldaa  Y                  ;reload number
            suba  #$2F               ;subtract ascii '/'
            bgt   notOp              ;print error if greater than ascii '/'
            ldaa  Y                  
            cmpa  #$2C               ;make sure char is not ','
            beq   notOp
            ldaa  Y               
            cmpa  #$2E               ;make sure char is not '.'
            beq   notOp
isOp        ldaa  numCount
            beq   format             ;if numCount is 0 then print format error
            ldaa  Y
            jsr   putchar
            ldx   #operator          ;store operator   
            staa  1,X+               ;store data to operator buffer
            jmp   endNum1            ;finish adding numbers and move on saving operation
notOp       jsr   checkDec           ;check if decimal number
            ldaa  numCount          
            suba  #2           
            bgt   overFlow           ;if numCount is greater than 3 then print overflow error
            ldaa  Y
            jsr   getNum             ;translate ascii to character
            staa  1,X+               ;store data to num1_4 buffer
            inc   numCount           ;increase number counter
            ldaa  Y
            jsr   putchar
            ldaa  1,Y+
            rts

overFlowJ2  jmp   overFlow
formatJ0    jmp   format

checkNum2   ldaa  Y                  ;load char
            cmpa  #0                 ;check if at end of data
            beq   endNum2J           ;finish writing if at end of data
            jsr   checkDec           ;check if decimal number
            ldaa  numCount          
            suba  #2           
            bgt   overFlowJ2         ;if numCount is greater than 3 then print overflow error
            ldaa  Y
            jsr   getNum             ;translate ascii to character
            staa  1,X+               ;store data to num2_4 buffer
            inc   numCount           ;increase number counter
            ldaa  Y
            jsr   putchar
            ldaa  1,Y+
            rts    
    
checkDec    ldaa  Y                  ;load number
            suba  #$30               ;subtract ascii '0'
            blt   formatJ0           ;print error if less than ascii '0'
            ldaa  Y                  ;reload number
            suba  #$39               ;subtract ascii '9'
            bgt   formatJ0           ;print error if greater than ascii '9'
            rts     
            
checkMin    ldaa  Y                  ;load number
            suba  #$30               ;subtract ascii '0'
            blt   timeFormatJ2       ;print error if less than ascii '0'
            ldaa  Y                  ;reload number
            suba  #$39               ;subtract ascii '9'
            bgt   timeFormatJ2       ;print error if greater than ascii '9'
            ldaa  1,Y+               ;reload number
            suba  #$30               ;subtract ascii '0'
            ldx   #minute            ;store the minute
            staa  X
            rts     
 
checkSec1   ldaa  Y                  ;load number
            suba  #$30               ;subtract ascii '0'
            blt   timeFormatJ2       ;print error if less than ascii '0'
            ldaa  Y                  ;reload number
            suba  #$35               ;subtract ascii '5'
            bgt   timeFormatJ2         ;print error if greater than ascii '5'
            ldaa  1,Y+               ;reload number
            suba  #$30               ;subtract ascii '0'            
            ldx   #second1           ;store the second
            staa  X
            rts     
            
checkSec2   ldaa  Y                  ;load number
            suba  #$30               ;subtract ascii '0'
            blt   timeFormatJ2       ;print error if less than ascii '0'
            ldaa  Y                  ;reload number
            suba  #$39               ;subtract ascii '9'
            bgt   timeFormatJ2       ;print error if greater than ascii '9'
            ldaa  1,Y+               ;reload number
            suba  #$30               ;subtract ascii '0'
            ldx   #second2           ;store the second
            staa  X
            rts                              
;***********check numbers end******************

timeFormatJ2  jmp   timeFormat

;***********ascci translations*****************

getAscii    psha
            suba  #$9                ;subtract 9
            bgt   letter1            ;jump to letters if greater than 9
            pula
            adda  #$30               ;subtract $30 to get digit character
            rts
letter1     pula                  
            adda  #$37               ;subtract $37 to get letter character
            rts

getNum      suba  #$30               ;subtract $30 to get digit character
            rts

;***********ascci translations end*************        

;***********typer writer loop******************
typeWriter  ldx   #msg2              ; print the fourth message
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
   
;****************Print Menu********************    
printMenu   ldx   #msg1              ; print the 1st message
            jsr   printmsg
            jsr   nextline

            ldx   #msg3              ; print the 3rd message
            jsr   printmsg
            jsr   nextline

            ldx   #msg4              ; print the 4th message
            jsr   printmsg
            jsr   nextline
            
            ldx   #msg5              ; print the 5th message
            jsr   printmsg
            jsr   nextline

            ldx   #msg6              ; print the 6th message
            jsr   printmsg
            jsr   nextline
;****************Print Menu ends*****************    
            
;****************nextline**********************
nextline    psha
            ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            pula
            rts
;****************end of nextline***************        

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
NULL           equ     $00
printmsg       psha                   ;Save registers
               pshx
printmsgloop   ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
               cmpa    #NULL
               beq     printmsgdone   ;end of strint yet?
               jsr     putchar        ;if not, print character and do next
               bra     printmsgloop

printmsgdone   pulx 
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
putchar        brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
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
getchar        brclr SCISR1,#%00100000,getchar7
               ldaa  SCIDRL
               rts
getchar7       clra
               rts
;****************end of getchar**************** 


;OPTIONAL
;more variable/data section below
; this is after the program code section
; of the RAM.  RAM ends at $3FFF
; in MC9S12C128 chip


msg1        DC.B        'Hello', $00
msg2        DC.B        'You may type below', $00
msg3        DC.B        'Welcome to the Calculator and Digital Clock Program!', $00
msg4        DC.B        'Please enter an addition, subtraction, multiplication, or division problem.', $00
msg5        DC.B        'Or enter "s " and a time between  0:00 and 9:59 to set the clock', $00
msg6        DC.B        'Or "q" to quit the clock', $00
formatEr    DC.B        'Invalid input format', $00
overFlowEr  DC.B        'Overflow error', $00
timeEr      DC.B        'Invalid time format. Correct example => 0:00 to 9:59', $00


            END               ; this is end of assembly source file
                              ; lines below are ignored - not assembled/compiled
