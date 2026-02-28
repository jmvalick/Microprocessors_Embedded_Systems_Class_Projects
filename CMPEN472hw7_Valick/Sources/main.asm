***********************************************************************
*
* Title:          Homework 7
*
* Objective:      To learn how to use arithmetic instructions, 
*                 simple command line parsing, and write basic I/O system subroutines. 
*
* Date:	          October 21, 2022
*
* Programmer:     Jamin Valick
*
* Program:        Simple SCI Serial Port I/O 
*                 elementary calculator
*
* Memory use:     RAM Locations from $3000 for data, 
*                 RAM Locations from $3100 for program
*
* Output:         hyper terminal
*                 
*
* Observation:    show calculation based on input
*
***********************************************************************
* Parameter Declearation Section
*
* Export Symbols
            XDEF        pstart       ; export 'pstart' symbol
            ABSENTRY    pstart       ; for assembly entry point
  
* Symbols and Macros
PORTB       EQU         $0001        ; i/o port B addresses
DDRB        EQU         $0003

SCIBDH      EQU         $00C8        ; Serial port (SCI) Baud Register H
SCIBDL      EQU         $00C9        ; Serial port (SCI) Baud Register L
SCICR2      EQU         $00CB        ; Serial port (SCI) Control Register 2
SCISR1      EQU         $00CC        ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00CF        ; Serial port (SCI) Data Register

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

; Each message ends with $00 (NULL ASCII character) for your program.
;
; There are 256 bytes from $3000 to $3100.  If you need more bytes for
; your messages, you can put more messages 'msg3' and 'msg4' at the end of 
; the program - before the last "END" line.
                                     ; Remaining data memory space for stack,
                                     ;   up to program memory start

*
***********************************************************************
* Program Section: address used [ $3100 to $3FFF ] RAM memory
*
            ORG        $3100         ; Program start address, in RAM
pstart      LDS        #$3100        ; initialize the stack pointer

            LDAA       #%11111111    ; Set PORTB bit 0,1,2,3,4,5,6,7
            STAA       DDRB          ; as output

            LDAA       #%00000000
            STAA       PORTB         ; clear all bits of PORTB

            ldaa       #$0C          ; Enable SCI port Tx and Rx units
            staa       SCICR2        ; disable SCI interrupts

            ldd        #$0001        ; Set SCI Baud Register = $0001 => 1.5M baud at 24MHz (for simulation)
            std        SCIBDH        ; SCI port baud rate change

            jsr   printMenu          ;print instruction menu
            ldy   #buffer            ;load address of instruction buffer
            
mainLoop    jsr   getchar            ; type writer - check the key board
            cmpa  #$00               ;  if nothing typed, keep checking
            beq   mainLoop
            
            cmpa  #CR
            beq   enter              ;if Enter/Return key is pressed
            staa  1,Y+               ;if not Enter add character to instrusction buffer
            inc   buffCount          ;add 1 to buffCount
            jsr   putchar            ;character is displayed on the terminal window - echo print
            bra   mainLoop
            
            
enter       ldaa  #LF                ;move the cursor to next line
            jsr   putchar
            
            ldaa  buffCount
            suba  #9                 ;check if character count is greater than 12
            bgt   formatJ            ;print error message if more than 16 characters

            ldy   #buffer            ;load address of instruction buffer for reading
            
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
            ldaa  #45                ;clear 43 bytes of buffers
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
;***********error message end******************
add         ldd   num2
            addd  num1
            pshd
            subd  #999
            bgt   overFlow
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
            bne   overFlow
            pshd
            subd  #999
            bgt   overFlow
            puld
            jmp   instrEnd

divide      ldd   num1
            ldx   num2
            idiv
            tfr   X,D
            jmp   instrEnd
            
;***********error message end******************



;***********error message**********************
format      ldaa  #$0A
            jsr   putchar
            ldx   #formatEr          ; print the error message
            jsr   printmsg
            jsr   return
            jmp   endPrint
            
overFlow    ldaa  #$0A
            jsr   putchar
            ldx   #overFlowEr        ; print the error message
            jsr   printmsg
            jsr   return
            jmp   endPrint

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

overFlowJ   jmp   overFlow
formatJ0    jmp   format

checkNum2   ldaa  Y                  ;load char
            cmpa  NULL               ;check if at end of data
            beq   endNum2J           ;finish writing if at end of data
            jsr   checkDec           ;check if decimal number
            ldaa  numCount          
            suba  #2           
            bgt   overFlowJ          ;if numCount is greater than 3 then print overflow error
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
            
;***********check numbers end******************


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


;***********typer writer loop ends*************   
    
printMenu   ldx   #msg1              ; print the third message
            jsr   printmsg
            jsr   return

            ldx   #msg2              ; print the fourth message
            jsr   printmsg
            jsr   return
            
return      ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ; Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            rts          

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


msg1        DC.B        'Welcome to the Elementary Calculator Program!', $00
msg2        DC.B        'Please enter an addition, subtraction, multiplication, or division problem.', $00
formatEr    DC.B        'Invalid input format', $00
overFlowEr  DC.B        'Overflow error', $00


            END               ; this is end of assembly source file
                              ; lines below are ignored - not assembled/compiled
