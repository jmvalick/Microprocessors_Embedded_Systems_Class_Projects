***********************************************************************
*
* Title:          Homework 6
*
* Objective:      To show contents and wrtie to memory location on demand
*
* Date:	          October 11, 2022
*
* Programmer:     Jamin Valick
*
* Program:        Simple SCI Serial Port I/O 
*                 store and read on demand
*
* Memory use:     RAM Locations from $3000 for data, 
*                 RAM Locations from $3100 for program
*
* Output:         hyper terminal
*                 
* Observation:    shows content of given adress and writes content to given address
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
            ORG         $3000        ; Reserved RAM memory starting address 
msg1        DC.B        'Hello', $00
msg2        DC.B        'You may type below', $00
buffCount   DS.B        2                    ;counter for number of characters in buffer
buffer      DS.B        13                   ;buffer to hold message from terminal
addr4       DS.B        5                    ;buffer to hold seperated characters for address
addr        DS.B        3                    ;buffer to hold address
data4       DS.B        5                    ;buffer to hold seperated characters for data
data        DS.B        3                    ;buffer to hold data
decimal4    DS.B        6                    ;buffer to hold seperated decimal numbers for data
decimal     DS.B        5                    ;buffer to hold decimal number

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
            suba  #12                ;check if character count is greater than 12
            bgt   errorJump0         ;print error message if more than 16 characters

            ldy   #buffer            ;load address of instruction buffer for reading
            ldaa  1,Y+               ;pick up an ASCII character from buffer
            cmpa  #NULL              ;see if first character is NULL
            beq   errorJump0           
            cmpa  #S                 ;see if first character is 'S'
            beq   show
            cmpa  #W                 ;see if first character is 'W'
            beq   writeJump
            cmpa  #Q                 ;see if first character is 'Q'
            beq   QsJump0
            bra   errorJump0         ;print error mesage if no first characters match
            
mainJump    bra   mainLoop
            
show        ldaa  1,Y+
            cmpa  #dollar            ;see if second character is '$'
            bne   errorJump0         ;if not print error message
            
            ldx   #addr4             ;load address of seperated adress buffer
            jsr   checkAddr          ;check address and store converted characters to seperated adress buffer
            jsr   checkAddr
            jsr   checkAddr
            jsr   checkAddr

            ldx   #addr4             ;reload address of seperated adress buffer
            ldaa  $00
            ldab  1,X+               ;combine characters into address
            ldy   #16
            emul
            addb  1,X+
            ldy   #16
            emul
            addb  1,X+
            ldy   #16
            emul 
            addb  1,X+
            
            ldx   #addr               
            std   X                  ;store characters into address buffer
            
            ldy   addr               ;load data into d
            ldd   Y
            ldy   #data4+4
loop1       ldx   #16
            idiv
            beq   done
            stab  1,Y- 
            tfr   X,D
            bra   loop1

QsJump0     bra   QsJump       
errorJump0  bra   errorJump2
mainJump1   bra   mainJump
writeJump   bra   write
      
            
done        stab  1,Y-
            ldx   #data4+1           ;load address of seperated data buffer
            ldaa  $00
            ldab  1,X+               ;combine characters into data
            ldy   #16
            emul
            addb  1,X+
            ldy   #16
            emul
            addb  1,X+
            ldy   #16
            emul 
            addb  1,X+
            
            ldx   #data               
            std   X                  ;store characters into address buffer
            jsr   printS            ;print confirmation message
            ldaa  #$A
            jsr   putchar 
            bra   instrEndJ
            
write       ldaa  1,Y+
            cmpa  #dollar            ;see if second character is '$'
            bne   errorJump2         ;if not print error message
            
            ldx   #addr4             ;load address of seperated adress buffer
            jsr   checkAddr          ;check address and store converted characters to seperated adress buffer
            jsr   checkAddr
            jsr   checkAddr
            jsr   checkAddr
            bra   endAWrite

errorJump2  bra   errorJump3
QsJump      bra   QsJump2
instrEndJ   bra   instrEndJ2
mainJump2   bra   mainJump1
            
endAWrite   pshy                     ;save location of message buffer
            ldx   #addr4             ;reload address of seperated adress buffer
            ldaa  $00
            ldab  1,X+               ;combine characters into address
            ldy   #16
            emul
            addb  1,X+
            ldy   #16
            emul
            addb  1,X+
            ldy   #16
            emul 
            addb  1,X+
            
            ldx   #addr               
            std   X                  ;store characters into address buffer
            
            puly                     ;restore location of message buffer
            ldaa  1,Y+               
            cmpa  #$20               ;check if charater is space
            bne   errorJump3         ;print error message if not space
            
            ldaa  Y 
            cmpa  NULL                
            beq   errorJump3
            cmpa  #dollar            ;check if data is in hex
            bne   writeDec
            iny                      ;move to next character
            
            ldx   #data4             ;load address of seperated data buffer
            jsr   checkHdata         ;check if hex data is valid and store converted character to seperated data buffer
            jsr   checkHdata
            jsr   checkHdata
            jsr   checkHdata
            bra   endHwrite

errorJump3  bra   errorJump4
QsJump2     bra   QsJump3
instrEndJ2  bra   instrEndJ3
mainJump3   bra   mainJump2 
endAWriteJ  bra   endAWrite
            
endHwrite   ldx   #data4             ;reload address of seperated adress buffer
            ldaa  $00
            ldab  1,X+               ;combine characters into address
            ldy   #16
            emul
            addb  1,X+
            ldy   #16
            emul
            addb  1,X+
            ldy   #16
            emul 
            addb  1,X+
            
            ldx   #data              ;load data buffer 
            std   X                  ;store characters into data buffer
            
            ldy   addr               ;load address buffer
            std   Y                  ;store data at address
            
            jsr   printWH            ;print confirmation message
            ldaa  #$A
            jsr   putchar 
            bra   instrEndJ3
            
writeDec    ldx   #data4
            jsr   checkHdata         ;check if decimal data is valid and store converted character to seperated data buffer
            jsr   checkHdata
            jsr   checkHdata
            jsr   checkHdata
            bra   endDwrite

errorJump4  bra   errorJump5
QsJump3     bra   QsJump4
instrEndJ3  bra   instrEndJ4
mainJump4   bra   mainJump3 
endAWriteJ3 bra   endAWriteJ
endHwriteJ3 bra   endHwrite 

            
            
endDwrite   ldx   #data4             ;reload address of seperated data buffer
            ldaa  $00
            ldab  1,X+               ;combine characters into decimal data
            ldy   #16
            emul
            addb  1,X+
            ldy   #16
            emul
            addb  1,X+
            ldy   #16
            emul 
            addb  1,X+
            
            ldx   #decimal           ;load decimal buffer 
            std   X                  ;store characters into decimal buffer
            
            ldx   #data4             ;reload address of seperated adress buffer
            ldaa  $00
            ldab  1,X+               ;calculate hex value
            ldy   #$A
            emul
            addb  1,X+
            ldy   #$A
            emul
            addb  1,X+
            ldy   #$A
            emul 
            addb  1,X+                
            
            ldx   #data              ;load data buffer 
            std   X                  ;store characters into data buffer
            
            ldy   addr               ;load address buffer
            std   Y                  ;store data at address
            
            jsr   printWD            ;print confirmation message
returnWD5   ldaa  #$A
            jsr   putchar   
            bra   instrEnd
                     

errorJump5  bra   error
QsJump4     bra   Qs
instrEndJ4  bra   instrEnd
endAWriteJ2 bra   endAWriteJ3 
endHwriteJ2 bra   endHwriteJ3
mainJump5   bra   mainJump4 
endDwriteJ2 bra   endDwrite          
  
 

Qs          ldaa  1,Y+
            cmpa  #U                 ;see if second character is 'U'
            bne   errorJump              ;if not print error message
            ldaa  1,Y+
            cmpa  #I                 ;see if third character is 'I'
            bne   errorJump              ;if not print error message
            ldaa  1,Y+
            cmpa  #T                 ;see if fourth character is 'T'
            bne   errorJump              ;if not print error message
            bra   typeJump           ;if quit branch to typeWriter program
            
errorJump   bra   error

instrEnd    ldaa  #41                ;clear 40 bytes of buffers
            ldx   #buffCount
clearLoop   clr   X
            inx         
            deca
            beq   clearEnd
            bra   clearLoop
clearEnd    ldy   #buffer            ;reset address of instruction buffer
            bra   mainJump5           

;subroutine section below
;***********error message**********************
error       LDX   #msg12           ; print the error message
            JSR   printmsg
            JSR   return
            BRA   instrEnd

;***********error message end******************

errorJump1  bra   error
typeJump    bra   typeJump1
endHwriteJ1 bra   endHwriteJ2
endDwriteJ1 bra   endDwriteJ2
returnWD4   bra   returnWD5          
 
;***********check numbers**********************
checkAddr   ldaa  Y                  ;load character
            cmpa  #$20               ;check if character is space
            beq   endAWriteJ2        ;if address is done branch to store
            jsr   checkHex           ;check to make sure charater is hex number
            jsr   getChar            ;translate ascii to character
            staa  1,X+               ;store number address buffer
            ldaa  1,Y+
            rts   

checkHex    ldaa  Y                  ;load number
            suba  #$30               ;subtract ascii '0'
            blt   errorJump1         ;print error if less than ascii '0'
            ldaa  Y                  ;reload number
            suba  #$46               ;subtract ascii 'F'
            bgt   errorJump1         ;print error if greater than ascii 'F'
            ldaa  Y                  ;reload number
            cmpa  #$40               ;compare ascii '@'
            beq   errorJump1         ;print error message if '@'
            rts
            
checkDec    ldaa  Y                  ;load number
            suba  #$30               ;subtract ascii '0'
            blt   errorJump1         ;print error if less than ascii '0'
            ldaa  1,Y+               ;reload number
            suba  #$39               ;subtract ascii '9'
            bgt   errorJump1         ;print error if greater than ascii '9'
            rts
            
checkHdata  ldaa  Y               
            cmpa  NULL               ;check if at end of data
            beq   endHwriteJ1        ;finish writing if at end of data
            jsr   checkHex           ;check to make sure charater is hex number
            jsr   getChar            ;translate ascii to character
            staa  1,X+               ;store data to data buffer
            inc   buffCount          ;increase buffer counter
            ldaa  1,Y+
            rts

checkDdata  ldaa  Y               
            cmpa  NULL               ;check if at end of data
            beq   endDwriteJ1        ;finish writing if at end of data
            jsr   checkDec
            jsr   getChar            ;translate ascii to character
            staa  1,X+               ;store data to data buffer
            inc   buffCount          ;increase buffer counter
            ldaa  1,Y+
            rts
           
;***********check numbers end******************

typeJump1   bra   typeJump2
returnWD3   bra   returnWD4 

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

getChar     psha
            suba  #$39               ;subtract ascii '9'
            bgt   letter2            ;jump to letters if greater than '9'
            pula
            suba  #$30               ;subtract $30 to get digit character
            rts
letter2     pula                
            suba  #$37               ;subtract $37 to get letter character
            rts

;***********ascci translations end*************

typeJump2   bra   typeJump3
returnWD2   bra   returnWD3 

;***********confirmation messages**************
printWH     ldy   #buffer            ;begin printing confirmation message
            iny   
loopPrint1  ldaa  1,Y+
            cmpa  #$20               ;check if character is space
            beq   contPrint1           
            jsr   putchar
            bra   loopPrint1
contPrint1  ldaa  #$3D               ;print '='
            jsr   putchar
loopPrint2  ldaa  1,Y+
            cmpa  NULL               ;check if character is NULL
            beq   printDec           ;move to print decimal  
            jsr   putchar
            bra   loopPrint2
printDec    ldaa  #$20
            jsr   putchar 
            ldy   #decimal+4
            ldd   data               ;print decimal equivalent
loopPrint3  ldx   #$A
            idiv  
            beq   endPrintWH
            tba   
            stab  1,Y-
            tfr   X,D
            bra   loopPrint3
endPrintWH  tba   
            stab  1,Y-
            ldy   #decimal
            ldaa  Y
            jsr   getAscii
            jsr   putchar
            iny
            ldaa  Y
            jsr   getAscii
            jsr   putchar
            iny
            ldaa  Y
            jsr   getAscii
            jsr   putchar
            iny
            ldaa  Y
            jsr   getAscii
            jsr   putchar
            iny
            ldaa  Y
            jsr   getAscii
            jsr   putchar            
            rts
            
typeJump3   bra   typeJump4 
returnWD    bra   returnWD2       
          
printWD     ldy   #buffer            ;begin printing confirmation message
            iny   
loopPrint4  ldaa  1,Y+
            cmpa  #$20               ;check if character is space or NULL
            beq   contPrint2
            cmpa  NULL   
            beq   contPrint2        
            jsr   putchar
            bra   loopPrint4
contPrint2  pshy
            ldaa  #$3D               ;print '='
            jsr   putchar
            ldaa  #dollar
            jsr   putchar
            ldy   #decimal4+3
            ldd   data              ;load hex data
printLoop5  ldx   #16
            idiv
            beq   printDec2
            stab  1,Y- 
            tfr   X,D
            bra   printLoop5
printDec2   stab  1,Y-
            ldab  #5
            ldy   #decimal4
loopPrint6  decb  
            beq   printDec3          ;move to print decimal
            ldaa  Y
            jsr   getAscii
            jsr   putchar
            iny
            bra   loopPrint6
printDec3   ldaa  #$20               ;print space
            jsr   putchar             
            ldab  #5
            ldy   #data4
loopPrint7  decb  
            beq   endPrintWD         ;move to print decimal
            ldaa  Y
            jsr   getAscii
            jsr   putchar
            iny
            bra   loopPrint7
endPrintWD  bra   returnWD


typeJump4   bra   typeJump5


printS      ldy   #buffer            ;begin printing confirmation message
            iny   
loopPrint8  ldaa  1,Y+
            cmpa  NULL               ;check if character is NULL
            beq   contPrint8           
            jsr   putchar
            bra   loopPrint8
contPrint8  ldaa  #$3D               ;print '='
            jsr   putchar
            ldaa  #dollar
            jsr   putchar
            ldy   #data4+1
            ldaa  1,Y+
            jsr   getAscii
            jsr   putchar
            ldaa  1,Y+
            jsr   getAscii
            jsr   putchar
            ldaa  1,Y+
            jsr   getAscii
            jsr   putchar
            ldaa  1,Y+
            jsr   getAscii
            jsr   putchar
            ldaa  #$20
            jsr   putchar 
            ldy   #decimal+4
            ldd   data               ;print decimal equivalent
loopPrint9  ldx   #$A
            idiv  
            beq   endPrintS
            tba   
            stab  1,Y-
            tfr   X,D
            bra   loopPrint9
            
typeJump5   bra   typeWriter 
            
endPrintS   tba   
            stab  1,Y-
            ldy   #decimal
            ldaa  Y
            jsr   getAscii
            jsr   putchar
            iny
            ldaa  Y
            jsr   getAscii
            jsr   putchar
            iny
            ldaa  Y
            jsr   getAscii
            jsr   putchar
            iny
            ldaa  Y
            jsr   getAscii
            jsr   putchar
            iny
            ldaa  Y
            jsr   getAscii
            jsr   putchar            
            rts
            
;***********confirmation messages end**********          


;***********typer writer loop******************
typeWriter  jsr   getchar            ; type writer - check the key board
            cmpa  #$00               ;  if nothing typed, keep checking
            beq   typeWriter
                                     ;  otherwise - what is typed on key board
            jsr   putchar            ; is displayed on the terminal window - echo print
            
            staa  PORTB              ; show the character on PORTB

            cmpa  #CR
            bne   typeWriter         ; if Enter/Return key is pressed, move the
            ldaa  #LF                ; cursor to next line
            jsr   putchar
            bra   typeWriter  
;***********typer writer loop ends*************   
    
printMenu   ldx   #msg3              ; print the third message
            jsr   printmsg
            jsr   return

            ldx   #msg4              ; print the fourth message
            jsr   printmsg
            jsr   return

            ldx   #msg5              ; print the fifth message
            jsr   printmsg
            jsr   return
            
            ldx   #msg6              ; print the sixth message
            jsr   printmsg
            jsr   return
            
            ldx   #msg7              ; print the seventh message
            jsr   printmsg
            jsr   return
            
            ldx   #msg8              ; print the eigth message
            jsr   printmsg
            jsr   return
            
            ldx   #msg9              ; print the nineth message
            jsr   printmsg
            jsr   return
            
            ldx   #msg10             ; print the tenth message
            jsr   printmsg
            jsr   return
            
            ldx   #msg11             ; print the eleventh message
            jsr   printmsg
            jsr   return
            
            ldx   #msg13             ; print the 13th message
            jsr   printmsg
            jsr   return
            rts
            
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


msg3        DC.B        'Welcome to the Simple Memory Access Program!', $00
msg4        DC.B        'Enter one of the following commands and hit Enter', $00
msg5        DC.B        'S:  Show the contents of memory location in word', $00
msg6        DC.B        'W:  Write the data word (not byte) to memory location', $00
msg7        DC.B        'QUIT:   Quit the main program, run Type writer program', $00
msg8        DC.B        'Examples:', $00
msg9        DC.B        '>S$3156        to see the memory content at $3156 and $3157', $00
msg10       DC.B        '>W$3103 $126A  to write $126A to memory locations $3103 and $3104', $00
msg11       DC.B        '>W$3103 4714   to write $126A to memory location $3103 and $3104', $00
msg12       DC.B        'Invalid Command', $00
msg13       DC.B        'please use only FOUR characters for the address and data', $00

               END               ; this is end of assembly source file
                                 ; lines below are ignored - not assembled/compiled
