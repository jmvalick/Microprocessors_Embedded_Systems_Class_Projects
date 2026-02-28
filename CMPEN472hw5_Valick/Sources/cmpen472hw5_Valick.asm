***********************************************************************
*
* Title:          Homework 5
*
* Objective:      CMPEN 472 Homework 5, o learn how to use serial 
*                 port for cummunications, how to write subroutines, 
*                 how to construct LOOPs, and how to do user interaction.
**
* Date:	          October 2, 2022
*
* Programmer:     Jamin Valick
*
* Program:        Simple SCI Serial Port I/O 
*                 Typewriter program and 4 LED, at PORTB
*                 
* Algorithm:      Simple Serial I/O use, typewriter
*
* Register use:	  A: LED Light on/off state and switch 1 on/off state
*                 X,Y: Delay loop counters
*
* Memory use:     RAM Locations from $3000 for data, 
*                 RAM Locations from $3100 for program
*
* Output:         
*                 LED 1 at PORTB bit 4
*                 LED 2 at PORTB bit 5
*                 LED 3 at PORTB bit 6
*                 LED 4 at PORTB bit 7
*
* Observation:    control LEDs by following the printed intructions
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
F           EQU         $46          ; ASCII 'F'
L           EQU         $4C          ; ASCII 'L'
ONE         EQU         $31          ; ASCII '1'
TWO         EQU         $32          ; ASCII '2'
THREE       EQU         $33          ; ASCII '3'
FOUR        EQU         $34          ; ASCII '4'
Q           EQU         $51          ; ASCII 'Q
U           EQU         $55          ; ASCII 'U
I           EQU         $49          ; ASCII 'I
T           EQU         $54          ; ASCII 'T'

***********************************************************************
* Data Section: address used [ $3000 to $30FF ] RAM memory
*
            ORG         $3000        ; Reserved RAM memory starting address 
Counter1    DC.W        $000F        ; X register count number for time delay
                                     ;   inner loop for msec
msg1        DC.B        'Hello', $00
msg2        DC.B        'You may type below', $00
buffCount   DC.W        $0000        ;counter for number of characters in buffer
buffer      DC.W        $00000000    ;buffer to hold instruction charaters

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
            suba  #4                 ;check if character count is greater than 5
            bgt   errorJump          ;print error message if more than five characters
           
            
            ldy   #buffer            ;load address of instruction buffer for reading
bufferLoop  ldaa  1,Y+               ;pick up an ASCII character from buffer
            cmpa  #NULL              ;see if first character is NULL
            beq   errorJump           
            cmpa  #F                 ;see if first character is 'F'
            beq   Fs
            cmpa  #L                 ;see if first character is 'L'
            beq   Ls
            cmpa  #Q                 ;see if first character is 'Q'
            beq   Qs
            bra   errorJump          ;print error mesage if no first characters match
            
mainJump    bra   mainLoop
            
Fs          ldaa  buffCount          ;make sure there are only 2 characters
            cmpa  #2
            bne   errorJump
            ldaa  1,Y+  
            cmpa  #ONE               ;see if second character is '1'
            beq   F1
            cmpa  #TWO               ;see if second character is '2'
            beq   F2
            cmpa  #THREE             ;see if second character is '3'
            beq   F3
            cmpa  #FOUR              ;see if second character is '4'
            beq   F4
            bra   errorJump
Ls          ldaa  buffCount          ;make sure there are only 2 characters
            cmpa  #2
            bne   errorJump
            ldaa  1,Y+
            cmpa  #ONE               ;see if second character is '1'
            beq   L1Jump
            cmpa  #TWO               ;see if second character is '2'
            beq   L2Jump
            cmpa  #THREE             ;see if second character is '3'
            beq   L3Jump
            cmpa  #FOUR              ;see if second character is '4'
            beq   L4Jump
            bra   errorJump
Qs          ldaa  1,Y+
            cmpa  #U                 ;see if second character is 'U'
            bne   errorJump              ;if not print error message
            ldaa  1,Y+
            cmpa  #I                 ;see if third character is 'I'
            bne   errorJump              ;if not print error message
            ldaa  1,Y+
            cmpa  #T                 ;see if fourth character is 'T'
            bne   errorJump              ;if not print error message
            bra   typeJump
errorJump   bra   error
                                     ;if quit branch to typeWriter program
bufferEnd   clr   buffCount          ;reset buffer counter
            ldy   #buffer            ;reset address of instruction buffer
            bra   mainJump           

;subroutine section below
;***********LED instructions****************

error     LDX         #msg13           ; print the error message
          JSR         printmsg
          JSR         return
          BRA         bufferEnd

F1        PSHA                         ;save A register
          LDAA        #%11101111       ;turn off LED1 at portB bit 4
          ANDA        PORTB
          STAA        PORTB
          PULA                         ;restor A register
          BRA         bufferEnd
          
F2        PSHA                         ;save A register
          LDAA        #%11011111       ;turn off LED2 at portB bit 5
          ANDA        PORTB
          STAA        PORTB
          PULA                         ;restor A register
          BRA         bufferEnd
          

F3        PSHA                         ;save A register
          LDAA        #%10111111       ;turn off LED3 at portB bit 6
          ANDA        PORTB
          STAA        PORTB
          PULA                         ;restor A register
          BRA         bufferEnd
   
                  
L1Jump    BRA         L1
L2Jump    BRA         L2
L3Jump    BRA         L3          
L4Jump    BRA         L4
typeJump  BRA         typeWriter
bufEndJ   BRA         bufferEnd


F4        PSHA                         ;save A register
          PSHB                         ;save B register
          LDAB        #84              ;b register holds portion of time spent on
OUTLOOP1  TBA
loop1     JSR         LED4on           ;jump to LED on instruction
          DECA                         ;decrease counter by one
          BNE         loop1
          
          LDAA        #85
          SBA                          ;get the fraction time off
loop2     JSR         LED4off          ;jump to LED off instruction
          DECA                         ;decrease counter by one
          BNE         loop2            ;if not at end jump to loop2
          SUBB        #1
          BNE         OUTLOOP1         ;stop loop when 100% off
          LDAA        #%01111111       ;turn off LED4 at portB bit 67
          ANDA        PORTB
          STAA        PORTB
          PULA                         ;restor A register
          PULB                         ;restor B register
          BRA         bufferEnd
            
L1        PSHA                         ;save A register
          LDAA        #%00010000       ;turn on LED1 at portB bit 4
          ORAA        PORTB
          STAA        PORTB
          PULA                         ;restor A register
          BRA         bufferEnd

L2        PSHA                         ;save A register
          LDAA        #%00100000       ;turn on LED2 at portB bit 5
          ORAA        PORTB
          STAA        PORTB
          PULA                            ;restor A register
          BRA         bufferEnd          
          
L3        PSHA                         ;save A register
          LDAA        #%01000000       ;turn on LED3 at portB bit 6
          ORAA        PORTB
          STAA        PORTB
          PULA                         ;restor A register
          BRA         bufferEnd  
                 
L4        PSHA                         ;save A register
          PSHB                         ;save B register
          LDAB        #1               ;b register holds portion of time spent on
OUTLOOP2  TBA                          
loop3     JSR         LED4on           ;jump to LED on instruction
          DECA                         ;decrease counter by one
          BNE         loop3
          
          LDAA        #85
          SBA                          ;get the fraction time off
loop4     JSR         LED4off          ;jump to LED off instruction
          DECA                         ;decrease counter by one
          BNE         loop4            ;if not at end jump to loop2
          ADDB        #1
          CMPB        #85
          BNE         OUTLOOP2         ;stop loop when 100% on
          LDAA        #%10000000       ;turn on LED4 at portB bit 7
          ORAA        PORTB
          STAA        PORTB
          PULA                         ;restor A register
          PULB                         ;restor B register
          BRA         bufEndJ
;***********LED instructions end***************

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

;***********LED4 controls**********************
LED4off 
          PSHA                         ;save A register
          LDAA        #%01111111       ;turn off LED4 at portB bit 7
          ANDA        PORTB
          STAA        PORTB
          JSR         delay10us        ;wait 10 us
          PULA                         ;restor A register
          RTS
 
LED4on    PSHA                         ;save A register
          LDAA        #%10000000       ;turn on LED4 at portB bit 7
          ORAA        PORTB
          STAA        PORTB
          JSR         delay10us        ;wait 10 us
          PULA                         ;restor A register
          RTS
                     
delay10us
          PSHX                         ;save X
          LDX         Counter1         ;short delay
                                    
dlyusLoop NOP                          ;total time delay = X * NOP
          DEX
          BNE         dlyusLoop
          
          PULX                         ;retore X
          RTS                          ;return
;***********LED4 controls end*****************      

printMenu   ldx   #msg3              ; print the third message
            jsr   printmsg
            jsr   return

            ldx   #msg4              ; print the fourth message
            jsr   printmsg
            jsr   return

            ldx   #msg5              ; print the fifth message
            jsr   printmsg
            jsr   return
            
            ldx   #msg6              ; print the sith message
            jsr   printmsg
            jsr   return
            
            ldx   #msg7              ; print the seventh message
            jsr   printmsg
            jsr   return

            ldx   #msg8              ; print the eighth message
            jsr   printmsg
            jsr   return
            
            ldx   #msg9              ; print the ninth message
            jsr   printmsg
            jsr   return
            
            ldx   #msg10             ; print the tenth message
            jsr   printmsg
            jsr   return

            
            ldx   #msg11             ; print the eleventh message
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


msg3           DC.B        'L1: Turn on LED1', $00
msg4           DC.B        'F1: Turn off LED1', $00
msg5           DC.B        'L2: Turn on LED2', $00
msg6           DC.B        'F2: Turn off LED2', $00
msg7           DC.B        'L3: Turn on LED3', $00
msg8           DC.B        'F3: Turn off LED3', $00
msg9           DC.B        'L4: LED 4 goes from 0% light level to 100% light level in 6 seconds', $00
msg10          DC.B        'F4: LED 4 goes from 100% light level to 0% light level in 6 seconds', $00
msg11          DC.B        'QUIT: Quit menu program, run Type Writer program', $00
msg12          DC.B        'Enter your command below:', $00
msg13          DC.B        'Error: Invalid command', $00




               END               ; this is end of assembly source file
                                 ; lines below are ignored - not assembled/compiled
