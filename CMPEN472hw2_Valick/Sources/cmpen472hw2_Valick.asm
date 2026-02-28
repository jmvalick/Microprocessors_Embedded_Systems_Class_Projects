*Title: LED Light Blinking
*
*Objective: CMPEN 472 Homework
*
*Date: 9/6/2022
*
*Programmer: Jamin Valick
*
*Register use: A: LED Light on/off state and switch 1 on/off state
*             X,Y: Delayloop counters
*              
*Memory use: RAM Location from $3000 for data
*            RAM Locations from $3100 for program
*           
*Input: Parameters hard-coded in the program - PORTB
*       Switch 1 at PORTB bit 0
*       Switch 2 at PORTB bit 1
*       Switch 3 at PORTB bit 2
*       Switch 4 at PORTB bit 3
*       
*Output: LED 1 at PORTB bit 4
*        LED 2 at PORTB bit 5
*        LED 3 at PORTB bit 6
*        LED 4 at PORTB bit 7
*        
*Observation: Blinks LEDs and blinking period can be changed with the delay loop counter value.
*
******************************************************************************************************
*Parameter Decleration Section
*
*Export Symbols
          XDEF        pstart       ;export 'pstart' symbol
          ABSENTRY    pstart       ;for assemble entry point
*
*Symbols and Macros
PORTA     EQU         $0000        ;i/o port A addressess
DDRA      EQU         $0002
PORTB     EQU         $0001        ;i/o port B addressess
DDRB      EQU         $0003
*
******************************************************************************************************
*Data Section: address used [ $3000 to $30FF ] RAM memory
*
          ORG         $3000        ;Reserved RAM memory starting address
                                   ;
Counter1  DC.W        $0100        ;X register count number for time delay
                                   ;inner loop for msec
Counter2  DC.W        $0040        ;Y register count number for time delay
                                   ;outer loop for sec
                                   ;Remaining data memory space for stack
                                   ; up to prgram memory start 
*
******************************************************************************************************
*Program Section: address used [ $3100 to $3FFF ] RAM memory
*
          ORG         $3100        ;Program start address in RAM
pstart    LDS         #$3100       ;initialize the stack pointer

;          LDAA        #%11110000   ;LED 1,2,3,4 at PORTB bit 4,5,6,7 FOR CSM-12C128 board
          LDAA        #%11111111   ;LED 1,2,3,4 at PORTB bit 4,5,6,7 FOR Simulation
          STAA        DDRB         ;set PORTB bit 4,5,6,7 as output
          
          LDAA        #%00000000
          STAA        PORTB        ;Turn off LED 1,2,3,4 (all bits in PORTB for simulation)
          
mainLoop
          LDAA        PORTB
          ANDA        #%00000001       ;read switch 1 at PORTB bit 0
          BNE         sw1pushed        ;check to see if it is pushed
          
          STAA        PORTB            ;Turn off LED 1,2,3,4 (all bits in PORTB for simulation)
          BSET        PORTB,%10000000  ;Turn ON LED 4 at PORTB bit 7
          JSR         delay1sec        ;wait for 1 second
          
          STAA        PORTB            ;Turn off LED 1,2,3,4 (all bits in PORTB for simulation)
          BSET        PORTB,%00010000  ;Turn ON LED 1 at PORTB bit 4
          JSR         delay1sec        ;wait for 1 second
          
sw1notpsh BRA         mainLoop
       
sw1pushed STAA        PORTB            ;Turn off LED 1,2,3,4 (all bits in PORTB for simulation)
          JSR         delay1sec        ;wait for 1 second
          BSET        PORTB,%10000000  ;Turn ON LED 4 at PORTB bit 7
          JSR         delay1sec        ;wait for 1 second
          BSET        PORTB,%01000000  ;Turn ON LED 3 at PORTB bit 6
          JSR         delay1sec        ;wait for 1 second
          BSET        PORTB,%00100000  ;Turn ON LED 2 at PORTB bit 5
          JSR         delay1sec        ;wait for 1 second
          BSET        PORTB,%00010000  ;Turn ON LED 1 at PORTB bit 4
          JSR         delay1sec        ;wait for 1 second
          BRA         mainLoop
*
******************************************************************************************************
*Subroutine Section: address used [ $3100 to $3FFF ] RAM memory
*
*delay1sec subroutine
*
delay1sec
          PSHY                    ;save Y
          LDY         Counter2    ;long delay 
          
dly1Loop  JSR         delayMS     ;total time delay = Y * delayMS
          DEY
          BNE         dly1Loop
          
          PULY                    ;retore Y
          RTS                     ;return
******************************************************************************************************
*delayMS subroutine
*
delayMS
          PSHX                    ;save X
          LDX         Counter1    ;short delay
          
dlyMSLoop NOP                     ;total time delay = X * NOP
          DEX
          BNE         dlyMSLoop
          
          PULX                    ;retore X
          RTS                     ;return
*
*
*
          end                     ;last line of file