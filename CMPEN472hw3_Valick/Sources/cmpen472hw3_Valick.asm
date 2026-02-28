*Title: LED Light Blinking PWM
*
*Date: 9/12/2022
*
*Programmer: Jamin Valick
*
*Register use: A: LED Light on/off state and switch 1 on/off state
*              X,Y: Delayloop counters
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
*Purpose: Pulse the LED lights
*         Initial state of LED lights: (0) LED 1 is OFF (0%), LED 2 is ON (100%), and LED 3 is OFF (0%)
*         SW1 NOT pressed: (1) LED 4 turn on for 0.12 second, then turn off for 0.88 second, and repeat
*         SW1 pressed: (2) LED 4 turn on for 0.48 second, then turn off for 0.52 second, and repeat
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
Counter1  DC.W        $000F        ;X register count number for time delay
                                   ;loop for usec
                                   ;Remaining data memory space for stack
                                   ; up to prgram memory start 
*
******************************************************************************************************
*Program Section: address used [ $3100 to $3FFF ] RAM memory
*
          ORG         $3100        ;Program start address in RAM
pstart    LDS         #$3100       ;initialize the stack pointer

          LDAA        #%11110001   ;LED 1,2,3,4 at PORTB bit 4,5,6,7 FOR Simulation
          STAA        DDRB         ;set PORTB bit 4,5,6,7 as output
                                   ;plus bit 0 for switch 1
          
          LDAA        #%00000000
          STAA        PORTB        ;Clear all bits
          
mainLoop
          BSET        PORTB,%00100000  ;Turn ON LED 2 at PORTB bit 5
          LDAA        PORTB
          ANDA        #%00000001       ;read switch 1 at PORTB bit 0
          BNE         p48LED4          ;check to see if it is pushed
          
          
p12LED4   
          LDAB        #$000C           ;set counter to 12
loop1
          JSR         LED4on           ;jump to LED on instruction
          DECB                         ;decrease counter by one
          BNE         loop1
          
          LDAB        #$0058           ;set counter to 88
loop2
          JSR         LED4off          ;jump to LED off instruction
          DECB                         ;decrease counter by one
          BNE         loop2            ;if not at end jump to loop2
          BRA         mainLoop
          
       
p48LED4   
          LDAB        #$0030           ;set counter to 48
loop3
          JSR         LED4on           ;jump to LED on instruction
          DECB                         ;decrease counter by one
          BNE         loop3
          
          LDAB        #$0034            ;set counter to 52
loop4
          JSR         LED4off          ;jump to LED off instruction
          DECB                         ;decrease counter by one
          BNE         loop4            ;if not at end jump to loop4
          BRA         mainLoop
*
******************************************************************************************************
*Subroutine Section: address used [ $3100 to $3FFF ] RAM memory
*
LED4off 
          PSHA                         ;save A register
          LDAA        #%01111111       ;turn off LED4 at portB bit 7
          ANDA        PORTB
          STAA        PORTB
          JSR         delay10us        ;wait 10 us
          PULA                         ;restor A register
          RTS
* 
LED4on    PSHA                         ;save A register
          LDAA        #%10000000       ;turn on LED4 at portB bit 7
          ORAA        PORTB
          STAA        PORTB
          JSR         delay10us        ;wait 10 us
          PULA                         ;restor A register
          RTS
*
                     
delay10us
          PSHX                         ;save X
          LDX         Counter1         ;short delay
                                    
dlyusLoop NOP                          ;total time delay = X * NOP
          DEX
          BNE         dlyusLoop
          
          PULX                         ;retore X
          RTS                          ;return
*
          end                          ;last line of file