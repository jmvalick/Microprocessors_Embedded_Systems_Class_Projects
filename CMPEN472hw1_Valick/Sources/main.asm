;***********************************************************************
;
; Title:          Homework 1: Load memory loop
;
; Date:           August 26, 2022
;
; Programmer:     Jamin Valick
;
; Register use:   A accumulator: character data to be filled 
;                 B accumulator: counter, number of filled locations
;                 X Register: memory address pointer
;
; Memory use:     RAM Locations from $3000 to $30CA for charactar store
;
; Input:          Parameters hard coded in the program
;
; Output:         Data filled in memory locations,
;                 from $3000 to $30CA
;
; Observtaion:    Memory spaces $3000 to $30CA
;                 are filled with '*' charactars
;
;****************************************************************************** 
; Parameter Declaration
;
; Export Symbols
        XDEF      pgstart  ;export 'pgstart' symbol
        ABSENTRY  pgstart  ;for assembly entry point
; Symbols and Macros
PORTA   EQU        $0000   ;i/o port addresses
PORTB   EQU        $0001
DDRA    EQU        $0002
DDRB    EQU        $0003
;
;******************************************************************************
; Data
;
        ORG        $3000   ;reserved memory starting address
here    DS.B       $CA     ;define storage, 202 memory locations reserved
count   DC.B       $CA     ;define constant, star count = 202
;
;******************************************************************************
; Program 
;
        ORG        $3100   ;Program Start address in RAM
pgstart ldaa       #'*'    ;load '*' into accumulator A
        ldab       count   ;load star counter into B
        ldx        #here   ;load address pointer into X
loop    staa       0,x     ;put a star
        inx                ;point to next location
        decb               ;dcrease counter
        bne        loop    ;if not done, repeat
done    bra        done    ;task finished
                           ;do nothing
;
; Add any subroutines here
;
        END                ;last line of a file
