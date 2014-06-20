ORG 32768    ; Start address of the routine
START:
        LD A,2       ; set the output channel
        CALL $1601   ; to channel 2 (main part of TV display)
        LD HL,MSG    ; Set HL register pair to address of the message
LOOP:
        LD A,(HL)    ; De-reference HL and store in A
        CP 0         ; Null terminator?
        RET Z        ; If so, return
        RST $10      ; Print the character in A
        INC HL       ; HL points at the next char to be printed
        JR LOOP
MSG:
        DEFM "Hello, world!"
        DEFB 13      ; carriage return
        DEFB 0       ; null terminator
