; Hello World in Z80 assembly language.
;  Assembles for ZX Spectrum
;
; Assembled/Tested under linux with the following tools...
; z80asm $@.asm ; utils/bin2tap a.bin $@.tap ; xspect $@.tap
;
; Author: Nathanial Hendler ; retards.org
; Adapted from Damien Guard's 99 Bottles of Beer on the wall...
; Adapted from the Alan deLespinasse's Intel 8086 version

org 32768

start:
        ld      a, 2         ; channel 2 = "S" for screen
        call    $1601        ; Select print channel using ROM

        ld hl,line                    ; Print line
        call printline
        ret

printline:                     ; Routine to print out a line
        ld a,(hl)                     ; Get character to print
        cp '$'                        ; See if it '$' terminator
        jp z,printend                 ; We're done if it is
        rst 16                        ; Spectrum: Print the character in 'A'
        inc hl                        ; Move onto the next character
        jp printline                  ; Loop round

printend:

        ld b, 30
osloop:
        ld d, b
        push bc
        ld b, 12
sloop:
        ld a, 22                ; start with 30
        sub d                   ; subtract outer loop value
        add a, b                ; add inner loop value

        push af                 ; print value - ish
        call draw               ; rst works too?
        rr a
        add a, 30
        call 16
        pop af                  ; end print

        ld h, a                 ; load pitch into (top byte) of hl
        rl h
        push bc
        push de
        ld de, 3                ; set length
        call 949                ; make beep
        pop de
        pop bc
        djnz sloop              ; do loops
        pop bc
        djnz osloop
        ret
draw:
        push bc
        ld hl, 5800h            ; 5800
        ld a, b
        add a, 190
        ld b, a
dloop:
        inc hl
        ld (hl), b
        inc hl
        ld (hl), b
        inc hl
        ld (hl), b
        inc hl
        ld (hl), b
        djnz dloop
        pop bc
        ret
dend:

; Data
line:
        defm "Hello, world."
        defb 13
        defm "$"
