org 32768

start:
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
        ld de, 3               ; set length
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
