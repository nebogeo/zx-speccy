org 32768

MAIN:

        call game_start

        DI                         ; Make sure no interrupts are called!
	            LD HL,Interrupt            ; Address of the interrupt routine
	            LD IX,00fff0h                ; Where to stick this code
	            LD (IX+0004h),00C3h            ; Z80 opcode for JP
	            LD (IX+0005h),L              ; Where to JP to (in HL)
	            LD (IX+0006h),H
	            LD (IX+000Fh),0018h            ; Z80 Opcode for JR
	            LD A,0039h                   ; High byte address of vector table
	            LD I,A                     ; Set I register to this
	            IM 2                       ; Set Interrupt Mode 2
	            EI                         ; Enable interrupts again

	LOOP:       HALT                      ; Wait for interrupt	            CALL game_update          ; Do some foreground stuff
	            JR LOOP                    ; Loop around forever

	Interrupt:  DI                         ; Disable interrupts
	            PUSH AF                    ; Preserve all registers
	            PUSH BC
	            PUSH DE
	            PUSH HL
	            PUSH IX
	            EXX
	            EX AF,AF'
	            PUSH AF
	            PUSH BC
	            PUSH DE
	            PUSH HL
	            PUSH IY

        call game_update
        ;;         CALL Some_Interrupt_Code ; Do some interrupt stuff
	            POP IY                     ; Restore all registers
	            POP HL
	            POP DE
	            POP BC
	            POP AF
	            EXX
	            EX AF,AF'
	            POP IX
	            POP HL
	            POP DE
	            POP BC
	            POP AF
                EI                         ; Enable interrupts
	            RETI                       ; Return from interrupt


game_start:


        ld b, 30
        ld c, 8
        ld hl, test_image
        call draw_image

        ld b, 33
        ld c, 19
        ld hl, test_image
        call draw_image

        ld b, 50
        ld c, 50
        ld d, 120
        ld e, 100
        call draw_rect

        ld b, 55
        ld c, 55
        ld d, 125
        ld e, 105
        call draw_rect

        ld h, 20

        ld b, 120
        ld c, 156
        ld d, 125
        ld e, 170

        ld a, %10101010
        ld (pix_mask), a

pyramid:
        dec b
        dec b
        dec c
        dec c
        inc d
        inc d
        inc e
        inc e
        call draw_rect
        dec h
        jp nz,pyramid

        ret

game_update:

        ld hl, my_sprite_a
        call keyboard_move_sprite
        call draw_large_sprite
        ld hl, my_sprite_b
        call move_sprite
        call draw_sprite
        ld hl, my_sprite_c
        call move_sprite
        call draw_sprite
        ld hl, my_sprite_d
        call move_sprite
        call draw_sprite
        ld hl, my_sprite_e
        call move_sprite
        call draw_sprite
        ld hl, my_sprite_f
        call move_sprite
        call draw_sprite
        ld hl, my_sprite_g
        call move_sprite
        call draw_sprite


        ld hl, my_sprite_a
        call draw_large_sprite
        ld hl, my_sprite_b
        call draw_sprite
        ld hl, my_sprite_c
        call draw_sprite
        ld hl, my_sprite_d
        call draw_sprite
        ld hl, my_sprite_e
        call draw_sprite
        ld hl, my_sprite_f
        call draw_sprite
        ld hl, my_sprite_g
        call draw_sprite

        ret

make_sound:
        ld hl, my_sprite_a
        push de
        push hl
        inc hl
        ld a, (hl)
        ld l, 0
        srl a
        srl a
        add a, -10
        ld h, a                 ; load pitch into (top byte) of hl
        ld de, 1               ; set length
        call 949               ; make beep
        pop hl
        pop de
        ret


draw_sprite:                    ; hl = sprite_addr
        push hl
        push bc
        ld c, (hl)
        inc hl
        ld b, (hl)
        inc hl
        inc hl
        inc hl
        call draw_image
        pop bc
        pop hl
        ret

draw_large_sprite:                    ; hl = sprite_addr
        push hl
        push bc
        push de
        ld c, (hl)
        inc hl
        ld b, (hl)
        inc hl
        inc hl
        inc hl
        call draw_image
        ld a, c
        add a, 8
        ld c, a
        ld d, 0
        ld e, 8
        add hl, de
        call draw_image
        ld a, b
        add a, 8
        ld b, a
        ld d, 0
        ld e, 8
        add hl, de
        call draw_image
        ld a, c
        sub 8
        ld c, a
        ld d, 0
        ld e, 8
        add hl, de
        call draw_image
        pop de
        pop bc
        pop hl
        ret


move_sprite:                    ; add direction to position
        push hl                 ; rather think there must be
        inc hl                  ; a faster way...
        inc hl
        ld b, (hl)
        inc hl
        ld c, (hl)
        pop hl
        push hl
        ld a, (hl)
        add a, b
        ld (hl), a
        inc hl
        ld a, (hl)
        add a, c
        ld (hl), a
        pop hl
        ret

keyboard_move_sprite:
        push hl
        call read_keyboard
        cp 'A'
        jp z, key_left
        cp 'D'
        jp z, key_right
        inc hl
        cp 'W'
        jp z, key_up
        cp 'S'
        jp z, key_down
        cp ' '
        jp z, key_space
        pop hl
        ret
key_left:
        dec (hl)
        pop hl
        ret
key_right:
        inc (hl)
        pop hl
        ret
key_up:
        dec (hl)
        pop hl
        ret
key_down:
        inc (hl)
        pop hl
        ret
key_space:
        pop hl
        call make_sound
        ret

my_sprite_a:
        db 128
        db 64
        db 1
        db 0
        db %00000011
        db %00011100
        db %00100000
        db %01000000
        db %01000110
        db %01000110
        db %10000000
        db %10000000

        db %11000000
        db %00111000
        db %00000100
        db %00000010
        db %01100010
        db %01100010
        db %00000001
        db %00000001

        db %00000001
        db %00000001
        db %00010010
        db %00100010
        db %11000010
        db %00000100
        db %00111000
        db %11000000

        db %10000000
        db %10000000
        db %01001000
        db %01000100
        db %01000011
        db %00100000
        db %00011100
        db %00000011


my_sprite_b:
        db 228
        db 64
        db 2
        db 0
        db %00011000
        db %00100100
        db %01011010
        db %10100101
        db %10100101
        db %01011010
        db %00100100
        db %00011000

my_sprite_c:
        db 28
        db 64
        db 2
        db 1
        db %00011000
        db %00100100
        db %01011010
        db %10100101
        db %10100101
        db %01011010
        db %00100100
        db %00011000
my_sprite_d:
        db 38
        db 164
        db 2
        db -1
        db %00011000
        db %00111100
        db %01111110
        db %00001111
        db %00001111
        db %01111110
        db %00111100
        db %00011000
my_sprite_e:
        db 228
        db 200
        db 0
        db -1
        db %11110000
        db %11110000
        db %00001111
        db %11110000
        db %11110000
        db %00001111
        db %11110000
        db %11110000
my_sprite_f:
        db 138
        db 12
        db 2
        db 0
        db %11111111
        db %00100100
        db %01011010
        db %10100101
        db %10100101
        db %01011010
        db %00100100
        db %11111111
my_sprite_g:
        db 128
        db 164
        db -2
        db -1
        db %00011000
        db %00111100
        db %01111110
        db %11111111
        db %11111111
        db %01111110
        db %00111100
        db %00011000


test_image:
        db %00011000
        db %00100100
        db %01011010
        db %10100101
        db %10100101
        db %01011010
        db %00100100
        db %00011000

test_image2:
        db %11111111
        db %10000000
        db %11111111
        db %00000001
        db %11111111
        db %10000000
        db %11111111
        db %00000001

;; ------------------------------------------------------

print_a:
        push af
        add a,30h
        call 16
        pop af
        ret


draw_image:  ; c=x, b=y, hl=image data
        push hl
        push bc
        push de
        ld a, b                 ; calc end y
        add a, 8
        ld e, a                 ; put in e
draw_image_loop:
        ld d, (hl)
        call draw_byte
        inc hl
        inc b
        ld a, b
        cp e                    ; check end pos
        jp nz, draw_image_loop
        pop de
        pop bc
        pop hl
        ret


draw_rect:                      ; c=x, b=y, e=x2, d=y2
        push bc
        push de
        call draw_vline
        ld d,e
        call draw_hline
        pop de
        ld c,e
        call draw_vline
        pop bc
        push bc
        push de
        ld b,d
        ld d,e
        call draw_hline
        pop de
        pop bc
        ret

draw_vline:                     ; c=y,b=x,d=end y
        push bc
vline_loop:
        call draw_pix
        inc b
        ld a,b
        cp d
        jp nz,vline_loop
        pop bc
        ret

draw_hline:                     ; c=y,b=x,d=end x
        push bc
hline_loop:
        call draw_pix
        inc c
        ld a,c
        cp d
        jp nz,hline_loop
        pop bc
        ret

draw_byte:                      ; with b,c = y,x, d=bitmap byte
        ld a, c
        and %00000111           ; get x%8
        jp z, draw_byte_aligned ; no need to slide around!
        push de
        ld e, a                 ; store offset in e
        push hl
        call pix_addr           ; get pixel address
        push bc
        ld b,e                  ; shift byte based on x%8
        push de
draw_byte_rotate_loop_right:
        srl d
        djnz draw_byte_rotate_loop_right
        ld a, (hl)
        xor d                    ; or over what's there now
        ld (hl), a              ; draw left half
        pop de
        ld a, 8                  ; now start right half
        sub e                    ; take shift away from 8
        ld b, a                  ; stick it in the loop reg
        inc hl                   ; draw to next byte to the right
        ld a, d                  ; quicker to put the thing in a for and below
draw_byte_rotate_loop_left:
        sll a                   ; rotate left
        and %11111110           ; need to manually clear bit 0 :(
        djnz draw_byte_rotate_loop_left
        ld e, a                 ; juggle registers
        ld a, (hl)              ; ok to trash them now
        xor e                    ; or over what's there
        ld (hl), a              ; draw the right half
        pop bc
        pop hl
        pop de
        ret

draw_byte_aligned:              ; with b,c = y,x, d=bitmap byte
        push hl
        call pix_addr           ; get pixel address
        ld a, d                 ; load bitmap
        xor (hl)                 ; or with bg
        ld (hl), a              ; write pattern to pixel
        pop hl
        ret

draw_pix:                       ; with b,c = y,x
        push bc
        push hl
        call pix_addr           ; get pixel address
        push hl                 ; save hl, which contains dst address
        ld a, c                 ; load x into a
        ld b, 0                 ; clear b
        and %00000111           ; mask x to get bit position
        ld c, a                 ; stick masked in c
        ld hl, table            ; load table position
        add hl, bc              ; add offset
        ld a, (hl)              ; load bit pattern
        pop hl                  ; get old pixel address back
        or (hl)                 ; don't clear existing pixels here
        push hl                 ; apply the mask pattern
        ld hl, pix_mask         ; load locaton
        and (hl)                ; apply it
        pop hl
        ld (hl), a              ; write pattern to pixel
        pop hl
        pop bc
        ret
table:
        db %10000000
        db %01000000
        db %00100000
        db %00010000
        db %00001000
        db %00000100
        db %00000010
        db %00000001

pix_mask:
        db %11111111

;; ------------------------------------------------------

clear_screen:                   ; write 0 to the whole screen
        ld a, 0
fillpattern:                    ; fill screen with bitpattern in a
        ld hl, 4000h

        push bc                 ; screen third loop
        ld b, 3
oodloop:
        push bc                 ;
        ld b, 8
odloop:
        push bc
        ld b, 256
dloop:
        ld (hl), a
        inc hl
        djnz dloop
        pop bc
        djnz odloop
        pop bc
        djnz oodloop
        pop bc
        ret

;; ------------------------------------------------------
; get screen address
; b = y pixel position
; c = x pixel position
; returns address in hl
pix_addr:
        push bc
        ld a,b          ; Calculate Y2,Y1,Y0
	    and %00000111   ; Mask out unwanted bits
	    or %01000000    ; Set base address of screen
	    ld h,a          ; Store in H
	    ld a,b          ; Calculate Y7,Y6
	    rra             ; Shift to position
        rra
	    rra
	    and %00011000   ; Mask out unwanted bits
        or h            ; OR with Y2,Y1,Y0
	    ld h,a          ; Store in H
	    ld a,b          ; Calculate Y5,Y4,Y3
	    rla             ; Shift to position
	    rla
	    and %11100000   ; Mask out unwanted bits
	    ld l,a          ; Store in L
	    ld a,c          ; Calculate X4,X3,X2,X1,X0
	    rra             ; Shift into position
	    rra
	    rra
	    and %00011111   ; Mask out unwanted bits
	    or l            ; OR with Y5,Y4,Y3
	    ld l,a          ; Store in L
        pop bc
	    ret


	; Title:    ZX Spectrum Keyboard Routines
	; Author:   Dean Belfield
	; Created:  29/07/2011
	; Last Updated: 29/07/2011
	;
	; Requires:
	;
	; Modinfo:
	;

	; Read the keyboard and return an ASCII character code
	;
 	read_keyboard:
        push hl
        push bc
        push de
        LD HL,keyboard_map      ; Point HL at the keyboard list
 	    LD D,8              ; This is the number of ports (rows) to check
	    LD C,00FEh            ; B is always FEh for reading keyboard ports
	read_keyboard_0:
        LD B,(HL)           ; Get the keyboard port address
	    INC HL              ; Increment to keyboard list of table
	    IN A,(C)            ; Read the row of keys in
	    AND 001Fh             ; We are only interested in the first five bits
	    LD E,5              ; This is the number of keys in the row
	read_keyboard_1:
        SRL A               ; Shift A right; bit 0 sets carry bit
	    JR NC,read_keyboard_2       ; If the bit is 0, we've found our key
	    INC HL              ; Go to next table address
	    DEC E               ; Decrement key loop counter
	    JR NZ,read_keyboard_1       ; Loop around until this row finished
	    DEC D               ; Decrement row loop counter
	    JR NZ,read_keyboard_0       ; Loop around until we are done
	    AND A               ; Clear A (no key found)
        pop de
        pop bc
        pop hl
	    RET
	read_keyboard_2:
        LD A,(HL)           ; We've found a key at this point; fetch the character code!
        pop de
        pop bc
        pop hl
        RET

	keyboard_map:
        DB 00FEh
        db "#"
        db "Z"
        db "X"
        db "C"
        db "V"
	    DB 00FDh
        db "A"
        db "S"
        db "D"
        db "F"
        db "G"
	    DB 00FBh
        db "Q"
        db "W"
        db "E"
        db "R"
        db "T"
	    DB 00F7h
        db "1"
        db "2"
        db "3"
        db "4"
        db "5"
	    DB 00EFh
        db "0"
        db "9"
        db "8"
        db "7"
        db "6"
	    DB 00DFh
        db "P"
        db "O"
        db "I"
        db "U"
        db "Y"
	    DB 00BFh
        db "#"
        db "L"
        db "K"
        db "J"
        db "H"
	    DB 007Fh
        db " "
        db "#"
        db "M"
        db "N"
        db "B"
