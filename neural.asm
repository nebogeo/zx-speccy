		org 8000h

		;; neuron looks like:
		;; 0. activation level
		;; 1. thresh (signed 8bit)
		;; 2. conn1-addr-h
		;; 3. conn1-addr-l
		;; 4. conn2-addr-h
		;; 5. conn2-addr-l
		;; 6. conn3-addr-h
		;; 7. conn3-addr-l
		;; 8. conn4-addr-h
		;; 9. conn4-addr-l
        ;; 10 spacer


		brain_base equ 4000h
        brain_size equ 16
        neuron_size equ 11
        brain_mask equ %00001111
main:
        call clear_screen
        call randomise_brain

loop:
        call brain_think

        ld a, (brain_base)
        ld (noise), a

        jp loop
        ret


		;; update:
		;; - add up connections->activation
		;; - mulitply by thresh
		;; - set activation
brain_think:
        ld b, brain_size
		ld ix, brain_base
loop_brain_think:
        call play
        push bc
        call neuron_think
        ld bc, neuron_size
        add ix, bc
        pop bc
        djnz loop_brain_think
        ret

noise:
        db 0

play:
        ld a, (noise)
        rlca
        ld (noise), a
        and %0010010
        out (00feh), a
        ret

neuron_think:
        ld a, (ix+1)
        and %00000111
        ld b, a

        sub a
        ld l, (ix+2)
        ld h, (ix+3)
        add a, (hl)
        ld h, (ix+4)
        ld l, (ix+5)
        add a, (hl)
        ld h, (ix+6)
        ld l, (ix+7)
        add a, (hl)
        ld h, (ix+8)
        ld l, (ix+9)
        add a, (hl)

think_rot_loop:
        rrca
        djnz think_rot_loop

        ld (ix+0),a

        ret





randomise_brain:
        ld b, brain_size
		ld ix, brain_base
loop_randomise_brain:
        push bc
        call randomise_neuron
        ld bc, neuron_size
        add ix, bc
        pop bc
        djnz loop_randomise_brain
        ret

        ;; random neuron pointed at by ix
        ;; clobbers bc (index_to_addr), hl
randomise_neuron:
        call rnd
        ld (ix+0), a            ; activation level
        call rnd
        ld (ix+1), a            ; thresh

        call rnd
        and brain_mask
        call index_to_addr
        ld (ix+2), l            ; connection 1
        ld (ix+3), h            ; connection 1

        call rnd
        and brain_mask
        call index_to_addr
        ld (ix+4), l            ; connection 2
        ld (ix+5), h            ; connection 2

        call rnd
        and brain_mask
        call index_to_addr
        ld (ix+6), l            ; connection 3
        ld (ix+7), h            ; connection 3

        call rnd
        and brain_mask
        call index_to_addr
        ld (ix+8), l            ; connection 4
        ld (ix+9), h            ; connection 4

        ld (ix+10), %11111111
        ret

        ;; index in a, return address in hl
        ;; clobbers bc
index_to_addr:
        ld h, 0                 ; clear h (top byte of hl)
        ld l, a                 ; load offset into lower byte of hl
        ld bc, neuron_size
        push bc
        call mul16
        ld bc, brain_base
        add hl, bc              ; add to neural net base
        ret

;;;;;;;;;;;;;;; mathematics ;;;;;;;;;;;;;;;;;;;;;;;;;;;

rnd:
        ld a, (rnd_state)
        ld b, a
        rrca ; multiply by 32
        rrca
        rrca
        xor 1fh
        add a, b
        sbc a, 255 ; carry
        ld (rnd_state), a
        ret
rnd_state:
        db 4h

mul8:
__MUL8:
__MUL8_FAST: ; __FASTCALL__ entry, a = a * h (8 bit mul) and Carry
		ld b, 8
		ld l, a
		xor a
__MUL8LOOP:
		add a, a ; a *= 2
		sla l
		jp nc, __MUL8B
		add a, h
__MUL8B:
		djnz __MUL8LOOP
		ret		; result = HL


mul16:
__MUL16:	; Mutiplies HL with the last value stored into de stack
			; Works for both signed and unsigned
		ex de, hl
		pop hl		; Return address
		ex (sp), hl ; CALLEE caller convention
__MUL16_FAST:
        ld b, 16
        ld a, d
        ld c, e
        ex de, hl
        ld hl, 0
__MUL16LOOP:
        add hl, hl  ; hl << 1
        sla c
        rla         ; a,c << 1
        jp nc, __MUL16NOADD
        add hl, de
__MUL16NOADD:
        djnz __MUL16LOOP
		ret	; Result in hl (16 lower bits)

;;; spritee ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; graphcs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

patt_0:
        db %00111100
        db %01000010
        db %10000001
        db %10000001
        db %10000001
        db %10000001
        db %01000010
        db %00111100
patt_1:
        db %00111100
        db %01000010
        db %10000001
        db %10011001
        db %10011001
        db %10000001
        db %01000010
        db %00111100
patt_2:
        db %00111100
        db %01000010
        db %10011001
        db %10111101
        db %10111101
        db %10011001
        db %01000010
        db %00111100
patt_3:
        db %00111100
        db %01111110
        db %11111111
        db %11111111
        db %11111111
        db %11111111
        db %01111110
        db %00111100

print_a:
        push af
        add a,30h
        call 16
        pop af
        ret

shwnum:
        push de
        push hl
       ld a,48      ; leading zeroes (or spaces).
       ld de,10000         ; ten thousands column.
       call shwdg          ; show digit.
       ld de,1000          ; thousands column.
       call shwdg          ; show digit.
       ld de,100           ; hundreds column.
       call shwdg          ; show digit.
       ld de,10            ; tens column.
       call shwdg          ; show digit.
       or 16               ; last digit is always shown.
       ld de,1             ; units column.
       call shwdg          ; show digit.
       ld a, 40
       rst 16
       pop hl
       pop de
        ret
shwdg: and 48              ; clear carry, clear digit.
shwdg1: sbc hl,de           ; subtract from column.
       jr c,shwdg0         ; nothing to show.
       or 16               ; something to show, make it a digit.
       inc a               ; increment digit.
       jr shwdg1           ; repeat until column is zero.
shwdg0: add hl,de           ; restore total.
       push af
       rst 16              ; show character.
       pop af
       ret
