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
        ;; 10 x
        ;; 11 y

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		brain_base equ 7000h
        brain_size equ 64
        neuron_size equ 12
        brain_mask equ %00111111
main:
        call clear_screen
        call randomise_brain
        call draw_interface
loop:

        call brain_think
        ld a, (brain_base)
        ld (noise), a

        ld a, r
        rr a
        rr a
        rr a
        and 1
        cp 1
        jp z, input_stuff
        jp loop
input_stuff:
        call draw_cursor
        call do_input
        call draw_cursor
        ;;         call draw_neurons

        jp loop
        ret

;;; input and interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cursor_x:
        db 0
cursor_y:
        db 0

draw_cursor:
        ld a, (cursor_y)
        ld b, a
        ld a, (cursor_x)
        ld c, a
        call ataddr
        xor %01001000
        ld (hl),a
        ret

;; find neuron under the cursor if there is one, addr in hl, 0 if not found
select:
        ld b, brain_size
		ld ix, brain_base
        ld hl, brain_base
loop_select:
        push bc
        ld b, (ix+10)           ; load x coord of neuron
        ld a, (cursor_x)        ; cant load (addr) into anywhere but a
        cp b
        jp z, find_x_same
        jp find_x_end
find_x_same:
        ld b, (ix+11)           ; load y coord of neuron
        ld a, (cursor_y)
        cp b                    ; check
        jp z, find_found
        jp find_x_end
find_found:
        pop bc
        ;;  result in ix
        ret
find_x_end:
        ld bc, neuron_size
        add ix, bc              ; add size to addr
        add hl, bc              ; replicate on hl
        pop bc
        djnz loop_select
        ld hl, 0
        ret

do_input:
        call read_keyboard
        cp 'W'
        jp z, input_up
        cp 'S'
        jp z, input_down
        cp 'A'
        jp z, input_left
        cp 'D'
        jp z, input_right
        cp ' '
        jp z, input_select
        cp 'T'
        jp z, input_tl
        cp 'Y'
        jp z, input_tr
        cp 'G'
        jp z, input_bl
        cp 'H'
        jp z, input_br
        ret
input_up:
        ld a, (cursor_y)
        add a, -1
        ld (cursor_y), a
        ret
input_down:
        ld a, (cursor_y)
        add a, 1
        ld (cursor_y), a
        ret
input_left:
        ld a, (cursor_x)
        add a, -1
        ld (cursor_x), a
        ret
input_right:
        ld a, (cursor_x)
        add a, 1
        ld (cursor_x), a
        ret
input_select:
        call select
        ld hl, pattern_active
        ld b, h
        ld c, l
        call set_pattern
        ret
input_tl:
        call select
        ld hl, pattern_tl
        ld b, h
        ld c, l
        call set_pattern
        ret
input_bl:
        call select
        ld hl, pattern_bl
        ld b, h
        ld c, l
        call set_pattern
        ret
input_br:
        call select
        ld hl, pattern_br
        ld b, h
        ld c, l
        call set_pattern
        ret
input_tr:
        call select
        ld hl, pattern_tr
        ld b, h
        ld c, l
        call set_pattern
        ret

;; hl=ix = neuron, bc is pattern
set_pattern:
        ld a, l
        cp 0
        jp z, input_select_2
        jp found_neuron
input_select_2:
        ld a, h
        cp 0
        jp z, input_select_end
found_neuron:
        ;; found a neuron! hl and ix are set up
        ld h, b
        ld l, c
        ld c, (ix+10)
        ld b, (ix+11)
        call draw_char_xor
input_select_end:
        ret



draw_interface:
        ld b, brain_size
		ld ix, brain_base
loop_draw_interface:
        push bc
        ld c, (ix+10)
        ld b, (ix+11)
        ld hl, pattern_base
        call draw_char
        ld c, (ix+10)
        ld b, (ix+11)
        call ataddr
        ld (hl), %00101010
        ld bc, neuron_size
        add ix, bc
        pop bc

        djnz loop_draw_interface
        ret

draw_neurons:
        ld b, brain_size
		ld hl, brain_base
        ld c, 0
loop_draw_neurons:
        ;;         call play
        push bc
        ld c,b
        rl c
        rl c
        rl c
        rl c
        ld b,10
        ld d, (hl)
        call draw_byte

        ld bc, neuron_size
        add hl, bc
        pop bc
        djnz loop_draw_neurons
        ret

;;; thinking part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        push bc
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
        pop bc

        ;; screen location
        ;; b should contain loop count?
        ld a, b
        dec a
        and %00000111           ; mod 8
        sla a                    ;* 2 (leave gaps)
        add a, 8
        ld (ix+10), a           ; set x

        ld a, b
        and %00000001
        ld c, a

        ld a, b                 ; reload b
        dec a
        sra a
        sra a                   ; quotient 4
        add a, c
        add a, 4
        ld (ix+11), a
        ld h, 0
        ld l, (ix+11)
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
        db 2h

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

pattern_base:
        db %00011000
        db %00100100
        db %01000010
        db %10000001
        db %10000001
        db %01000010
        db %00100100
        db %00011000
pattern_active:
        db %00000000
        db %00000000
        db %00011000
        db %00111100
        db %00111100
        db %00011000
        db %00000000
        db %00000000
pattern_tl:
        db %11100000
        db %11000000
        db %10000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
pattern_tr:
        db %00000111
        db %00000011
        db %00000001
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
pattern_bl:
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %10000000
        db %11000000
        db %11100000
pattern_br:
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000001
        db %00000011
        db %00000111

        ;; display hl at bc
draw_char:
        call chaddr         ; find screen address for char.
        ld b,8              ; number of pixels high.
char0:
        ld a,(hl)           ; source graphic.
        ld (de),a           ; transfer to screen.
        inc hl              ; next piece of data.
        inc d               ; next pixel line.
        djnz char0          ; repeat
        ret

        ;; display hl at bc
draw_char_xor:
        call chaddr         ; find screen address for char.
        ld b,8              ; number of pixels high.
char0_xor:
        ld a, (de)
        xor (hl)
        ld (de),a           ; transfer to screen.
        inc hl              ; next piece of data.
        inc d               ; next pixel line.
        djnz char0_xor      ; repeat
        ret

draw_byte:              ; with bc = xy, d=bitmap byte
        call chaddr         ; find screen address for char.
        ld a, d                 ; load bitmap
        xor (hl)                 ; or with bg
        ld (hl), a              ; write pattern to pixel
        ret


;; bc -> de
chaddr:
        ld a,b              ; vertical position.
        and 24              ; which segment, 0, 1 or 2?
        add a,64            ; 64*256 = 16384, Spectrum's screen memory.
        ld d,a              ; this is our high byte.
        ld a,b              ; what was that vertical position again?
        and 7               ; which row within segment?
        rrca                ; multiply row by 32.
        rrca
        rrca
        ld e,a              ; low byte.
        ld a,c              ; add on y coordinate.
        add a,e             ; mix with low byte.
        ld e,a              ; address of screen position in de.
        ret

;; bc -> a/hl
ataddr:
        ld a,b              ; x position.
        rrca                ; multiply by 32.
        rrca
        rrca
        ld l,a              ; store away in l.
        and 3               ; mask bits for high byte.
        add a,88            ; 88*256=22528, start of attributes.
        ld h,a              ; high byte done.
        ld a,l              ; get x*32 again.
        and 224             ; mask low byte.
        ld l,a              ; put in l.
        ld a,c              ; get y displacement.
        add a,l             ; add to low byte.
        ld l,a              ; hl=address of attributes.
        ld a,(hl)           ; return attribute in a.
        ret










print_a:
        push af
        add a,30h
        call 16
        pop af
        ret

print_hl:
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
