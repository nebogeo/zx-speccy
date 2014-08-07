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
        ;; 12 control byte

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		brain_base equ 28600
        brain_size equ 32
        neuron_size equ 13
        brain_mask equ %00011111
        input_neuron1 equ brain_base+(neuron_size*7)
        input_neuron2 equ brain_base+(neuron_size*31)
        weave_end equ 800h
main:
        call unit_test
        ld a, %00000111
        call set_all_attr

        call clear_screen
        call init_brain

        ld ix, input_neuron1
        ld (ix+12), %00000000
        ld ix, input_neuron2
        ld (ix+12), %00000000

        ;;         call draw_neuron_attrs
        call draw_interface


        ld hl, input_neuron1


loop:
        ld a, (clock1)
        inc a
        ld (clock1), a
        jp z, clk2_tick
        jp clk_end

clk2_tick:
        ld a, (clock2)
        inc a
        ld (clock2), a

clk_end:
        ld a, (clock2)
        ld (input_neuron1), a
        ld a, (clock1)
        ld (input_neuron2), a

        call draw_neurons

        call brain_think
        ld a, (brain_base)
        ld (noise), a           ; update noise byte


        ld a, (weave_pos)
        ld l, a
        ld a, (weave_pos+1)
        ld h, a

        inc hl

        ld a, l
        ld (weave_pos), a
        ld a, h
        and %0100111
        ld (weave_pos+1), a


        ld a, (brain_base)
        ld bc, 4000h
        add hl, bc
        ld (hl), a


        ld a, r
        rr a
        rr a
        and %00011000


        jp z, input_stuff
        jp loop
input_stuff:
        call draw_cursor
        call do_input
        call draw_cursor

        jp loop
        ret

;;; input and interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

clock1:
        db 0
clock2:
        db 0
clock3:
        db 0
cursor_x:
        db 5
cursor_y:
        db -1
weave_pos:
        dw 0
debounce:
        db 0
test:
        db 0

draw_cursor:
        call select_cursor
        ld b, 0
        ld c, 10
        add hl, bc
        ld c, (hl)
        inc hl
        ld b, (hl)
        ld d, %01000001
        call attr_xor_2X2
        ret


        ;; cursor -> addr in hl/ix
select_cursor:
        ld a, (cursor_y)
        sla a
        sla a
        sla a                   ; quotient 8
        ld b, a
        ld a, (cursor_x)
        add a, b
        call index_to_addr_ix              ; a -> ix & hl
        ret

        ;; hx -> addr in hl/ix
neuron_xy_to_addr:
        ld a, l
        sla a
        sla a
        sla a                   ; quotient 8
        ld b, a
        ld a, h
        add a, b
        call index_to_addr_ix              ; a -> ix & hl
        ret


do_input:
        call read_keyboard

        cp 0
        jp z, clear_debounce    ; no key pressed



        ld a, (debounce)        ; return if key is still pressed
        cp 0
        ret nz
        ld a, 1                 ; key is pressed for the first time, set debounce
        ld (debounce), a
        call read_keyboard

        cp 'E'
        jp z, input_up
        cp 'S'
        jp z, input_down
        cp 'W'
        jp z, input_left
        cp 'D'
        jp z, input_right
        cp ' '
        jp z, input_thresh
        cp 'T'
        jp z, input_tl
        cp 'Y'
        jp z, input_tr
        cp 'G'
        jp z, input_bl
        cp 'H'
        jp z, input_br
        ret
clear_debounce:
        ld a, 0                 ; no keys pressed, so clear debounce
        ld (debounce), a
        ret

input_up:
        ld a, (cursor_x)
        ld b, a
        ld a, (cursor_y)
        ld c, a
        call iso_move_tr
        ld a, b
        ld (cursor_x), a
        ld a, c
        ld (cursor_y), a
        ret
input_down:
        ld a, (cursor_x)
        ld b, a
        ld a, (cursor_y)
        ld c, a
        call iso_move_bl
        ld a, b
        ld (cursor_x), a
        ld a, c
        ld (cursor_y), a
        ret
input_left:
        ld a, (cursor_x)
        ld b, a
        ld a, (cursor_y)
        ld c, a
        call iso_move_tl
        ld a, b
        ld (cursor_x), a
        ld a, c
        ld (cursor_y), a
        ret
input_right:
        ld a, (cursor_x)
        ld b, a
        ld a, (cursor_y)
        ld c, a
        call iso_move_br
        ld a, b
        ld (cursor_x), a
        ld a, c
        ld (cursor_y), a
        ret
input_thresh:
        call select_cursor
        ld a, (ix+1)           ; load thresh
        inc a
        ld (ix+1), a

        ld c, (ix+10)
        ld b, (ix+11)
        ld hl, pattern_thresh_clear
        call draw_char_and_2X2

        ld a, (ix+1)           ; load thresh
        and %00000111
        sla a
        sla a
        sla a
        sla a
        sla a
        ld d, 0
        ld e, a
        ld hl, pattern_thresh0
        add hl, de
        call draw_char_or_2X2

        ret


input_tl:
        call select_cursor
        ld a, (ix+12)           ; load control byte
        xor %00000001
        ld (ix+12), a
        ld c, (ix+10)
        ld b, (ix+11)
        ld hl, pattern_one
        call draw_char_xor_2X2
        ret
input_bl:
        call select_cursor
        ld a, (ix+12)           ; load control byte
        xor %00000010
        ld (ix+12), a
        ld c, (ix+10)
        ld b, (ix+11)
        ld hl, pattern_three
        call draw_char_xor_2X2
        ret
input_br:
        call select_cursor
        ld a, (ix+12)           ; load control byte
        xor %00000100
        ld (ix+12), a
        ld c, (ix+10)
        ld b, (ix+11)
        ld hl, pattern_four
        call draw_char_xor_2X2
        ret
input_tr:
        call select_cursor
        ld a, (ix+12)           ; load control byte
        xor %00001000
        ld (ix+12), a
        ld c, (ix+10)
        ld b, (ix+11)
        ld hl, pattern_two
        call draw_char_xor_2X2
        ret

;; hl=ix = neuron, bc is pattern
set_pattern:
        ld h, b
        ld l, c
        ld c, (ix+10)
        ld b, (ix+11)
        call draw_char_2X2
        ret



draw_interface:

        ld c, 1
        ld b, 21
        ld hl, slub
        ;;         call draw_char_2X2
        ld c, 1
        ld b, 21
        ld d, %01111000
        ;;         call attr_2X2


        ld b, brain_size
		ld ix, brain_base
loop_draw_interface:
        push bc
        ld c, (ix+10)
        ld b, (ix+11)
        ld hl, pattern_base
        call draw_char_2X2

        call ataddr
        ;;    ld (hl), %00101010
        ld bc, neuron_size
        add ix, bc
        pop bc

        djnz loop_draw_interface

;;; highlight noise neuron

        ld ix, brain_base
        ld c, (ix+10)
        ld b, (ix+11)
        ld d, %00000011
        call attr_2X2

        ret

draw_neuron_attrs:
        ld b, brain_size
		ld ix, brain_base
loop_draw_neuron_attrs:
        push bc
        ld c, (ix+10)
        inc c
        inc c
        ld b, (ix+11)
        ld h, (ix+0)
        call ataddr
        ld (hl), %00000100
        ld bc, neuron_size
        add ix, bc
        pop bc
        djnz loop_draw_neuron_attrs
        ret


draw_neurons:
        ld b, brain_size
		ld ix, brain_base
loop_draw_neurons:
        push bc
        ld c, (ix+10)
        inc c
        inc c
        ld b, (ix+11)
        inc b
        ld h, (ix+0)
        call draw_byte
        ld bc, neuron_size
        add ix, bc
        pop bc
        djnz loop_draw_neurons
        ret

;;; iso - bc=xy

iso_move_tr:
        ld a, b
        add a, -1
        and %00000111
        ld b, a
        and %00000001
        ret z
        ld a, c
        add a, 1
        and %00000011
        ld c, a
        ret
iso_move_bl:
        ld a, b
        add a, 1
        and %00000111
        ld b, a
        and %00000001
        ret nz
        ld a, c
        add a, -1
        and %00000011
        ld c, a
        ret

iso_move_tl:
        ld a, b
        add a, 1
        and %00000111
        ld b, a
        and %00000001
        ret z
        ld a, c
        add a, 1
        and %00000011
        ld c, a
        ret

iso_move_br:
        ld a, b
        add a, -1
        and %00000111
        ld b, a
        and %00000001
        ret nz
        ld a, c
        add a, -1
        and %00000011
        ld c, a
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
        ld a, (ix+12)           ; load control byte
        and %00010000           ; dummy
        cp 0
        jp z, neuron_think_end

        ld e, 0

        ld a, (ix+12)           ; load control byte
        and %00000001           ; first neuron
        cp 0
        jp z, neuron2
        ld l, (ix+2)
        ld h, (ix+3)
        ld a, e
        add a, (hl)
        jp c, neuron_think_clear
        ld e, a

neuron2:
        ld a, (ix+12)           ; load control byte
        and %00000010           ; second neuron
        cp 0
        jp z, neuron3
        ld l, (ix+4)
        ld h, (ix+5)
        ld a, e
        add a, (hl)
        jp c, neuron_think_clear
        ld e, a

neuron3:
        ld a, (ix+12)           ; load control byte
        and %00000100           ; third neuron
        cp 0
        jp z, neuron4
        ld l, (ix+6)
        ld h, (ix+7)
        ld a, e
        add a, (hl)
        jp c, neuron_think_clear
        ld e, a

neuron4:
        ld a, (ix+12)           ; load control byte
        and %00001000           ; third neuron
        cp 0
        jp z, think_end_add
        ld l, (ix+8)
        ld h, (ix+9)
        ld a, e
        add a, (hl)
        jp c, neuron_think_clear
        ld e, a

think_end_add:
        ld a, e                 ; load accumulation into a
        ld (ix+0),a             ; update value

        ld a, (ix+1)
        and %00000111
        cp 0
        jp z, neuron_think_end    ; skip rotate if 0
        ld b, a                 ; load thresh/rotate into b
think_rot_loop:
        srl (ix+0)
        djnz think_rot_loop

neuron_think_end:
        ret
neuron_think_clear:
        ld a, 0
        ld (ix+0), a
        ret


;;; brain setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

init_brain:
        ld b, brain_size
		ld ix, brain_base
loop_init_brain:
        push bc
        call init_neuron
        ld bc, neuron_size
        add ix, bc
        pop bc
        djnz loop_init_brain
        ret

        ;; init neuron pointed at by ix
        ;; clobbers bc (index_to_addr), hl
init_neuron:
        push bc
        ld (ix+0), 0            ; activation level
        ld (ix+1), 0            ; thresh

        ld a, brain_size
        sub b
        ld b, a

        and 1
        jp nz, init_alternate

        ;; tl
        ld a, b
        add a, 9
        call index_to_addr
        ld (ix+2), l            ; connection 1
        ld (ix+3), h            ; connection 1

        ;; bl
        ld a, b
        inc a
        call index_to_addr
        ld (ix+4), l            ; connection 2
        ld (ix+5), h            ; connection 2

        ;; br
        ld a, b
        dec a
        call index_to_addr
        ld (ix+6), l            ; connection 3
        ld (ix+7), h            ; connection 3

        ;; tr
        ld a, b
        add a, 7
        call index_to_addr
        ld (ix+8), l            ; connection 4
        ld (ix+9), h            ; connection 4


        jp init_end
init_alternate:

        ;; tl
        ld a, b
        inc a
        call index_to_addr
        ld (ix+2), l            ; connection 1
        ld (ix+3), h            ; connection 1

        ;; bl
        ld a, b
        add a, -7
        call index_to_addr
        ld (ix+4), l            ; connection 2
        ld (ix+5), h            ; connection 2

        ;; br
        ld a, b
        add a, -9
        call index_to_addr
        ld (ix+6), l            ; connection 3
        ld (ix+7), h            ; connection 3

        ;; tr
        ld a, b
        dec a
        call index_to_addr
        ld (ix+8), l            ; connection 4
        ld (ix+9), h            ; connection 4

init_end:

        pop bc

        ld a, b
        and %00000001
        add a, a
        ld c, a

        ;; screen location
        ;; b should contain loop count
        ld a, b
        dec a
        and %00000111           ; mod 8

        add a, a

        add a, 8
        ld (ix+10), a           ; set x


        ld a, b                 ; reload b
        dec a
        sra a
        sra a
        sra a                   ; quotient 4

        add a, a
        add a, a

        add a, 8
        add a, c
        ld (ix+11), a

        ;;  control byte
        ld (ix+12), %00010000

        ret


        ;; index in a, return address in hl
index_to_addr:
        push de
        push bc
        ld hl, brain_base       ; clear h (top byte of hl)
        and a
        jp z, index_end
        ld b, a
index_addr_loop:
        ld de, neuron_size
        add hl, de              ; add to neural net base
        djnz index_addr_loop
index_end:
        pop bc
        pop de
        ret

        ;; index in a, return address in ix and hl
index_to_addr_ix:
        push de
        push bc
        ld ix, brain_base       ; clear h (top byte of hl)
        ld hl, brain_base       ; clear h (top byte of hl)
        and a
        jp z, index_ix_end
        ld b, a
index_addr_ix_loop:
        ld de, neuron_size
        add ix, de              ; add to neural net base
        add hl, de              ; add to neural net base
        djnz index_addr_ix_loop
index_ix_end:
        pop bc
        pop de
        ret


;;; graphics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

set_all_attr:                   ; write a to all the attrs
        ld hl, 5800h
        ld b, 3
oaloop:
        push bc
        ld b, 256
aloop:
        ld (hl), a
        inc hl
        djnz aloop
        pop bc
        djnz oaloop
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
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000001
        db %00000010
        db %00000100
        db %00001000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %10000000
        db %01000000
        db %00100000
        db %00010000

        db %00001000
        db %00000100
        db %00000010
        db %00000001
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00010000
        db %00100000
        db %01000000
        db %10000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
pattern_one:
        db %10001000
        db %01001000
        db %00101000
        db %00011000
        db %11111000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

pattern_two:

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00010001
        db %00010010
        db %00010100
        db %00011000
        db %00011111
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

pattern_three:
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %11111000
        db %00011000
        db %00101000
        db %01001000
        db %10001000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
pattern_four:
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00011111
        db %00011000
        db %00010100
        db %00010010
        db %00010001
pattern_thresh_clear:
        db %11111111
        db %11111111
        db %11111111
        db %11111111
        db %11111111
        db %11111110
        db %11111100
        db %11111000

        db %11111111
        db %11111111
        db %11111111
        db %11111111
        db %11111111
        db %01111111
        db %00111111
        db %00011111

        db %11111000
        db %11111100
        db %11111110
        db %11111111
        db %11111111
        db %11111111
        db %11111111
        db %11111111

        db %00011111
        db %00111111
        db %01111111
        db %11111111
        db %11111111
        db %11111111
        db %11111111
        db %11111111

pattern_thresh0:
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
pattern_thresh1:
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %10000000
        db %11000000
        db %10000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
pattern_thresh2:
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %10000000
        db %11000000
        db %11100000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
pattern_thresh3:
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %10000000
        db %11000000
        db %11100000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %11100000
        db %01000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
pattern_thresh4:
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %10000000
        db %11000000
        db %11100000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %11100000
        db %11000000
        db %10000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
pattern_thresh5:
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %10000000
        db %11000000
        db %11100000

        db %00000001
        db %00000011
        db %00000001
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %11100000
        db %11000000
        db %10000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
pattern_thresh6:
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %10000000
        db %11000000
        db %11100000

        db %00000111
        db %00000011
        db %00000001
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %11100000
        db %11000000
        db %10000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
pattern_thresh7:
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000001
        db %00000011
        db %00000111

        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %10000000
        db %11000000
        db %11100000

        db %00000111
        db %00000011
        db %00000001
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000

        db %11100000
        db %11000000
        db %10000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
        db %00000000
slub:
        db %00000011
        db %00000110
        db %00001100
        db %00011000
        db %00110000
        db %01100000
        db %11001100
        db %10010100

        db %11000000
        db %01100000
        db %00110000
        db %00011000
        db %00001100
        db %00000110
        db %01101011
        db %01010001

        db %10010101
        db %11010010
        db %01100000
        db %00110000
        db %00011000
        db %00001100
        db %00000110
        db %00000011

        db %01010001
        db %10101011
        db %00000110
        db %00001100
        db %00011000
        db %00110000
        db %01100000
        db %11000000

        ;; cb = pos d, attr
attr_2X2:
        call ataddr
        ld (hl), d
        inc c
        call ataddr
        ld (hl), d
        dec c
        inc b
        call ataddr
        ld (hl), d
        inc c
        call ataddr
        ld (hl), d
        ret

        ;; cb = pos d, attr
attr_xor_2X2:
        call ataddr
        ld a, (hl)
        xor d
        ld (hl), a
        inc c
        call ataddr
        ld a, (hl)
        xor d
        ld (hl), a
        dec c
        inc b
        call ataddr
        ld a, (hl)
        xor d
        ld (hl), a
        inc c
        call ataddr
        ld a, (hl)
        xor d
        ld (hl), a
        ret


draw_char_2X2:
        push bc
        call draw_char
        pop bc
        push bc
        inc c
        call draw_char
        pop bc
        push bc
        inc b
        call draw_char
        pop bc
        push bc
        inc b
        inc c
        call draw_char
        pop bc
        ret

draw_char_or_2X2:
        push bc
        call draw_char_or
        pop bc
        push bc
        inc c
        call draw_char_or
        pop bc
        push bc
        inc b
        call draw_char_or
        pop bc
        push bc
        inc b
        inc c
        call draw_char_or
        pop bc
        ret

draw_char_xor_2X2:
        push bc
        call draw_char_xor
        pop bc
        push bc
        inc c
        call draw_char_xor
        pop bc
        push bc
        inc b
        call draw_char_xor
        pop bc
        push bc
        inc b
        inc c
        call draw_char_xor
        pop bc
        ret

draw_char_and_2X2:
        push bc
        call draw_char_and
        pop bc
        push bc
        inc c
        call draw_char_and
        pop bc
        push bc
        inc b
        call draw_char_and
        pop bc
        push bc
        inc b
        inc c
        call draw_char_and
        pop bc
        ret


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

                ;; display hl at bc
draw_char_or:
        call chaddr         ; find screen address for char.
        ld b,8              ; number of pixels high.
char0_or:
        ld a, (de)
        or (hl)
        ld (de),a           ; transfer to screen.
        inc hl              ; next piece of data.
        inc d               ; next pixel line.
        djnz char0_or      ; repeat
        ret


        ;; display hl at bc
draw_char_and:
        call chaddr         ; find screen address for char.
        ld b,8              ; number of pixels high.
char0_and:
        ld a, (de)
        and (hl)
        ld (de),a           ; transfer to screen.
        inc hl              ; next piece of data.
        inc d               ; next pixel line.
        djnz char0_and      ; repeat
        ret

draw_byte:                  ; with bc = xy, h=bitmap byte
        call chaddr         ; find screen address for char.
        ld a, h
        ld (de), a          ; load bitmap
        ret

draw_byte_stretch:                  ; with bc = xy, h=bitmap byte
        call chaddr         ; find screen address for char.
        ld b, 8
stretch_loop:
        ld a, h
        ld (de), a          ; load bitmap
        inc d
        djnz stretch_loop
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


test16:
        ld a, h
        cp d
        jp nz, fail
        ld a, l
        cp e
        jp nz, fail
        ret

unit_test:

        ld hl, 1
        call print_hl

        ld a, 0
        ld hl, brain_base
        ld d, h
        ld e, l
        call index_to_addr
        call test16

        ld hl, 2
        call print_hl

        ld a, 1
        ld hl, brain_base+neuron_size
        ld d, h
        ld e, l
        call index_to_addr
        call test16

        ld hl, 3
        call print_hl

        ld a, 10
        ld hl, brain_base+(neuron_size*10)
        ld d, h
        ld e, l
        call index_to_addr
        call test16

        ret
fail:
        call print_hl
        ld h, d
        ld l, e
        call print_hl
        ld hl, 9999
        call print_hl
