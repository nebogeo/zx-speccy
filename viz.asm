

		org 32768
		
main:
		call clear_screen
		call print_titles

		ld b, 12
		ld c, 15
		ld hl, line_one
		call print_line_at

		ld hl, 3333
		push hl
		ld hl, 65535
		call __DIVU16

ret	

line_one:		
		defm "DIV16 65535/3333$"
line_two:		
		defm "MUL16 20123*8738$"
line_three:		
		defm "MUL8 2*85$"


print_line_at:	
		ld a, b
		ld (xcoord), a
		ld a, c
		ld (ycoord), a
		call setxy
		call print_line
		ret
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
__MUL8_FAST: ; __FASTCALL__ entry, a = a * h (8 bit mul) and Carry

	call draw_registers
    ld b, 8
	call draw_registers
    ld l, a
	call draw_registers
    xor a

__MUL8LOOP:
	call draw_registers
    add a, a ; a *= 2
	call draw_registers
    sla l
	call draw_registers
    jp nc, __MUL8B
	call draw_registers
    add a, h
	call draw_registers

__MUL8B:
	call draw_registers
    djnz __MUL8LOOP

	call draw_registers

	ret		; result = HL




		
__MUL16:	; Mutiplies HL with the last value stored into de stack
			; Works for both signed and unsigned
	call draw_registers

		ex de, hl
	call draw_registers
		pop hl		; Return address
	call draw_registers
		ex (sp), hl ; CALLEE caller convention

__MUL16_FAST:
	call draw_registers
        ld b, 16
	call draw_registers
        ld a, d
	call draw_registers
        ld c, e
	call draw_registers
        ex de, hl
	call draw_registers
        ld hl, 0
	call draw_registers

__MUL16LOOP:
        add hl, hl  ; hl << 1
	call draw_registers
        sla c
	call draw_registers
        rla         ; a,c << 1
	call draw_registers
        jp nc, __MUL16NOADD
	call draw_registers
        add hl, de
	call draw_registers

__MUL16NOADD:
        djnz __MUL16LOOP
	call draw_registers

		ret	; Result in hl (16 lower bits)


; Negates HL value (16 bit)
__ABS16:
	call draw_registers
	bit 7, h
	call draw_registers
	ret z

__NEGHL:
	call draw_registers
	ld a, l			; HL = -HL
	call draw_registers
	cpl
	call draw_registers
	ld l, a
	call draw_registers
	ld a, h
	call draw_registers
	cpl
	call draw_registers
	ld h, a
	call draw_registers
	inc hl
	call draw_registers
	ret


__DIVU16:    ; 16 bit unsigned division
             ; HL = Dividend, Stack Top = Divisor
	call draw_registers

	;   -- OBSOLETE ; Now uses FASTCALL convention
	ex de, hl
	call draw_registers
    pop hl      ; Return address
	call draw_registers
   	ex (sp), hl ; CALLEE Convention
	call draw_registers

__DIVU16_FAST:
    ld a, h
	call draw_registers
    ld c, l
	call draw_registers
    ld hl, 0
	call draw_registers
    ld b, 16

__DIV16LOOP:
	call draw_registers
    sll c
	call draw_registers
	rla
	call draw_registers
    adc hl,hl
	call draw_registers
    sbc hl,de
	call draw_registers
    jr  nc, __DIV16NOADD
	call draw_registers
    add hl,de
	call draw_registers
    dec c

__DIV16NOADD:
	call draw_registers
    djnz __DIV16LOOP
	call draw_registers

    ex de, hl
	call draw_registers
    ld h, a
	call draw_registers
    ld l, c
	call draw_registers

    ret     ; HL = quotient, DE = Mudulus



__MODU16:    ; 16 bit modulus
             ; HL = Dividend, Stack Top = Divisor

    ;ex de, hl
    ;pop hl
    ;ex (sp), hl ; CALLEE Convention

	call draw_registers
    call __DIVU16_FAST
    ex de, hl	; hl = reminder (modulus)
	call draw_registers
				; de = quotient

    ret


__DIVI16:	; 16 bit signed division
	;	--- The following is OBSOLETE ---
	;	ex de, hl
	;	pop hl
	;	ex (sp), hl 	; CALLEE Convention

__DIVI16_FAST:
	call draw_registers
	ld a, d
	call draw_registers
	xor h
	call draw_registers
	ex af, af'		; BIT 7 of a contains result
	call draw_registers

	bit 7, d		; DE is negative?
	call draw_registers
	jr z, __DIVI16A	
	call draw_registers

	ld a, e			; DE = -DE
	call draw_registers
	cpl
	call draw_registers
	ld e, a
	call draw_registers
	ld a, d
	call draw_registers
	cpl
	call draw_registers
	ld d, a
	call draw_registers
	inc de

__DIVI16A:
	call draw_registers
	bit 7, h		; HL is negative?
	call draw_registers
	call nz, __NEGHL
	call draw_registers

__DIVI16B:
	call draw_registers
	call __DIVU16_FAST
	call draw_registers
	ex af, af'
	call draw_registers

	or a	
	call draw_registers
	ret p	; return if positive
	call draw_registers
    jp __NEGHL

	
__MODI16:    ; 16 bit modulus
             ; HL = Dividend, Stack Top = Divisor

    ;ex de, hl
    ;pop hl
    ;ex (sp), hl ; CALLEE Convention

	call draw_registers
    call __DIVI16_FAST
	call draw_registers
    ex de, hl	; hl = reminder (modulus)
				; de = quotient

	call draw_registers
    ret


		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		

draw_registers:
		push bc
		push de
        push hl
		push af
		push bc
		ld a, (draw_loc)
		ld b, a
		ld c, 0
		
		call pix_addr
		pop bc
		pop af
		
        ld (hl), a              ; write pattern to pixel
		inc hl
		inc hl
        ld (hl), b              ; write pattern to pixel
		inc hl
		inc hl
        ld (hl), c              ; write pattern to pixel
		inc hl
		inc hl
        ld (hl), d              ; write pattern to pixel
		inc hl
		inc hl
        ld (hl), e              ; write pattern to pixel
		inc hl
		inc hl

		;; output original hl value
		ld d, h	  				; stores addr in de
		ld e, l
		pop hl					; get original version		
		ld b, h                 ; copy to bc
		ld c, l
		push hl					; put back on stack
		ld h, d                 ; load addr back into hl
		ld l, e
		ld (hl), b
		inc hl
		inc hl
		ld (hl), c

		call inc_pos

		
        pop hl
		pop de
		pop bc
        ret

inc_pos:		
		push af
		ld a, (draw_loc)
		inc a
		ld (draw_loc), a
		pop af
		ret

		
print_a:
        push af
        add a,30h
        call 16
        pop af
        ret

		
pix_addr:
								; get screen address
								; b = y pixel position	
								; c = x pixel position
								; returns address in hl
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


draw_loc:
		db 32

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

setxy:	ld a,22         ; ASCII control code for AT.
		rst 16          ; print it.
		ld a,(xcoord)   ; vertical position.
		rst 16          ; print it.
		ld a,(ycoord)   ; y coordinate.
		rst 16          ; print it.
		ret

xcoord:	db 0
ycoord:	db 0
	
print_line:                     ; Routine to print out a line		
        ld a,(hl)                     ; Get character to print
        cp '$'                        ; See if it '$' terminator
        jp z,print_end                 ; We're done if it is
        rst 16                        ; Spectrum: Print the character in 'A'
        inc hl                        ; Move onto the next character
        jp print_line                  ; Loop round

print_end:
        ret


print_titles:
		ld a,'a'				
        rst 16                  
        ld a,' '				
        rst 16                  
        ld a,'b'				
        rst 16                  
        ld a,' '				
        rst 16                  
        ld a,'c'				
        rst 16                  
        ld a,' '				
        rst 16                  
        ld a,'d'				
        rst 16                  
        ld a,' '				
        rst 16                  
        ld a,'e'				
        rst 16                  
        ld a,' '				
        rst 16                  
        ld a,'h'				
        rst 16                  
        ld a,' '				
        rst 16                  
        ld a,'l'				
        rst 16                  
        ld a,' '				
        rst 16                  

		ret

