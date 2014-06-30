org 32768

main:
        ;; Port 0xfe
        ;; Every even I/O address will address the ULA, but to avoid problems
        ;; with other I/O devices only Port 0xfe should be used. If this port
        ;; is written to, bits have the following meaning:
        ;;
        ;;  Bit   7   6   5   4   3   2   1   0
        ;;      +-------------------------------+
        ;;      |   |   |   | E | M |   Border  |
        ;;      +-------------------------------+
        ;;
        ;; The lowest three bits specify the border colour a zero in bit 3
        ;; activates the MIC output, whilst a one in bit 4 activates the EAR
        ;; output and the internal speaker. However, the EAR and MIC sockets
        ;; are connected only by resistors, so activating one activates the
        ;; other the EAR is generally used for output as it produces a louder
        ;; sound. The upper two bits are unused.

        ld hl,4000h

loop:
		ld c, 8
		ld e, 0 				; target byte
bitloop:		
		in a,(254)              ; query state
		;; ld a, %01000000
		and %01000000
		rl a
		rl a
		rl a
		ld b,c
rotloop:
		rl a
		djnz rotloop
		or e
		ld e, a                 ; e |= a
		
		dec c
		cp c
		jp nz, bitloop
		
        ld (hl), a              ; send to screen

        inc hl
        ld a, h
        cp 58h
        jp z, main
        jp loop
