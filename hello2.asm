    ;Hello world for the zx-spectrum in z80 assembler
    ;by Chris Francis, c_francis1@yahoo.com
    ;This version repeats forever
    ;'org' and 'ent' are directives probably specific to
    ;the Zeus assembler from Crystal Computing that I
    ;used to compile and test the code.
    ;defm might be aswell, but it just defines a series
    ;of bytes.
    ;It can be run by typing RANDOMIZE USR 32768
    ;in Spectrum Basic

    ORG 32768


    LD IY, 5C3Ah
    RES 0, (IY+02)
    RES 1, (IY+01)

    LD HL, HELLO
    LD A,22
    RST 10h
    LD A,0
    RST 10h
    LD A,0
    RST 10h

LOOP    LD A,(HL)
    PUSH AF
    PUSH HL
    AND 7Fh
    RST 10h
    POP HL
    INC HL
    POP AF
    BIT 7,A
    JR Z, LOOP

    LD A,13
    RST 10h
    LD HL, HELLO
    JR LOOP
    RET

HELLO   DEFM /Hello World/
    DEFB 161
