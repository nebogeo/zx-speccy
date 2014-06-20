#include <input.bas>

REM MicroHobby Num. 16 :')

   1 BORDER 1: PAPER 1: INK 7: CLS : PRINT AT 10,10; FLASH 1;"PARE LA CINTA": PAUSE 200
   4 PAPER 1: BORDER 6: CLS
   5 INK 4: BRIGHT 1: PRINT AT 0,0;"HIPERBOLOIDE"
   7 PRINT AT 22, 0; "Angulo";: LET s1$ = INPUT(16): LET s = VAL s1$
   8 PRINT AT 22, 0; "Resolucion";: LET s1$ = INPUT(16): LET k = VAL s1$: PRINT AT 22, 0; "                        ";
   9 INK 4: BRIGHT 1: PRINT AT 21,0;"S= ";s
  10 FOR w=0 TO 2*PI STEP PI/100
  20 LET x1=50*COS w
  30 LET y1=15*SIN w
  40 LET x2=50*COS (w+s)
  50 LET y2=15*SIN (w+s)
  60 INK 7: BRIGHT 1: PLOT x1+120,y1+145
  70 INK 7: BRIGHT 1: PLOT x2+120,y2+25
  80 NEXT w
 100 FOR w=0 TO PI STEP PI/k
 200 LET x1=50*COS w
 300 LET y1=15*SIN w
 400 LET x2=50*COS (w+s)
 500 LET y2=15*SIN (w+s)
 600 PLOT x1+120,y1+145
 700 PLOT x2+120,y2+25
 900 PLOT x1+120,y1+145
1000 INK 7: BRIGHT 1: DRAW x2-x1,y2-y1-120
1060 IF w=PI THEN GO TO 1110: END IF
1065 NEXT w
1110 FOR w=PI TO 2*PI STEP PI/(k+1)
1120 LET x1=50*COS w
1130 LET y1=15*SIN w
1140 LET x2=50*COS (w+s)
1150 LET y2=15*SIN (w+s)
1160 PLOT x1+120,y1+145
1170 PLOT x2+120,y2+25
1180 PLOT x1+120,y1+145
1190 INK 7: BRIGHT 1: DRAW x2-x1,y2-y1-120
1205 IF w=2*PI THEN GO TO 1220: END IF
1210 NEXT w
1220 INK 5: IF s=0 THEN PRINT AT 0,20;"CILINDRO": END IF
1230 INK 5: IF s=PI THEN PRINT AT 0,20;"SISTEMA CONICO": END IF
1240 INK 5: IF s<>0 AND s<>PI THEN PRINT AT 0,20;"HIPERBOLOIDE": END IF
1250 PRINT AT 23,0; "REPITES (S/N)";: LET Z$ = INPUT(1)
1260 IF Z$="S" THEN GO TO 1: END IF
1270 CLS : PRINT "ADIOS"
