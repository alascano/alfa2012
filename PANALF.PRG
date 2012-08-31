/**********************
* Surround Versioning
**********************
* $Author: admin$
* $Date: S·bado, 05 de Noviembre de 2005 7:25:39$
* $File: PANALF.PRG$
* $Revision: 1.0$
* $Log$
* admin - S·bado, 05 de Noviembre de 2005 7:25:39
* Alfredo - lunes, 08 de diciembre de 2003 7:32:07
* En Pantalla principal: VersiÛn y fecha
En FacturaciÛn: Leyenda EN PESOS / EN U$S  en color inverso.
* Alfredo - s·bado, 29 de noviembre de 2003 11:01:32
* Mostrar fecha (stoalf.fec)+ moneda (stoalf.ori)+ tipo de articulo (stoalf.tip) al facturar.
*****************************************************/

****Y2K****

PROCEDURE PANPRI
SET COLOR TO &COLSIS
@ 00,00,24,79 BOX "∞∞∞∞∞∞∞∞∞"
SET COLOR TO &COLNEG
@ 02,03,04,78 BOX "€€€€€€€€€"
SET COLOR TO &COLINV
@ 01,01 TO 3,76
SET COLOR TO &COLINV,G
*@ 01,03 SAY [ SISTEMA DE GESTION COMERCIAL - A•O 2000 COMPATIBLE ]
@ 01,03 SAY [ SISTEMA DE GESTION COMERCIAL - Ver. 3.1 25/04/2011 ]
@ 01,59 SAY [ ALFA REPUESTOS ]
SET COLOR TO &COLINV
@ 02,02 SAY [ Ventas Compras Clientes Proveedores Vendedores Produc Contab Bancos Util ]
@ 24,44 SAY [ F1 AYUDA ]
@ 24,56 SAY [ F2 TECLAS DE FUNCION ]
@ 24,02 SAY SPACE(10)
@ 24,03 SAY DATE()
@ 24,13 SAY SPACE(10)
SET COLOR TO &COLCOL
BAS = C
DO MENU000
SET COLOR TO &COLSIS
@ 5,0,23,79 BOX "∞∞∞∞∞∞∞∞∞"
SET COLOR TO &COLNUL
DO MENU2
RETURN

PROCEDURE MENU01
CA = C
F = 1
IF OPCION = 19
   C = C - 1
   IF  C = 0
       C = TOPEC
   ENDIF
ELSE
   C = C + 1
   IF C = TOPEC+1
      C = 1
   ENDIF
ENDIF
BAS = CA
SET COLOR TO &COLSIS
@ 5,0,23,79 BOX "∞∞∞∞∞∞∞∞∞"
SET COLOR TO &COLINV
DO MENU000
BAS = C
SET COLOR TO &COLCOL
DO MENU000
DO MENU2
SET COLOR TO &COLNUL
RETURN

PROCEDURE MENU000
RESTSCREEN(05,42,09,78,PANTPAS)
****       [ Ventas Compras Clientes Proveedores Vendedores Produc Contab Bancos Util ]
****       123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 
****        2      9      17       26          38         49     56     63     70
DO CASE
   CASE BAS = 1
     @ 2,02 SAY [ Ventas ]
     CBAS   = 2
   CASE BAS = 2
     @ 2,09 SAY [ Compras ]
     CBAS   = 8
   CASE BAS = 3
     @ 2,17 SAY [ Clientes ]
     CBAS   = 15
     BANDERA= 1
   CASE BAS = 4
     @ 2,26 SAY [ Proveedores ]
     CBAS   = 27
     BANDERA= 2
   CASE BAS = 5
     @ 2,38 SAY [ Vendedores ]
     CBAS   = 39
     BANDERA= 3
   CASE BAS = 6
     @ 2,49 SAY [ Produc ]
     CBAS   = 46
   CASE BAS = 7
     @ 2,56 SAY [ Contab ]
     CBAS   = 48
   CASE BAS = 8
     @ 2,63 SAY [ Bancos ]
     CBAS   = 50
   CASE BAS = 9
     @ 2,70 SAY [ Util ]
     CBAS   = 52
ENDCASE
RETURN

PROCEDURE MENU2
SET COLOR TO &COLNEG
DO CASE
   CASE C = 1
     @ 06,CBAS+1,20,CBAS+26 BOX "€€€€€€€€€"
     SET COLOR TO &COLINV
     @ 05,CBAS-1 TO 19,CBAS+24
     DO PANVEN
   CASE C = 2
     DO PANCOM
   CASE C = 3
     @ 06,CBAS+1,20,CBAS+26 BOX "€€€€€€€€€"
     SET COLOR TO &COLINV
     @ 5,CBAS-1 TO 19,CBAS+24
     DO PANCTA
     @ 17,CBAS TO 17,CBAS+23
     @ 18,CBAS SAY [ TARJETAS DE CREDITO    ]
   CASE C = 4
     @ 06,CBAS+1,16,CBAS+26 BOX "€€€€€€€€€"
     SET COLOR TO &COLINV
     @ 5,CBAS-1 TO 15,CBAS+24
     DO PANCTA
   CASE C = 5
     @ 06,CBAS+1,13,CBAS+26 BOX "€€€€€€€€€"
     SET COLOR TO &COLINV
     @ 5,CBAS-1 TO 12,CBAS+24
     DO PANCTA
   CASE C = 6
     DO PANSTO
   CASE C = 7
     DO PANCON
   CASE C = 8
     DO PANBAN
   CASE C = TOPEC
     DO PANUTI
ENDCASE
SET COLOR TO &COLNUL
RETURN

PROCEDURE MENU02
FA = F
IF OPCION = 5
   F = F - 1
   IF F = 0
      DO CASE
         CASE C = 1
           F = 10
         CASE C = 2
           F = 08
         CASE C = 3
           F = 09
         CASE C = 4
           F = 06
         CASE C = 5
           F = 04
	 CASE C = 6 
	   F = 12
         CASE C = 7
           F = 07
         CASE C = 8
           F = 11
         CASE C = 9 
           F = 07
      ENDCASE
   ENDIF
ELSE
   F = F + 1
   DO CASE
      CASE C = 1 .AND. F = 11
        F = 1
      CASE C = 2 .AND. F = 09
        F = 1
      CASE C = 3 .AND. F = 10
        F = 1
      CASE C = 4 .AND. F = 07
        F = 1
      CASE C = 5 .AND. F = 05
        F = 1
      CASE C = 6 .AND. F = 13
        F = 1
      CASE C = 7 .AND. F = 08
        F = 1
      CASE C = 8 .AND. F = 12
        F = 1
      CASE C = 9 .AND. F = 08
        F = 1
   ENDCASE
ENDIF
COL = FA
MEN = "MENU"+STRZERO(C,3)
SET COLOR TO &COLINV
FOR X=1 TO 2
    DO CASE
       CASE C=1
	 DO MENVEN
       CASE C=2
	 DO MENCOM
       CASE C=3
         DO MEN003
       CASE C=6
	 DO MENSTO
       CASE C=7
	 DO MENCON
       CASE C=8
	 DO MENBAN
       CASE C=TOPEC
	 DO MENUTI
       OTHERWISE
	 DO MENCTA
    ENDCASE
    COL=F
    SET COLOR TO &COLFIL
NEXT
RETURN

PROCEDURE MEN003
DO CASE
   CASE COL=09
     @ 18,CBAS SAY [ TARJETAS DE CREDITO    ]
   OTHERWISE
     DO MENCTA
ENDCASE
RETURN


