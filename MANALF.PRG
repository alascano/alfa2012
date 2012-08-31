****Y2K****

PROCEDURE MANSIS
CG4    = 1
CG9    = NIL
MODCOD = 1
MAXIMO = 0
BAND1 = 0
BAND3 = 0
ULTAUXIM = 0
IF PAU = 2 .AND. BAND4 # 3
   SELE 2
   USE &BAUX SHARED
   MAXIMO = &ULTAUXI
   SELE 1
ENDIF
SET COLOR TO W+
CG1='N'
CG3='MAXIMO'
CG2=4
DO CASE
   CASE BAND5=25 && Tarjetas
     @ 07,COL+3 SAY [C¢digo           :]
     @ 08,COL+3 SAY [Nombre           :]
     @ 09,COL+3 SAY [Recargo Cuota-01 :        %]
     @ 10,COL+3 SAY [Recargo Cuota-02 :        %]
     @ 11,COL+3 SAY [Recargo Cuota-03 :        %]
     @ 12,COL+3 SAY [Recargo Cuota-04 :        %]
     @ 13,COL+3 SAY [Recargo Cuota-05 :        %]
     @ 14,COL+3 SAY [Recargo Cuota-06 :        %]
     @ 15,COL+3 SAY [Recargo Cuota-07 :        %]
     @ 16,COL+3 SAY [Recargo Cuota-08 :        %]
     @ 17,COL+3 SAY [Recargo Cuota-09 :        %]
     @ 18,COL+3 SAY [Recargo Cuota-10 :        %]
     @ 19,COL+3 SAY [Recargo Cuota-11 :        %]
     @ 20,COL+3 SAY [Recargo Cuota-12 :        %]
   CASE BAND5=28 && Proveedores
     @ 07,COL+3 SAY [C¢digo           :]
     @ 08,COL+3 SAY [Raz¢n Social     :]
     @ 09,COL+3 SAY [Domicilio        :]
     @ 10,COL+3 SAY [Localidad / C.P. :]
     @ 11,COL+3 SAY [Provincia        :]
     @ 12,COL+3 SAY [Tel‚fono/s       :]
     @ 13,COL+3 SAY [C.U.I.T. / I.V.A.:]
     @ 14,COL+3 SAY [Descuentos       :        +       %  Vto:     D¡as]
     @ 15,COL+3 SAY [Observaciones    :]
     @ 16,COL+3 SAY [C¢digo de Rubro  :]
     @ 17,COL+3 SAY [Cuenta Proveedor :]
     @ 18,COL+3 SAY [Cuenta Deudora   :]
   CASE BAND5=29
     @ 07,COL+3 SAY [C¢digo           :]
     @ 08,COL+3 SAY [Raz¢n Social     :]
     @ 09,COL+3 SAY [Domicilio        :]
     @ 10,COL+3 SAY [Localidad / C.P. :]
     @ 11,COL+3 SAY [Provincia        :]
     @ 12,COL+3 SAY [Telefono/s       :]
     @ 13,COL+3 SAY [Cuit. - Iva.     :]
     @ 14,COL+3 SAY [Utilidad / C.Finc:        %                       ]
     @ 15,COL+3 SAY [Observaciones    :]
     @ 16,COL+3 SAY [L¡mite de Cr‚dito:]
     @ 17,COL+3 SAY [Transporte       :]
     @ 18,COL+3 SAY [Vendedor/Comisi¢n:]
     @ 19,COL+3 SAY [Rubro            :]
     @ 20,COL+3 SAY [Zona             :]
     @ 21,COL+3 SAY [Cuenta Cliente   :]
ENDCASE
MANTE("MOVSIS","EDISIS","REPSIS")
RETURN

PROCEDURE MOVSIS
SET COLOR TO
clave1=cod
@ 07,col+23 say clave1
DO CASE
   CASE BAND5=25
     car02=nom
     num01=c01
     num02=c02
     num03=c03
     num04=c04
     num05=c05
     num06=c06
     num07=c07
     num08=c08
     num09=c09
     num10=c10
     num11=c11
     num12=c12
   CASE BAND5=28
     car02=nom
     car21=dom
     car08=loc
     num01=cpo
     car12=PRV
     car22=tel
     car07=cui
     num03=iva
     num06=de1
     num07=de2        
     num08=vto
     num09=rub
     num10=deu
     num11=acr
     car25=ob1
   CASE BAND5=29
     car02=nom
     car21=dom
     car08=loc
     num01=cpo
     car12=PRV
     car22=tel
     car07=cui
     num03=iva
     num04=de1
     num05=de2        
     num08=cre
     num11=ven
     num12=pco
     num13=rub
     num14=zon
     num15=deu
     num07=vto
     num09=tra
     car25=ob1
ENDCASE
DO MUESIS
RETURN

PROCEDURE EDISIS
SET COLOR TO I
DO MUESIS
SET COLOR TO
DO CASE
   CASE BAND5=25
     @ 08,COL+23 GET CAR02 PICT "@!" VALID CAR02 # SPACE(30)               
     @ 09,COL+23 GET NUM01 PICT '@Z 99.99'
     @ 10,COL+23 GET NUM02 PICT '@Z 99.99'
     @ 11,COL+23 GET NUM03 PICT '@Z 99.99'
     @ 12,COL+23 GET NUM04 PICT '@Z 99.99'
     @ 13,COL+23 GET NUM05 PICT '@Z 99.99'
     @ 14,COL+23 GET NUM06 PICT '@Z 99.99'
     @ 15,COL+23 GET NUM07 PICT '@Z 99.99'
     @ 16,COL+23 GET NUM08 PICT '@Z 99.99'
     @ 17,COL+23 GET NUM09 PICT '@Z 99.99'
     @ 18,COL+23 GET NUM10 PICT '@Z 99.99'
     @ 19,COL+23 GET NUM11 PICT '@Z 99.99'
     @ 20,COL+23 GET NUM12 PICT '@Z 99.99'
     READ
   CASE BAND5=28
     FOR W=1 TO 14
         DO CASE
	    CASE W=01
	      @ 08,COL+23 GET CAR02  PICT "@!" VALID CAR02 # SPACE(30)               
	      READ
	    CASE W=02
	      @ 09,COL+23 GET CAR21
	      READ
	    CASE W=03
	      @ 10,COL+23 GET CAR08
	      READ
	    CASE W=04  
	      @ 10,COL+49 GET NUM01 PICT '@Z 9999'
	      READ
	    CASE W=05
              BUWDES(BPRV,'CAR12',11)
	    CASE W=06
	      @ 12,COL+23 GET CAR22
	      READ
	    CASE W=07
              @ 13,COL+23 GET CAR07 PICT "@!"
	      READ
	    CASE W=08
              @ 13,COL+40 GET NUM03 PICT "9" RANGE 0,3
	      READ
	    CASE W=09  
              @ 14,COL+23 GET NUM06 PICT "@Z 99.99"
	      READ
	    CASE W=10  
              @ 14,COL+31 GET NUM07 PICT "@Z 99.99"
	      READ
	    CASE W=11  
              @ 14,COL+45 GET NUM08 PICT "@Z 999"
              @ 15,COL+23 GET CAR25 PICT "@!" 
	      READ
	    CASE W=12
              BUWDES(BRUB,'NUM09',16)
	    CASE W=13
              BUWDES(BPLA,'NUM10',17)
	    CASE W=14
              BUWDES(BPLA,'NUM11',18)
	 ENDCASE  
	 IF W=05.OR.W=12.OR.W=13.OR.W=14
	 ELSE
	    LASTTEC=LASTKEY()
	 ENDIF
	 DO CASE
	    CASE LASTTEC=3
	      EXIT
	    CASE LASTTEC=5.AND.W#1
              W-=2
	      LOOP
	 ENDCASE
     NEXT	      
   CASE BAND5=29
     FOR W=1 TO 18
         DO CASE
	    CASE W=01
	      @ 08,COL+23 GET CAR02  PICT "@!" VALID CAR02 # SPACE(30)               
	      READ
	    CASE W=02
	      @ 09,COL+23 GET CAR21
	      READ
	    CASE W=03
	      @ 10,COL+23 GET CAR08
	      READ
	    CASE W=04  
	      @ 10,COL+49 GET NUM01 PICT '@Z 9999'
	      READ
	    CASE W=05
              BUWDES(BPRV,'CAR12',11)
	    CASE W=06
	      @ 12,COL+23 GET CAR22
	      READ
	    CASE W=07
              @ 13,COL+23 GET CAR07 PICT "@!"
	      READ
	    CASE W=08
              @ 13,COL+40 GET NUM03 PICT "9" RANGE 0,3
	      READ
	    CASE W=09
              @ 14,COL+23 GET NUM04 PICT "@Z 99.99"
	      READ
	    CASE W=10  
	      @ 14,COL+40 GET NUM07 PICT "9"
	      READ
	    CASE W=11  
              @ 15,COL+23 GET CAR25 PICT "@!" 
	      READ
            CASE W=12
              @ 16,COL+23 GET NUM08 PICT "@Z 9999999"
	      READ
	    CASE W=13
              BUWDES(BPRO,'NUM09',17)
	    CASE W=14
              BUWDES(BVEN,'NUM11',18,23)
	    CASE W=15  
              @ 18,COL+48 GET NUM12 PICT '@Z 99.99'
	      READ
	    CASE W=16
              BUWDES(BRUB,'NUM13',19)
	    CASE W=17
              BUWDES(BZON,'NUM14',20)
	    CASE W=18
              BUWDES(BPLA,'NUM15',21)
	 ENDCASE  
	 IF W=05.OR.W=13.OR.W=14.OR.W=16.OR.W=17.OR.W=18
	 ELSE
	    LASTTEC=LASTKEY()
	 ENDIF
	 DO CASE
	    CASE LASTTEC=3
	      EXIT
	    CASE LASTTEC=5.AND.W#1
              W-=2
	      LOOP
	 ENDCASE
     NEXT	      
ENDCASE
SET COLOR TO I
DO MUESIS
SET COLOR TO
RETURN

PROCEDURE REPSIS
IF PAU=2
   BAND1=1
   INSERTAREG(0)
   REPL COD WITH CLAVE1
ELSE
   BLOQUEAREG(0)
ENDIF
DO CASE
   CASE BAND5=25
     REPL NOM WITH CAR02
     REPL C01 WITH NUM01,C02 WITH NUM02,C03 WITH NUM03
     REPL C04 WITH NUM04,C05 WITH NUM05,C06 WITH NUM06
     REPL C07 WITH NUM07,C08 WITH NUM08,C09 WITH NUM09
     REPL C10 WITH NUM10,C11 WITH NUM11,C12 WITH NUM12
   CASE BAND5=28
     REPL NOM WITH CAR02,DOM WITH CAR21,LOC WITH CAR08
     REPL CPO WITH NUM01,PRV WITH CAR12,TEL WITH CAR22
     REPL CUI WITH CAR07,IVA WITH NUM03,CIV WITH 1
     REPL DE1 WITH NUM06,DE2 WITH NUM07,OB1 WITH CAR25
     REPL VTO WITH NUM08,RUB WITH NUM09
     REPL DEU WITH NUM10,ACR WITH NUM11
   CASE BAND5=29
     REPL NOM WITH CAR02,DOM WITH CAR21,LOC WITH CAR08
     REPL CPO WITH NUM01,PRV WITH CAR12,TEL WITH CAR22
     REPL CUI WITH CAR07,IVA WITH NUM03,DE1 WITH NUM04
     REPL DE2 WITH NUM05,VTO WITH NUM07,CRE WITH NUM08
     REPL VEN WITH NUM11,PCO WITH NUM12,TRA WITH NUM09
     REPL RUB WITH NUM13,ZON WITH NUM14,DEU WITH 112100
     REPL OB1 WITH CAR25
ENDCASE
COMMIT
RETURN

PROCEDURE MUESIS
DO CASE
   CASE BAND5=25
     @ 08,COL+23 SAY CAR02
     @ 09,COL+23 SAY NUM01 PICT '@Z 99.99'
     @ 10,COL+23 SAY NUM02 PICT '@Z 99.99'
     @ 11,COL+23 SAY NUM03 PICT '@Z 99.99'
     @ 12,COL+23 SAY NUM04 PICT '@Z 99.99'
     @ 13,COL+23 SAY NUM05 PICT '@Z 99.99'
     @ 14,COL+23 SAY NUM06 PICT '@Z 99.99'
     @ 15,COL+23 SAY NUM07 PICT '@Z 99.99'
     @ 16,COL+23 SAY NUM08 PICT '@Z 99.99'
     @ 17,COL+23 SAY NUM09 PICT '@Z 99.99'
     @ 18,COL+23 SAY NUM10 PICT '@Z 99.99'
     @ 19,COL+23 SAY NUM11 PICT '@Z 99.99'
     @ 20,COL+23 SAY NUM12 PICT '@Z 99.99'
   CASE BAND5=28
     do case
        case num03=1
	  @ 13,col+39 say [-RNI]
        case num03=2 
	  @ 13,col+39 say [-C.F]
        case num03=3
	  @ 13,col+39 say [-EXE]
        otherwise
	  @ 13,col+39 say [-R.I]
     endcase
     @ 08,col+23 say car02
     @ 09,col+23 say car21
     @ 10,col+23 say car08
     @ 10,col+49 say num01 pict "@z 9999"
     MUEDES(BPRV,CAR12,11)
     @ 12,col+23 say car22
     @ 13,col+23 say car07
     @ 13,col+38 say num03 pict "9"
     @ 14,col+23 say num06 pict "@z/@e 99.99"
     @ 14,col+31 say num07 pict "@z/@e 99.99"
     @ 14,col+48 say num08 pict "@z 999"
     @ 15,col+23 say car25
     MUEDES(BRUP,NUM09,16)
     MUEDES(BPLA,NUM10,17)
     MUEDES(BPLA,NUM11,18)
   CASE BAND5=29
     do case
        case num03=1
	  @ 13,col+41 say [-RNI]
        case num03=2 
	  @ 13,col+41 say [-C.F]
        case num03=3
	  @ 13,col+41 say [-EXE]
        otherwise
	  @ 13,col+41 say [-R.I]
     endcase
     @ 07,col+23 say clave1
     @ 08,col+23 say car02
     @ 09,col+23 say car21
     @ 10,col+23 say car08
     @ 10,col+49 say num01 pict "@z 9999"
     MUEDES(BPRV,CAR12,11)
     @ 12,col+23 say car22
     @ 13,col+23 say car07
     @ 13,col+40 say num03 pict "9"
     @ 14,col+23 say num04 pict "@z/@e 99.99"
     @ 14,col+31 say num05 pict "@z/@e 99.99"
     @ 14,col+40 say num07 pict "9"
     @ 15,col+23 say car25
     @ 16,col+23 say num08 pict "@z 9999999"
     MUEDES(BPRO,NUM09,17)
     MUEDES(BVEN,NUM11,18,23)
     @ 18,col+48 say num12 pict '@z 99.99'
     MUEDES(BRUB,NUM13,19)
     MUEDES(BZON,NUM14,20)
     MUEDES(BPLA,NUM15,21)
ENDCASE
RETURN
