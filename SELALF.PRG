****Y2K****

PROCEDURE SELCLI
PUBLIC VT2M,DV2M,VT3M,DV3M,VTOM,TARM,TARN,CUOM
PUBLIC TRAX,NOMX,DOMX,CUIX,LOCX,NPRM,DFIM,FINM
PUBLIC MEN1,MEN2
MEN1=SPACE(30)
MEN2=SPACE(30)
DFIM=' '
MPRE=0
MDE1=0
IF MODDTOM='S'
   MPRE=1
   MDE1=1
ENDIF
MDE2=0
CUOM=0
TARM=0
DO CASE
   CASE COMM='PR'
     MDE1=1
     MDE2=1
     MPRE=1
ENDCASE   
DO CASE
   CASE C=1
     BUSDAX(BCLI,'PANXXX','INGXXX','Cliente  ','AUDXXX',00,00,07,'ULTCLI')
   CASE C=2
     IF F=5
        BUSDAX(BPRO,'PANXXX','INGXXX','Proveedor','AUDXXX',02,32,09,'ULTPRO')
     ELSE	
        BUSDAX(BPRO,'PANXXX','INGXXX','Proveedor','AUDXXX',00,00,07,'ULTPRO')
     ENDIF	
   CASE C=3
     BUSDAX(BCLI,'PANXXX','INGXXX','Cliente  ','AUDXXX',00,08,07,'ULTCLI')
   CASE C=4
     BUSDAX(BPRO,'PANXXX','INGXXX','Proveedor','AUDXXX',00,08,08,'ULTPRO')
   CASE C=5
     BUSDAX(BVEN,'PANXXX','INGXXX','Vendedor ',,00,08,05,'ULTVEN')
ENDCASE
RETURN

****//************************************************************************
PROCEDURE PANXXX
DO CASE
   CASE BASX=BCLI
     @ 01+LIX,02+LOX say [Cliente   :                                ]
     @ 02+LIX,02+LOX say [Domicilio :                                ]
     @ 03+LIX,02+LOX say [Localidad :                 CP:            ]
     @ 04+LIX,02+LOX say [Utilidad  :      % Cuo:    Tar:            ]
     @ 05+LIX,02+LOX say [C.Financ. :      d    d      %    d      % ]
     DO CASE
        CASE COMM='FC'
	  @ 06+LIX,01+LOX to 06+LIX,44+LOX double
	  @ 07+LIX,02+LOX say [Precio    :           Stock:               ]
	OTHERWISE
	  @ 06+LIX,01+LOX to 06+LIX,44+LOX double
          @ 07+LIX,02+LOX say [Cta. Cont.:                                ]
     ENDCASE	   
   CASE BASX=BPRO
     @ 01+LIX,02+LOX say [Proveedor :                                ]
     @ 02+LIX,02+LOX say [Domicilio :                                ]
     @ 03+LIX,02+LOX say [Localidad :                 Vto       d¡as ]
     @ 04+LIX,02+LOX say [Descuentos:       +       %       +      % ]
     DO CASE
        CASE COMM='FC'
	  @ 05+LIX,01+LOX to 05+LIX,44+LOX double
          @ 06+LIX,02+LOX say [Dtos.Prod.:       +       %       +      % ]
	  @ 07+LIX,02+LOX say [Remito N§ :           Stock:               ]
        OTHERWISE     
	  @ 05+LIX,02+LOX say [N§ de Cuit:                                ]
	  @ 06+LIX,02+LOX say [Cta. Prov.:                                ]
	  @ 07+LIX,02+LOX say [Cta. Deud.:                                ]
      ENDCASE
   CASE BASX=BVEN
     @ 01+LIX,02+LOX say [Vendedor  :                                ]
     @ 02+LIX,02+LOX say [Domicilio :                                ]
     @ 03+LIX,02+LOX say [Localidad :                                ]
     @ 04+LIX,02+LOX say [Cta. Vend.:                                ]
ENDCASE	

****//************************************************************************
PROCEDURE INGXXX
@ 01+LIX,14+LOX get nomm pict "@!"
@ 02+LIX,14+LOX get domm pict "@!"
@ 03+LIX,14+LOX get locm pict "@!"
DO CASE
   CASE BASX=BCLI
     @ 03+LIX,40+LOX get posm pict "@z 9999"
     @ 04+LIX,14+LOX get de1m pict "@z 99.99"
     @ 05+LIX,14+LOX get finm pict "9"
     IF COMM='FC'
	@ 07+LIX,31+LOX get deum pict "@z 999999"
     ELSE
	@ 07+LIX,14+LOX get deum pict "@z 999999"
     ENDIF	   
   CASE BASX=BPRO
     @ 03+LIX,35+LOX get vtom pict "@z 999"
     @ 04+LIX,14+LOX get de1m pict "@z 99.99"
     @ 04+LIX,22+LOX get de2m pict "@z 99.99"
     @ 04+LIX,31+LOX get fi1m pict "@z 99.99"
     @ 04+LIX,37+LOX get fi2m pict "@z 99.99"
     DO CASE
	CASE COMM='PC'
	CASE COMM='FC'
	  @ 07+LIX,31+LOX get deum pict "@z 999999"
	  @ 07+LIX,38+LOX get acrm pict "@z 999999"
	OTHERWISE
	  @ 05+LIX,14+LOX get cuim
	  @ 05+LIX,28+LOX get ivam pict "9"
	  @ 06+LIX,14+LOX get deum pict "@z 999999"
	  @ 07+LIX,14+LOX get acrm pict "@z 999999"
     ENDCASE
   CASE BASX=BVEN
     @ 04+LIX,14+LOX get deum pict "@z 999999"
ENDCASE
READ
RETURN

****//************************************************************************
PROCEDURE GRAXXX
repl nom with nomm
repl dom with domm,loc with locm
repl deu with deum,cpo with posm
DO CASE
  CASE BASX=BCLI
    repl de1 with de1m
    repl cui with cuim,iva with ivam
    repl tra with trax,vto with finm
    repl ven with venm,pco with pcom
  CASE BASX=BPRO
    repl de1 with de1m,de2 with de2m
    repl acr with acrm,vto with vtom
ENDCASE

RETURN

****//************************************************************************
PROCEDURE DATXXX
codm=cod
nomm=nom
domm=dom
locm=subs(loc,1,15)
posm=cpo
telm=tel
deum=deu
de1m=de1
de2m=de2
DO CASE
   CASE BASX=BCLI
     trax=tra
     zonm=zon
     rubm=rub
     cuim=cui
     ivam=iva
     deum=deu
     venm=ven
     crem=cre
     pcom=pco
     nprm=prv
     finm=vto
     DO CONPRV
   CASE BASX=BPRO
     vtom=vto
     cuim=cui
     ivam=iva
     deum=deu
     civm=civ
     fi1m=0
     fi2m=0
     nprm=prv
     DO CONPRV
     acrm=acr
   CASE BASX=BVEN
     prom=pro  
ENDCASE   
vencim=vtom
RETURN

****//************************************************************************
PROCEDURE MUEXXX
set color to w+
@ 01+LIX,14+LOX say [               ]
@ 01+LIX,14+LOX say strzero(codm,4)+" "+subs(nomm,1,25)
@ 02+LIX,14+LOX say domm
@ 03+LIX,14+LOX say subs(locm,1,15)
DO CASE
   CASE BASX=BVEN
     @ 04+LIX,14+LOX say deum pict "@z 999999"
   CASE BASX=BCLI
     NOMX=SPACE(30)
     DOMX=SPACE(20)
     LOCX=SPACE(16)
     CUIX=SPACE(13)
     SELE 2
     USE &BPRO INDEX &ICPRO SHARED
     SEEK TRAX
     IF FOUND()
        NOMX=NOM
	DOMX=DOM
	LOCX=LOC
	CUIX=CUI
     ENDIF
     TARN=SPACE(10)
     DE2M=0
     IF CUOM>0
	USE &BTAR INDEX &ICTAR SHARED
	SEEK TARM
	IF FOUND()
	   TARN=SUBS(NOM,1,10)
	   CUOX='C'+STRZERO(CUOM,2)
	   DE2M=&CUOX
	ENDIF
     ENDIF
     SELE 1
     @ 03+LIX,40+LOX say posm pict "@z 9999"
     @ 03+LIX,40+LOX say posm pict "@z 9999"
     @ 04+LIX,14+LOX say de1m pict "@z 99.99"
     @ 04+LIX,26+LOX say cuom pict "@z 99"
     @ 04+LIX,34+LOX say tarn
     @ 05+LIX,14+LOX say finm pict "9"
     IF FINM=0
        @ 05+LIX,16+LOX say [ C O N T A D O               ]
	VTOM=0
	VT2M=0
	VT3M=0
	DV2M=0
	DV3M=0
     ELSE	
	VTOM=FINVTOM
	VT2M=FINVT2M
	VT3M=FINVT3M
	DV2M=FINDV2M
	DV3M=FINDV3M
	@ 05+LIX,16+LOX say vtom pict "999"
	@ 05+LIX,21+LOX say vt2m pict "999"
	@ 05+LIX,26+LOX say dv2m pict "99.99"
	@ 05+LIX,33+LOX say vt3m pict "999"
	@ 05+LIX,38+LOX say dv3m pict "99.99"
     ENDIF
     IF COMM='FC'
	@ 07+LIX,31+LOX say deum pict "@z 999999"
     ELSE
	@ 07+LIX,14+LOX say deum pict "@z 999999"
     ENDIF	   
   CASE BASX=BPRO
     @ 03+LIX,35+LOX say vtom pict "@z 999"
     @ 04+LIX,14+LOX say de1m pict "@z 99.99"
     @ 04+LIX,22+LOX say de2m pict "@z 99.99"
     @ 04+LIX,31+LOX say fi1m pict "@z 99.99"
     @ 04+LIX,37+LOX say fi2m pict "@z 99.99"
     DO CASE
	CASE COMM='PC'
	CASE COMM='FC'
	  @ 07+LIX,31+LOX say deum pict "@z 999999"
	  @ 07+LIX,38+LOX say acrm pict "@z 999999"
	OTHERWISE
	  @ 05+LIX,14+LOX say cuim
          @ 05+LIX,28+LOX say ivam pict "9"
	  @ 06+LIX,14+LOX say deum pict "@z 999999"
	  @ 07+LIX,14+LOX say acrm pict "@z 999999"
     ENDCASE
ENDCASE
DO CASE
   CASE EMPCATM#0
     TDOC='C'
   CASE IVAM<2
     TDOC='A'
   OTHERWISE
     TDOC='B'    
ENDCASE
DO CASE
   CASE IVAM = 0
     CIVAM = "Responsable Inscripto   "
   CASE IVAM = 1
     CIVAM = "Responsable No Inscripto"
   CASE IVAM = 2
     CIVAM = "Consumidor Final        "
   CASE IVAM = 3
     CIVAM = "Exento                  "
ENDCASE
CUEM=ACRM
RETURN

PROCEDURE AUDXXX
if deum=0
   DEUM=1
endif
if acrm=0
   ACRM=1
endif
RETURN
