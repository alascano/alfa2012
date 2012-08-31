****Y2K****

PROCEDURE FINFAC(IVEN,RVEN,MVEN,MCOP)
****// IVEN 1 impresion - 2 graba    ****************************************
****// RVEN 1 reimprime - 2 no       ****************************************
****// MVEN 1 modifica  - 2 no       ****************************************
****// MCOP 1 modifica copias - 2 no ****************************************
DOCXXX='DOC'+ABRCLIM
LPTI=1
*PFA_MESSAGEBOX("DOCXXX->"+DOCXXX,"-")
DO &DOCXXX
IF PALCLA='26595'
   COP=0
ELSE
   COP=COPXXX
ENDIF
LVEN=10+LIN+URM
FACP='N' && factura presupuesto
FREM='N' && impre remito desde factura
do case
   case consult=0
     pedi=0
   case pedi=3
     pedi=2
endcase
salir=0
accion=2
bandcla=2
set color to i,w+/r
do case
   case iven=1.and.pedi=0
     @ lven,01 PROMPT "    IMPRIMIR    "
   case iven=1.and.rven=1
     @ lven,01 PROMPT " REIMPRIMIR "
     cop=0
   case iven=2.and.pedi=0
     @ lven,01 PROMPT "     GRABAR     "
   otherwise
     @ lven,01 PROMPT " FINALIZAR  "
endcase
if pedi=0
   @ lven,17 PROMPT "  REINGRESAR  "
   @ lven,31 PROMPT "   CANCELAR   "
else
   @ lven,13 PROMPT " REINGRESAR "
   @ lven,25 PROMPT " MODIFICAR "
   @ lven,36 PROMPT " ANULAR  "
endif
menu to accion
set color to w+
MODIPAU=0
IF SUPERVISOR=1.OR.PERMI=3
   MODIPAU=1
ENDIF

* CONTROLA QUE NO HAYA ARTICULOS REPETIDOS
*PFA_MESSAGEBOX('MUESTRA EL PFA_CONSULTANDO',PFA_CONSULTANDO)
if accion = 1 .AND. PFA_CONSULTANDO = 'NO'
	Cant_repetidos = PFA_CTRART()

	if Cant_Repetidos > 0
		pfa_messagebox('Sr. Operador','El articulo indicado ya fue facturado en lineas anteriores. Controle.')
		y = Cant_Repetidos
		accion = 2
	endif
else
	Cant_Repetidos = 0
endif


do case
   case accion=2.or.lastkey()=27
*     @ lven,01 say [      Ingrese el Numero de Renglon :        ]
*     set color to
*     y=1
*     @ lven,38 get y pict "@z 999" range 1,max
*     read
*     @ lven,38 say y pict "@z 999"

*
* LAS SIGUIENTES LINEAS REEMPLAZARON LAS ANTERIORES DENTRO DE LA OPCION DEL CASE
*
    	if .not. Cant_Repetidos > 0
    		@ lven,01 say [      Ingrese el Numero de Renglon :        ]
    		set color to
    		y=1
    		@ lven,38 get y pict "@z 999" range 1,max
    		read
    		@ lven,38 say y pict "@z 999"
    	endif

   case (accion=3.and.pedi=0).or.(accion=1.and.pedi#0.and.rven=2)
     salir=1
     bandcla=1
   case accion=3.or.accion=4
     do case
        case MODIPAU=0.and.(comm='FC'.or.comm='NC'.or.comm='ND'.or.comm='RV'.or.comm='PR'.or.comm='RC')
          AVISO("USTED NO TIENE AUTORIZACION PARA MODIFICAR O ANULAR ESTE COMPROBANTE",1,22,6)
          return
        case accion=3.and.tipm='V'.and.(comm='FC'.or.comm='NC'.or.comm='ND')
          AVISO("ESTE COMPROBANTE NO PUEDE MODIFICARSE",1,22,20)
          return
        case tipm='C'.and.(comm='FC'.or.comm='NC'.or.comm='ND').and.pagm#0
          AVISO("ESTE COMPROBANTE NO PUEDE MODIFICARSE, (ANULAR PAGO)",1,22,20)
          return
        case accion=3.and.mven=2
          AVISO("ESTE COMPROBANTE NO PUEDE MODIFICARSE",1,22,20)
          return
        case motm#'A'.and.motm#'F'.and.motm#'H'.and.fexm<fecstom.and.(comm='FC'.or.comm='NC')
	  IF FEXM<FECSTOM.AND.PALCLA='99823'
	  ELSE
             AVISO("ESTE COMPROBANTE ES ANTERIOR AL CIERRE DE STOCK, NO PUEDE MODIFICARSE",1,22,6)
             return
          ENDIF
     endcase
     conf='N'
     @ lven,01 say [    Confirma el Proceso (S/N) ?    :        ]
     set color to
     @ lven,38 get conf pict "@!"
     read
     @ lven,38 say conf
     if conf#'S'
        return
     endif
     mcop=1
     if accion=3
        q=1
	w=priren
        pedi=0
     else
        setcolor("w+/*")
        @ LVEN,01 SAY CEDEIZ('P R O C E S A N D O',0,44)
	set color to
        IF TIPM='V'
           DO BAJVEN
        ELSE
	   DO BAJCOM
	ENDIF
	salir=1
	bandcla=1
     endif
   case accion=1
     do case
        case marc='X'.and.tipm='C'.and.(comm='FC'.or.comm='NC')
	  cop=0
*	  PFA_MESSAGEBOX('DOCVYC 1','INI')
	  DO DOCVYC
*	  PFA_MESSAGEBOX('DOCVYC 1','FIN')
	case iven=2.and.pedi=0.and.COMM='RC'
	  @ lven,01 say [    Retiene este Comprobante (S/N) :        ]
	  retm='N'
	  set color to
	  @ lven,39 get retm pict '@!'
	  read
	  if lastkey()=27
	     return
	  endif
        case iven=1
	  DO CASE
	     CASE PEDI=0.AND.COMM='PR'.AND.PERMI=0
               @ lven,01 say [   Desea Facturar  (S/N)  :       Copias:   ]
               facp='N'
               lcop=43
	       set color to
	       @ lven,29 get facp pict '@!'
	     CASE PEDI=0.AND.(COMM='RV'.OR.COMM='DV'.OR.COMM='PC'.OR.COMM='DC'.OR.(COMM='PV'.AND.BANDRES='Y'))
               @ lven,01 say [Retiene este Comprobante (S/N):   Copias:   ]
               resx=bandres
               retm='N'
               lcop=43
	       set color to
	       @ lven,33 get retm pict '@!'
	     CASE PEDI=0.AND.COMM='PV'
               @ lven,01 say [ Reserva Stock :    Retiene :    Copias :   ]
               resx=bandres
               retm='N'
               lcop=43
	       set color to
	       @ lven,18 get resx pict '@!'
	       @ lven,31 get retm pict '@!'
	     CASE FACREMM='S'.AND.COMM='FC'.AND.CONSULT=0.AND.MARC=' '
	       @ lven,01 say [   Imprime Remito  (S/N)  :       Copias:   ]
	       lcop=43
	       set color to
	       @ lven,29 get frem pict '@!'
             OTHERWISE
               @ lven,01 say [ Cantidad de Copias                :        ]
	       lcop=38
	       set color to
          ENDCASE
	  if mcop=1.or.cop=0.or.consult#0
	     @ lven,lcop get cop pict "9"
	  else
	     cox=cop
	     @ lven,lcop get cop pict "9" valid cop=cox
	  endif
	  read
	  if marc='X'
	     AVISO("FORMULARIO COMUN",1,22,25)
	  endif
	  if lastkey()=27
	     return
	  endif
	  if cop>0

	IF PF_SINFISCAL > 0
		bandloop = 0
	ELSE
	     abrprn(lven,01,44,LPTI)
	ENDIF


	     if bandloop=1
		return
	     endif
	  endif
*	  PFA_MESSAGEBOX('DOCVYC 2','INI')
	  DO DOCVYC
*	  PFA_MESSAGEBOX('DOCVYC 2','FIN')
	  IF MOTM='C'.AND.MARC#'X'
	     PROCES='IMPFCR'
	     DO &PROCES
	  ENDIF
	  IF FREM='S'
	     ANRO=NROM
	     ACOM=COMM
	     ACOP=COP
	     COMM='RV'
             DO &DOCXXX
	     COP=COPXXX
       	IF PF_SINFISCAL > 0
       		bandloop = 0
       	ELSE
       	     abrprn(lven,01,44,LPTI)
       	ENDIF

	     if bandloop=1
		return
	     endif
*		  PFA_MESSAGEBOX('DOCVYC 3','INI')
	     DO DOCVYC
*		  PFA_MESSAGEBOX('DOCVYC 3','FIN')
	     FOR T=1 TO LEN(TCOD)
	         DO CASE
		    CASE TCOD[T]=0
		      EXIT
		    CASE TREM[T]=0
		      TREM[T]=NROM
		 ENDCASE
	     NEXT
	     NROM=ANRO
	     COMM=ACOM
	  ENDIF
     endcase
     if pedi=0
        setcolor("w+/*")
        @ LVEN,01 SAY CEDEIZ('P R O C E S A N D O',0,44)
	set color to
        IF TIPM='V'
           DO GRAVEN
           * IMPRESORA FISCAL
           IF AT(COMM,'FC.NC.ND') > 0
               @ LVEN,01 SAY CEDEIZ('I M P R E S I O N   F I S C A L',0,44)
               PFA_ImpFiscal(codm,comm,nrom,nrosucm)
			   nada = PFA_CtrNum()
           ENDIF
        ELSE
	   DO GRACOM
	ENDIF
     endif
     salir=1
     bandcla=1
endcase
set color to
RETURN
