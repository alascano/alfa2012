****Y2K****
#include "fileio.ch"
#include "pfiscal.ch"
#define fiscal_log
*#define PF_SINFISCAL
*#undef PF_SINFISCAL
#define Arc_Salida 'fi'+left(dtos(date()),6)+'.log'

PUBLIC nArcSalida,PFA_consultando
PFA_consultando = 'NO'
#ifdef fiscal_log
   if file(Arc_Salida)
	   nArcSalida = FOPEN(Arc_Salida,FO_READWRITE+FO_EXCLUSIVE)
   else
	   nArcSalida = FCREATE(Arc_Salida,FC_NORMAL)
   endif
   FSEEK(nArcSalida, 0, 2)   &&se posiciona al final del archivo
   FWRITE(nArcSalida,chr(13)+'***'+dtoc(date())+' '+TIME()+'***'+chr(13))
#endif
#ifdef fiscal_log
*    pfa_messagebox('fiscal_log definida','')
#endif
#ifdef Arc_Salida
*    pfa_messagebox('Arc_salida definida','')
#endif

*PFA_TIEMPOMAXIMO=0
BANDLOT='N'
INFFINC='N'         && INFORME FINANCIERO
DESADIM='N'         && DESCRIPCION DEL PRODUCTO ADICIONAL (2 RENGLONES)
BANDBAR='N'         && LECTOR DE CODIGO DE BARRAS
BANDOFI='N'         && RECIBO SIN NRO OFICIAL
FACREMM='N'         && IMPRIME REMITO DESDE FACTURACION
DESPEVM=0           && NO GRABA DESCRIPCION DE REMITOS
VALREMI=1           && SI VALORIZA REMITOS EN EL MOMENTO DE LA FACTURACION
BANDRES='N'         && RESERVA STOCK EN PEDIDOS DE VENTAS
DESFINM=1           && DESCUENTO FINANCIERO EN FACTURAS DE VENTA
PERIBRM=0           && APLICA PERCEPCION DE INGRESOS BRUTOS EN VENTAS
ESTSTOM=0           && PERMITE ESTADISTICA DE CIERRE DE STOCK
DATOESP=NIL         && CARGA DE DATOS ADICIONALES DESPUES DE LA FECHA VENTAS
PENDESP='PENALF'    && CARGA DE DATOS ADICIONALES DESPUES DE BUSQ PENDIENTES
MUECONM=NIL         && MUESTRA DATOS ADICIONALES EN CONSULTAS DE VENTAS
OBSVENM='OBSVEN'    && PEDIDO DE OBSERVACIONES DE VENTAS
BUPESPM='BUSALF'    && BUSQUEDA DE PRODUCTOS POR MONODROGA
BUSESPM=NIL         && BUSQUEDA FILTRO ESPECIAL DE PRODUCTOS DE VENTAS
GRAFAVM='GRAALF'    && PROCESO DE GRABACION ADICIONAL EN VENTAS
BAJFAVM=NIL         && PROCESO DE BAJAS ADICIONAL EN VENTAS
GRAFACM=NIL         && PROCESO DE GRABACION ADICIONAL EN COMPRAS
BAJFACM=NIL         && PROCESO DE BAJAS ADICIONAL EN COMPRAS
ADESOBT=0           && DESCUENTOS OBTENIDOS PAGO A PROVEEDORES
ARETGAN=0           && APLICA RETENCIONES DE GANANCIAS EN REGISTRO DE PAGOS
AJUFIN =8           && GRABA DOS DESCUENTOS FINANCIEROS
ARETIBR=0           && APLICA RETENCIONES DE ING.BRUTOS EN REGISTRO DE PAGOS
ARETIVA=0           && APLICA RETENCIONES DE I.V.A. EN REGISTRO DE PAGOS
OFERTA =0           && OFERTAS POR CANTIDAD EN VENTAS Y COMPRAS
VENDED =1           && PERMITE MODULO DE VENDEDORES
GRAVEN =0           && GRABA COMISIONES DE VENDEDORES A PARTIR DE FACTURACION
PARCON ='PARCON'    && PARAMETROS CONTABLES
CONTAC =7           && UBICACION DEL MODULO DE CONTABILIDAD
CONIND =5           && UBICACION INDEXACION DE CONTABILIDAD
BLANDES=SPACE(13)
VENSAB =0
NROCAMB=260472
FECCAMB=CTOD('04/26/1972')
STOCKM ='(STO+REC+DEV)-(RES+REV+DEC)'
PF_LEYENDADO = ''
PF_CLABAL = 'QQWWE'


****/ datos del cliente *******************************************************
EMPRESA = "ALFA REPUESTOS                "                && 30
CUITEMP = "30-61895175-4"                                 && 13
DIREMPR = "                 "                             && 17
TELEMPR = "                                             " && 45
LOCEMPR = "ROSARIO          "                             && 17
EMPIVAR = "RESPONSABLE INSCRIPTO   "                      && 24
EMPCATM = 0
ABRCLIM = "ALF"
CON_CTA = "CONCTA"
MAN_CTA = "MANCTA"
BANDERA = 0
LCO     = 4
DECLARE DIA[3],POR[3],FINA[8],DCUE[0],TCUE[0],TNOM[0]
DECLARE XCOD[0],XNOM[0],XPRE[0],XDE1[0],XDE2[0],LREN[0]
DECLARE XCAN[0],XIVA[0],XTOT[0],XREM[0],XLOC[0]

****/ PROTECCION DEL PROGRAMA *************************************************
IF .NOT. FILE("\DOS\LODTAK.COM")
   IF .NOT. FILE("F:\PUBLIC\LODTAK.COM")
       ERASE SISALF.EXE
       RETURN
   ENDIF
ENDIF

****/ SETEO DEL DIRECTORIO DE DATOS ******************************************
USE PATALF.DTO
PATDATOM = rtrim(PATDATO)
PATSISTM = rtrim(PATSIST)
PATTRANM = rtrim(PATTRAN)
PATRECIM = rtrim(PATRECI)
PATCIERM = rtrim(PATCIER)
PATDISKM = rtrim(PATDISK)
PATLISTM = rtrim(PATLIST)
COLSIS   = RTRIM(C_SIS)
COLCOL   = RTRIM(C_COL)
COLFIL   = RTRIM(C_FIL)
COLSEL   = RTRIM(C_SEL)
COLPMT   = "I,"+COLSEL
COLneg   = RTRIM(C_neg)
COLinv   = RTRIM(C_inv)
COLblb   = RTRIM(C_blb)
COLnul   = RTRIM(C_nul)
SET DEFAULT TO &PATDATOM
DO CASE
   CASE PATCIERM='XXXX'
     DISKM=''
   OTHERWISE
     DISKM=STRTRAN(PATDATOM,'DAT','TEM')
ENDCASE
DRIDIS=DISKM
DISKM='Z'


****/ SETEO DEL PROGRAMA*******************************************************
SET SCOREBOARD OFF
SET DELETE ON
SET DATE BRITISH
SET STATUS OFF
SET ESCAPE ON
SET WRAP ON
SET EXACT OFF
SET SOFTSEEK ON
READEXIT(.T.)
CLOSE ALL

TOPEC = 9
A  = 2
F  = 1
FA = 1
CBAS = 1
C  = 1
CA = 1
FAYU = STR(C,2)+STR(F,2)

****/ APERTURA DEL PUERTO DE IMPRESORA FISCAL ******************************************
nada = .f.
do while .not. nada
	*HASAR===============================================
    nada = PFA_AbrirPuerto()
    if .not. nada
        pfa_messagebox('','Error al abrir el puerto')
    else
        * Controla si hace falta realizar un Cierre Z
        * Si hace falta lo ejecuta
		*@@@@@@@@@@ VER COMO RESOLVER ESTA FUNCION
        *PFA_HayCierre()
*        PFA_messagebox('Sr. operador','Impresora Fiscal habilitada')
*        Este mensaje fue pasado a pfiscal.prg cuando abre satisfactoriamente el puerto. 16/09/07
    endif
enddo
* xvar1=PFA_Cript("Sr. operador","funcion")
* xvar2=PFA_Decript(xvar1,"funcion")
* PFA_messagebox('var encript: '+xvar1,'var desencrip: '+xvar2)


*nada = .f.
*do while nada = .f.
*    nada = PF_SINCRONIZAR()
*    if nada = .f.
*        pfa_MueMen('Error!','Error al chequear el estado de la impresora')
*    endif
*enddo
PFA_CtrSuc()
**********


DO BASALF  && bases de datos del sistema
DO BASCTA  && bases de datos de cuentas corriente
DO BASCON  && bases de datos de contabilidad
DECLARE OPEC[0],OPEN[0]
declare tcba[10],tnba[10],tpba[10],txba[10],tvba[10]
declare tnch[10],tfch[10],tplz[10],tfar[10]
declare tich[10],tinc[10],tgbc[10],tgac[10]
declare dfer[00],tfde[10],tcnr[10],tclb[10]
DO BASBAN  && bases de datos de bancos
DO CARFUN  && teclas de funcion
MODPER=0
DO PANPRI  && pantalla principal
INGPAS(0)  && ingreso de clave de acceso
IF BANDPAS=0
   RETURN
ENDIF
DECLARE PNOM[5],PNEG[5],PNOR[5],PINI[5],PSBR[5],PNSB[5],PCOD[5]
DECLARE PP05[5],PP06[5],PP10[5],PP12[5],PP16[5],PP20[5],PP24[5]
DECLARE PNGO[5],PCYA[5],PFUZ[5],PAZU[5],PAMA[5],PVER[5],PROJ[5]
DO PARVYC  && parametros generales del sistema
******************************************************************************
SETKEY(28,{|A|;
       Ayuda(A,," ","w+/rb",SETCOLOR(),.T.)})
SET KEY -1 TO MUESFUN
WHILE .T.
   TIEINI=SECONDS()
   WHILE .T.
      SET COLOR TO I
      @ 24,14 SAY TIME()
      SET COLOR TO
      OPCION=INKEY()
      PECO=0
      DO CASE
         CASE OPCION = 28
           AYUDA("SISALF",189,STR(PECO),"W+/G",SETCOLOR(),.T.)
         CASE OPCION = -1
           DO MUESFUN
         CASE OPCION = 19 .OR. OPCION = 4
           DO MENU01
         CASE OPCION = 5 .OR. OPCION = 24
           DO MENU02
         CASE OPCION = 13
           EXIT
	 CASE OPCION=49
	   STEC=0
	   DO TECLAS
	   IF STEC=5
	      PECO=1
	      EXIT
	   ENDIF
      ENDCASE
      LIMPANT(TIEMPOM)
   ENDDO
   MOTM=' '
   LART=6
   TPRINT=1
   IF PCOD[1]>=300
      TPRINT=0
   ENDIF
   **** BUSQUEDA DE PERMISO DE ACCESO ****************************************
   PERMI=0
   DO BUSPAS
   IF PERMI=1000
      LOOP
   ENDIF
   **** SETEO DE VARIABLES ***************************************************
   SAVE SCREEN TO PANT00
   SET COLOR TO &COLSIS
   @ 24,13 SAY "°°°°°°°°°°°"
   SET COLOR TO
   BANINV=0
   BANDTOT=0
   BAND5=0
   ARTM=' '
   FLEM=0
   COMM='XX'
   PANTBC = ""
   LIN =0
   LON =0
   FIL1=5
   FIL2=24
   FAYU=STR(C,2)+STR(F,2)
   COLV=80
   LINV=00
   IF C < 5
      COL = 25
      POS = 20
      POL = 0   && ventana a la izquierda
      COC = 0
   ELSE
      COL = 0
      POL = 1   && ventana a la derecha
      POS = 46
      COC = 55
   ENDIF
   RENCLI=NIL
   TITCLI=NIL
   LISTAD =0
   BANDL  =0
   NUM ="COD"
   BANDC  =1
   MANUAL =0
   BUSIND =0
   PINTA  =0
   BAND4  =2
   XCOL   =0
   BUSCLAVE="BUSQCN"
   MASTIT=NIL
   ***************************************************************************
   SELE 1
   LPTI=1
   TIPM=' '
   LCLA=19
   DO CASE
      CASE C = 1
        LDES=62
        SPA1=03
        SPA2=04
        MOBS=01
        DO ELEVEN
      CASE C = 2
        DO ELECOM
      CASE C = 3
        LDES=62
        SPA1=03
        SPA2=04
        DO CASE
	   CASE F=09
	     BAND5=25
             ULTAUXI  = "PROTAR"
             BASEM    = "TARJETAS"
             USE &BTAR INDEX &ICTAR,&INTAR SHARED
             MANTENIMIENTO("MANSIS","LIXCTA")
	   OTHERWISE
             DO ELECTA2
	ENDCASE
      CASE C = 4
        DO ELECTA2
      CASE C = 5
        DO ELECTA2
      CASE C = 6
****************************************************************************
*   ESTA LINEA FUE ELIMINADA PARA QUE ELESTO LLAME A ACPALF2 EN VEZ DE
*   ACPALF, QUE ES UN PROGRAMA MELLIZO QUE ACTUALIZA LA ADEMÁS DEL PRECIO
*   LA FECHA DE ACTUALIZACION Y EL COSTO PROMEDIO
*   ELESTO2 Y ACPALF2 ESTÁN EN PFISCAL.PRG
*       DO ELESTO
****************************************************************************

        DO ELESTO2
      CASE C = 7
        DO ELECON
      CASE C = 8
        LIMPIA=1
        DO ELEBAN
      CASE C = TOPEC
        DO CASE
	   CASE F=8
             USE &BCLA INDEX &ICCLA,&INCLA,&IXCLA SHARED
             MANTENIMIENTO("MANCLA","LISCLA")
	   CASE F=9
	     DO REGCLA
	   OTHERWISE
             DO ELEUTI
	ENDCASE
        IF BANDERA = 1000
           RETURN
        ENDIF
   ENDCASE
   ***************************************************************************
   CLOSE DATA
   SET COLOR TO
   RESTORE SCREEN FROM PANT00
   IF C=TOPEC.AND.F=3
      SET COLOR TO I
      @ 24,70 SAY DATE()
      SET COLOR TO
   ENDIF
ENDDO
#ifdef fiscal_log
  FCLOSE(Arc_Salida)
#endif


PROCEDURE TECLAS
DECLARE TTEC[5]
TTEC[1]=54
TTEC[2]=48
TTEC[3]=53
TTEC[4]=52
TTEC[5]=57
FOR P=1 TO 5
    INKEY(0)
    IF LASTKEY()#TTEC[P]
       EXIT
    ENDIF
    STEC+=1
NEXT
RETURN
