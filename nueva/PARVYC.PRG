****Y2K****
/**********************
* Surround Versioning
**********************
* $Author: admin$
* $Date: Sábado, 05 de Noviembre de 2005 7:26:49$
* $File: PARVYC.PRG$
* $Revision: 1.0$
* $Log$
* admin - Sábado, 05 de Noviembre de 2005 7:26:49
* Alfredo - sábado, 20 de diciembre de 2003 7:33:09
* 1) Modificaciones a mostrar moneda de artículos en la facturación
2) Agregue la eliminación de facturas de utilidad negativa
* Alfredo - sábado, 04 de octubre de 2003 10:10:53
* Corrección de comentarios en las leyendas de versionado
* Alfredo - sábado, 04 de octubre de 2003 9:48:40
* Modificaciónes:
1. Restricción de acceso por usuario a la estadistica de ventas, cierre de stock y listado comparativo de stock
* admin - sábado, 27 de septiembre de 2003 4:31:41
**********************/

PUBLIC BAUX
  BAUX="AUX"+abrclim
ABREDBF(BAUX,.F.,0)
****/ VENTAS       ***********************************************************
PUBLIC PROFAAM,PROFABM,PROREVM,PROPEVM,PRODEVM,PROPREM,PROFCRM,PRONCAM,PRONCBM
PUBLIC NROSUCM,PROFAVM,IMPFCRM,CDIS
profaam=profaa
profabm=profab
prorevm=prorev
propevm=propev
prodevm=prodev
proprem=propre
proncam=pronca  && próximo nro de N. credito A
proncbm=proncb  && próximo nro de N. credito A
nrosucm=nrosuc
profavm=profav
profcrm=profcr
impfcrm=impfcr
****/ COMPRAS      ***********************************************************
PUBLIC PROPECM,PRODECM,PROFACM,HOJLICM,MESIVAM,ANIIVAM,HOJLIVM
propecm=propec
prodecm=prodec
profacm=profac
hojlicm=hojlic
mesivam=mesiva
aniivam=aniiva
hojlivm=hojliv
****/ CLIENTES     ***********************************************************
PUBLIC ULTCLIM,ULTRUBM,ULTZONM,ULTVENM,IMPGASM
PUBLIC ULTRECM,COPRECM,NRORESM,UTIVENM,FECCTAM
ultclim=ultcli
ultrubm=ultrub
ultzonm=ultzon
ultvenm=ultven
ultrecm=ultrec
coprecm=coprec
nroresm=nrores
utivenm=utiven
fecctam=feccta
impgasm=impgas
****/ PROVEEDORES  ***********************************************************
PUBLIC ULTPROM,ULTRUPM,ULTOPAM,COPPAGM
ultprom=ultpro
ultrupm=ultrup
ultopam=ultopa
coppagm=coppag
****/ CONTABILIDAD ***********************************************************
PUBLIC PROASIM,PROSUBM,PRIBOR,PROFINM,CUEVALM,CUECOBM
proasim=proasi
prosubm=prosub
pribor=pribox
IF INFFINC='S'
   profinm=profin
   cuevalm=cueval
   cuecobm=cuecob
ENDIF
****/ BANCOS       ***********************************************************
PUBLIC ULTDEPM,COPDEPM,ULTBAPM,SALEFEM,IMPCHQM,LARPBAM
ultdepm=ultdep
copdepm=copdep
ultbapm=ultbap
salefem=salefe
impchqm=impchq
larpbam=larpba
****/ IMPUESTOS    ***********************************************************
PUBLIC TASIVAM,TASACRM,TASDIFM,TASPERM,DISDGIM
PUBLIC TASRIVM,MINRIVM,PRORIVM
PUBLIC TA1PIBM,TA2PIBM,MINPIBM
tasivam=tasiva
tasacrm=tasacr
tasdifm=tasdif
tasperm=tasper
lini=08
lriv=0
lpib=0
IF ARETIVA=1
   lini=05
   tasrivm=tasriv
   minrivm=minriv
   prorivm=proriv
   lpib=4
ENDIF
IF PERIBRM=1
   lini=05
   ta1pibm=ta1pib
   ta2pibm=ta2pib
   minpibm=minpib
ENDIF
****/ CONTROLES    **********************************************************ii
PUBLIC PRIRECM,PRIOPAM,PRIDEPM,PRIFAAM,PRIFABM
PUBLIC TIEMPOM,FORCONM,LARHOJM,COTDOLM
prirecm=prirec
priopam=priopa
pridepm=pridep
prifaam=prifaa
prifabm=prifab
tiempom=tiempo
forconm=forcon
larhojm=larhoj
cotdolm=cotdol
****/ ETIQUETAS    ***********************************************************
PUBLIC IMPETIM,ETICANM,ETIRENM,ETIANCM,ETIIZQM,ETIDERM,ETIINFM
impetim=impeti
eticanm=etican
etirenm=etiren
etiancm=etianc
etiizqm=etiizq
etiderm=etider
etiinfm=etiinf
DO PARPUB
DO PARPRI
IF C#TOPEC.OR.F#2
   RETURN
ENDIF
CLOSE DATA
ABREDBF(BAUX,.F.,0)

******************************************************************************
*SET COLOR TO
*@ 05,00 CLEAR TO 19,50
*SET COLOR TO I
*@ 05,00 TO 19,50
*@ 05,00 TO 18,15
*@ 05,15 SAY [Â]
*@ 18,00 say [Ã]

SET COLOR TO
@ 05,00 CLEAR TO 21,50
SET COLOR TO I
@ 05,00 TO 21,50
@ 05,00 TO 20,15
@ 05,15 SAY [Â]
@ 20,00 say [Ã]



WHILE .T.
   SET COLOR TO I,W+/R
   @ 06,01 PROMPT " VENTAS       "
   @ 07,01 PROMPT " COMPRAS      "
   @ 08,01 PROMPT " CLIENTES     "
   @ 09,01 PROMPT " PROVEEDORES  "
   @ 10,01 PROMPT " CONTABILIDAD "
   @ 11,01 PROMPT " BANCOS       "
   @ 12,01 PROMPT " IMPUESTOS    "
   @ 13,01 PROMPT " CONTROLES    "
   @ 14,01 PROMPT " PRODUCTOS    "
   @ 15,01 PROMPT " VARIOS       "
   @ 16,01 PROMPT " IMPRESORAS   "
   @ 17,01 PROMPT " ETIQUETAS    "
   @ 18,01 PROMPT " IMP. FISCAL  "
   @ 19,01 PROMPT " TALLER       "
   MENU TO AREA
   SET COLOR TO W+
   DO CASE
      CASE LASTKEY()=27
        CLOSE DATA
        RETURN
      OTHERWISE
        PARAMETRO="PAR_"+STRZERO(AREA,2)
        DO &PARAMETRO
   ENDCASE
   @ 06,17 CLEAR TO 20,49
ENDDO

PROCEDURE PAR_01
IF EMPCATM=0
   @ 06,17 SAY [Pr¢x N§ de FC (A)   :            ]
   @ 07,17 SAY [Impresora  FC (A)   :            ]
   @ 08,17 SAY [Pr¢x N§ de FC (B)   :            ]
   @ 09,17 SAY [Impresora  FC (B)   :            ]
   LI=0
ELSE
   @ 06,17 SAY [Pr¢x N§ de FC (C)   :            ]
   @ 07,17 SAY [Impresora  FC (C)   :            ]
   LI=2
ENDIF
@ 10-LI,17 SAY [Pr¢x N§ de Remitos  :            ]
@ 11-LI,17 SAY [Impresora de Remitos:            ]
@ 12-LI,17 SAY [Pr¢x N§ de Devoluc. :            ]
@ 13-LI,17 SAY [Pr¢x N§ de Pedido   :            ]
@ 14-LI,17 SAY [Pr¢x N§ de Presup.  :            ]
@ 15-LI,17 SAY [Impresora  Presup.  :            ]
@ 16-LI,17 SAY [N£mero de Sucursal  :            ]
@ 17-LI,17 SAY [Pr¢x N§ de N.Cre. A :            ]
@ 18-LI,17 SAY [Pr¢x N§ de N.Cre. B :            ]
SET COLOR TO
ANTPREM=PROPREM
@ 06,40 GET PROFAAM PICT '@Z 99999999'    VALID PROFAAM>0
@ 07,47 GET IMPFAAM PICT '@Z 9'           VALID IMPFAAM>0
IF EMPCATM=0
   @ 08,40 GET PROFABM PICT '@Z 99999999' VALID PROFABM>0
   @ 09,47 GET IMPFABM PICT '@Z 9'        VALID IMPFABM>0
ENDIF
@ 10-LI,40 GET PROREVM PICT "@Z 99999999" VALID PROREVM>0 WHEN SUPERVISOR=1
@ 11-LI,47 GET IMPREMM PICT '@Z 9'        VALID IMPREMM>0 WHEN SUPERVISOR=1
@ 12-LI,40 GET PRODEVM PICT "@Z 99999999" VALID PRODEVM>0 WHEN SUPERVISOR=1
@ 13-LI,40 GET PROPEVM PICT "@Z 99999999" VALID PROPEVM>0 WHEN SUPERVISOR=1
@ 14-LI,40 GET PROPREM PICT "@Z 99999999" VALID PROPREM>0 WHEN SUPERVISOR=1
@ 15-LI,47 GET IMPPRSM PICT "@Z 9"
@ 16-LI,44 GET NROSUCM PICT '9999'
@ 17-LI,40 GET PRONCAM PICT '@Z 99999999' VALID PRONCAM>0 WHEN SUPERVISOR=1
@ 18-LI,40 GET PRONCBM PICT '@Z 99999999' VALID PRONCBM>0 WHEN SUPERVISOR=1
READ
IF PROPREM=NROCAMB
   @ 14-LI,40 GET PROFAVM PICT "@Z 99999999" VALID PROFAVM>0
   READ
   PROPREM=ANTPREM
ENDIF
BLOQUEADBF(0)
REPL PROFAA WITH PROFAAM,PROFAB WITH PROFABM,PROREV WITH PROREVM
REPL PROPEV WITH PROPEVM,PRODEV WITH PRODEVM,PROPRE WITH PROPREM
REPL PROFCR WITH PROFCRM,IMPFCR WITH IMPFCRM,PROFAV WITH PROFAVM
REPL NROSUC WITH NROSUCM,PRONCA WITH PRONCAM,PRONCB WITH PRONCBM
IF FILE("C:\DOS\IMPRES.DBF")
   USE C:\DOS\IMPRES.DBF
ELSE
   USE A:\DOS\IMPRES.DBF
ENDIF
REPL IMPFAA WITH IMPFAAM,IMPFAB WITH IMPFABM
REPL IMPREM WITH IMPREMM,IMPPRS WITH IMPPRSM
ABREDBF(BAUX,.F.,0)
RETURN

PROCEDURE PAR_02
@ 08,17 SAY [Pr¢x N§ Ped. Compra :            ]
@ 09,17 SAY [Pr¢x N§ Rem. Devol. :            ]
@ 10,17 SAY [Mes de IVA. Compras :            ]
@ 11,17 SAY [A¤o de IVA. Compras :            ]
@ 12,17 SAY [N§Hoja IVA. Compras :            ]
@ 13,17 SAY [N§Hoja IVA. Ventas  :            ]
SET COLOR TO
ANTPECM=PROPECM
@ 08,40 GET PROPECM PICT "@Z 99999999" VALID PROPECM > 0
@ 09,40 GET PRODECM PICT "@Z 99999999" VALID PRODEVM > 0
@ 10,46 GET MESIVAM PICT "@Z 99"
@ 11,46 GET ANIIVAM PICT "@Z 99"
@ 12,44 GET HOJLICM PICT "@Z 9999" VALID HOJLICM > 0
@ 13,44 GET HOJLIVM PICT "@Z 9999" VALID HOJLIVM > 0
READ
IF PROPECM=NROCAMB
   @ 08,40 GET PROFACM PICT "@Z 99999999" VALID PROFACM>0
   READ
   PROPECM=ANTPECM
ENDIF
BLOQUEADBF(0)
REPL PROPEC WITH PROPECM,PRODEC WITH PRODECM,PROFAC WITH PROFACM
REPL MESIVA WITH MESIVAM,ANIIVA WITH ANIIVAM,HOJLIC WITH HOJLICM
REPL HOJLIV WITH HOJLIVM
RETURN

PROCEDURE PAR_03
@ 06,17 SAY [Pr¢x N§ de Cliente  :            ]
@ 07,17 SAY [Pr¢x N§ de Rubro    :            ]
@ 08,17 SAY [Pr¢x N§ de Zona     :            ]
@ 09,17 SAY [Pr¢x N§ de Vendedor :            ]
@ 10,17 SAY [Pr¢x N§ Rec. Cobros :            ]
@ 11,17 SAY [Cantidad de Copias  :            ]
@ 12,17 SAY [Pr¢x N§ de Resumen  :            ]
@ 13,17 SAY [Utilidad M¡n. Venta :            ]
@ 14,17 SAY [Fecha Inicial Busq. :            ]
@ 15,17 SAY [Importe x Gastos Adm:            ]
@ 16,17 SAY [Impresora de Recibos:            ]
SET COLOR TO
@ 06,44 GET ULTCLIM PICT "@Z 9999"  VALID ULTCLIM > 0
@ 07,46 GET ULTRUBM PICT "@Z 99"    VALID ULTRUBM > 0
@ 08,46 GET ULTZONM PICT "@Z 99"    VALID ULTZONM > 0
@ 09,46 GET ULTVENM PICT "@Z 99"    VALID ULTVENM > 0
@ 10,43 GET ULTRECM PICT "@Z 99999" VALID ULTRECM > 0
@ 11,47 GET COPRECM PICT "9"
@ 12,43 GET NRORESM PICT "@Z 99999" VALID NRORESM > 0
@ 13,42 GET UTIVENM PICT "@Z 999.99"  WHEN SUPERVISOR=1
READ
FECCTAM=GETFEC(FECCTAM,14,40)
@ 14,40 GET FECCTAM
@ 15,40 GET IMPGASM PICT "@Z 99999.99"
@ 16,47 GET IMPRECM PICT "@Z 9"
READ
BLOQUEADBF(0)
REPL ULTCLI WITH ULTCLIM,ULTRUB WITH ULTRUBM,ULTZON WITH ULTZONM
REPL ULTVEN WITH ULTVENM,ULTREC WITH ULTRECM,COPREC WITH COPRECM
REPL NRORES WITH NRORESM,UTIVEN WITH UTIVENM
REPL FECCTA WITH FECCTAM,IMPGAS WITH IMPGASM
IF FILE("C:\DOS\IMPRES.DBF")
   USE C:\DOS\IMPRES.DBF
ELSE
   USE A:\DOS\IMPRES.DBF
ENDIF
REPL IMPREC WITH IMPRECM
ABREDBF(BAUX,.F.,0)
RETURN

PROCEDURE PAR_04
@ 09,17 SAY [Pr¢x N§ Proveedor   :            ]
@ 10,17 SAY [Pr¢x N§ de Rubro    :            ]
@ 11,17 SAY [Prox N§ Orden Pago  :            ]
@ 12,17 SAY [Cantidad de Copias  :            ]
SET COLOR TO
@ 09,44 GET ULTPROM PICT "@Z 9999"  VALID ULTPROM > 0
@ 10,46 GET ULTRUPM PICT "@Z 99"    VALID ULTRUPM > 0
@ 11,43 GET ULTOPAM PICT "@Z 99999" VALID ULTOPAM > 0
@ 12,47 GET COPPAGM PICT "9"
READ
BLOQUEADBF(0)
REPL ULTPRO WITH ULTPROM,ULTRUP WITH ULTRUPM
REPL ULTOPA WITH ULTOPAM,COPPAG WITH COPPAGM
RETURN

PROCEDURE PAR_05
@ 09,17 SAY [Pr¢x N§ de Asiento   :           ]
@ 10,17 SAY [Pr¢x N§ de Subdiario :           ]
@ 11,17 SAY [Inicio del Borrador  :           ]
IF INFFINC='S'
   @ 12,17 SAY [Pr¢x N§ Asient.Financ:           ]
   @ 13,17 SAY [Cuenta de Cobranzas  :           ]
   @ 14,17 SAY [Cuenta de Valores    :           ]
ENDIF
SET COLOR TO
@ 09,43 GET PROASIM PICT "@Z 99999" VALID PROASIM > 0
@ 10,47 GET PROSUBM PICT "@Z 9"     VALID PROSUBM > 0
@ 11,40 GET PRIBOR
IF INFFINC='S'
   @ 12,43 GET PROFINM PICT "@Z 99999" VALID PROFINM > 0
   @ 13,42 GET CUECOBM PICT "@Z 999999" VALID CUECOBM > 0
   @ 14,42 GET CUEVALM PICT "@Z 999999" VALID CUEVALM > 0
ENDIF
READ
BLOQUEADBF(0)
REPL PROASI WITH PROASIM,PROSUB WITH PROSUBM,PRIBOX WITH PRIBOR
IF INFFINC='S'
   REPL PROFIN WITH PROFINM,CUECOB WITH CUECOBM,CUEVAL WITH CUEVALM
ENDIF
RETURN

PROCEDURE PAR_06
@ 08,17 SAY [Pr¢ximo N§ Deposito :           ]
@ 09,17 SAY [Cantidad de Copias  :           ]
@ 10,17 SAY [Pr¢x C¢digo de Banco:           ]
@ 11,17 SAY [Saldo en Efectivo   :           ]
@ 12,17 SAY [Impresora Cheques   :           ]
@ 13,17 SAY [Largo Hoja Planilla :           ]
SET COLOR TO
@ 08,43 GET ULTDEPM PICT "@Z 99999" VALID ULTDEPM > 0
@ 09,47 GET COPDEPM PICT "9"
@ 10,46 GET ULTBAPM PICT "@Z 99"    VALID ULTBAPM > 0
@ 11,39 GET SALEFEM PICT "@Z 999999.99"
@ 12,47 GET IMPCHQM PICT "9"
@ 13,45 GET LARPBAM PICT "999" VALID LARPBAM>0
READ
BLOQUEADBF(0)
REPL ULTDEP WITH ULTDEPM,COPDEP WITH COPDEPM
REPL ULTBAP WITH ULTBAPM,SALEFE WITH SALEFEM
REPL IMPCHQ WITH IMPCHQM,LARPBA WITH LARPBAM
RETURN

PROCEDURE PAR_07
@ LINI+1,17 SAY [Tasa Resp.Insc. IVA:           % ]
@ LINI+2,17 SAY [Tasa Acrec. RNI IVA:           % ]
@ LINI+3,17 SAY [Tasa Diferenc.  IVA:           % ]
@ LINI+4,17 SAY [Tasa Percepci¢n IVA:           % ]
IF ARETIVA=1
   @ 12+LRIV,17 SAY [Tasa Retenci¢n  IVA:           % ]
   @ 13+LRIV,17 SAY [Importe M¡nimo     :             ]
   @ 14+LRIV,17 SAY [Pr¢ximo Nro. Ret.  :             ]
ENDIF
IF PERIBRM=1
   @ 12+LPIB,17 SAY [Tasa Perc.I.B. Mul.:           % ]
   @ 13+LPIB,17 SAY [Tasa Perc.I.B. Loc.:           % ]
   @ 14+LPIB,17 SAY [Importe M¡nimo     :             ]
ENDIF
SET COLOR TO
@ LINI+1,42 GET TASIVAM PICT "@Z 99.99" VALID TASIVAM > 0
@ LINI+2,42 GET TASACRM PICT "@Z 99.99" VALID TASACRM > 0
@ LINI+3,42 GET TASDIFM PICT "@Z 99.99" VALID TASDIFM > 0
@ LINI+4,42 GET TASPERM PICT "@Z 99.99"
IF ARETIVA=1
   @ 12+LRIV,42 GET TASRIVM PICT "@Z 99.99"    VALID TASRIVM>0
   @ 13+LRIV,39 GET MINRIVM PICT "@Z 99999.99" VALID MINRIVM>0
   @ 14+LRIV,42 GET PRORIVM PICT "@Z 99999"    VALID PRORIVM>0
ENDIF
IF PERIBRM=1
   @ 12+LPIB,42 GET TA1PIBM PICT "@Z 99.99"    VALID TA1PIBM>0
   @ 13+LPIB,42 GET TA2PIBM PICT "@Z 99.99"    VALID TA2PIBM>0
   @ 14+LPIB,39 GET MINPIBM PICT "@Z 99999.99" VALID MINPIBM>0
ENDIF
READ
BLOQUEADBF(0)
REPL TASIVA WITH TASIVAM,TASACR WITH TASACRM
REPL TASDIF WITH TASDIFM,TASPER WITH TASPERM
IF ARETIVA=1
   REPL TASRIV WITH TASRIVM,MINRIV WITH MINRIVM,PRORIV WITH PRORIVM
ENDIF
IF PERIBRM=1
   REPL TA1PIB WITH TA1PIBM,TA2PIB WITH TA2PIBM,MINPIB WITH MINPIBM
ENDIF
RETURN

PROCEDURE PAR_08
@ 07,17 SAY [Minutos de Espera   :            ]
@ 08,17 SAY [1 Dollar equivale a :          $ ]
@ 09,17 SAY [Largo de Hoja Imp.  :            ]
@ 10,17 SAY [Formulario Continuo :            ]
@ 11,17 SAY [1er. N§ de Recibo   :            ]
@ 12,17 SAY [1er. N§ de O.de Pago:            ]
@ 13,17 SAY [1er. N§ de Deposito :            ]
IF EMPCATM=0
   @ 14,17 SAY [1er. N§ de FC (A)   :            ]
   @ 15,17 SAY [1er. N§ de FC (B)   :            ]
ELSE
   @ 14,17 SAY [1er. N§ de FC (C)   :            ]
ENDIF
SET COLOR TO
@ 07,46 GET TIEMPOM PICT "@Z 99" VALID TIEMPOM > 0
@ 08,40 GET COTDOLM PICT "@Z 99999.99" VALID COTDOLM > 0
@ 09,45 GET LARHOJM PICT "@Z 999" VALID LARHOJM > 0
@ 10,47 GET FORCONM PICT "@!" VALID (FORCONM$"SN")
@ 11,43 GET PRIRECM PICT "@Z 99999" VALID PRIRECM > 0
@ 12,43 GET PRIOPAM PICT "@Z 99999" VALID PRIOPAM > 0
@ 13,43 GET PRIDEPM PICT "@Z 99999" VALID PRIDEPM > 0
@ 14,40 GET PRIFAAM PICT "@Z 99999999" VALID PRIFAAM > 0
IF EMPCATM=0
   @ 15,40 GET PRIFABM PICT "@Z 99999999" VALID PRIFABM > 0
ENDIF
READ
BLOQUEADBF(0)
REPL COTDOL WITH COTDOLM,TIEMPO WITH TIEMPOM
REPL LARHOJ WITH LARHOJM,FORCON WITH FORCONM
REPL PRIREC WITH PRIRECM,PRIOPA WITH PRIOPAM
REPL PRIDEP WITH PRIDEPM,PRIFAA WITH PRIFAAM
REPL PRIFAB WITH PRIFABM
RETURN

PROCEDURE PAR_12
@ 08,17 SAY [Impresora de Etiq.  :            ]
@ 09,17 SAY [Cant. de Etiquetas  :            ]
@ 10,17 SAY [Cant. de Renglones  :            ]
@ 11,17 SAY [Ancho del Rengl¢n   :            ]
@ 12,17 SAY [Margen Izquierdo    :            ]
@ 13,17 SAY [Margen Derecho      :            ]
@ 14,17 SAY [Margen Inferior     :            ]
SET COLOR TO
@ 08,47 GET IMPETIM PICT '@Z 9'   VALID IMPETIM>0
@ 09,45 GET ETICANM PICT '@Z 999' VALID ETICANM>0
@ 10,45 GET ETIRENM PICT '@Z 999' VALID ETIRENM>0
@ 11,45 GET ETIANCM PICT '@Z 999' VALID ETIANCM>0
@ 12,45 GET ETIIZQM PICT '@Z 999'
@ 13,45 GET ETIDERM PICT '@Z 999'
@ 14,45 GET ETIINFM PICT '@Z 999'
READ
BLOQUEADBF(0)
REPL IMPETI WITH IMPETIM
REPL ETICAN WITH ETICANM,ETIREN WITH ETIRENM,ETIIZQ WITH ETIIZQM
REPL ETIANC WITH ETIANCM,ETIDER WITH ETIDERM,ETIINF WITH ETIINFM
RETURN

PROCEDURE PAR_13
LOCAL OP
WHILE .T.
   SET COLOR TO I,W+/R
   @ 06,17 PROMPT " CIERRE X                 "
   @ 07,17 PROMPT " CIERRE Z                 "
   @ 08,17 PROMPT " CANCELAR CBTE. FISCAL    "
   @ 09,17 PROMPT " CANCELAR CBTE. NO FISCAL "
   @ 10,17 PROMPT " ESTADO IMPRESORA         "
   @ 11,17 PROMPT " ESTADO FISCAL            "
   @ 12,17 PROMPT " PRUEBA                   "
   @ 13,17 PROMPT " ACT. PRECIOS x CD        "
   MENU TO OP
   SET COLOR TO W+
   DO CASE
      CASE LASTKEY()=27
        RETURN
      CASE OP = 1
        DO PFA_CIEX
      CASE OP = 2
        DO PFA_CIEZ
      CASE OP = 3
        DO PFA_CanCbte
      CASE OP = 4
        DO PFA_CanCNF
      CASE OP = 5
        DO PFA_Est1
      CASE OP = 6
        DO PFA_Est2
*      CASE OP = 7
*        DO PFA_NCR
*			CDIS=0
*		 	DO RECSTO
      CASE OP = 8
         DO PFA_ActPre
   ENDCASE
ENDDO
RETURN

PROCEDURE PAR_14
LOCAL OP
WHILE .T.
   SET COLOR TO I,W+/R
   @ 06,17 PROMPT "  ACTUALIZAR ART TALLER      "
   @ 07,17 PROMPT "  LISTADO DE COMPROBANTES    "
   @ 08,17 PROMPT "  ESTADISTICA DE VENTAS      "
   @ 09,17 PROMPT "  GENERAR CIERRE STOCK       "
   @ 10,17 PROMPT "  EMITIR COMPARATIVO DE STOCK"
   @ 11,17 PROMPT "  EMITIR STOCK VALORIZADO    "
   @ 12,17 PROMPT "  CARGA DE INVENTARIO        "
   @ 13,17 PROMPT "  RESTRICCIONES DE ACCESO    "
   MENU TO OP
   SET COLOR TO W+
   DO CASE
      CASE LASTKEY()=27
        RETURN
      CASE OP = 1
      	DO PFA_TALLER
      CASE OP = 2
        DO PFA_IMPTALLER
      CASE OP = 3
        **** BUSQUEDA DE PERMISO DE ACCESO ****************************************
		* Pone c y F en valores que son comparados en buspas con un registro en la
		* tabla accalf.dbf. Los PFA_C y PFA_A son para reservar los valores de C y F
		* y luego restaurarlos despues de ejecutar buspas. Aparentemente C y F son usados
        * para indicar el nivel de menu en el que estoy parado
		PFA_C = C
		PFA_F = F
		C = 6
		F = 8
        PERMI=0
        DO BUSPAS
		C = PFA_C
		F = PFA_F 
        IF PERMI=1000
           LOOP
        ENDIF
        DO PFA_IMPESTAD
      CASE OP = 4
        **** BUSQUEDA DE PERMISO DE ACCESO ****************************************
		PFA_C = C
		PFA_F = F
		C = 6
		F = 8
        PERMI=0
        DO BUSPAS
		C = PFA_C
		F = PFA_F 
        IF PERMI=1000
           LOOP
        ENDIF
        DO PFA_GENCIERRE
      CASE OP = 5
        **** BUSQUEDA DE PERMISO DE ACCESO ****************************************
		PFA_C = C
		PFA_F = F
		C = 6
		F = 8
        PERMI=0
        DO BUSPAS
		C = PFA_C
		F = PFA_F 
        IF PERMI=1000
           LOOP
        ENDIF
        DO PFA_ImpComparativo	
      CASE OP = 6
        **** BUSQUEDA DE PERMISO DE ACCESO ****************************************
		PFA_C = C
		PFA_F = F
		C = 6
		F = 8
        PERMI=0
        DO BUSPAS
		C = PFA_C
		F = PFA_F 
        IF PERMI=1000
           LOOP
        ENDIF
        DO PFA_UtiNeg	
      CASE OP = 7
        **** BUSQUEDA DE PERMISO DE ACCESO ****************************************
		PFA_C = C
		PFA_F = F
		C = 6
		F = 8
        PERMI=0
        DO BUSPAS
		C = PFA_C
		F = PFA_F 
        IF PERMI=1000
           LOOP
        ENDIF
        DO STKALFA	
      CASE OP = 8
        **** BUSQUEDA DE PERMISO DE ACCESO ****************************************
		PFA_C = C
		PFA_F = F
		C = 6
		F = 8
        PERMI=0
        DO BUSPAS
		C = PFA_C
		F = PFA_F 
        IF PERMI=1000 .OR. PFA_Decript(palcla,"funcion")=PF_CLABAL
           LOOP
        ENDIF
        DO PFA_ACTCLAVE	
   ENDCASE
ENDDO
RETURN
