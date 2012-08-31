****Y2K****

PROCEDURE PARPUB
****/ PRODUCTOS    ***********************************************************
PUBLIC PROSTOM,PROCEDM,PROTIPM,FECSTOM
prostom=prosto
procedm=procex
protipm=protip
fecstom=fecsto
****/ VARIOS       ***********************************************************
PUBLIC MODDTOM,PROTARM,COTDOLM,TASAUMM
PUBLIC FINVTOM,FINVT2M,FINVT3M,FINDV2M,FINDV3M
moddtom=moddto
protarm=protar
cotdolm=cotdol
tasaumm=tasaum
finvtom=finvto
finvt2m=finvt2
finvt3m=finvt3
findv2m=findv2
findv3m=findv3
RETURN

PROCEDURE PAR_09
@ 10,17 SAY [Pr¢x N§ de Producto  :           ]
@ 11,17 SAY [Pr¢x N§ Procedencia  :           ]
@ 12,17 SAY [Pr¢x N§ Tipo Produc. :           ]
@ 13,17 SAY [Ultimo Cierre Stock  :           ]
SET COLOR TO
@ 10,42 GET PROSTOM PICT "@Z 999999" VALID PROSTOM>0
@ 11,46 GET PROCEDM PICT "@Z 99"    VALID PROCEDM>0
@ 12,46 GET PROTIPM PICT "@Z 99"    VALID PROTIPM>0
READ
FECSTOM=GETFEC(FECSTOM,13,40)
BLOQUEADBF(0)
REPL PROSTO WITH PROSTOM,PROCEX WITH PROCEDM,PROTIP WITH PROTIPM
REPL PROTIP WITH PROTIPM,FECSTO WITH FECSTOM
RETURN

PROCEDURE PAR_10
@ 08,17 SAY [Modifica Precios     :           ]
@ 09,17 SAY [Pr¢ximo N§ Tarjeta   :           ]
@ 10,17 SAY [Cotizaci¢n del Dollar:           ]
@ 12,17 SAY [Aumento Cta. Cte.    :           ]
@ 13,17 SAY [D¡as Primer  Plazo   :           ]
@ 14,17 SAY [D¡as Segundo Plazo   :           ]
@ 15,17 SAY [Dcto Segundo Plazo   :           ]
@ 16,17 SAY [D¡as Tercer  Plazo   :           ]
@ 17,17 SAY [Dcto Tercer  Plazo   :           ]
SET COLOR TO
@ 08,47 GET MODDTOM PICT "@!"
@ 09,46 GET PROTARM PICT '@Z 99' VALID PROTARM>0
@ 10,40 GET COTDOLM PICT '@Z 99999.99' VALID COTDOLM>0
@ 12,43 GET TASAUMM PICT '@Z 99.99'
@ 13,45 GET FINVTOM PICT '@Z 999'
@ 14,45 GET FINVT2M PICT '@Z 999'
@ 15,43 GET FINDV2M PICT '@Z 99.99'
@ 16,45 GET FINVT3M PICT '@Z 999'
@ 17,43 GET FINDV3M PICT '@Z 99.99'
READ
BLOQUEADBF(0)
REPL MODDTO WITH MODDTOM,PROTAR WITH PROTARM
REPL COTDOL WITH COTDOLM,TASAUM WITH TASAUMM
REPL FINVTO WITH FINVTOM
REPL FINVT2 WITH FINVT2M,FINDV2 WITH FINDV2M
REPL FINVT3 WITH FINVT3M,FINDV3 WITH FINDV3M
RETURN

