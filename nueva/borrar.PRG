*****************************************************
* Empresa : ALas Software
* Autor   : A.U.S. Alfredo Lascano
* Fecha   : 01/07/2001
*****************************************************
*                                                   *
* Modulo de impresi¢n de comprobantes fiscales      *
*                                                   *
*****************************************************
*Funciones:
*   Comprobantes no fiscales
*   function PF_PuertoInit ( PuertoNro, PuertoIO, PuertoIRQ )
*   function PF_AgregaCampoSalida ( dato )
*   function PF_ControlaCRC ( )
*   function PF_CRC ( entrada )
*   function PF_Sincronizar
*   function PF_EnviarComando( Comando )
*   function PF_ComandoOK(Comando )
*   function PF_MensajeEstado( opcion )
*   function PF_EnviaString( Comando )
*   function PF_InicioPaquete
*   function PF_FinPaquete
*   function PF_NroPaquete ( opcion )
*   Procedure PF_LimpiarBufferCom

*   function PFA_ImpFiscal	 Imprime todos los comprobantes fiscales
*   function PFA_ImpFactura	 Imprime una factura (llamada por PF_ImpFiscal)
*   function PFA_ImpNCredito Imprime una Notas de crédito como comprobante no fiscal (llamada por PF_ImpFiscal)
*   function PFA_ImpNCFiscal Imprime una Notas de crédito como comprobante fiscal (llamada por PF_ImpFiscal)
*   function PFA_CerrDB 	 Cierra las bases abiertas
*   function PFA_Limpiar 	 Quita los caracteres no imprimibles
*   FUNCTION PFA_ImpCheques  Imprime los cheques de un pago
*   function PFA_arregla_cuit Quita todos los caracteres no numericos
*   function PFA_AbreNF()	  Abre comprobante no fiscal
*   function PFA_LineaNF	  Imprime una linea no fiscal
*   function PFA_CerrarNF()	  Cierra un comprobante no fiscal
*   function PFA_MueMen       Muestra el mensaje y el estado de la la impresora y el estado fiscal
*   function PFA_messagebox	  Muestra un mensaje de dos lineas
*   function PFA_questionbox  Muestra un mensaje de dos lineas y pide una respuesta (si/no)
*   function PFA_AbrirPuerto  Inicializa y abre el puerto serial
*   function PFA_CtrNum()		Controla la numeración con la numeración de la impresora
*   function PFA_CtrEst()		CONTROL DE ESTADO NORMAL DE FUNCIONAMIENTO de la impresora
*   FUNCTION PFA_HayCierre()	Controla si es necesario realizar un cierre fiscal
*   FUNCTION PFA_BusNBlock(P_Desc) Busca el número de bloque
*	procedure PFA_mostrar_datos
*   PROCEDURE PFA_CanCbte		Cancela un comprobante fiscal
*   PROCEDURE PFA_CanCNF		Cancela un comprobante no fiscal
*   PROCEDURE PFA_CIEX				realiza un cierre X
*   PROCEDURE PFA_CIEZ				realiza un cierre Z
*   PROCEDURE PFA_CanCbte           cancelar un comprobante
*   PROCEDURE PFA_Est1              muestra el estado de la impresora
*   PROCEDURE PFA_Est2              muestra el estado fiscal

#define PF_DEBUG
#define PF_SINFISCAL
*#undef PF_SINFISCAL
#define fiscal_log
#undef PF_DEBUG

#include "pfiscal.ch"
#include "Fileio.ch"

*--------------------------------
function PFA_ImpFiscal(P_Cliente,P_TipoCbte,P_NroCbte,P_Sucursal)
*--------------------------------
* Codigo 100
* Imprime un comprobante fiscal
*PFA_MESSAGEBOX('MOTM',MOTM)

*************************************************
#ifdef PF_SINFISCAL
   RETURN .T.
#endif
************************************************
IF MARC='X'
	Return .T.
ENDIF

error = .f.

*PFA_MESSAGEBOX(STR(PFA_TIEMPOMAXIMO),'')

if P_TipoCbte = 'FC'
   error = PFA_ImpFactura(P_Cliente,P_TipoCbte,P_NroCbte,P_Sucursal)
else
   if P_TipoCbte = 'ND'
       error = PFA_ImpFactura(P_Cliente,P_TipoCbte,P_NroCbte,P_Sucursal)
   else
       if P_TipoCbte = 'NC'
*           error = PFA_ImpNCredito(P_Cliente,P_TipoCbte,P_NroCbte,P_Sucursal)

           error = PFA_ImpNCFiscal(P_Cliente,P_TipoCbte,P_NroCbte,P_Sucursal)
       else
           error = PFA_Messagebox('No es un comprobante fiscal',P_TipoCbte,P_Sucursal)
       endif
   endif
endif
Return error

*--------------------------------
function PFA_ImpFactura(P_Cliente,P_TipoCbte,P_NroCbte,P_Sucursal)
*--------------------------------
* Imprime una factura
local fcodigo,xarea,k,xtarjeta,MRemito1,MRemito2, xMbancos[0]
xtarjeta=''
xarea=SELECT()
fcodigo='110'
MBase_Total = ' '     && indica precio base (B) o precio total (T)
MNroBlock = ' '
*if .not. PF_Sincronizar( )
*    do pfa_mostrar_datos
*    PFA_MueMen(' Error: 3 en control estado de impresora',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
*    Return .F.
*endif
****************************
* Las observaciones se cargan en Mremito1 y Mremito2
* Si se paga con tarjeta de crédito el detalle de la tarjeta y cuotas
*	se imprime en Mdomici3
* Las leyendas de forma de pago y equivalencia a dolar van en el pie 11 a 14
*

* limpia las lineas de header
for k = 2 to 5
      continuar=SeteoHeader( k ,PF_DEL)
next

***** AUXIL  contiene el detalle de las formas de pago
SELECT 50
USE AUXIL
k=1
GO TOP
* saltea el primero porque solo hay lugar para cuatro lineas
skip

DO WHILE .NOT. EOF() .and. k <= 4
    tira = auxil->linea
    if len(alltrim(tira)) > 0
        ** setea el header 11, 12, 13, 14
        continuar=SeteoHeader( k+10 ,tira)
        k = k + 1
    endif
    skip
enddo
* Limpia el resto de las 4 lineas
i = 0
w = 0
if k < 5
   ik = k + 10
   do while ik <= 14
        continuar = SeteoHeader( ik ,' ' )
        ik = ik + 1
   enddo
endif

***** OBSER contiene el detalle de las observaciones
USE obser
GO TOP
k=1
MRemito1 = PF_DEL
MRemito2 = PF_DEL
DO WHILE .NOT. EOF() .and. k<=2
    tira = left(alltrim(obser->linea),40)
	if k=1
		MRemito1 = tira
	else
		if k=2
			MRemito2 = tira
		end if
	end if
    k=k+1
    skip
enddo

if len(alltrim(MRemito1)) > 0 .and. Mremito1 <> PF_DEL
      if .not. SeteoHeader( 4 ,MRemito1 )
         do pfa_mostrar_datos
         PFA_MueMen(' Error: 1 Al enviar Linea 1 de observaciones',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
         Return
      endif
endif
if len(alltrim(Mremito2)) > 0 .and. Mremito2 <> PF_DEL
      if .not. SeteoHeader( 5 ,MRemito2 )
         do pfa_mostrar_datos
         PFA_MueMen(' Error: 2 Al enviar Linea 2 de observaciones',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
         Return
      endif
endif
MRemito1 = PF_DEL
MRemito2 = PF_DEL


**************TARJETA
* si se emitio con tarjeta imprime los datos de la tarjeta y si hay lugar para imprimir
xtarjeta = PF_DEL
use ncredito
go top
do while .not. eof()
	if ncredito->cod='CAB'
		xpos =AT('TARJETA DE CREDITO',ncredito->linea)
		if xpos > 0
			tira=left(substr(ncredito->linea,xpos),44)
			xtarjeta = tira
	        k=k+1
	        exit
		endif
	endif
	skip
enddo
if xtarjeta <> PF_DEL
      do PF_IniciarSalida
      Continuar = PF_AgregaCampoSalida("3")
      Continuar = PF_AgregaCampoSalida(xtarjeta)
      if .not. PF_EnviarComando( PH_PoneEncabe )
         do pfa_mostrar_datos
         PFA_MueMen(' Error: 3 Al enviar Linea 3 de Tarjeta Cr.',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
         Return
      endif
end if

***** PARAMETROS
sele 50
use param alias param
*Mtip_cbte = param->tip_cbte
if P_TipoCbte = 'FC'
    Mtip_cbte = 'F'
else
    Mtip_cbte = 'D'
endif

Mformulario = param->formulario
Mcopias = param->copias
Mpreimpreso = param->preimpreso
Mcpi = param->cpi
Miva_vende = param->iva_vende
Mbienuso = param->bienuso
Mtas_iva=param->tas_iva
Mtip_monto=param->tip_monto
Mbultos=param->bultos
Mtas_impint=param->tas_impint
*Mtas_ivani=param->tas_ivani
Mmfi_impint=param->mfi_impint

Mtip_cbte_HAS = ''

***** COMPROBANTES
IF VTOM#0
   Mformapago = 'CUENTA CORRIENTE'
ELSE
   Mformapago = 'CONTADO'
ENDIF

*Mimporte=cbte->imp
Mimporte=impm
Mimporte=Mimporte * 100
Mimporte2=alltrim(str(Mimporte,8))

*****CLIENTES
sele 52
use clialf index cclialf alias cliente shared
*locate for cod=P_Cliente
seek P_Cliente
IF .not. found()
   PFA_Messagebox(fcodigo+' Error: 4 ','Cliente no encontrado: '+P_tipoCbte+str(P_nrocbte))
   Return .f.
endif
Mnom_comprador = alltrim(cliente->nom) + ' ('+alltrim(str(cod))+')'
Mnro_doc = PFA_arregla_cuit(cliente->cui)

do case
case cliente->iva = 0
    Mletra_cbte = 'A'
    Miva_comprador='I'
    Mtip_doc = "C"
    Mtas_ivani = "0"
    if Mtip_cbte = 'F'
		Mtip_cbte_HAS = 'A'
	else
		Mtip_cbte_HAS = 'D'
	endif
	MBase_Total = 'B'
case cliente->iva = 1
    Mletra_cbte = 'A'
    Miva_comprador='N'
    Mtip_doc = "C"
    Mtas_ivani=param->tas_ivani
    if Mtip_cbte = 'F'
		Mtip_cbte_HAS = 'A'
	else
		Mtip_cbte_HAS = 'D'
	endif
	MBase_Total = 'B'
case cliente->iva = 2
    Mletra_cbte = 'B'
    Miva_comprador='C'
	do case
	case len(alltrim(MNro_doc)) = 11
	    Mtip_doc = "C"
	case len(alltrim(MNro_doc)) > 0
	    Mtip_doc = "2"
	otherwise
	    Mtip_doc = PF_DEL
	    MNro_doc = PF_DEL
	endcase
    Mtas_ivani = "0"
    if Mtip_cbte = 'F'
		Mtip_cbte_HAS = 'B'
	else
		Mtip_cbte_HAS = 'E'
	endif
	MBase_Total = 'T'
case cliente->iva = 3
    Mletra_cbte = 'B'
    Miva_comprador='E'
    Mtip_doc = "C"
    Mtas_ivani = "0"
    if Mtip_cbte = 'F'
		Mtip_cbte_HAS = 'B'
	else
		Mtip_cbte_HAS = 'E'
	endif
	MBase_Total = 'T'
end case


Mcodprovin = cliente->prv

***** PROVINCIAS
sele 53
use prvalf index cprvalf alias provin shared
*locate for cod=Mcodprovin
SEEK Mcodprovin
IF .not. found()
   PFA_Messagebox(fcodigo+' Error: 5 ','Provincia no encontrada: '+P_tipoCbte+str(P_nrocbte))
   Return .f.
endif
Mnomprovin=alltrim(provin->nom)
Mdomici1 = left(alltrim(cliente->dom),43)
Mdomici2 = left(alltrim('(' + str(cliente->cpo) + ') '+ ;
           alltrim(cliente->loc) + ' ' +Mnomprovin),43)
Mdomici3 = PF_DEL

* HASAR envía datos de clientes
do PF_IniciarSalida
Continuar = PF_AgregaCampoSalida(MNom_Comprador)
Continuar = PF_AgregaCampoSalida(MNro_doc)
Continuar = PF_AgregaCampoSalida(Miva_comprador)
Continuar = PF_AgregaCampoSalida(MTip_doc)
Continuar = PF_AgregaCampoSalida(left(alltrim(Mdomici1)+ ' ' +alltrim(Mdomici2),50))
if .not. PF_EnviarComando( PH_DatosCli )
   do pfa_mostrar_datos
   PFA_MueMen(' Error: 6 Al enviar Datos de cliente',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return
endif


* HASAR Abre el comprobante fiscal
do PF_IniciarSalida
nada = PF_AgregaCampoSalida( Mtip_cbte_HAS )          && Tipo de comprobante
* 								Factura A (A), Factura B/C (B), Recibo A (a), Recibo B/C (b)
*						 		Nota de Débito A (D)  o Nota de Débito B/C (E)
nada = PF_AgregaCampoSalida( "T" )                  && Tiquet/Factura o SLip
if .not. PF_EnviarComando( PH_FCAbre )
   Do PFA_mostrar_datos
   PFA_MueMen('Error: 7 en apertura de cbte',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return .f.
endif

*****ITEMS
sele 54
use stoalf index cstoalf alias productos shared
sele 55

if P_TipoCbte = 'FC'
    use favalf index cfavalf alias items shared
else
    use notalf index nnotalf alias items shared
endif

*locate for cod=P_Cliente .and. com=P_TipoCbte .and. nro=P_NroCbte
xnroitem=0

if P_TipoCbte = 'FC'
    xclave=str(P_cliente,4)+str(P_NroCbte,8)
else
    xclave=str(P_cliente)+str(P_NroCbte)
endif

*PFA_MESSAGEBOX('va a hacer el seek','asdasd')
SEEK xclave
es_nd_cheque = .f.

*--------------------------------------
* NOTA DE DEBITO POR CHEQUE DEVUELTO
IF (items->cod<>P_Cliente .OR. items->nro<>P_NroCbte ).and. P_TipoCbte='ND'
*	PFA_MESSAGEBOX('son distintos',str(P_cliente,4)+str(P_NroCbte,8)+'=='+str(items->cod,4)+str(items->nro,8))

   SELE 3

   USE &BBAN INDEX &ICBAN alias bancos SHARED
   go top
   do while .not. eof()
		xtira=bancos->cod+bancos->nom
		AADD(xMbancos,xtira)
		skip
   	enddo
   	use

   	SELE 56
   	USE &BLBA INDEX &ICLBA alias locali SHARED
	select 55
   	use chralf index nchralf alias items
   	xclave=str(P_cliente)+str(P_NroCbte)
   	SEEK xclave
   	es_nd_cheque = .t.
endif
*
*--------------------------------------

do while .not. eof()
      xnroitem = xnroitem + 1

      MdescAdi1 = PF_DEL
      MdescAdi2 = PF_DEL
      if P_TipoCbte = 'FC'
          Mcod_prod=items->art
          select productos
          seek Mcod_prod
          if .not. found()
            PFA_Messagebox('Error: 8 ','Articulo no encontrado: '+items->arp)
            Return .f.
          endif
*          mcodprod = left(alltrim(productos->art),6)
          mcodprod = alltrim(productos->art)
          *Mdescripcion=strzero(xnroitem,2)+' '+padl(mcodprod,6)+' '+alltrim(productos->nom)
          *Mdescripcion=padl(mcodprod,6)+' '+alltrim(productos->nom)
          Mdescripcion=mcodprod+' '+alltrim(productos->nom)

*          if len(alltrim(productos->art)) > 6
*               MdescAdi1 = alltrim(productos->art)
*          else
*               MdescAdi1 = PF_DEL
*          endif

          mcanti=items->can
          * mcanti=mcanti*1000
          mcanti2=alltrim(str(mcanti))
		  * si tiene recargo por tarjeta de credito
		  recargo1 = 0
		  recargo2 = 0
		  IF items->de1 > 0
		  	  recargo1 = ROUND((items->pre * items->de1 / 100 ),2)
		  ENDIF
		  IF items->de2 > 0
	          recargo2 = ROUND((items->pre * items->de2 / 100 ),2)
		  ENDIF
		  Mprecio = int((items->pre + recargo1 + recargo2) * 100 ) / 100

*		  IF items->de2 > 0
*	          Mprecio=int((items->pre + ROUND((items->pre * items->de2 / 100 ),2))* 100)/100
*		  ELSE
*	          Mprecio=int(items->pre * 100)/100
*		  ENDIF
*         Mprecio=Mprecio*100

          Mprecio2=alltrim(str(Mprecio))
      else
		  if es_nd_cheque = .f.
	          Mdescripcion=alltrim(items->des)
    	      Mprecio=int(items->imp * 100)/100
        	  *Mprecio=Mprecio*100
	          Mprecio2=ltrim(rtrim(str(Mprecio,8)))
    	      mcanti2='1'
			  if items->gra <> 'S'
 			  	Mtas_IVA = "0"
			  endif
		  else
			  xbanco=left(items->ban,3)
			  xlocalidad=right(items->ban,4)

			  xnombanco=''
			  AEVAL(xMbancos, {|cValor,nIndice| IF(left(cValor,3) == xbanco, xnombanco := substr(xMbancos[nIndice],4),)})
			  xnombanco=left(xnombanco,10)

			  select 56
			  seek xlocalidad
			  if found()
				xlocalidad=left(alltrim(locali->nom),10)
			  endif

			  select 55
	          Mdescripcion="Chq: "+xnombanco+ " " + xlocalidad + " N:" + items->che
    	      Mprecio=int(items->imp * 100)/100
        	  *Mprecio=Mprecio*100
	          Mprecio2=ltrim(rtrim(str(Mprecio,8)))
    	      mcanti2='1'
			  Mtas_IVA = "0"
		  endif
      endif

	  * Carga el nro de block de motor
	  Mdescripcion=PFA_Limpiar(Mdescripcion)
*      if MdescAdi1=PF_DEL
*      	 MdescAdi1=PFA_BusNBlock(Mdescripcion)
*      	 MdescAdi2=PF_DEL
*      else
*     	 MdescAdi2=PFA_BusNBlock(Mdescripcion)*
*	  endif

   	 MNroBlock = PFA_BusNBlock(Mdescripcion)
   	 if MNroBlock <> PF_DEL
   	 	Mdescripcion = Mdescripcion +MNroBlock
   	 endif

	Mdescripcion = alltrim(Mdescripcion)
	if len(Mdescripcion) > 50
		MdescAdi1 = Left(Mdescripcion,50)
		Mdescripcion = Substr(Mdescripcion,51)
    	if len(Mdescripcion) > 50
    		MdescAdi2 = Left(Mdescripcion,50)
    		Mdescripcion = Substr(Mdescripcion,51)
    	endif
	endif
	 if len(alltrim(MdescAdi1)) > 0 .and. MdescAdi1 <> PF_DEL
		do PF_IniciarSalida
      	nada = PF_AgregaCampoSalida( PFA_Limpiar(MdescAdi1) )
        if .not. PF_EnviarComando( PH_FCTextoFiscal )
           do pfa_mostrar_datos
           PFA_MueMen(' Error: 9 Al enviar Descripcion Adic. 1',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
           Return
        endif
	 endif

	 if len(alltrim(MdescAdi2)) > 0 .and. MdescAdi2 <> PF_DEL
		do PF_IniciarSalida
      	nada = PF_AgregaCampoSalida( PFA_Limpiar(MdescAdi2) )
        if .not. PF_EnviarComando( PH_FCTextoFiscal )
           do pfa_mostrar_datos
           PFA_MueMen(' Error: 10 Al enviar Descripcion Adic. 2',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
           Return
        endif
	 endif

      do PF_IniciarSalida
      nada = PF_AgregaCampoSalida( left(PFA_Limpiar(Mdescripcion),50) )  && Descripci¢n
      nada = PF_AgregaCampoSalida( mcanti2 )       && Cantidad  3,000
      nada = PF_AgregaCampoSalida( Mprecio2 )      && Precio   12,50
      nada = PF_AgregaCampoSalida( Mtas_iva)       && Tasa IVA 21,00%
      nada = PF_AgregaCampoSalida( Mtip_monto )    && Monto vendido de mercaderia
      nada = PF_AgregaCampoSalida( '0' )   	       && Tasa de Ajuste Imp.Int.
      nada = PF_AgregaCampoSalida( '' )            && Display
      nada = PF_AgregaCampoSalida( MBase_Total )   && T: precio total; otro carácter: precio base

      if .not. PF_EnviarComando( PH_FCItemDeLinea )
          do PFA_mostrar_datos
          PFA_MueMen(fcodigo+' Error: 11 en impresi¢n de item',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
          PFA_CerrDB(xarea)
          Return .f.
      endif

      select items
      skip
      if xclave # str(items->cod,4)+str(items->nro,8)
            exit
      endif
enddo

* IMPRESION DEL PAGO
if es_nd_cheque = .T.
    select 56
    use cargos alias xcargos
    go top
	IF .NOT. eof()
        Mdescadi1=PF_DEL
        MdescAdi2 = PF_DEL
    	Mdescripcion=alltrim(xcargos->concepto)
        Mprecio=int(xcargos->importe * 100)/100
        *Mprecio=Mprecio*100
    	Mprecio2=ltrim(rtrim(str(Mprecio,8)))
        mcanti2='1'
    	Mtas_iva=param->tas_iva
    	MBase_Total = 'B'

    	do PF_IniciarSalida
        nada = PF_AgregaCampoSalida( left(PFA_Limpiar(Mdescripcion),50) )  && Descripci¢n
        nada = PF_AgregaCampoSalida( mcanti2 )       && Cantidad  3,000
        nada = PF_AgregaCampoSalida( Mprecio2 )      && Precio   12,50
        nada = PF_AgregaCampoSalida( Mtas_iva)       && Tasa IVA 21,00%
        nada = PF_AgregaCampoSalida( Mtip_monto )    && Monto vendido de mercaderia
        nada = PF_AgregaCampoSalida( '0' )   	       && Tasa de Ajuste Imp.Int.
        nada = PF_AgregaCampoSalida( '' )            && Display
        nada = PF_AgregaCampoSalida( MBase_Total )   && T: precio total; otro carácter: precio base

        if .not. PF_EnviarComando( PH_FCItemDeLinea )
    		do PFA_mostrar_datos
            PFA_MueMen(fcodigo+' Error: 12 en impresi¢n de item',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
            PFA_CerrDB(xarea)
            Return .f.
        endif
	ENDIF
ELSE
    select 56
    use cargos alias xcargos
    go top
    DO WHILE .not. eof()
       do PF_IniciarSalida
       xcon=alltrim(xcargos->concepto)
       ximp=xcargos->importe
       *ximp=ximp * 100
       ximp2=alltrim(str(ximp))
       nada = PF_AgregaCampoSalida(xcon)
       nada = PF_AgregaCampoSalida(ximp2)
       nada = PF_AgregaCampoSalida("M")
       nada = PF_AgregaCampoSalida("0")
	   IF AT('FLETE',UPPER(xcon)) > 0         && SI ES FLETE, EL IMPORTE ES BASE (SIN IVA)
	       nada = PF_AgregaCampoSalida("B")
		ELSE
	       nada = PF_AgregaCampoSalida("T")
		ENDIF
       if .not. PF_EnviarComando( PH_FCRecargos )
          do PFA_mostrar_datos
          PFA_MueMen(fcodigo+' Error: 13 en impresi¢n de recargos',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
          PFA_CerrDB(xarea)
          Return .f.
       ENDIF
       skip
    ENDDO
ENDIF

* IMPRESION DEL PAGO
sele 50
use PAGOS
go top
// el primer registro es el total
skip
*PFA_MESSAGEBOX('VA A ENTRAR EN EL DO WHILE','')
do while .not. eof()
*	PFA_MESSAGEBOX('ENTRO EN EL DO WHILE PAGOS->LINEA',PAGOS->LINEA)
    xpos=at('|',pagos->linea)
    xconcepto=alltrim(left(pagos->linea,xpos - 1))
    ximporte=alltrim(substr(pagos->linea,xpos + 1))
    xpos2=at(',',ximporte)
    ximporte=left(ximporte,xpos2 - 1) + '.'+substr(ximporte,xpos2 + 1)
    *PFA_Messagebox(str(val(ximporte)),ximporte)
    ximporte2=val(ximporte)
    IF ximporte2 > 0
        * ximporte2=ximporte2*100
        ximporte3=alltrim(str(ximporte2))
        do PF_IniciarSalida
        nada = PF_AgregaCampoSalida(xconcepto)
        nada = PF_AgregaCampoSalida(ximporte3)
        nada = PF_AgregaCampoSalida("T")
        * El comando PF_AgregaCampoSalida da error si se sobrepasan los 20 elementos
        if .not. PF_EnviarComando( PH_FCPago )
            do PFA_mostrar_datos
            PFA_MueMen(fcodigo+' Error: 14 en impresi¢n de pagos',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
            PFA_CerrDB(xarea)
            Return .f.
        endif
    endif
    skip
enddo

******CIERRE COMPROBANTE
do PF_IniciarSalida
if .NOT. PF_EnviarComando( PH_FCCerrar )
    do PFA_mostrar_datos
    PFA_MueMen(fcodigo+' Error: 15 en al cerrar el comprobante',alltrim(PF_MensajeEstado( PF_ModuloImpresor ))+CHR(13)+CHR(10)+ alltrim(PF_MensajeEstado( PF_ModuloFiscal )))
    PFA_CerrDB(xarea)
    Return .f.
endif

** Impresion de cheques mediante comprobante no fiscal
sele 50
use CHEQUES
go top
if .not. eof()
    xdatoscli="Cliente: "+Mnom_comprador
    xdatosfac="Comprobante: "+P_TipoCbte+' '+strzero(P_Sucursal,4)+'-'+strzero(P_NroCbte,8)
    IF PFA_ImpCheques(xdatoscli,xdatosfac) = .F.
        Return .f.
    endif
*    PFA_Messagebox("1","genero NO fiscal")
*else
*    PFA_Messagebox("2","no genero")
end if

** F I N A L
PFA_CerrDB(xarea)
Return .t.

*--------1234567890------------------------
FUNCTION PFA_ImpNCredito(P_Cliente,P_TipoCbte,P_NroCbte,P_Sucursal)
*--------------------------------
LOCAL AREAACTUAL
AREAACTUAL=ALIAS()

* limpia las lineas de header
for k = 2 to 5
      continuar=SeteoHeader( k ,PF_DEL)
next

* Limpia las 4 lineas
ik = 11
do while ik <= 14
     continuar = SeteoHeader( ik ,PF_DEL )
     ik = ik + 1
enddo


SELECT 68
USE NCREDITO ALIAS NCR

* Abre no fiscal
if PFA_AbreNF() = .f.
    Do PFA_mostrar_datos
    PFA_MueMen(' Error: 16 al inicio comprobante no fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
    Return .f.
end if

index on cod to ncreind
go top
do while .not. EOF()
* 	pfa_MESSAGEBOX('entra',str(recno()))
 	if len(alltrim(ncr->linea)) > 0 .and. ncr->cod <> '999'
		ikj=1
		for ikj=1 to 10000
		next
*	 	pfa_MESSAGEBOX('imprime',str(recno()))

 	    * Carga el nro de block de motor
*        if ncr->cod = 'CUE'
*      	   xMlinea=PFA_BusNBlock(ncr->linea)
*		   if xMlinea = PF_DEL
*			  xMlinea = ncr->linea
*			endif
*        else
*      	   xMlinea=ncr->linea
*	    endif
*		xMlinea=PFA_Limpiar(xMlinea)
		xMlinea=rtrim(ncr->linea)
        if PFA_LineaNF(xMlinea) = .f.
             Do PFA_mostrar_datos
             PFA_MueMen('Error: 17 al imprimir N.Credito',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
             Return .f.
        endif
	else
*	 	pfa_MESSAGEBOX('no imprime',str(recno()))
 	endif
    skip
enddo

if PFA_CerrarNF() = .f.
    PFA_MueMen(' Error: 18 al cerrar comprobante no fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
    Return .f.
end if

*COPIA LA BASE Y LUEGO LA BORRA
*COPY TO NCRED
*ZAP

use
IF LEN(ALLTRIM(AREAACTUAL)) > 0
    SELECT &AreaActual
ENDIF
RETURN .T.

*--------1234567890------------------------
FUNCTION PFA_ImpNCFiscal(P_Cliente,P_TipoCbte,P_NroCbte,P_Sucursal)
*--------------------------------
* Imprime una nota de credito como comprobante no fiscal homologado        ** N.CREDITO **
local fcodigo,xarea,k,xtarjeta,MRemito1,MRemito2, xMbancos[0]
xtarjeta=''
xarea=SELECT()
MBase_Total = ' '     && indica precio base (B) o precio total (T)
MNroBlock = ' '

* limpia las lineas de header
for k = 2 to 5
      continuar=SeteoHeader( k ,PF_DEL)
next

***** AUXIL  contiene el detalle de las formas de pago                      ** N.CREDITO **
SELECT 50
USE AUXIL
k=1
GO TOP
* saltea el primero porque solo hay lugar para cuatro lineas               ** N.CREDITO **
skip

DO WHILE .NOT. EOF() .and. k <= 4
    tira = auxil->linea
    if len(alltrim(tira)) > 0
        ** setea el header 11, 12, 13, 14
        continuar=SeteoHeader( k+10 ,tira)
        k = k + 1
    endif
    skip
enddo
* Limpia el resto de las 4 lineas                                         ** N.CREDITO **
i = 0
w = 0
if k < 5
   ik = k + 10
   do while ik <= 14
        continuar = SeteoHeader( ik ,' ' )
        ik = ik + 1
   enddo
endif

***** OBSER contiene el detalle de las observaciones                     ** N.CREDITO **
USE obser
GO TOP
k=1
MRemito1 = PF_DEL
MRemito2 = PF_DEL
DO WHILE .NOT. EOF() .and. k<=2
    tira = left(alltrim(obser->linea),40)
	if k=1
		MRemito1 = tira
	else
		if k=2
			MRemito2 = tira
		end if
	end if
    k=k+1
    skip
enddo

if len(alltrim(MRemito1)) > 0 .and. Mremito1 <> PF_DEL
      if .not. SeteoHeader( 4 ,MRemito1 )
         do pfa_mostrar_datos
         PFA_MueMen(' Error: 1 Al enviar Linea 1 de observaciones',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
         Return
      endif
endif
if len(alltrim(Mremito2)) > 0 .and. Mremito2 <> PF_DEL
      if .not. SeteoHeader( 5 ,MRemito2 )
         do pfa_mostrar_datos
         PFA_MueMen(' Error: 2 Al enviar Linea 2 de observaciones',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
         Return
      endif
endif
MRemito1 = PF_DEL
MRemito2 = PF_DEL


**************TARJETA                                                ** N.CREDITO **
* si se emitio con tarjeta imprime los datos de la tarjeta y si hay lugar para imprimir
xtarjeta = PF_DEL
use ncredito
go top
do while .not. eof()
	if ncredito->cod='CAB'
		xpos =AT('TARJETA DE CREDITO',ncredito->linea)
		if xpos > 0
			tira=left(substr(ncredito->linea,xpos),44)
			xtarjeta = tira
	        k=k+1
	        exit
		endif
	endif
	skip
enddo
if xtarjeta <> PF_DEL
      do PF_IniciarSalida
      Continuar = PF_AgregaCampoSalida("3")
      Continuar = PF_AgregaCampoSalida(xtarjeta)
      if .not. PF_EnviarComando( PH_PoneEncabe )
         do pfa_mostrar_datos
         PFA_MueMen(' Error: 3 Al enviar Linea 3 de Tarjeta Cr.',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
         Return
      endif
end if

***** PARAMETROS                                                           ** N.CREDITO **
sele 50
use param alias param
*Mtip_cbte = param->tip_cbte
if P_TipoCbte = 'FC'
    Mtip_cbte = 'F'
else
    Mtip_cbte = 'D'
endif

Mformulario = param->formulario
Mcopias = param->copias
Mpreimpreso = param->preimpreso
Mcpi = param->cpi
Miva_vende = param->iva_vende
Mbienuso = param->bienuso
Mtas_iva=param->tas_iva
Mtip_monto=param->tip_monto
Mbultos=param->bultos
Mtas_impint=param->tas_impint
Mmfi_impint=param->mfi_impint

Mtip_cbte_HAS = ''

***** COMPROBANTES                                                         ** N.CREDITO **
IF VTOM#0
   Mformapago = 'CUENTA CORRIENTE'
ELSE
   Mformapago = 'CONTADO'
ENDIF

Mimporte=impm
Mimporte=Mimporte * 100
Mimporte2=alltrim(str(Mimporte,8))

*****CLIENTES                                                              ** N.CREDITO **
sele 52
use clialf index cclialf alias cliente shared
*locate for cod=P_Cliente
seek P_Cliente
IF .not. found()
   PFA_Messagebox(fcodigo+' Error: 4 ','Cliente no encontrado: '+P_tipoCbte+str(P_nrocbte))
   Return .f.
endif
Mnom_comprador = alltrim(cliente->nom) + ' ('+alltrim(str(cod))+')'
Mnro_doc = PFA_arregla_cuit(cliente->cui)

do case
case cliente->iva = 0
    Mletra_cbte = 'A'
    Miva_comprador='I'
    Mtip_doc = "C"
    Mtas_ivani = "0"
	Mtip_cbte_HAS = 'R'
	MBase_Total = 'B'
case cliente->iva = 1
    Mletra_cbte = 'A'
    Miva_comprador='N'
    Mtip_doc = "C"
    Mtas_ivani=param->tas_ivani
	Mtip_cbte_HAS = 'R'
	MBase_Total = 'B'
case cliente->iva = 2
    Mletra_cbte = 'B'
    Miva_comprador='C'
	do case
	case len(alltrim(MNro_doc)) = 11
	    Mtip_doc = "C"
	case len(alltrim(MNro_doc)) > 0
	    Mtip_doc = "2"
	otherwise
	    Mtip_doc = PF_DEL
	    MNro_doc = PF_DEL
	endcase
    Mtas_ivani = "0"
	Mtip_cbte_HAS = 'S'
	MBase_Total = 'T'
case cliente->iva = 3
    Mletra_cbte = 'B'
    Miva_comprador='E'
    Mtip_doc = "C"
    Mtas_ivani = "0"
	Mtip_cbte_HAS = 'S'
	MBase_Total = 'T'
end case


Mcodprovin = cliente->prv

***** PROVINCIAS                                                           ** N.CREDITO **
sele 53
use prvalf index cprvalf alias provin shared
*locate for cod=Mcodprovin
SEEK Mcodprovin
IF .not. found()
   PFA_Messagebox(fcodigo+' Error: 5 ','Provincia no encontrada: '+P_tipoCbte+str(P_nrocbte))
   Return .f.
endif
Mnomprovin=alltrim(provin->nom)
Mdomici1 = left(alltrim(cliente->dom),43)
Mdomici2 = left(alltrim('(' + str(cliente->cpo) + ') '+ ;
           alltrim(cliente->loc) + ' ' +Mnomprovin),43)
Mdomici3 = PF_DEL

* HASAR envía datos de clientes                                            ** N.CREDITO **
do PF_IniciarSalida
Continuar = PF_AgregaCampoSalida(MNom_Comprador)
Continuar = PF_AgregaCampoSalida(MNro_doc)
Continuar = PF_AgregaCampoSalida(Miva_comprador)
Continuar = PF_AgregaCampoSalida(MTip_doc)
Continuar = PF_AgregaCampoSalida(left(alltrim(Mdomici1)+ ' ' +alltrim(Mdomici2),50))
if .not. PF_EnviarComando( PH_DatosCli )
   do pfa_mostrar_datos
   PFA_MueMen(' Error: 6 Al enviar Datos de cliente',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return
endif

* HASAR Inserta el número de comprobante asociado                          ** N.CREDITO **
do PF_IniciarSalida
nada = PF_AgregaCampoSalida( '1' )          && Número de línea ( 1 o 2)
nada = PF_AgregaCampoSalida( '1' )          && Números de documento
if .not. PF_EnviarComando( PH_NroRemito )
   Do PFA_mostrar_datos
   PFA_MueMen('Error: 7 en apertura de cbte',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return .f.
endif


* HASAR Abre el comprobante fiscal                                         ** N.CREDITO **
do PF_IniciarSalida
nada = PF_AgregaCampoSalida( Mtip_cbte_HAS )          && Tipo de comprobante
* 								N.Credito A (R), N.Credito B/C (S)
nada = PF_AgregaCampoSalida( "T" )                  && Tiquet/Factura o SLip
if .not. PF_EnviarComando( PH_FCAbre )
   Do PFA_mostrar_datos
   PFA_MueMen('Error: 7 en apertura de cbte',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return .f.
endif

*****ITEMS                                                                 ** N.CREDITO **
sele 54
use stoalf index cstoalf alias productos shared
sele 55

use favalf index cfavalf alias items shared

xnroitem=0

xclave=str(P_cliente,4)+str(P_NroCbte,8)

SEEK xclave

do while .not. eof()
      xnroitem = xnroitem + 1

      MdescAdi1 = PF_DEL
      MdescAdi2 = PF_DEL
      Mcod_prod=items->art
      select productos
      seek Mcod_prod
      if .not. found()
        PFA_Messagebox('Error: 8 ','Articulo no encontrado: '+items->arp)
        Return .f.
      endif
      mcodprod = alltrim(productos->art)
      Mdescripcion=mcodprod+' '+alltrim(productos->nom)

      mcanti=items->can

      mcanti2=alltrim(str(mcanti))
	  * si tiene recargo por tarjeta de credito
	  recargo1 = 0
	  recargo2 = 0
	  IF items->de1 > 0
	  	  recargo1 = ROUND((items->pre * items->de1 / 100 ),2)
	  ENDIF
	  IF items->de2 > 0
          recargo2 = ROUND((items->pre * items->de2 / 100 ),2)
	  ENDIF
	  Mprecio = int((items->pre + recargo1 + recargo2) * 100 ) / 100

      Mprecio2=alltrim(str(Mprecio))

	  * Carga el nro de block de motor                                        ** N.CREDITO **
	  Mdescripcion=PFA_Limpiar(Mdescripcion)

   	 MNroBlock = PFA_BusNBlock(Mdescripcion)
   	 if MNroBlock <> PF_DEL
   	 	Mdescripcion = Mdescripcion +MNroBlock
   	 endif

	Mdescripcion = alltrim(Mdescripcion)
	if len(Mdescripcion) > 50
		MdescAdi1 = Left(Mdescripcion,50)
		Mdescripcion = Substr(Mdescripcion,51)
    	if len(Mdescripcion) > 50
    		MdescAdi2 = Left(Mdescripcion,50)
    		Mdescripcion = Substr(Mdescripcion,51)
    	endif
	endif
	 if len(alltrim(MdescAdi1)) > 0 .and. MdescAdi1 <> PF_DEL
		do PF_IniciarSalida
      	nada = PF_AgregaCampoSalida( PFA_Limpiar(MdescAdi1) )
        if .not. PF_EnviarComando( PH_FCTextoFiscal )
           do pfa_mostrar_datos
           PFA_MueMen(' Error: 9 Al enviar Descripcion Adic. 1',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
           Return
        endif
	 endif

	 if len(alltrim(MdescAdi2)) > 0 .and. MdescAdi2 <> PF_DEL
		do PF_IniciarSalida
      	nada = PF_AgregaCampoSalida( PFA_Limpiar(MdescAdi2) )
        if .not. PF_EnviarComando( PH_FCTextoFiscal )
           do pfa_mostrar_datos
           PFA_MueMen(' Error: 10 Al enviar Descripcion Adic. 2',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
           Return
        endif
	 endif

                                                                           ** N.CREDITO **
      do PF_IniciarSalida
      nada = PF_AgregaCampoSalida( left(PFA_Limpiar(Mdescripcion),50) )  && Descripci¢n
      nada = PF_AgregaCampoSalida( mcanti2 )       && Cantidad  3,000
      nada = PF_AgregaCampoSalida( Mprecio2 )      && Precio   12,50
      nada = PF_AgregaCampoSalida( Mtas_iva)       && Tasa IVA 21,00%
      nada = PF_AgregaCampoSalida( Mtip_monto )    && Monto vendido de mercaderia
      nada = PF_AgregaCampoSalida( '0' )   	       && Tasa de Ajuste Imp.Int.
      nada = PF_AgregaCampoSalida( '' )            && Display
      nada = PF_AgregaCampoSalida( MBase_Total )   && T: precio total; otro carácter: precio base

      if .not. PF_EnviarComando( PH_FCItemDeLinea )
          do PFA_mostrar_datos
          PFA_MueMen(fcodigo+' Error: 11 en impresi¢n de item',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
          PFA_CerrDB(xarea)
          Return .f.
      endif

      select items
      skip
      if xclave # str(items->cod,4)+str(items->nro,8)
            exit
      endif
enddo

* IMPRESION DE LOS CARGOS                                                  ** N.CREDITO **
select 56
use cargos alias xcargos
go top
DO WHILE .not. eof()
   do PF_IniciarSalida
   xcon=alltrim(xcargos->concepto)
   ximp=xcargos->importe
   *ximp=ximp * 100
   ximp2=alltrim(str(ximp))
   nada = PF_AgregaCampoSalida(xcon)
   nada = PF_AgregaCampoSalida(ximp2)
   nada = PF_AgregaCampoSalida("M")
   nada = PF_AgregaCampoSalida("0")
	   IF AT('FLETE',UPPER(xcon)) > 0         && SI ES FLETE, EL IMPORTE ES BASE (SIN IVA)
	       nada = PF_AgregaCampoSalida("B")
		ELSE
	       nada = PF_AgregaCampoSalida("T")
		ENDIF
   if .not. PF_EnviarComando( PH_FCRecargos )
      do PFA_mostrar_datos
      PFA_MueMen(fcodigo+' Error: 13 en impresi¢n de recargos',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
      PFA_CerrDB(xarea)
      Return .f.
   ENDIF
   skip
ENDDO

* IMPRESION DEL PAGO                                                       ** N.CREDITO **
sele 50
use PAGOS
go top
* el primer registro es el total
skip
*PFA_MESSAGEBOX('VA A ENTRAR EN EL DO WHILE','')
do while .not. eof()
*	PFA_MESSAGEBOX('ENTRO EN EL DO WHILE PAGOS->LINEA',PAGOS->LINEA)
    xpos=at('|',pagos->linea)
    xconcepto=alltrim(left(pagos->linea,xpos - 1))
    ximporte=alltrim(substr(pagos->linea,xpos + 1))
    xpos2=at(',',ximporte)
    ximporte=left(ximporte,xpos2 - 1) + '.'+substr(ximporte,xpos2 + 1)
    *PFA_Messagebox(str(val(ximporte)),ximporte)
    ximporte2=val(ximporte)
    IF ximporte2 > 0
        * ximporte2=ximporte2*100
        ximporte3=alltrim(str(ximporte2))
        do PF_IniciarSalida
        nada = PF_AgregaCampoSalida(xconcepto)
        nada = PF_AgregaCampoSalida(ximporte3)
        nada = PF_AgregaCampoSalida("T")
        * El comando PF_AgregaCampoSalida da error si se sobrepasan los 20 elementos
        if .not. PF_EnviarComando( PH_FCPago )
            do PFA_mostrar_datos
            PFA_MueMen(fcodigo+' Error: 14 en impresi¢n de pagos',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
            PFA_CerrDB(xarea)
            Return .f.
        endif
    endif
    skip
enddo

******CIERRE COMPROBANTE                                                   ** N.CREDITO **
do PF_IniciarSalida
if .NOT. PF_EnviarComando( PH_CierDNFH )
    do PFA_mostrar_datos
    PFA_MueMen(fcodigo+' Error: 15 en al cerrar el comprobante',alltrim(PF_MensajeEstado( PF_ModuloImpresor ))+CHR(13)+CHR(10)+ alltrim(PF_MensajeEstado( PF_ModuloFiscal )))
    PFA_CerrDB(xarea)
    Return .f.
endif


** F I N A L                                                               ** N.CREDITO **
PFA_CerrDB(xarea)
RETURN .T.

*--------------------------------
FUNCTION PFA_CerrDB(Parea)
*--------------------------------
sele 50
use
sele 51
use
sele 52
use
sele 53
use
sele 54
use
sele 55
use
sele 56
use
sele 57
use
sele 58
use

SELECT(Parea)
Return

*--------------------------------
FUNCTION PFA_Limpiar(PTira)
*--------------------------------
* Quita los caracteres no imprimibles de los strings
* y los reemplaza por blancos
xstring = PTira
for i = 1 to len(xstring)
    x= asc(substr(xstring,i,1))
    do case
    case x = 160
       xstring=substr(xstring,1,i-1)+'a'+substr(xstring,i+1)
    case x = 130
       xstring=substr(xstring,1,i-1)+'e'+substr(xstring,i+1)
    case x = 161
       xstring=substr(xstring,1,i-1)+'i'+substr(xstring,i+1)
    case x = 162
       xstring=substr(xstring,1,i-1)+'o'+substr(xstring,i+1)
    case x = 163
       xstring=substr(xstring,1,i-1)+'u'+substr(xstring,i+1)
    case x = 164
       xstring=substr(xstring,1,i-1)+'n'+substr(xstring,i+1)
    case x = 165
       xstring=substr(xstring,1,i-1)+'N'+substr(xstring,i+1)
    case x <= 31
       xstring=substr(xstring,1,i-1)+' '+substr(xstring,i+1)
    case x >= 126
       xstring=substr(xstring,1,i-1)+' '+substr(xstring,i+1)
    endcase
next
Return xstring


*--------------------------------
FUNCTION PFA_ImpCheques(P_datoscli,P_datosfac)
*--------------------------------
ik = 11
do while ik <= 14
    continuar = SeteoHeader( ik ,' ' )
    ik = ik + 1
enddo

if PFA_AbreNF() = .f.
    Do PFA_mostrar_datos
    PFA_MueMen(' Error: 19 al inicio comprobante no fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
    Return .f.
end if

* Imprime lineas en blanco iniciales
*for ik = 1 to 5
*    if  PFA_LineaNF(".") = .f.
*        Do PFA_mostrar_datos
*        PFA_MueMen('Error: 20 al inicio cbte. no fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
*        Return .f.
*    end if
*next

* Imprime cabecera
if  PFA_LineaNF(left(P_Datoscli,40)) = .f.
    Do PFA_mostrar_datos
    PFA_MueMen(' Error: 21 al inicio comprobante no fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
    Return .f.
end if
if  PFA_LineaNF(left(P_Datosfac,40)) = .f.
    Do PFA_mostrar_datos
    PFA_MueMen(' Error: 22 al inicio comprobante no fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
    Return .f.
end if
if  PFA_LineaNF("Cheques recibidos") = .f.
    Do PFA_mostrar_datos
    PFA_MueMen(' Error: 23 al inicio comprobante no fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
    Return .f.
end if
*****************123456789 123456789 123456789 1234567890
if  PFA_LineaNF("****************************************") = .f.
    Do PFA_mostrar_datos
    PFA_MueMen(' Error: 24 al inicio comprobante no fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
    Return .f.
end if

go top
i = 0
do while .not. EOF()
    i = i + 1
    xlinea = PFA_Limpiar(alltrim(cheques->linea))
    if  PFA_LineaNF(xlinea) = .f.
        Do PFA_mostrar_datos
        PFA_MueMen(' Error: 25 al imprimir linea NF:'+str(i),PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
        Return .f.
    end if

    skip
enddo
if  PFA_LineaNF("****************************************") = .f.
    Do PFA_mostrar_datos
    PFA_MueMen(' Error: 26 al finalizar cbte. fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
    Return .f.
end if
if PFA_CerrarNF() = .f.
    PFA_MueMen(' Error: 27 al cerrar comprobante no fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
    Return .f.
end if

RETURN

*--------------------------------
function PFA_arregla_cuit(Pnumero)
*--------------------------------
* Funci¢n   : Quita todos los caracteres no numericos
* Respuesta : string con numeros
Mnumero=LTRIM(RTRIM(Pnumero))
salida=''
for i=1 to len(Mnumero)
  car=substr(Mnumero,I,1)
  if Isdigit(car)
     salida = salida + car
  endif
end if
Return salida

*--------1234567890------------------------
function PFA_AbreNF()
*--------------------------------
* Abre documento no fiscal
*
do PF_IniciarSalida
if .not. PF_EnviarComando( PH_NFAbre )
   Do PFA_mostrar_datos
   PFA_MueMen(' Error: 28 en apertura de cbte No Fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return .f.
endif
Return .t.

*--------1234567890------------------------
function PFA_LineaNF(PA_texto)
*--------------------------------
* Imprime linea no fiscal
*
xlinea=left(PFA_Limpia(rtrim(PA_texto)),80)
*pfa_messagebox(xlinea,"")
do PF_IniciarSalida
nada = PF_AgregaCampoSalida( xlinea )            && En papel continuo
if .not. PF_EnviarComando( PH_NFItem )
   *Do PFA_mostrar_datos
   PFA_MueMen(' Error: 28 en impresion linea NF',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return .f.
endif
Return .t.

*--------1234567890------------------------
function PFA_CerrarNF()
*--------------------------------
* Abre documento no fiscal
*
do PF_IniciarSalida
if .not. PF_EnviarComando( PH_NFCerrar )
   Do PFA_mostrar_datos
   PFA_MueMen(' Error: 29 en cerrar cbte No Fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return .f.
endif
Return .t.

*------------123456--------------------
function PFA_MueMen(mensaje1, mensaje2)
*--------------------------------
* Funci¢n   : Muestra el mensaje y el estado de la la impresora y el estado fiscal
* Respuesta : nada
LOCAL ACTUAL,xvec[0] && color actual antes de ingresar a este proceso
ACTUAL = SETCOLOR()
SAVE SCREEN
SET COLOR TO
AADD(xvec,alltrim(mensaje1))
xmen = mensaje2
xpos=AT(chr(13),xmen)
do while xpos > 0
    AADD(xvec,alltrim(substr(xmen,1,xpos-1)))
    xmen=substr(xmen,xpos + 2)
    xpos=AT(chr(13),xmen)
enddo
AADD(xvec,alltrim(xmen))
@ 10,09 clear TO 19,71
@ 10,09 TO 19,71 DOUBLE
xpos=ACHOICE(11,10,18,70,xvec)
restore screen
RETURN


*--------------------------------
function PFA_messagebox(mnsj1, mnsj2)
*--------------------------------
* Funci¢n   : Muestra un mensaje de dos lineas
* Respuesta : nada
LOCAL ACTUAL && color actual antes de ingresar a este proceso
ACTUAL = SETCOLOR()
SAVE SCREEN
SET COLOR TO
nada = ' '
@ 10,09 CLEAR TO 19,71
SET COLOR TO
@ 10,09 TO 19,71 DOUBLE
a_mnj1=alltrim(mnsj1)
a_mnj2=alltrim(mnsj2)
if len(a_mnj1) > 60
   a_mnj11 = left(a_mnj1,60)
   a_mnj12 = left(substr(a_mnj1,61),60)
else
   a_mnj11 = a_mnj1
   a_mnj12 = ""
endif
if len(a_mnj2) > 60
   a_mnj21 = left(a_mnj2,60)
   a_mnj22 = left(substr(a_mnj2,61),60)
else
   a_mnj21 = a_mnj2
   a_mnj22 = ""
endif
@ 12, 11 say a_mnj11
@ 13, 11 say a_mnj12
@ 14, 11 say a_mnj21
@ 15, 11 say a_mnj22
@ 17, 11 get nada
READ
SETCOLOR(ACTUAL)
RESTORE SCREEN
RETURN

*--------------------------------
function PFA_questionbox(mnsj1, mnsj2)
*--------------------------------
* Funci¢n   : Muestra un mensaje de dos lineas
* Respuesta : 'SI' o 'NO'
*
LOCAL ACTUAL && color actual antes de ingresar a este proceso
LOCAL OP
ACTUAL = SETCOLOR()
SAVE SCREEN
SET COLOR TO
nada = ' '
@ 10,19 CLEAR TO 15,61
SET COLOR TO I
@ 10,19 TO 15,61 DOUBLE
SET COLOR TO
@ 12, 21 say left(mnsj1,40)
@ 13, 21 say left(mnsj2,40)
WHILE .T.
    @ 14, 21 prompt ' ACEPTA '
    @ 14, 31 prompt ' CANCELA '
    MENU TO OP
    IF LASTKEY()=27
       RETURN 'NO'
    ENDIF
    IF OP = 1
       RESPUESTA = 'SI'
       EXIT
    ELSE
       RESPUESTA = 'NO'
       EXIT
    ENDIF
ENDDO
SETCOLOR(ACTUAL)
RESTORE SCREEN
RETURN RESPUESTA

*--------------------------------
function PFA_AbrirPuerto( )
*--------------------------------
* Funci¢n   : Inicializa y abre el puerto serie para la impresora
*             segun los datos de parametros
* Respuesta : Verdadero, puerto abierto

*************************************************
#ifdef PF_SINFISCAL
   RETURN .T.
#endif
************************************************



if .not. lower(dbf(50))='param'
    select 50
    use param
    go top
    *pfa_messagebox('','use param')
else
    *pfa_messagebox('','select param')
    select 50
    go top
endif
m_puerto = param->puerto
m_dir_io = param->dir_io
m_nro_int = param->nro_int
m_com 	  = alltrim(str(param->puerto))
PFA_MESSAGEBOX('Configuracion:','Puerto: COM'+m_com)
PFA_TIEMPOMAXIMO = param->tiempomax

* Abro el Port de Comunicaciones
Handler = OpenPort (m_com)

if Handler < 0
	mens1= PF_MensajeEstado( PF_ModuloImpresor )
    mens2= PF_MensajeEstado( PF_ModuloFiscal )
    PFA_Messagebox(mens1,mens2)
    PFA_CerrDB(xarea)
    Return .F.
else
	BaudRate = SearchPr (Handler)
	if .not. at(alltrim(str(BaudRate)),'2400.4800.9600.19200.115200') > 0
       PFA_Messagebox('Sr. operador','No se pudo detectar el controlador fiscal.'+PF_TAB+'Controle las conexiones.')
       PFA_CerrDB(xarea)
       Return .F.
	else
	    Return .T.
	endif
endif

*--------------------------------
function PF_PuertoInit ( PuertoNro, PuertoIO, PuertoIRQ )
*--------------------------------
* Funci¢n   : Inicializa y abre el puerto serie para la impresora
*             PuertoNro  = N£mero de puerto serie 1 a 4
*             PuertoIO   = Direcci¢n de Entrada/Salida asignada al puerto
*             PuertoIRQ  = N£mero de Interrupci¢n asignada al puerto
* Respuesta : Verdadero, puerto abierto

* NOTA
* Direcci¢n de memoria de Entrada / Salida ( Num‚rico es direcci¢n decimal
* alfab‚tico es direcci¢n Hexadecimal

*  INFORMACION SOBRE LAS ALTERNATIVAS DE PARAMETROS
*    Puerto Direcci¢n   IRQ
*     COM1    3F8H       4  Usualmente
*     COM2    2F8H       3  Usualmente
*     COM3    3E8H       4  Sin definir especificamente
*     COM4    2E8H       3  Sin definir especificamente
*
*    Si tiene problemas para abrir el puerto debe verificar que las
*    direcciones de memoria y el IRQ correspondan con el hardware
*    Para mayor informaci¢n lea la ayuda de Clipper Tools

private ComOK
private Continuar
private iret

* Carga Variable publica
PF_PuertoNro = PuertoNro

Continuar = .T.

* Setea la direccion de memoria del puerto
if .not. COM_SETIO( PF_PuertoNro, PuertoIO )
   iret = alert( "Imposible inicializar posici¢n de memoria puerto com"+ltrim(str(PF_PuertoNro)) )
   continuar =.F.
endif
if continuar
    * Setea la interrupcion del puerto
    if .not. COM_SETIRQ(PF_PuertoNro, PuertoIRQ )
       iret = alert( "Imposible inicializar IRQ puerto com"+ltrim(str(pf_PuertoNro)) )
       continuar =.F.
    endif
endif
if continuar
    * El COM_OPEN debe configurarse con Buffer de salida porque el Handshake por
    * Hardware requiere comunicaci¢n en background
    if .not. COM_OPEN(PF_PuertoNro, 1024, 1024)
       iret = alert( "Imposible abrir puerto com"+ltrim(str(PF_PuertoNro)) )
       continuar =.F.
    endif
endif
if continuar
    * El comando COM_HARD activa el Handshake por Hardware
    COM_HARD(PF_PuertoNro,.T.,.T.)
endif
if continuar
    * Inicializa parametros de configuraci¢n de com
    if .not. COM_INIT(PF_PuertoNro,9600,"N",8,1)
       iret = alert( "Imposible configurar el puerto com"+ltrim(str(PF_PuertoNro)) )
       continuar =.F.
    endif
endif

return continuar

*--------------------------------
Function  PF_PuertoCierra
*--------------------------------
* Funcion   : Cierra el puerto utilizado por la impresora
*--------------------------------

COM_CLOSE( PF_PuertoNro)

return

*--------------------------------
function PF_AgregaCampoSalida ( dato )
*--------------------------------
* Funci¢n   : Agrega un campo a el array de datos a enviar del comando
* Entrada   : dato ( string a agregar )
* Salida    : True Ejecuci¢n correcta
*             False Error
*
LOCAL nro
LOCAL iRet

nro = ascan(PF_DatoaEnviar,NIL)
if nro = 0
   iret = alert( "Error intento enviar mas de 25 par metros" )
   iRet = .F.
else
   PF_DatoaEnviar[ nro ] = dato
   iRet = .T.
endif

return iRet

*--------------------------------
procedure PF_IniciarSalida
*--------------------------------
* Funcion   : Inicializa el array de campos a enviar
* Entrada   :
* Salida    :
*
LOCAL nro

for nro = 1 to 25
   PF_DatoaEnviar[ nro ] = NIL
next

return


*--HASAR------------------------------
procedure PF_LeeCamposRecibidos
* Nombre    : PF_LeeCamposRecibidos
*--------------------------------
* Funcion   : Toma el PF_StringRecibido y lo descompone por campos en PF_DatoRecibido[]
*
LOCAL cant
LOCAL StrAux
LOCAL Pos1
LOCAL Pos2
LOCAL continuar

StrAux = PF_TAB + PF_StringRecibido

for cant = 1 to 25
   PF_DatoRecibido[ cant ] = NIL
next

continuar = .T.
cant = 0
*wait "descompone respuesta"
do while continuar
   pos1 = at( PF_TAB, StrAux )
   if pos1 > 0
      StrAux = substr( StrAux, pos1 + 1 )
      pos2 = at( PF_TAB, StrAux )
      if pos2 > 0
         cant = cant + 1
         PF_DatoRecibido[ cant ] = left( StrAux, pos2 - 1 )

		*pfa_messagebox('0 '+str(cant),pf_datorecibido[cant])

      elseif at( PF_ETX, StrAux ) > 0
         cant = cant + 1
         PF_DatoRecibido[ cant ] = left( StrAux, at( PF_ETX, StrAux ) - 1 )

		*pfa_messagebox('1 '+str(cant),pf_datorecibido[cant])

         continuar = .F.
      elseif len( StrAux ) > 0
         cant = cant + 1
         PF_DatoRecibido[ cant ] =  StrAux

		*pfa_messagebox('2 '+str(cant),pf_datorecibido[cant])

         continuar = .F.
      else
         continuar = .F.
      endif
   else
      continuar = .F.
   endif
enddo

return


*--------------------------------
function PF_ControlaCRC ( )
*--------------------------------
* Funcion   : Lee los Ultimos 4 Bytes del buffer para chequear el CRC
* Entrada   : Ninguna
* Respuesta :
*             true  ==> En gDatoRecibido, VARIABLE GLOBAL QUEDA EL COMANDO
*             false ==> NO Encontrado. No llego o un Time out o error
*

STATIC EnviadoPrimerPaquete :=.F.    // FLAG Importante, indica si es
                                     // primer paquete enviado
LOCAL Continuar
LOCAL CRCDatoRecibido
LOCAL CantBytesCRC
LOCAL TInicio
LOCAL LapTime
LOCAL AscEntrada
LOCAL iret
LOCAL crc
LOCAL nada

*  Flag para saber si hay error ***
   continuar = .T.

*  Contador para saber si Llegaron los 4 Bytes del CRC ***
   CantBytesCRC = 0

*  El CRC lo voy a calcular ahora, por lo tanto lo pongo a CERO
   CRCDatoRecibido = ""

*  asigno a LapTime el PF_TIEMPOMAXIMO de segundos y cargo el tiempo de inicio
*   LapTime = PF_TIEMPOMAXIMO
   LapTime = PFA_TIEMPOMAXIMO
   TInicio =  seconds()

   do while ( CantBytesCRC < 4 ) .and.;
            ( mod( seconds() + 86400.1 - TInicio, 86400 ) <= LapTime ).and.;
            ( continuar = .T. )
*      veo si llego algo ***
       do while COM_COUNT(PF_PuertoNro) = 0 .and.;
                ( mod( seconds() + 86400.1 - TInicio, 86400 ) <= LapTime)
       enddo
       * veo si no Leyo algo
       if COM_COUNT(PF_PuertoNro) = 0
           continuar = .F.  // tengo un time out
           iret = .F.
           CarEntrada = ""
       else
           CarEntrada = COM_READ( PF_PuertoNro, 1 )
       endif
       do case
          case CarEntrada = PF_ETX
             * Encontre un ETX ==> Hay error ***
               iret = .F.
             * Para salir loop ***
               Continuar = .F.
          case CarEntrada = PF_TIEMPO
                * si el comando consume mucho tiempo
                LapTime = LapTime + 1.1
          otherwise
                CRCDatoRecibido = CRCDatoRecibido + CarEntrada
                CantBytesCRC = CantBytesCRC + 1

        endcase
   enddo

*  Si sali hasta aqui, controlo el CRC ***
   if Continuar
*  El string Ingresado lo tengo en una Variable GLOBAL ***
      if len(PF_StringRecibido) < 3
         iret = alert(" string muy corto ")
         continuar = .F.
         iret = .F.
      else
         crc = alltrim(ntoc( asciisum( PF_StringRecibido + PF_STX + PF_ETX) ,16 ))  // Funcion CTOOLS convierte a base 16
         crc = right("0000" + crc, 4 )
         if crc = CRCDatoRecibido
           *? "CRC Ok"
            Iret = .T.
         else
           *? "CRC Mal"
            continuar = .F.
            iret = .F.
         endif
      endif
   endif

*  veo si los numeros de paquetes son los correctos ***
   if Continuar
      if substr(PF_StringEnviado, 2, 1) <> substr(PF_StringRecibido, 1, 1)
       * tengo error en el numero de paquete ***
         iret = .F.
       * para salir loop ***
         continuar = .F.
#ifdef PF_DEBUG
         ?"Nro paquete enviado " +str(asc(substr(PF_stringEnviado,2,1)),3,0) +;
          "Nro paquete recibido " +str(asc(substr(PF_stringRecibido,1,1)),3,0)
         ?PF_StringEnviado
         ?PF_StringRecibido
         wait
#endif
      endif
   endif

*  veo si los numeros de Comando son los correctos ***
   if Continuar
      if substr(PF_StringEnviado, 3, 1) <> substr(PF_StringRecibido, 2, 1)
       * tengo error en el numero de Comando ***
         if EnviadoPrimerPaquete = .F.
            * Incremento el Nro de paquete para retransmitir el comando, solo
            * si es el primer paquete a enviar
              nada = PF_NroPaquete( "P" )
         endif
       * Indico que se debe retransmitir el comando
         iret = .F.
       * para salir loop ***
         continuar = .F.
#ifdef PF_DEBUG
         ?"Nro Comando mal"
         wait
#endif
      endif
   endif
   if Continuar = .T. .and. iret = .T.
      nada = PF_NroPaquete( "P" )
      *Indico que ya se envio el primer paquete
      EnviadoPrimerPaquete = .T.
   else
      PF_StringRecibido = ""
   endif

return iret


*--------------------------------
function PF_CRC ( entrada )
*--------------------------------
* Nombre    : PF_CRC
*--------------------------------
* Funcion   : Calcula el CRC para el comando a enviar
* Entrada   : Cadena de caracteres a enviar al impresor
* Respuesta :
*             CRC , si la cadena es v lida
*             ""  , si la cadena es inv lida
*
LOCAL Continuar
LOCAL iret
LOCAL crc

continuar = .T.

if len( entrada ) < 3
   iret = alert("Cadena muy corta en PF_CRC")
   Continuar = .F.
endif
if continuar
   crc = alltrim(ntoc( asciisum( entrada ), 16 ))  // Funcion CTOOLS convierte a base 16
   crc = right("0000" + crc, 4 )
else
   crc = ""
endif

return crc

*--------------------------------
function PF_Sincronizar
*--------------------------------
* Funci¢n   : Controla el estado de la impresora y lo restablece
*             de forma de dejarla preparada para generar un nuevo comprobante
*             Se usa en caso de querer cancelar un comprobante abierto
* Entrada   : Nada
* Respuesta :
*             True,  Si se ejecut¢ correctamente
*             False, Si hubo problemas
*
LOCAL iRet
LOCAL Continuar
LOCAL TipoDocu
LOCAL CerroDocu
STATIC sincroniza :=  .F.
* La variable est tica sincroniza se usa para saber si esta funci¢n fue llamada
* por la ejecuci¢n de un comando o llamado por ella misma, de modo que si esta
* en true sale de la funci¢n sin ejecutarla

if sincroniza = .T.
        return .T.
endif

sincroniza = .T.

do PF_IniciarSalida                 // Inicializa los par metros del comando
iRet = PF_AgregaCampoSalida ("D")   // Env¡a "D" para pedir estado de documento

if PF_EnviarComando( PF_Estado )
        Continuar = .T.
else
        Continuar = .F.
endif

TipoDocu = alltrim(PF_DatoRecibido[ 3 ])   // El parametro 3 contiene el tipo de documento
* tipodocu tendra el tipo de documento abierto
*     T=tique
*     F=factura
*     I=tique-factura
*     O=documento no fiscal,
*     H=documento no fiscal homologado
* si no es ninguno presumo que el equipo es una versi¢n que no soporta
* el estado(D) ni factura o tique-factura
if .not. tipodocu $ "TFIOH"
   tipodocu = "T"
endif

CerroDocu = .F.
if Continuar = .T.
        * Analizo el estado del m¢dulo Fiscal
        StatusFiscal = PF_DatoRecibido[2]
        if isbit(StatusFiscal, 12 ) .or.;    // Se requiere cierre de D¡a
           isbit(StatusFiscal, 13 ) .or.;    // Documento Fiscal
           isbit(StatusFiscal, 14 )          // Documento Abierto
           if isbit(StatusFiscal, 13 ) .or.; // Documento Fiscal
              isbit(StatusFiscal, 14 )       // Documento Abierto
              if TipoDocu = "F" .or. TipoDocu = "I"     //Factura o Tique-Factura
                 do PF_IniciarSalida
                 iRet = PF_AgregaCampoSalida ("CANCELA")  // Env¡a "CANCELA" como 1er param.
                 iRet = PF_AgregaCampoSalida ("00000000") // Env¡a "00000000" como 2do param.
                 iRet = PF_AgregaCampoSalida ("C")        // Env¡a "C" como 3er param.
                 if PF_EnviarComando( PF_FCPago )   // Ejecuta comando de Factura pago opci¢n cancelar
                    Continuar = .T.
                    Cerrodocu = .T.
                 else
                    Continuar = .F.
                 endif
              elseif TipoDocu = "T"  //Tique
                 do PF_IniciarSalida
                 iRet = PF_AgregaCampoSalida ("CANCELA")  // Env¡a "CANCELA" como 1er param.
                 iRet = PF_AgregaCampoSalida ("00000000") // Env¡a "00000000" como 2do param.
                 iRet = PF_AgregaCampoSalida ("C")        // Env¡a "C" como 3er param.
                 if PF_EnviarComando( PF_TQPago )   // Ejecuta comando de Tique pago opci¢n cancelar
                    Continuar = .T.
                    Cerrodocu = .T.
                 else
                    Continuar = .F.
                 endif
              elseif TipoDocu = "O" // Otros NO Fiscal
                 do PF_IniciarSalida
                 iRet = PF_AgregaCampoSalida ("T")  // Env¡a "T" Corte de papel
                 if PF_EnviarComando( PF_NFCerrar ) // Ejecuta comando de Cierre no Fiscal
                    Continuar = .T.
                    Cerrodocu = .T.
                 else
                    Continuar = .F.
                 endif
              endif
           endif
        endif
        if isbit(StatusFiscal, 12 )  .and. (.not. isbit(StatusFiscal, 7 )) // Se requiere cierre de D¡a
           iRet = Alert( "ATENCION, Deber  Ejecutar un cierre Z" )
           Continuar = .F.
        endif
endif

* Variable sincroniza indica con false que termino la funci¢n
sincroniza = .F.

return Continuar


*--------------------------------
function PF_EnviarComando( Comando )
*--------------------------------
* Funci¢n   : Env¡a un comando a la impresora Fiscal
* Entrada:
*             comando= n£mero del comando que se ejecutara
* Respuesta :
*             true  ==> OK
*             false ==> Problemas


LOCAL datoout
LOCAL Continuar

    Continuar = .T.

    * controlo valor del comando ****
    if comando > 177
	   pfa_messagebox('Valor de comando incorrecto',str(comando))
       *iret = Alert( "Valor de comando muy grande" )
       continuar = .F.
    endif

    if continuar = .T.
        * Arma el string a enviar
        datoout =  chr(comando)
        i = 1
        do while PF_DatoaEnviar[ i ] <> NIL
            datoout = datoout + PF_TAB + PF_DatoaEnviar[ i ]
            i = i + 1
        enddo

        * en PF_StringEnviado se almacena el ultimo dato enviado
        PF_StringEnviado = datoout

		Enviar(PF_StringEnviado)
	endif
#ifdef fiscal_log
     FWRITE(nArcSalida, datoout)
     FWRITE(nArcSalida, '{{'+PF_StringRecibido+'}}')
#endif

return Continuar
*



*--------------------------------
function PF_ComandoOK(Comando )
* Nombre    : PF_ComandoOK
*--------------------------------
* Funcion   : Controla la ejecuci¢n correcta del comando
* Entrada:
*             comando= numero del comando que se ejecuta
* Respuesta :
*             true  ==> OK
*             false ==> Problemas

LOCAL StatusPrint
LOCAL StatusFiscal
LOCAL Continuar

StatusPrint  = PF_DatoRecibido[1]
StatusFiscal = PF_DatoRecibido[2]

if StatusPrint = NIL .or. StatusFiscal = NIL
   Continuar = .F.
else
   Continuar = .T.
endif

* ATENCION : La funci¢n ISBIT() de las CATOOLS testea los bit desde 1 a 16.
* En la documentaci¢n de la impresora los bits se indican de 0 a 15, de modo
* que para controlar el bit 0 debo preguntar por el 1, hasta el 15 por el 16

if continuar
* si estan en 1 los bits 9 y 16 corresponde a un impresor que
* esta por llenarse de cierres z y si no tiene ningun otro
* error se puede continuar
   if isbit(StatusFiscal, 9) .and. isbit(StatusFiscal, 16) //Memoria fiscal por llenarse
      if isbit(StatusFiscal, 12 ).or.;                     //Requiere cierre Z
         isbit(StatusFiscal, 8 ).or.;                      //Memoria fiscal llena
         isbit(StatusFiscal, 4 ).or.;                      //Comando no reconocido
         isbit(StatusFiscal, 2 ).or.;                      //Error memoria de trabajo
         isbit(StatusFiscal, 1 )                           //Error memoria fiscal
              continuar = .F.
      endif
   else
      if isbit(StatusFiscal, 16 ).or.;                     //Bit indicador de errores
         isbit(StatusFiscal, 12 ).or.;                     //Requiere cierre Z
         isbit(StatusFiscal, 8 ).or.;                      //Memoria fiscal llena
         isbit(StatusFiscal, 4 ).or.;                      //Comando no reconocido
         isbit(StatusFiscal, 2 ).or.;                      //Error memoria de trabajo
         isbit(StatusFiscal, 1 )                           //Error memoria fiscal
              continuar = .F.
      endif
   endif
end if
if continuar
* si la placa fiscal esta ok controlo el estado de la impresora
   if isbit(StatusPrint, 15 ).or.;            //Impresora sin papel
      isbit(StatusPrint, 7 ).or.;             //Buffer impresora lleno
      isbit(StatusPrint, 4 ).or.;             //Impresora fuera de linea
      isbit(StatusPrint, 3 )                  //Error en impresora
           continuar = .F.
   endif
endif

return continuar


*--------1234567890------------------------
function PF_MensOLDEstado( opcion )
*function PF_MensajeEstado( opcion )
*--------------------------------
* Funci¢n   : Devuelve un string con el detalle del estado fiscal y de la impresora
* Entrada:
*             opcion = 1 estado de la impresora
*                      2 estado fiscal
* Respuesta :
*             String con el detalle del estado

LOCAL mensaje

if opcion = 1
   if PF_DatoRecibido[1] = NIL
      mensaje = "Ultimo comando no se ejecut¢ correctamente"
   else
      mensaje = "Estado de la impresora " + PF_DatoRecibido[1]
      if isbit(PF_DatoRecibido[1], 1)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  0 " + "Impresora Ocupada"
      endif
      if isbit(PF_DatoRecibido[1], 2)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  1 " + "Impresora Seleccionada"
      endif
      if isbit(PF_DatoRecibido[1], 3)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  2 " + "Error en la Impresora o Falta papel"
      endif
      if isbit(PF_DatoRecibido[1], 4)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  3 " + "Impresora Fuera de L¡nea"
      endif
      if isbit(PF_DatoRecibido[1], 5)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  4 " + "Poco papel auditor¡a"
      endif
      if isbit(PF_DatoRecibido[1], 6)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  5 " + "Poco papel"
      endif
      if isbit(PF_DatoRecibido[1], 7)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  6 " + "Buffer impresora lleno"
      endif
      if isbit(PF_DatoRecibido[1], 8)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  7 " + "Buffer impresora vacio"
      endif
      if isbit(PF_DatoRecibido[1], 9)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  8 " + "Tapa de impresora abierto"
      endif
      if isbit(PF_DatoRecibido[1], 10)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  9 " + "Sin uso"
      endif
      if isbit(PF_DatoRecibido[1], 11)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 10 " + "Sin uso"
      endif
      if isbit(PF_DatoRecibido[1], 12)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 11 " + "Sin uso"
      endif
      if isbit(PF_DatoRecibido[1], 13)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 12 " + "Sin uso"
      endif
      if isbit(PF_DatoRecibido[1], 14)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 13 " + "Sin uso"
      endif
      if isbit(PF_DatoRecibido[1], 15)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 14 " + "Caj¢n de dinero ausente"
      endif
      if isbit(PF_DatoRecibido[1], 16)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 15 " + "L¢gica OR Bits 2-5, 8 y 14 "
      endif
   endif
elseif opcion = 2
   if PF_DatoRecibido[2] = NIL
      mensaje = "Ultimo comando no se ejecut¢ correctamente"
   else
      mensaje = "Estado Fiscal " + PF_DatoRecibido[2]
      if isbit(PF_DatoRecibido[2], 1)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  0 " + "Checkeo de Memoria Fiscal !MAL!"
      endif
      if isbit(PF_DatoRecibido[2], 2)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  1 " + "Checkeo RAM de Trabajo !MAL!"
      endif
      if isbit(PF_DatoRecibido[2], 3)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  2 " + "Sin uso"
      endif
      if isbit(PF_DatoRecibido[2], 4)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  3 " + "Comando NO Reconocido "
      endif
      if isbit(PF_DatoRecibido[2], 5)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  4 " + "Campo de Datos INVALIDO "
      endif
      if isbit(PF_DatoRecibido[2], 6)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  5 " + "Comando Inv lido para el Estado L¢gico del Equipo"
      endif
      if isbit(PF_DatoRecibido[2], 7)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  6 " + "Se va a producir el OVERFLOW en los Acumuladores del equipo"
      endif
      if isbit(PF_DatoRecibido[2], 8)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  7 " + "La memoria Fiscal esta LLENA "
      endif
      if isbit(PF_DatoRecibido[2], 9)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  8 " + "La memoria fiscal se esta por LLENAR"
      endif
      if isbit(PF_DatoRecibido[2], 10)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  9 " + "El Impresor tiene N£mero de Serie(Certificado)"
      endif
      if isbit(PF_DatoRecibido[2], 11)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 10 " + "El controlador Fiscal esta Fiscalizado"
      endif
      if isbit(PF_DatoRecibido[2], 12)
         *mensaje = mensaje + chr(13) + chr(10) + "Bit 11 " + "Se llego al M ximo de Items o se requiere un cierre del d¡a"
         mensaje = mensaje + chr(13) + chr(10) + "Bit 11 " + "Error en ingreso de fecha"
      endif
      if isbit(PF_DatoRecibido[2], 13)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 12 " + "Documento Fiscal Abierto"
      endif
      if isbit(PF_DatoRecibido[2], 14)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 13 " + "Documento Abierto "
      endif
      if isbit(PF_DatoRecibido[2], 15)
         *mensaje = mensaje + chr(13) + chr(10) + "Bit 14 " + "Factura abierta, Hoja Suelta"
         mensaje = mensaje + chr(13) + chr(10) + "Bit 14 " + "STATPRN activado"
      endif
      if isbit(PF_DatoRecibido[2], 16)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 15 " + "OR de bits 0-8"
      endif
   endif
else
   mensaje = "Debe informar opcion 1 = Impresora o 2 = Fiscal"
endif

return mensaje

*--HASAR-----------------------------------
function PF_MensajeEstado( opcion )
*--------------------------------
* Funci¢n   : Devuelve un string con el detalle del estado fiscal y de la impresora
* Entrada:
*             opcion = 1 estado de la impresora
*                      2 estado fiscal
* Respuesta :
*             String con el detalle del estado
********************************************************************************
LOCAL mensaje

* << ESTADO DE LA IMPRESORA >>
if opcion = 1
   if PF_DatoRecibido[1] = NIL
      mensaje = "Ultimo comando no se ejecut¢ correctamente"
   else
      mensaje = "Estado de la impresora " + PF_DatoRecibido[1]
      if isbit(PF_DatoRecibido[1], 1)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  0 " + "Impresora Ocupada"
      endif
      if isbit(PF_DatoRecibido[1], 2)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  1 " + "Impresora Seleccionada"
      endif
      if isbit(PF_DatoRecibido[1], 3)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  2 " + "Error en la Impresora"
      endif
      if isbit(PF_DatoRecibido[1], 4)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  3 " + "Impresora Fuera de L¡nea"
      endif
      if isbit(PF_DatoRecibido[1], 5)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  4 " + "Poco papel auditor¡a"
      endif
      if isbit(PF_DatoRecibido[1], 6)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  5 " + "Poco papel"
      endif
      if isbit(PF_DatoRecibido[1], 7)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  6 " + "Buffer impresora lleno"
      endif
      if isbit(PF_DatoRecibido[1], 8)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  7 " + "Buffer impresora vacio"
      endif
      if isbit(PF_DatoRecibido[1], 9)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  8 " + "Sin uso"
      endif
      if isbit(PF_DatoRecibido[1], 10)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  9 " + "Sin uso"
      endif
      if isbit(PF_DatoRecibido[1], 11)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 10 " + "Sin uso"
      endif
      if isbit(PF_DatoRecibido[1], 12)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 11 " + "Sin uso"
      endif
      if isbit(PF_DatoRecibido[1], 13)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 12 " + "Caj¢n de Dinero Abierto"
      endif
      if isbit(PF_DatoRecibido[1], 14)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 13 " + "Sin uso"
      endif
      if isbit(PF_DatoRecibido[1], 15)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 14 " + "Caj¢n de Dinero Ausente"
      endif
      if isbit(PF_DatoRecibido[1], 16)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 15 " + "L¢gica OR Bits 2-5,8 y 14"
      endif
   endif

* << ESTADO FISCAL >>
elseif opcion = 2
   if PF_DatoRecibido[2] = NIL
      mensaje = "Ultimo comando no se ejecut¢ correctamente"
   else
      mensaje = "Estado Fiscal " + PF_DatoRecibido[2]
      if isbit(PF_DatoRecibido[2], 1)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  0 " + "Checkeo de Memoria Fiscal !MAL!"
      endif
      if isbit(PF_DatoRecibido[2], 2)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  1 " + "Checkeo RAM de Trabajo !MAL!"
      endif
      if isbit(PF_DatoRecibido[2], 3)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  2 " + "Bater¡a BAJA "
      endif
      if isbit(PF_DatoRecibido[2], 4)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  3 " + "Comando NO Reconocido "
      endif
      if isbit(PF_DatoRecibido[2], 5)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  4 " + "Campo de Datos INVALIDO "
      endif
      if isbit(PF_DatoRecibido[2], 6)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  5 " + "Comando Inv lido para el Estado L¢gico del Equipo"
      endif
      if isbit(PF_DatoRecibido[2], 7)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  6 " + "Se va a producir el OVERFLOW en los Acumuladores del equipo"
      endif
      if isbit(PF_DatoRecibido[2], 8)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  7 " + "La memoria Fiscal esta LLENA "
      endif
      if isbit(PF_DatoRecibido[2], 9)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  8 " + "La memoria fiscal se esta por LLENAR"
      endif
      if isbit(PF_DatoRecibido[2], 10)
         mensaje = mensaje + chr(13) + chr(10) + "Bit  9 " + "El Impresor tiene N£mero de Serie(Certificado)"
      endif
      if isbit(PF_DatoRecibido[2], 11)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 10 " + "El controlador Fiscal esta Fiscalizado"
      endif
      if isbit(PF_DatoRecibido[2], 12)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 11 " + "Se llego al M ximo de Items o se requiere un cierre del d¡a"
      endif
      if isbit(PF_DatoRecibido[2], 13)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 12 " + "Documento Fiscal Abierto"
      endif
      if isbit(PF_DatoRecibido[2], 14)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 13 " + "Documento Abierto "
      endif
      if isbit(PF_DatoRecibido[2], 15)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 14 " + "Factura abierta, Hoja Suelta"
      endif
      if isbit(PF_DatoRecibido[2], 16)
         mensaje = mensaje + chr(13) + chr(10) + "Bit 15 " + "OR de bits 0-8 da 1 "
      endif
   endif
else
   mensaje = "Debe informar opcion 1 = Impresora o 2 = Fiscal"
endif

return mensaje


*--------------------------------
function PF_EnviaString( Comando )
* Nombre    : PF_EnviaString
*--------------------------------
* Funcion   : Envia un string a la impresora Fiscal
* Entrada:
*             comando= numero del comando que se ejecuta
* Respuesta :
*             true  ==> OK
*             false ==> Problemas

LOCAL datoout
LOCAL chk
LOCAL Continuar

    Continuar = .T.

    * controlo valor del comando ****
    if comando > 128
       iret = Alert( "Valor de comando muy grande" )
       continuar = .F.
    endif

    if continuar = .T.
        * Arma el string a enviar
        datoout = PF_STX + chr(PF_NroPaquete("U")) + chr(comando)
        i = 1
        do while PF_DatoaEnviar[ i ] <> NIL
            datoout = datoout + PF_TAB + PF_DatoaEnviar[ i ]
            i = i + 1
        enddo
        datoout = datoout + PF_ETX
        chk = PF_CRC(datoout)
        datoout = datoout + chk
        * en PF_StringEnviado se almacena el ultimo dato enviado
        PF_StringEnviado = datoout

        tinicio = seconds()
        do while !COM_CTS(PF_PuertoNro) .and. mod (seconds() - tinicio +86400.1, 86400) < .1
        enddo

        *Envia comando, cRest tiene la cantidad de caracteres pendientes de envio
#ifdef fiscal_log
     FWRITE(nArcSalida, datoout)
#endif

        cRest = COM_SEND( PF_PuertoNro, datoout )

        *Si queda algo se termina de enviar el comando, con un timeout de 10 segundos
        tinicio = seconds()
        do while cRest > 0
           datoout = right( datoout, cRest )
           cRest = COM_SEND( PF_PuertoNro, datoout )
*           if mod (seconds() - tinicio + 86400.1, 86400) > PF_TIEMPOMAXIMO
           if mod (seconds() - tinicio + 86400.1, 86400) > PFA_TIEMPOMAXIMO
              iret = alert( "ERROR DE TIME OUT (ENVIO)" )
              continuar = .F.
           endif
        enddo
        * Con COM_SCOUNT se chequea cuando se termino de enviar todo del buffer de
        * salida, para luego leer las respuestas, usa timeout de 1 seg
        tinicio = seconds()
        do while COM_SCOUNT(PF_PuertoNro) > 0 .and. mod (seconds() - tinicio +86400.1, 86400) < .8
        enddo
        if mod (seconds() - tinicio+86400.1 ,86400) > 1
           continuar = alert("ERROR TIME OUT (Vaciar Salida)")
           continuar = .T.
        endif
    endif
return Continuar


*--------------------------------
function PF_InicioPaquete
* Nombre    : PF_InicioPaquete
*--------------------------------
* Funcion   : Espera por el principio de un paquete
* Respuesta :
*             true  ==> encontrado
*             false ==> no encontrado no llego o un time out
*             nak   ==> la impresora fiscal informa error de recepcion

LOCAL Continuar
LOCAL STXencontrado
LOCAL TInicio
LOCAL LapTime
LOCAL AscEntrada
LOCAL CarEntrada
LOCAL iret

*   si voy a recibir un paquete, borro el ultimo recibido
    PF_StringRecibido = ""

*   flag para saber si hay error ***
    continuar = .T.

*   flag para saber si encontre un stx ***
    STXecontrado = .F.
    iret = .F.

*   asigno a LapTime el PF_TIEMPOMAXIMO de segundos y cargo el tiempo de inicio
*    LapTime = PF_TIEMPOMAXIMO
    LapTime = PFA_TIEMPOMAXIMO
    TInicio =  seconds()
    do while ( STXecontrado = .F.) .and.;
             ( mod( seconds() + 86400.1 - TInicio, 86400 ) <= LapTime ).and.;
             ( continuar = .T. )
*       veo si llego algo ***
        do while COM_COUNT(PF_PuertoNro) = 0 .and.;
                 ( mod( seconds() + 86400.1 - TInicio, 86400 ) <= LapTime)
        enddo
        * veo si no Leyo algo
        if COM_COUNT(PF_PuertoNro) = 0
            continuar = .F.  // tengo un time out
            iret = .F.
            CarEntrada = ""
#ifdef PF_DEBUG
            ?"Time out STX"
            wait
#endif
        else
            CarEntrada = COM_READ( PF_PuertoNro, 1 )
        endif
        if continuar
            * analizo el byte que entro
            do case
            case CarEntrada = PF_NAK
                  * encontre un nak ==> hay error ***
                  iret = .F.
                  * para salir loop ***
                  STXecontrado = .T.
#ifdef PF_DEBUG
                  ?"Inicio con NAK"
                  wait
#endif
            case CarEntrada = PF_STX
                  * encontre el stx ***
                  iret = .T.
                  * para salir loop ***
                  STXecontrado = .T.
            case CarEntrada = PF_TIEMPO
                  * si el comando consume mucho tiempo
                  LapTime = LapTime + 1.1
            otherwise
#ifdef PF_DEBUG
                  ?"Inicio con "+str(asc(carentrada))
                  wait
#endif
                  * si llega basura, limpio el buffer
                  do PF_LimpiarBufferCom
                  * incremento tiempo para no tener falso timeout
                  LapTime = LapTime + 1.1
            endcase
        endif
    enddo
return iret


*--------------------------------
function PF_FinPaquete
* Nombre    : PF_FinPaquete
*--------------------------------
* Funcion   : Espera por el FIN de un paquete
* Respuesta :
*             true  ==> encontrado
*             false ==> no encontrado no llego o un time out
*             nak   ==> la impresora fiscal informa error de recepcion

LOCAL Continuar
LOCAL ETXencontrado
LOCAL TInicio
LOCAL LapTime
LOCAL CarEntrada
LOCAL AscEntrada
LOCAL iret

*   flag para saber si hay error ***
    continuar = .T.

*   flag para saber si encontre un stx ***
    ETXecontrado = .F.
    iret = .F.

*   asigno a LapTime el PF_TIEMPOMAXIMO de segundos y cargo el tiempo de inicio
*    LapTime = PF_TIEMPOMAXIMO
    LapTime = PFA_TIEMPOMAXIMO
    TInicio =  seconds()

    do while ( ETXecontrado = .F.) .and.;
             ( mod( seconds() + 86400.1 - TInicio, 86400 ) <= LapTime ) .and.;
             ( continuar = .T. )
*       veo si llego algo ***
        do while COM_COUNT(PF_PuertoNro) = 0 .and.;
                 ( mod( seconds() + 86400.1 - TInicio, 86400 ) <= LapTime)
        enddo
        * veo si no Leyo algo
        if COM_COUNT(PF_PuertoNro) = 0
            continuar = .F.  // tengo un time out
            iret = .F.
            CarEntrada = ""
#ifdef PF_DEBUG
            ?"Time out ETX"
            wait
#endif
        else
            CarEntrada = COM_READ( PF_PuertoNro, 1 )
        endif
        * analizo la informacion que entro
        do case
        case CarEntrada = PF_NAK .or. CarEntrada = PF_STX
             * encontre un nak ==> hay error ***
             iret = .F.
             * para salir loop ***
             STXecontrado = .T.
             PF_StringRecibido = ""
#ifdef PF_DEBUG
             if CarEntrada = PF_NAK
                ?"EsperaFinal NAK"
                wait
             endif
#endif
        case CarEntrada = PF_ETX
             * Encontre el STX ***
             iret = .T.
             * Para salir loop ***
             ETXecontrado = .T.
        case CarEntrada = PF_TIEMPO
             * Si el Comando consume mucho tiempo
             LapTime = LapTime + 1.1
        otherwise
             PF_StringRecibido = PF_StringRecibido + CarEntrada
        endcase
    enddo
return iret


*--------------------------------
Procedure PF_LimpiarBufferCom
* Funcion   : Vaciar el buffer de entrada
*--------------------------------
* Respuesta : Ninguna

LOCAL basura

do while com_count(PF_PuertoNro) > 0
   basura = com_read(PF_PuertoNro)
enddo
return


*--------------------------------
function PF_NroPaquete ( opcion )
* Nombre    : PF_NroPaquete
*--------------------------------
* Funcion   : Maneja el numero de paquete del comando
*             opcion = "U" = Informa el ultimo numero de paquete
*             opcion = "P" = Pasa al proximo numero de paquete
* Respuesta : Numero de paquete pedido


STATIC numeropaquete := 0

   * para la primera vez ***
   if numeropaquete < 32
           numeropaquete = 32 +int( rand(seconds()) * (127-32) )
   endif
   * si pide el ultimo ****
   if upper(opcion) = "U"
           numeropaquete = numeropaquete
   endif
   * si pide el proximo ****
   if upper(opcion) = "P"
           numeropaquete = numeropaquete + 1
   endif
   * si llegue al limite superior ****
   if numeropaquete > 127
           numeropaquete = 32
   endif

return  numeropaquete

*----------------------------------------------------------------------
* FUNCIONES PARA EL MANEJO DE SLIP
*----------------------------------------------------------------------

*--------------------------------
*function SeteoDisenio ELIMINADO
*----------------------------------------------------------------------
* Setea el dise¤o de la Factura
*----------------------------------------------------------------------



*--------------------------------
function SeteoZona ( ZonaNro, Izquierda, Superior, Derecha, Inferior )
*----------------------------------------------------------------------
* Configura el dise¤o de cada Zona
*----------------------------------------------------------------------
Private Continuar

do PF_IniciarSalida
nada = PF_AgregaCampoSalida( "P" )
nada = PF_AgregaCampoSalida( "C" )
nada = PF_AgregaCampoSalida( "Z" )
nada = PF_AgregaCampoSalida( "U" )
nada = PF_AgregaCampoSalida( right("000"+alltrim(str(ZonaNro)), 3) )
nada = PF_AgregaCampoSalida( right("000"+alltrim(str(Izquierda)), 3) )
nada = PF_AgregaCampoSalida( right("000"+alltrim(str(Superior)), 3) )
nada = PF_AgregaCampoSalida( right("000"+alltrim(str(Derecha)), 3) )
nada = PF_AgregaCampoSalida( right("000"+alltrim(str(Inferior)), 3) )
* El comando PF_AgregaCampoSalida da error si se sobrepasan los 20 elementos
if PF_EnviarComando( PF_SetPrefer )
   Continuar = .T.
   ? "Zona "+right("000"+str(ZonaNro), 3) +" configurada"
else
   ?PF_DatoaEnviar[1],PF_DatoaEnviar[2],PF_DatoaEnviar[3],PF_DatoaEnviar[4],PF_DatoaEnviar[5],PF_DatoaEnviar[6],PF_DatoaEnviar[7],PF_DatoaEnviar[8] ,PF_DatoaEnviar[9]
   Continuar = .F.
   ? PF_MensajeEstado( PF_ModuloImpresor )
   ? PF_MensajeEstado( PF_ModuloFiscal )
   * La aplicaci¢n aca debe analizar por que se produjo el error del comando
   * y tomar la decisi¢n si cancela el comprobante, o avisa que no hay papel
endif

return Continuar


*--------------------------------
function SeteoHeader ( HeadNro, Descripcion )
*----------------------------------------------------------------------
* Configura los Headers
*----------------------------------------------------------------------
Private Continuar

do PF_IniciarSalida
nada = PF_AgregaCampoSalida( right("00"+alltrim(str(HeadNro)), 2) )
nada = PF_AgregaCampoSalida( Descripcion )
* El comando PF_AgregaCampoSalida da error si se sobrepasan los 20 elementos
if PF_EnviarComando( PH_PoneEncabe )
   Continuar = .T.
*   ? "Header "+right("000"+alltrim(str(HeadNro)), 2)
else
   Continuar = .F.
   *? PF_MensajeEstado( PF_ModuloImpresor )
   *? PF_MensajeEstado( PF_ModuloFiscal )
   * La aplicaci¢n aca debe analizar por que se produjo el error del comando
   * y tomar la decisi¢n si cancela el comprobante, o avisa que no hay papel
endif

return Continuar

*--------------------------------
procedure PFA_mostrar_datos
*--------------------------------
i=1
do while PF_DatoRecibido[i] <> NIL .and. i <= 25
#ifdef fiscal_log
    FWRITE(nArcSalida,'*['+PF_DatoRecibido[i]+']*')
#endif
   *? alltrim(str(i))+': '+PF_DatoRecibido[i]
   i = i + 1
enddo
Return

*--------------------------------
PROCEDURE PFA_CIEX
*--------------------------------
IF PFA_questionbox('Sr. Operador','Confirma emisi¢n Cierre X') = 'SI'
      do PF_IniciarSalida
      Continuar = PF_AgregaCampoSalida("X")
      nada= PF_EnviarComando( PH_CierreX )
ENDIF
RETURN

*--------------------------------
PROCEDURE PFA_CIEZ
*--------------------------------
IF PFA_questionbox('Sr. Operador','Confirma emisi¢n Cierre Z') = 'SI'
      do PF_IniciarSalida
      Continuar = PF_AgregaCampoSalida("Z")
      nada= PF_EnviarComando( PH_CierreZ )
ENDIF
RETURN

*---------1234567890-----------------------
PROCEDURE PFA_CanCbte
*--------------------------------
IF PFA_questionbox('Sr. Operador','Confirma la cancelaci¢n del comprobante actual?') = 'SI'
    do PF_IniciarSalida
    if .not. PF_EnviarComando( PH_Cancelar )
       do pfa_mostrar_datos
       PFA_MueMen(' Error: 30 en la cancelaci¢n de cbte.',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
       Return .F.
    endif
ENDIF
RETURN

*---------1234567890-----------------------
PROCEDURE PFA_CanCNF
*--------------------------------
IF PFA_questionbox('Sr. Operador','Confirma la cancelaci¢n cbte. No fiscal?') = 'SI'
    do PF_IniciarSalida
    if .not. PF_EnviarComando( PH_Cancelar )
       do pfa_mostrar_datos
       PFA_MueMen(' Error: 31 en la cancelaci¢n de cbte.',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
       Return .F.
    end if
ENDIF
RETURN

*--------------------------------
PROCEDURE PFA_Est1
*--------------------------------
local xmen,xvec[0],xpos
xcolor = SETCOLOR()
set color to
do PF_IniciarSalida
if .not. PF_EnviarComando( PH_Estado )
   do pfa_mostrar_datos
   PFA_MueMen(' Error: 32 Al solicitar estado.',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return
endif

xmen=PF_MensajeEstado(1)
if at('C080',xmen) > 0
    AADD(xvec,'< E S T A D O   N O R M A L >')
else
    AADD(xvec,'*** E S T A D O   E R R O N E O ***')
endif
xpos=100
xpos=AT(chr(13),xmen)
do while xpos > 0
    AADD(xvec,alltrim(substr(xmen,1,xpos-1)))
    xmen=substr(xmen,xpos + 2)
    xpos=AT(chr(13),xmen)
enddo
AADD(xvec,alltrim(xmen))
save screen
@ 10,09 clear TO 19,71
@ 10,09 TO 19,71 DOUBLE
xpos=ACHOICE(11,10,18,70,xvec)
set color to &xcolor
restore screen
RETURN

*--------------------------------
PROCEDURE PFA_Est2
*--------------------------------
local xmen,xvec[0],xpos
xcolor = SETCOLOR()
set color to
do PF_IniciarSalida
if .not. PF_EnviarComando( PH_Estado )
   do pfa_mostrar_datos
   PFA_MueMen(' Error: 33 Al solicitar estado.',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return
endif
xmen=PF_MensajeEstado(2)
if at('0000',xmen) > 0 .or. at('0600',xmen) > 0
    AADD(xvec,'< E S T A D O   N O R M A L >')
else
    AADD(xvec,'*** E S T A D O   E R R O N E O ***')
endif
xpos=AT(chr(13),xmen)
do while xpos > 0
    AADD(xvec,alltrim(substr(xmen,1,xpos-1)))
    xmen=substr(xmen,xpos + 2)
    xpos=AT(chr(13),xmen)
enddo
AADD(xvec,alltrim(xmen))
save screen
@ 10,09 clear TO 19,71
@ 10,09 TO 19,71 DOUBLE
xpos=ACHOICE(11,10,18,70,xvec)
set color to &xcolor
restore screen
RETURN

*--------------------------------
function PFA_CtrNum()
*--------------------------------
local PFacA,PFacb

*************************************************
#ifdef PF_SINFISCAL
   RETURN .T.
#endif
************************************************

* Rescata los valores actuales
AreaActual = alias()
select 62
use auxalf alias pfparam shared
PfacA = pfparam->profaa
PfacB = pfparam->profab
PncrA = pfparam->proncA
PncrB = pfparam->proncB

PfacA = PFacA - 1
PfacB = PFacB - 1
PncrA = PncrA - 1
PncrB = PncrB - 1

* Rescatar los valores de la impresora
do PF_IniciarSalida
if .not. PF_EnviarComando( PF_Estado )
    Do PFA_mostrar_datos
    PFA_MueMen(' Error: 34 en el pedido de datos',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
endif
mens1='2:'+PF_DatoRecibido[2]+' 3:'+PF_DatoRecibido[3]+' 4:'+PF_DatoRecibido[4]
mens2='5:'+PF_DatoRecibido[5]+' 7:'+PF_DatoRecibido[7]+' 8:'+PF_DatoRecibido[8]
pfa_messagebox(mens1,mens2)
faca = val(PF_DatoRecibido[5])
facb = val(PF_DatoRecibido[3])
ncra = val(PF_DatoRecibido[8])
ncrb = val(PF_DatoRecibido[7])

* Compara los numeros
if faca <> PFacA .or. facb <> PfacB .or. ncra <> Pncra .or. ncrb <> PncrB
    pfa_messagebox('Se proceder  a actualizarlos.','Nro. incorrecto de Fac.A: '+ ;
        alltrim(PF_DatoRecibido[5])+'<>'+ alltrim(str(PFacA-1))+' Fac.B: '+;
        PF_DatoRecibido[3]+'<>'+alltrim(str(PFacB-1))+' NCr.A: '+;
        PF_DatoRecibido[8]+'<>'+alltrim(str(PncrA-1))+' NCr.B: '+;
        PF_DatoRecibido[7]+'<>'+alltrim(str(PncrB-1))  )
    BLOQUEADBF(0)
    replace profaa with faca + 1, profab with facb + 1, pronca with ncra + 1, proncb with ncrb + 1
    profaam=faca + 1
    profabm=facb + 1
    proncam=ncra + 1
    proncbm=ncrb + 1
    use
    IF LEN(ALLTRIM(AREAACTUAL)) > 0
        SELECT &AreaActual
    ENDIF
    return .f.
endif
use
IF LEN(ALLTRIM(AREAACTUAL)) > 0
    SELECT &AreaActual
ENDIF


Return .t.

*--------------------------------
function PFA_CtrEst()
*--------------------------------
* CONTROL DE ESTADO NORMAL DE FUNCIONAMIENTO de la impresora

*************************************************
#ifdef PF_SINFISCAL
   RETURN .T.
#endif
************************************************



do PF_IniciarSalida
if .not. PF_EnviarComando( PH_Estado )
   do pfa_mostrar_datos
   PFA_MueMen(' Error: 35 Al solicitar estado.',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return
endif
xmen1=PF_MensajeEstado(1)
xmen2=PF_MensajeEstado(2)
*if .not. ( at('0080',xmen1) > 0 .and. ( at('0200',xmen2) > 0 .or. at('0600',xmen2) > 0 ) )
if .not. ( at('C080',xmen1) > 0 .and. ( at('0000',xmen2) > 0 .or. at('0600',xmen2) > 0 ) )
    PFA_Messagebox('Atenci¢n!!!','El estado de la impresora no es normal. Controle en Util >  Parametros generales > Imp. Fiscal')
*								  123456789 123456789 123456789 123456789 123456789 1234567890
	Return .f.
endif
Return .t.

*--------------------------------
FUNCTION PFA_HayCierre()
*--------------------------------
do PF_IniciarSalida
nada = PF_AgregaCampoSalida( 'A' )            && Consulta los numeros de cbte
if .not. PF_EnviarComando( PF_Estado )
   if isbit(PF_DatoRecibido[2], 12 )  .and. (.not. isbit(PF_DatoRecibido[2], 7 )) // Se requiere cierre de D¡a
      PFA_Messagebox('Atenci¢n!','Se procedera a realizar el Cierre Z')
      do PF_IniciarSalida
      Continuar = PF_AgregaCampoSalida("Z")
      Continuar = PF_AgregaCampoSalida("P")
      nada= PF_EnviarComando( PF_CierreZ )
   endif
endif
RETURN


*--------1234567890------------------------
FUNCTION PFA_BusNBlock(P_Desc)
*--------------------------------
LOCAL ACTUAL && color actual antes de ingresar a este proceso
LOCAL OP

xDesc=UPPER(P_Desc)
xpos1=AT('BLOCK',xDesc)
xpos2=AT('MOTOR',xDesc)
if .not. (xpos1 > 0 .and. xpos2 > 0)
	Return PF_DEL
endif

ACTUAL = SETCOLOR()
SAVE SCREEN
SET COLOR TO
nada = ' '
@ 9,19 CLEAR TO 15,61
SET COLOR TO I
@ 9,19 TO 15,61 DOUBLE
xblock=space(20)
SET COLOR TO
while .t.
	@ 11, 21 say 'Ingrese el nro de block'
	@ 12, 21 say P_Desc
	@ 13, 21 GET xblock
	READ
*                    ACEPTA  REINGRESA  CANCELA
	@ 14, 21 prompt ' ACEPTA '
    @ 14, 29 prompt ' REINGRESA '
    @ 14, 40 prompt ' CANCELA '
	MENU TO OP
    IF LASTKEY()=27
   		RESPUESTA =PF_DEL
   		EXIT
	ENDIF
	IF OP = 1
	    RESPUESTA =alltrim(xblock)
	ELSE
		IF OP = 2
			LOOP
		ELSE
			RESPUESTA = PF_DEL
		ENDIF
	ENDIF
	EXIT
ENDDO
SETCOLOR(ACTUAL)
RESTORE SCREEN
RETURN RESPUESTA

PROCEDURE PFA_NCR
ERROR=0
error = PFA_ImpNCredito(1,'NC',42,1)
Return


*------------------------------------------
FUNCTION PFA_CtrCui(Pnumero)
*------------------------------------------
* Controla el digito verificador del CUIT
Mnumero=ALLTRIM(PFA_arregla_cuit(Pnumero))
if len(Mnumero)<> 11
  Return .f.
endif

tira = '5432765432'
suma=0
for i=1 to 10
  suma = suma + val(substr(Mnumero,I,1)) * val(substr(tira,I,1))
end if
resultado = int(suma / 11)
modulo = suma - ( resultado * 11 )
digito = 11 - modulo
if digito <> val(substr(Mnumero,11))
	Return .f.
else
	Return .t.
endif








*--HASAR-----------------------------------
FUNCTION Enviar
*------------------------------------------

PARAMETERS String

PRIVATE Result, StatPrn, continuar

continuar = .t.
? "Comm: " + String

* Si la funcion MandaPaq retorna un numero menor que cero, retorna.
* Esto puede ser por un problema de comunicaciones con el impresor.

StatPrn = MandaPaq (Handler, String)

*? " StatPrn = " + alltrim (str(StatPrn))

IF ( StatPrn < 0 )
	DO WHILE StatPrn = -9
		*? "Impresora Ocupada"
		String = "¡"	// consulta de estado intermedio
		StatPrn = MandaPaq (Handler, String)
		IF LASTKEY() = 27
			*? "Fin de la consulta intermedia"
			*RETURN -9
			exit
		ENDIF
		inkey (1)
	ENDDO
	*? "Error enviando el comando"
	*RETURN -1
ENDIF

* Levanta la respuesta.
Result = Respuesta (Handler)


*<< ACTUALIZA EL STRING RECIBIDO
PF_StringRecibido = Result

*<< ACTUALIZA EL VECTOR DE CAMPOS RECIBIDOS
DO PF_LeeCamposRecibidos

*pfa_messagebox(PF_StringRecibido,'')

*? "Resp: " + Result

* Analiza si existe algun error.
if GetErrors (Result) =  .t.
	Continuar = .t.
else
	Continuar = .f.
endif

RETURN Continuar

****
* FUNCTION GetErrors
*
* Esta funcion levanta la respuesta del printer e imprime en
* el mensaje de error si es que existe.
****

*--HASAR-1234567890------------------------
FUNCTION GetErrors
*------------------------------------------

PARAMETERS Resp

PRIVATE Origen, OffsetSep, i, c, continuar

DECLARE FiscalErrors [16]
DECLARE PrinterErrors[16]

cotinuar = .t.

FiscalErrors[1] = 	"Error en chequeo de memoria fiscal"
FiscalErrors[2] = 	"Error en chequeo de la memoria de trabajo"
FiscalErrors[3] = 	"Carga de bateria baja"
FiscalErrors[4] = 	"Comando desconocido"
FiscalErrors[5] = 	"Datos no validos en un campo"
FiscalErrors[6] = 	"Comando no valido para el estado fiscal actual"
FiscalErrors[7] = 	"Desborde del total"
FiscalErrors[8] = 	"Memoria fiscal llena"
FiscalErrors[9] = 	"Memoria fiscal a punto de llenarse"
FiscalErrors[10] = 	""
FiscalErrors[11] = 	""
FiscalErrors[12] = 	"Error en ingreso de fecha"
FiscalErrors[13] = 	"Recibo fiscal abierto"
FiscalErrors[14] = 	"Recibo abierto"
FiscalErrors[15] = 	"Factura abierta"
FiscalErrors[16] = 	""

PrinterErrors[1]  = ""
PrinterErrors[2]  = ""
PrinterErrors[3]  = "Error de Impresora"
PrinterErrors[4]  = "Impresora Offline"
PrinterErrors[5]  = "Falta papel del diario"
PrinterErrors[6]  = "Falta papel de tickets"
PrinterErrors[7]  = "Buffer de Impresora lleno"
PrinterErrors[8]  = ""
PrinterErrors[9]  = ""
PrinterErrors[10] = ""
PrinterErrors[11] = ""
PrinterErrors[12] = ""
PrinterErrors[13] = ""
PrinterErrors[14] = ""
PrinterErrors[15] = ""
PrinterErrors[16] = ""

Origen = 0

OffsetSep = AT ( CHR(28), Resp )

* Convierte en hexa el status del impresor
PrinterStatus = HexaToInt (SUBSTR ( Resp, Origen, OffsetSep - 1))

IF PrinterStatus < 0
	continuar = .f.
	RETURN continuar
ENDIF

Origen = OffsetSep

* Analiza los bits comenzando del menos significativo
* FOR i = 1 TO 16
* 	IF ( INT (PrinterStatus % 2) == 1 )
* 		IF ( LEN (PrinterErrors[i]) > 0 )
* 			? "PrinterStatus: " + PrinterErrors[i]
* 		ENDIF
* 	ENDIF
* 	PrinterStatus = PrinterStatus / 2
* NEXT

OffsetSep = AT ( CHR(28), SUBSTR (Resp, Origen + 1) )

IF OffsetSep == 0
 	OffsetSep = LEN(Resp)
ENDIF

* Convierte en hexa el status fiscal
FiscalStatus = HexaToInt (SUBSTR (Resp, Origen + 1, OffsetSep - 1))

IF FiscalStatus < 0
 	continuar = .f.
 	RETURN continuar
ENDIF

* * Analiza los bits comenzando del menos significativo
* FOR i = 1 TO 16
* 	IF ( INT (FiscalStatus % 2) == 1 )
* 		IF ( LEN (FiscalErrors[i]) > 0 )
* 			? "FiscalStatus: " + FiscalErrors[i]
* 		ENDIF
* 	ENDIF
* 	FiscalStatus = FiscalStatus / 2
* NEXT

RETURN continuar

*--HASAR-1234567890------------------------
* FUNCTION HexaToInt
*
* Esta funcion convierte un numero hexadecimal en su equivalente
* en binario.
****

*--HASAR-----------------------------------
FUNCTION HexaToInt
*------------------------------------------
PARAMETERS HexValue

PRIVATE i, Value, Status

Status = 0

FOR i = 4 TO 1 STEP -1

	S = SUBSTR(HexValue, i, 1)

	Value = ASC (S)

	IF ( Value >= ASC("A") .AND. Value <= ASC("F") )

		Value = Value - ASC("A") + 10

	ELSEIF ( Value >= ASC("a") .AND. Value <= ASC("f") )

		Value = Value - ASC("a") + 10

	ELSEIF ( Value >= ASC("0") .AND. Value <= ASC("9") )

		Value = Value - ASC("0")

	ELSE
		? "HexaToInt: Numero hexadecimal incorrecto: " + HexValue
		RETURN -1
	ENDIF

	Status = Status + Value * (16 ** ( 4 - i ))

NEXT

RETURN Status

