* **********************
* Empresa : ALas Software
* Autor   : A.U.S. Alfredo Lascano
* Fecha   : 01/07/2001
*****************************************************
*                                                   *
* Modulo de impresi¢n de comprobantes fiscales      *
*                                                   *
*****************************************************
**********************
* Surround Versioning
**********************
* $Author: admin$
* $Date: Sábado, 19 de Noviembre de 2005 10:25:27$
* $File: PFISCAL.PRG$
* $Revision: 1.3$
* $Log$
* admin - Sábado, 19 de Noviembre de 2005 10:25:27
* Version con la primera instalación del Estadistico de ventas con separación de productos en dolares.
* admin - Martes, 08 de Noviembre de 2005 1:35:11
* Versión preliminar de Nuevo estad¡stico de ventas
* admin - Domingo, 06 de Noviembre de 2005 10:02:33
* Versión modificación de las cuatro arrobas en el procedimiento a modificar
* admin - Sábado, 05 de Noviembre de 2005 7:27:06
* Alfredo - sábado, 21 de febrero de 2004 7:40:39
* ISSUE NRO: 001
*FECHA: 21/11/2004
*SOLICITADO POR: Ricardo
*DESCRIPCIÓN: Cambiar la leyenda que aparece en los presupestos referida a dolares.
* Alfredo - sábado, 20 de diciembre de 2003 7:33:10
* 1) Modificaciones a mostrar moneda de art¡culos en la facturación
* 2) Agregue la eliminación de facturas de utilidad negativa
* Alfredo - sábado, 11 de octubre de 2003 10:47:59
* Agregue el costo en listado comparativo de stock PFA_ImpComparativo
* Alfredo - sábado, 04 de octubre de 2003 10:10:55
* Corrección de comentarios en las leyendas de versionado
* Alfredo - sábado, 04 de octubre de 2003 9:48:41
* Modificaciónes:
* 1. Restricción de acceso por usuario a la estadistica de ventas, cierre de stock y listado comparativo de stock
* admin - sábado, 27 de septiembre de 2003 4:31:42
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
*   FUNCTION PFA_CtrCui(Pnumero) Controla el número de CUIT
*   FUNCTION PFA_CtrSuc			Controla la sucursal con la que está trabajando
*	procedure PFA_mostrar_datos
*   PROCEDURE PFA_CanCbte		Cancela un comprobante fiscal
*   PROCEDURE PFA_CanCNF		Cancela un comprobante no fiscal
*   PROCEDURE PFA_CIEX				realiza un cierre X
*   PROCEDURE PFA_CIEZ				realiza un cierre Z
*   PROCEDURE PFA_CanCbte           cancelar un comprobante
*   PROCEDURE PFA_Est1              muestra el estado de la impresora
*   PROCEDURE PFA_Est2              muestra el estado fiscal
*   PROCEDURE PFA_ImpTaller         Imprime un listado de comprobantes marcando aquellos que son de taller
*   procedure pfa_panimpta
*   PROCEDURE PFA_ImpEstad              Imprime la estad¡stica de ventas, previa actualización del 
* 								archivo ctcalf para cada sucursal
*   procedure pfa_panESimp
*   procedure pfa_DescTaller
*   procedure pfa_dtmodif
*   procedure pfa_telim
*   procedure pfa_tmodif
*   procedure pfa_tmostrar
*   procedure pfa_pantaller
*   PROCEDURE PFA_TALLER
*   PROCEDURE PFA_UtiNeg
*   PROCEDURE PFA_ActPre	   Actualizar precios por CD
*   FUNCTION PFA_ActDol(ValDol)  Actualizar el valor dolar para Actualización de precios por CD
*   Procedure stkalfa            Nuevo programa para carga de stock
*   FUNCTION PFA_GraLin(Linea)  Graba una linea en la base del listado de nc por descuento
*   PROCEDURE PFA_ACTCLAVE       Nuevo programa de actualización de claves
*   Function  PFA_Cript(__xValue, __xPassWord)  Encripta un string en base a una clave
*   Function  PFA_DeCript(__xValue, __xPassWord) Desencripta un string en base a una clave
*   Function  PFA_SumChars(__cCharStr) Usado en las funciones de encriptacion


#define PF_DEBUG
*#define PF_SINFISCAL
*#undef PF_SINFISCAL
#define fiscal_log
#undef PF_DEBUG

#include "pfiscal.ch"
#include "Fileio.ch"
#include "inkey.ch"


*--------------------------------
function PFA_ImpFiscal(P_Cliente,P_TipoCbte,P_NroCbte,P_Sucursal)
*--------------------------------
* Codigo 100
* Imprime un comprobante fiscal
*PFA_MESSAGEBOX('NROSUCM',STR(NROSUCM))
*PFA_MESSAGEBOX('SUCM',STR(SUCM))

IF MARC='X'
	Return .T.
ENDIF

error = .f.


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
public MdescAdi1,MdescAdi2,MdescAdi3,MdescAdi4,dtlinea0,dtlinea1,dtlinea2,dtlinea3,dtlinea4
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
* Las observaciones se cargan en las lineas 4 y 5 del HEADER
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
es_contado = .f.
DO WHILE .NOT. EOF() .and. k <= 4
    tira = auxil->linea
    if len(alltrim(tira)) > 0
        ** setea el header 11, 12, 13, 14
        do case
            case k+10 = 11
                if at('CONTADO',tira) = 0
	            	tira = tira + '                 Firma:............'
	            else
	            	es_contado = .t.
	            endif
            case k+10 = 12
                if .not. es_contado
	            	tira = tira + '            Aclaracion:............'
	            endif
        endcase
        continuar=SeteoHeader( k+10 ,tira)
        k = k + 1
        wj = k
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
wj = wj - 1 + 10
leyenda='Importe en dolares estadounideses pagaderos en $ o U$S al cambio BNA.'
continuar = SeteoHeader( wj ,leyenda )



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
   PFA_Messagebox('Error: 4 ','Cliente no encontrado: '+P_tipoCbte+str(P_nrocbte))
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
   PFA_Messagebox('Error: 5 ','Provincia no encontrada: '+P_tipoCbte+str(P_nrocbte))
   Return .f.
endif
Mnomprovin=alltrim(provin->nom)
Mdomici1 = left(alltrim(cliente->dom),43)
Mdomici2 = left(alltrim('(' + str(cliente->cpo) + ') '+ ;
           alltrim(cliente->loc) + ' ' +Mnomprovin),43)
Mdomici3 = PF_DEL

* HASAR env¡a datos de clientes
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
    xclave=str(P_cliente,4)+str(P_NroCbte,8)
else
    use notalf index nnotalf alias items shared
    xclave=str(P_cliente)+str(P_NroCbte)
endif

xnroitem=0


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
		if .not. es_nd_cheque
		  if P_TipoCbte = 'FC'
		     IF items->fec <> fecm .or. items->com <> P_TipoCbte
*		     	 pfa_messagebox('Salteando',str(items->cod,4)+'|'+items->com+'|'+str(items->suc,4)+'|'+str(items->nro,8)+'|'+dtoc(items->fec))
		     	 SKIP
    		     if xclave # str(items->cod,4)+str(items->nro,8)
	       		       exit
        		 else
        		 	loop
		         endif
		     ENDIF
    	  else
    	  	IF items->suc <> sucm
*		     	 pfa_messagebox('Salteando',str(items->cod,4)+'|'+str(items->nro,8)+'|'+str(items->suc,4))
    		  	 skip
        		 if xclave # str(items->cod)+str(items->nro)
	        	       exit
	        	 else
	        	 	loop
	    	     endif
		    endif
		  endif
		endif

      xnroitem = xnroitem + 1

      MdescAdi1 = PF_DEL
      MdescAdi2 = PF_DEL
      MdescAdi3 = PF_DEL
      MdescAdi4 = PF_DEL
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
*		  	  recargo1 = ROUND((items->pre * items->de1 / 100 ),2)
		  	  recargo1 = items->pre * items->de1 / 100
		  ENDIF
		  Mprecionuevo = items->pre + recargo1
		  IF items->de2 > 0
*	          recargo2 = ROUND((items->pre * items->de2 / 100 ),2)
	          recargo2 = Mprecionuevo * items->de2 / 100
		  ENDIF
		  Mprecio = int((ROUND(items->pre + recargo1 + recargo2,2)) * 100 ) / 100

*		  pfa_messagebox(str(recargo1,10,4)+' '+str(Mprecio,10,4),str(recargo2,10,4)+' '+str(items->pre,10,4))


          Mprecio2=alltrim(str(Mprecio))
      else
		  if es_nd_cheque = .f.
	          Mdescripcion=alltrim(items->des)
    	      Mprecio=int(ROUND(items->imp,2) * 100)/100
        	  *Mprecio=Mprecio*100
	          Mprecio2=ltrim(rtrim(str(Mprecio)))
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
    	      Mprecio=int(ROUND(items->imp,2) * 100)/100
        	  *Mprecio=Mprecio*100
	          Mprecio2=ltrim(rtrim(str(Mprecio)))
    	      mcanti2='1'
			  Mtas_IVA = "0"
		  endif
      endif

	  * Carga el nro de block de motor
	  Mdescripcion=PFA_Limpiar(Mdescripcion)

   	 MNroBlock = PFA_BusNBlock(Mdescripcion)
   	 if MNroBlock <> PF_DEL
   	 	Mdescripcion = Mdescripcion +MNroBlock
   	 endif
******************** ARMADO DE LA DESCRIPCION *****************************9999
	Mdescripcion = alltrim(Mdescripcion)

	if at("TALLER",Mdescripcion) > 0
		do pfa_DescTaller
*		pfa_messagebox(Mdescripcion,mdescadi1)
*		pfa_messagebox(mdescadi2,mdescadi3)
*		pfa_messagebox(mdescadi4,"")
	else
    	if len(Mdescripcion) > 50
    		MdescAdi1 = Left(Mdescripcion,50)
    		Mdescripcion = Substr(Mdescripcion,51)
        	if len(Mdescripcion) > 50
        		MdescAdi2 = Left(Mdescripcion,50)
        		Mdescripcion = Substr(Mdescripcion,51)
        	endif
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

	 if len(alltrim(MdescAdi3)) > 0 .and. MdescAdi3 <> PF_DEL
		do PF_IniciarSalida
      	nada = PF_AgregaCampoSalida( PFA_Limpiar(MdescAdi3) )
        if .not. PF_EnviarComando( PH_FCTextoFiscal )
           do pfa_mostrar_datos
           PFA_MueMen(' Error: 10 Al enviar Descripcion Adic. 3',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
           Return
        endif
	 endif

	 if len(alltrim(MdescAdi4)) > 0 .and. MdescAdi4 <> PF_DEL
		do PF_IniciarSalida
      	nada = PF_AgregaCampoSalida( PFA_Limpiar(MdescAdi4) )
        if .not. PF_EnviarComando( PH_FCTextoFiscal )
           do pfa_mostrar_datos
           PFA_MueMen(' Error: 10 Al enviar Descripcion Adic. 4',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
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
          PFA_MueMen('Error: 11 en impresi¢n de item',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
          PFA_CerrDB(xarea)
          Return .f.
      endif

      select items
      skip
	  if P_TipoCbte = 'FC'
         if xclave # str(items->cod,4)+str(items->nro,8)
               exit
         endif
      else
         if xclave # str(items->cod)+str(items->nro)
               exit
         endif
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
        Mprecio=int(ROUND(xcargos->importe,2) * 100)/100
        *Mprecio=Mprecio*100
    	Mprecio2=ltrim(rtrim(str(Mprecio)))
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
            PFA_MueMen(' Error: 12 en impresi¢n de item',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
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
*	       nada = PF_AgregaCampoSalida("B")
	       nada = PF_AgregaCampoSalida(MBase_Total)
		ELSE
	       nada = PF_AgregaCampoSalida("T")
		ENDIF

       if .not. PF_EnviarComando( PH_FCRecargos )
          do PFA_mostrar_datos
          PFA_MueMen('Error: 13 en impresi¢n de recargos',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
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
            PFA_MueMen('Error: 14 en impresi¢n de pagos',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
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
    PFA_MueMen('Error: 15 en al cerrar el comprobante',alltrim(PF_MensajeEstado( PF_ModuloImpresor ))+CHR(13)+CHR(10)+ alltrim(PF_MensajeEstado( PF_ModuloFiscal )))
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
wj = 0
DO WHILE .NOT. EOF() .and. k <= 4
    tira = auxil->linea
    if len(alltrim(tira)) > 0
        ** setea el header 11, 12, 13, 14
        continuar=SeteoHeader( k+10 ,tira)
        k = k + 1
        wj = k
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
wj = wj - 1 + 10
leyenda='Importe en dolares estadounideses pagaderos en $ o U$S al cambio BNA.'
continuar = SeteoHeader( wj ,leyenda )

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
   PFA_Messagebox('Error: 4 ','Cliente no encontrado: '+P_tipoCbte+str(P_nrocbte))
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
   PFA_Messagebox('Error: 5 ','Provincia no encontrada: '+P_tipoCbte+str(P_nrocbte))
   Return .f.
endif
Mnomprovin=alltrim(provin->nom)
Mdomici1 = left(alltrim(cliente->dom),43)
Mdomici2 = left(alltrim('(' + str(cliente->cpo) + ') '+ ;
           alltrim(cliente->loc) + ' ' +Mnomprovin),43)
Mdomici3 = PF_DEL

* HASAR env¡a datos de clientes                                            ** N.CREDITO **
do PF_IniciarSalida
Continuar = PF_AgregaCampoSalida(MNom_Comprador)
Continuar = PF_AgregaCampoSalida(MNro_doc)
Continuar = PF_AgregaCampoSalida(Miva_comprador)
Continuar = PF_AgregaCampoSalida(MTip_doc)
Continuar = PF_AgregaCampoSalida(left(alltrim(Mdomici1)+ ' ' +alltrim(Mdomici2),50))
if .not. PF_EnviarComando( PH_DatosCli )
   do pfa_mostrar_datos
   PFA_MueMen('Error: 6 Al enviar Datos de cliente',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return
endif

* HASAR Inserta el número de comprobante asociado                          ** N.CREDITO **
do PF_IniciarSalida
nada = PF_AgregaCampoSalida( '1' )          && Número de l¡nea ( 1 o 2)
nada = PF_AgregaCampoSalida( alltrim(str(P_NroCbte)) )          && Números de documento
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
nada = PF_AgregaCampoSalida( alltrim(str(P_NroCbte)) )   && Numero de comprobante
if .not. PF_EnviarComando( PH_AbreDNFH )
   Do PFA_mostrar_datos
   PFA_MueMen('Error: 7 en apertura de cbte',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
   Return .f.
endif

*****ITEMS                                                                 ** N.CREDITO **
sele 54
use stoalf index cstoalf alias productos shared
sele 55

DO CASE
CASE MOTM = ' '
    use favalf index cfavalf alias items shared
	xclave=str(P_cliente,4)+str(P_NroCbte,8)
CASE MOTM = 'A'
	    use notalf index nnotalf alias items shared
		xclave=str(P_cliente)+str(P_NroCbte)
*	    PFA_MESSAGEBOX('CLAVE','>'+xclave+'<'+str(len(xclave)))
CASE MOTM = 'F'
	    use ncredito alias items
OTHERWISE
       PFA_Messagebox('Atención!','Tipo de nota de Cr‚dito no prevista.')
       PFA_CerrDB(xarea)
       Return .f.
ENDCASE

xnroitem=0

if MOTM = ' ' .or. MOTM = 'A'
	SEEK xclave
else
	GO TOP
	DO WHILE .NOT. EOF() .AND. COD<>'CUE'
		SKIP
	ENDDO
endif

if (MOTM=' ' .OR. MOTM = 'A') .AND.  .NOT. found()
       PFA_Messagebox('Atención!','Detalle de Nota de credito NO ENCONTRADA')
       PFA_CerrDB(xarea)
       Return .f.
endif

Mhay_ncredito = .f.
Mtot_ncredito = 0
Mcan_ncredito = 0
do while .not. eof()
		Mind_imprimir=.t.
		DO CASE
		  CASE MOTM = ' '
		     IF items->fec <> fecm .or. items->com <> P_TipoCbte
*		     	 pfa_messagebox('Salteando',str(items->cod,4)+'|'+items->com+'|'+str(items->suc,4)+'|'+str(items->nro,8)+'|'+dtoc(items->fec))
		     	 SKIP
    		     if xclave # str(items->cod,4)+str(items->nro,8)
	       		       exit
        		 else
        		 	loop
		         endif
		     ENDIF
    	  CASE MOTM = 'A'
    	  	IF items->suc <> sucm
*		     	 pfa_messagebox('Salteando',str(items->cod,4)+'|'+str(items->nro,8)+'|'+str(items->suc,4))
    		  	 skip
        		 if xclave # str(items->cod)+str(items->nro)
	        	       exit
	        	 else
	        	 	loop
	    	     endif
		      endif
	   ENDCASE
      xnroitem = xnroitem + 1

      MdescAdi1 = PF_DEL
      MdescAdi2 = PF_DEL
      MdescAdi3 = PF_DEL
      MdescAdi4 = PF_DEL
      if MOTM = ' '
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
    	  Mprecio = int((ROUND(items->pre + recargo1 + recargo2,2)) * 100 ) / 100

          Mprecio2=alltrim(str(Mprecio))

    	  * Carga el nro de block de motor                                        ** N.CREDITO **
    	  Mdescripcion=PFA_Limpiar(Mdescripcion)

       	 MNroBlock = PFA_BusNBlock(Mdescripcion)
       	 if MNroBlock <> PF_DEL
       	 	Mdescripcion = Mdescripcion +MNroBlock
       	 endif
************************** ARMADO DE LA DESCRIPCION *******************8888
    	Mdescripcion = alltrim(Mdescripcion)

    	if at("TALLER",Mdescripcion) > 0
    		do pfa_DescTaller
    	else
        	if len(Mdescripcion) > 50
        		MdescAdi1 = Left(Mdescripcion,50)
        		Mdescripcion = Substr(Mdescripcion,51)
            	if len(Mdescripcion) > 50
            		MdescAdi2 = Left(Mdescripcion,50)
            		Mdescripcion = Substr(Mdescripcion,51)
            	endif
        	endif
		end if
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
		if len(alltrim(MdescAdi3)) > 0 .and. MdescAdi3 <> PF_DEL
			do PF_IniciarSalida
      		nada = PF_AgregaCampoSalida( PFA_Limpiar(MdescAdi3) )
	        if .not. PF_EnviarComando( PH_FCTextoFiscal )
    	       do pfa_mostrar_datos
	           PFA_MueMen(' Error: 10 Al enviar Descripcion Adic. 3',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
	           Return
    	    endif
		endif

		if len(alltrim(MdescAdi4)) > 0 .and. MdescAdi4 <> PF_DEL
			do PF_IniciarSalida
      		nada = PF_AgregaCampoSalida( PFA_Limpiar(MdescAdi4) )
	        if .not. PF_EnviarComando( PH_FCTextoFiscal )
    	       do pfa_mostrar_datos
	       	   PFA_MueMen(' Error: 10 Al enviar Descripcion Adic. 4',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
	           Return
    	    endif
		endif
	  else
          if MOTM = 'A'
	          Mdescripcion=alltrim(items->des)
    	      Mprecio=int(ROUND(items->imp,2) * 100)/100
        	  *Mprecio=Mprecio*100
                  Mprecio2=alltrim(str(Mprecio))
    	      mcanti2='1'
			  if items->gra <> 'S'
 			  	Mtas_IVA = "0"
			  endif
              Mtip_Monto = 'M'
          else      					&& MOTM='F'
	          Mdescripcion=substr(items->linea,1,42)
	          zimporte=0
	          Simporte=alltrim(substr(items->linea,45))
*	          pfa_messagebox('importe',Simporte)
	          Spos=AT(',',Simporte)
	          Simporte=substr(Simporte,1,Spos - 1)+'.'+substr(Simporte,Spos+1)
	          zimporte=val(Simporte)
*	          pfa_messagebox('importe',str(zimporte))
    	      Mprecio=int(ROUND(zimporte,2) * 100)/100
              Mprecio2=alltrim(str(Mprecio))
    	      mcanti2='1'
			  ********************************************************************************
    	      * si es una nota de credito guardo los datos, no imprimo para imprimirlo despues
    	      * del bucle
			  ********************************************************************************
              if at(' NC ',Mdescripcion) > 0
              		Mcan_ncredito = Mcan_ncredito + 1
              		if Mcan_ncredito = 1
	              		MXdescripcion = Mdescripcion
	              	else
	              		MXdescripcion = 'Descuentos sobre Nota de credito ( ' + str(Mcan_ncredito,2,0) + ' )'
	              	endif
              		Mtot_ncredito = Mtot_ncredito + val(alltrim(Mprecio2))
              		MXprecio2     = alltrim(str(Mtot_ncredito))
              		MXcanti2      = Mcanti2
              		MXtas_iva     = Mtas_iva
              		MXBase_total  = MBase_total
              		Mind_imprimir = .f.
              		Mhay_ncredito = .t.
              endif
              Mtip_Monto = 'M'
          endif
      endif
																	      ** N.CREDITO **
      if Mind_imprimir
          do PF_IniciarSalida
          nada = PF_AgregaCampoSalida( left(PFA_Limpiar(Mdescripcion),50) )  && Descripci¢n
          nada = PF_AgregaCampoSalida( Mcanti2 )       && Cantidad  3,000
          nada = PF_AgregaCampoSalida( Mprecio2 )      && Precio   12,50
          nada = PF_AgregaCampoSalida( Mtas_iva)       && Tasa IVA 21,00%
          nada = PF_AgregaCampoSalida( Mtip_monto )    && Monto vendido de mercaderia
          nada = PF_AgregaCampoSalida( '0' )   	       && Tasa de Ajuste Imp.Int.
          nada = PF_AgregaCampoSalida( '' )            && Display
          nada = PF_AgregaCampoSalida( MBase_Total )   && T: precio total; otro carácter: precio base

          if .not. PF_EnviarComando( PH_FCItemDeLinea )
              do PFA_mostrar_datos
              PFA_MueMen('Error: 11 en impresi¢n de item',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
              PFA_CerrDB(xarea)
              Return .f.
          endif
      endif

      select items
      skip
      do case
	  case MOTM = ' '
         if xclave # str(items->cod,4)+str(items->nro,8)
               exit
         endif
      case MOTM = 'A'
         if xclave # str(items->cod)+str(items->nro)
               exit
         endif
      CASE MOTM = 'F'
      	 IF items->cod <> 'CUE'
      	 	exit
      	 end if
      ENDCASE
enddo
if MHay_ncredito
*    do PF_IniciarSalida
*    nada = PF_AgregaCampoSalida( left(PFA_Limpiar(MXdescripcion),50) )  && Descripci¢n
*    nada = PF_AgregaCampoSalida( MXcanti2 )       && Cantidad  3,000
*    nada = PF_AgregaCampoSalida( MXprecio2 )      && Precio   12,50
*    nada = PF_AgregaCampoSalida( MXtas_iva)       && Tasa IVA 21,00%
*    nada = PF_AgregaCampoSalida( 'm' )            && Monto vendido de mercaderia
*    nada = PF_AgregaCampoSalida( '0' )     	      && Tasa de Ajuste Imp.Int.
*    nada = PF_AgregaCampoSalida( '' )             && Display
*    nada = PF_AgregaCampoSalida( MXBase_Total )   && T: precio total; otro carácter: precio base
*
*    if .not. PF_EnviarComando( PH_FCDtoLinea )
*        do PFA_mostrar_datos
*        PFA_MueMen('Error: 11 en impresi¢n de item',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
*        PFA_CerrDB(xarea)
*        Return .f.
*    endif

    do PF_IniciarSalida
    nada = PF_AgregaCampoSalida( left(PFA_Limpiar(MXdescripcion),50) )  && Descripci¢n
    nada = PF_AgregaCampoSalida( MXprecio2 )      && Precio   12,50
    nada = PF_AgregaCampoSalida( 'm' )            && Monto vendido de mercaderia
    nada = PF_AgregaCampoSalida( '0' )     	      && Tasa de Ajuste Imp.Int.
    nada = PF_AgregaCampoSalida( MXBase_Total )   && T: precio total; otro carácter: precio base

   if .not. PF_EnviarComando( PH_FCRecargos )
        do PFA_mostrar_datos
        PFA_MueMen('Error: 11 en impresi¢n de item',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
        PFA_CerrDB(xarea)
        Return .f.
    endif
endif

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
      PFA_MueMen('Error: 13 en impresi¢n de recargos',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
      PFA_CerrDB(xarea)
      Return .f.
   ENDIF
   skip
ENDDO


******CIERRE COMPROBANTE                                                   ** N.CREDITO **
do PF_IniciarSalida
if .NOT. PF_EnviarComando( PH_CierDNFH )
    do PFA_mostrar_datos
    PFA_MueMen('Error: 15 en al cerrar el comprobante',alltrim(PF_MensajeEstado( PF_ModuloImpresor ))+CHR(13)+CHR(10)+ alltrim(PF_MensajeEstado( PF_ModuloFiscal )))
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
use obser alias xobser
IF .NOT. EOF()
	delete all
	pack
ENDIF
USE

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
    PFA_MueMen('Error: 19 al inicio comprobante no fiscal',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeEstado( PF_ModuloFiscal ))
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
LOCAL ACTUAL,xpantalla && color actual antes de ingresar a este proceso
ACTUAL = SETCOLOR()
SAVE SCREEN TO xpantalla
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
*@ 17, 11 get nada
*READ
inkey(0)  && espera que se presione una tecla y continua

SETCOLOR(ACTUAL)
RESTORE SCREEN FROM xpantalla
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
if .not. lower(dbf(50))='param'
    select 50
    use param
    go top
else
    select 50
    go top
endif

* OJO TOCO ACA PARA GENERAR LOS 2 COMPILADOS
* SI PF_SINFISCAL ESTA EN 0 ESTA HABILITADA LA IMPRESORA FISCAL
*SI ES MAYOR A CERO ESTA SIN LA IMPRESORA FISCAL

*PF_SINFISCAL = param->tiempomax



*###########yaya########### IMPRESORA FISCAL ########################
*PF_SINFISCAL = 0    &&CON IMPRESORA FISCAL
PF_SINFISCAL = 1    &&SIN IMPRESORA FISCAL




* VARIABLE GLOBAL PARA LA LEYENDA DOLAR. USADO EN FINALF PARA PONER LA LEYENDA EN LOS 
* PRESUPUESTOS
PF_LEYENDADO = param->leyendadol

if PF_SINFISCAL > 0
   RETURN .T.
endif
************************************************

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
        PFA_Messagebox('Sr. operador','No se pudo detectar el controlador fiscal.','Controle las conexiones.')
        Return .F.
    else
        PFA_messagebox('Sr. operador','Impresora Fiscal habilitada')
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

If .NOT. PF_SINFISCAL > 0
		Enviar(PF_StringEnviado)
else
        PF_StringRecibido = '**'
endif
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
if PF_SINFISCAL > 0
   RETURN .T.
endif
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
*mens1='2:'+PF_DatoRecibido[2]+' 3:'+PF_DatoRecibido[3]+' 4:'+PF_DatoRecibido[4]
*mens2='5:'+PF_DatoRecibido[5]+' 7:'+PF_DatoRecibido[7]+' 8:'+PF_DatoRecibido[8]
*pfa_messagebox(mens1,mens2)
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
if PF_SINFISCAL > 0
   RETURN .T.
endif
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

*************************************************
PROCEDURE PFA_ActPre
* Actualización de precios mediante CD
* Necesita de un campo pathActPre en param.dbf donde
* se guarda el path por defecto donde tomar el archivo de 
* precios.
* Objetivo: Actualización de precios de productos originales Mercedes Benz desde CD.
* Caracter¡sticas:
* . Genera un archivo de log con los artículos modificados. Genera uno distinto por cada actualización. Nomenclatura: AAMMDDSS.txt (donde SS es secuencial para el mismo día)
* . Se asume que lo precios están en dolares con lo cual se actualizan tal cual vienen.
* . También actualiza la descripción si la misma está en blanco.
* . También actualiza la fecha de actualización con la fecha de la lista que viene en el primer registro del archivo.
* . Toma de param->pathActPre el path donde va a buscar el archivo dbf que debe tener un campo cod, des y ppu
*************************************************
local PFacA,PFacb, AreaActual, BasePre, BasePre1, BasePre2, Actual, topmenu, ValDol, nValDol, Panta1, nArcLog, Arclog
local contador, nroLog, dFecPre

BasePre2="D:\actprecd.dbf"
BasePre1="actprecd.dbf"
ValDol=1
nroLog = 1
ArcLog="ActPre.txt"
do while .t.
    ArcLog=padl(substr(alltrim(str(year(date())),4,0),3,2),2,'0')+ ;
           padl(alltrim(str(month(date()),2,0)),2,'0')+ ;
           padl(alltrim(str(day(date()),2,0)),2,'0')+ ;
           padl(alltrim(str(nroLog,2,0)),2,'0')+ ;
           +".txt"



     nArcLog = FOPEN(ArcLog,FO_READWRITE+FO_EXCLUSIVE)
     IF nArcLog = -1 
*    	 PFA_MessageBox(ArcLog,'No lo encontro')
        EXIT
	 ELSE
*  	     PFA_MessageBox(ArcLog,'Si lo encontro')

        FCLOSE(nArcLog)
        nroLog++ 
	 ENDIF
enddo

ACTUAL = SETCOLOR()
SAVE SCREEN TO Panta1

SET COLOR TO
@ 5,5 CLEAR TO 18,78
SET COLOR TO I
@ 5,5 TO 18,78 DOUBLE
SET COLOR TO
*     .Procesar.  . Valor Dolar.9999.99. .Salir.


*************************************************************************
* Rescata el path de donde va a leer el archivo con los precios
select 62
use param alias param
BasePre2 = param->pathActPre
use

*************************************************************************
* Controla la existencia del archivo de lista de precios
contador= 1
DO WHILE .T.
			IF contador > 3
				SETCOLOR(ACTUAL)
				RESTORE SCREEN FROM Panta1
				RETURN
 			ENDIF
* 	      IF !FILE(BasePre1) .and. !FILE(BasePre2)
         BasePre1 = "XXX"
 	      IF !FILE(BasePre1) .and. !FILE(BasePre2)
          PFA_MessageBox('El CD no est  preparado o el archivo con la lista de precios no existe.','Controle.') 
					contador = contador + 1
 					LOOP
 	      ELSE
 		      IF FILE(BasePre1) 
 					BasePre = BasePre1
                    PFA_MessageBox('Se encontr¢ el archivo de precios en '+BasePre1,'Ok') 
 				ELSE
 					BasePre = BasePre2
                    PFA_MessageBox('Se encontr¢ el archivo de precios en '+BasePre2,'Ok') 
 				ENDIF
 				EXIT
 			ENDIF
ENDDO


** Actualizar Dolar
*nValDol = PFA_ActDol(ValDol)
**PFA_MESSAGEBOX(VALTYPE(nValDol),TYPE('nValDol'))
*
*IF VALTYPE(nValDol) = 'N'
*    ValDol = nValDol
*ENDIF


DO WHILE .T.
	@ 17,  7 prompt " Procesar "
*	@ 17, 19 prompt " Valor Dolar "+str(ValDol,6,2)
	@ 17, 19 prompt " Salir "
    MENU TO topmenu
*    pfa_messagebox(str(topmenu),'')
    DO CASE
    	CASE topmenu = 0		// ESCAPE
    		SETCOLOR(ACTUAL)
    		RESTORE SCREEN FROM PANTA1
    		return
    	CASE topmenu = 1		// PROCESAR
         IF !FILE(BasePre)
               PFA_MessageBox('El CD no est  preparado o el archivo de transferencia no existe.','Controle') 
					LOOP
	      ELSE
 				EXIT
			ENDIF
*    	CASE topmenu = 2		// VALOR DOLAR
*         * Actualizar Dolar
*         nValDol = PFA_ActDol(ValDol)
**         IF nValDol <> PF_DEL
**         PFA_MESSAGEBOX(VALTYPE(nValDol),TYPE('nValDol'))
*         IF VALTYPE(nValDol) = 'N'
*             ValDol = nValDol
*         ENDIF
*			LOOP
    	CASE topmenu = 2		// SALIR
    		SETCOLOR(ACTUAL)
    		RESTORE SCREEN FROM PANTA1
    		RETURN
    END CASE
ENDDO


* PROCESAR

** Abre archivos
if file(ArcLog)
	   nArcLog = FOPEN(ArcLog,FO_READWRITE+FO_EXCLUSIVE)
else
	   nArcLog = FCREATE(ArcLog,FC_NORMAL)
endif
FSEEK(nArcLog, 0, 2)   &&se posiciona al final del archivo
FWRITE(nArcLog,chr(13)+'***'+dtoc(date())+' '+TIME()+'***'+chr(13))


AreaActual = alias()
select 62
use stoalf alias STO index astoalf, nstoalf, lstoalf, tstoalf

select 63
use &BasePre alias PRE
GO TOP
CONTADOR=0
ArtBuscar=''
DO WHILE !PRE->(EOF())
*	pfa_messagebox('Apertura y cierre de archivos OK',PRE->cod)
   ArtBuscar=PRE->cod
   IF RECNO()=1
       dFecPre = ctod(left(pre->cod,10))
*       pfa_messagebox(dtoc(dFecPre),'>'+left(pre->cod,10)+'<')
   ENDIF

   SELECT STO
   seek ArtBuscar
   if .not. found()
*       PFA_Messagebox('','Articulo no encontrado: '+PRE->cod)
       @ 12, 7 say 'Articulo no encontrado: '+PRE->cod
		 FWRITE(nArcLog,'Articulo no encontrado: '+PRE->cod+chr(13))
	ELSE
       	IF PRE->ppu > 0
*		       PFA_Messagebox('Ant: '+str(STO->pre,8,2)+' Act: '+str(PRE->ppu / ValDol,8,2),'Articulo actualizado: '+PRE->cod)
       @ 12, 7 say 'Articulo actualizado: '+PRE->cod+' Ant: '+str(STO->pre,8,2)+'   Act: '+str(PRE->ppu / ValDol,8,2)
				 CONTADOR=CONTADOR + 1
				 FWRITE(nArcLog,'Act.: '+PRE->cod+'VAnt: '+str(STO->pre,8,2)+' VAct: '+str(PRE->ppu / ValDol,8,2)+chr(13))

			         	 REPLACE STO->pre with PRE->ppu / ValDol
							 REPLACE STO->fec with dFecPre
                      IF !LEN(ALLTRIM(STO->nom)) > 0
								 REPLACE STO->nom with PRE->des
							 ENDIF

			ENDIF
   ENDIF

	SELE PRE
   SKIP
*   IF CONTADOR > 19
*       EXIT
*	ENDIF
ENDDO


SETCOLOR(ACTUAL)
RESTORE SCREEN FROM Panta1

********************
* Cierra archivos
select 62
use 
select 63
use 
IF LEN(ALLTRIM(AREAACTUAL)) > 0
    SELECT &AreaActual
ENDIF
FCLOSE(nArcLog)

Return


****************************************************************
FUNCTION PFA_ActDol(ValDol)
*--------------------------------
LOCAL ACTUAL && color actual antes de ingresar a este proceso
LOCAL OP, pdol

ACTUAL = SETCOLOR()
SAVE SCREEN TO pdol
SET COLOR TO
nada = ' '
@ 9,19 CLEAR TO 15,61
SET COLOR TO I
@ 9,19 TO 15,61 DOUBLE
xvaldol=valdol
SET COLOR TO
while .t.
	@ 11, 21 say ' Ingrese nuevo valor del dolar:'
	@ 12, 21 GET xvaldol
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
	    RESPUESTA = xvaldol
       EXIT
	ELSE
		IF OP = 2
			LOOP
		ELSE
			RESPUESTA = PF_DEL
		ENDIF
	ENDIF
	EXIT
ENDDO
*PFA_MESSAGEBOX(VALTYPE(RESPUESTA),STR(RESPUESTA))
SETCOLOR(ACTUAL)
RESTORE SCREEN FROM pdol
RETURN RESPUESTA
****************************************************************

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
if modulo = 0
	digito = 0
else
	digito = 11 - modulo
endif
*pfa_messagebox('>'+PNumero+'<','Calculado: '+str(digito))
if digito <> val(substr(Mnumero,11))
	Return .f.
else
	Return .t.
endif


*------------------------------------------
FUNCTION PFA_CtrSuc()
*------------------------------------------
* Controla la sucursal con la que va a trabajar
sele 50
use auxalf alias param shared
mensaje1 = 'Sr. operador'+space(48)+'Esta en condiciones de realizar la TRANSMISION DE DATOS.    '
*12345678901234567890123456789012345678901234567890123456789012345678901234567890
mensaje2 = 'Si no desea transmitir, debe cambiar el parametro SUCURSAL, y volver a arrancar el sistema.'

if param->nrosuc <= 3
  TONE(300, 3)
  TONE(100, 2)
  TONE(300, 3)
  TONE(100, 2)

   pfa_messagebox(mensaje1,mensaje2)
endif
RETURN

*--------------------------------
function PFA_CtrArt()
*--------------------------------
* SOLO DEBE CONTROLAR CUANDO ESTA INGRESANDO UN NUEVO COMPROBANTE
* NO CUANDO LO ESTA CONSULTANDO
* SE DEFINIO UNA VARIABLE GLOBAL EN SISALF 'PFA_CONSULTANDO' QUE PUEDE SER IGUAL A
* 'NO' O 'SI' Y SE ACTUALIZA EN FACVEN DESPUES QUE SELECCIONA EL CLIENTE
* SI SE PRESIONO PGDN O PGUP Y ESTA CONSULTANDO SE ACTUALIZA A 'SI' EN FACVEN DES
* PUES QUE SE EJECUTO EL BUSVEN
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
local fila, i, j, nInicio, nPos, nCant, nEnFin
nEnFin = LEN(TCOD)
nPos = 0

for i = 1 to nEnfin - 1
	if TCOD[i] > 0
   		abuscar = TCOD[i]
*		pfa_messagebox('va a buscar',str(abuscar))
	   	nInicio = i + 1
       nCant = 0
   		DO WHILE (nPos := ASCAN(TCOD, aBuscar,   nInicio)) > 0
              //? nPos, aMatriz[nPos]
              ++nCant
* 			  pfa_messagebox('encontro',str(nCant)+' '+str(abuscar))
			  exit
	   	ENDDO
*		pfa_messagebox('posicion',str(nPos))
   		if nCant > 0
   			exit
	   	endif
*		pfa_messagebox('sigue buscando',str(i))
	endif
next
*pfa_messagebox(' encontro?',str(nPos))
Return nPos



*--HASAR-----------------------------------
FUNCTION Enviar
*------------------------------------------

PARAMETERS String

PRIVATE Result, StatPrn, continuar

continuar = .t.
*? "Comm: " + String

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

*--------------------------------
PROCEDURE PFA_TALLER
*--------------------------------
* Funci¢n   : Actualización de artículos de taller
* 7777
LOCAL ACTUAL && color actual antes de ingresar a este proceso
LOCAL topmenu, PANTA01, tmatriz

tmatriz := {}

tindi = 0

** APERTURA DE ARCHIVOS
SELE 57
USE taller
INDEX ON NOM TO TALLER01
INDEX ON ART TO TALLER02

GO TOP
DO WHILE .NOT. EOF()
***	pfa_messagebox('agregando',taller->art)
	AADD(tmatriz,taller->art)
	skip
ENDDO


sele 58
USE STOALF ALIAS ARTIC
SET INDEX TO ASTOALF
GO TOP
set softseek on
SEEK "TALLER001"
DO WHILE .T.
	tabuscar = ARTIC->ART
	IF .NOT. UPPER(LEFT(tabuscar,6)) = "TALLER"
		EXIT
	ENDIF
	replace artic->nom with upper(artic->nom)

	tabuscar=alltrim(upper(artic->art))

***	pfa_messagebox('buscando',tabuscar)

	if ASCAN( tmatriz,tabuscar) = 0
***		pfa_messagebox('no encontrado',tabuscar)

		sele taller
		append blank
		replace cod with artic->cod, nom with artic->nom,  art with artic->art
		sele artic
	else
		sele taller
		seek tabuscar
		replace taller->nom with artic->nom
		select artic
	endif
	skip
ENDDO
use


SELE 57
use
USE TALLER
INDEX ON NOM TO TALLER01
INDEX ON ART TO TALLER02
GO TOP

** PREPARAR PANTALLA
ACTUAL = SETCOLOR()
SAVE SCREEN

do pfa_pantaller

do pfa_tmostrar with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4



DO WHILE .T.
	@ 17,  7 prompt " Prim "
	@ 17, 13 prompt " Ante "
	@ 17, 19 prompt " Sigui "
	@ 17, 26 prompt " Ulti "
	@ 17, 32 prompt " Modif "
	@ 17, 39 prompt " Elim  "
	@ 17, 46 prompt " Salir "
    MENU TO topmenu
*    pfa_messagebox(str(topmenu),'')
    DO CASE
    	CASE topmenu = 0		// ESCAPE
    		SETCOLOR(ACTUAL)
    		RESTORE SCREEN
    		return
    	CASE topmenu = 1		// PRIMERO
    		GO TOP
			do pfa_tmostrar with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4
    	CASE topmenu = 2		// ANTERIOR
    		if .not. bof()
	    		SKIP -1
				do pfa_tmostrar with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4
			endif
    	CASE topmenu = 3		// SIGUI
    		SKIP 1
			if eof()
				go bott
			endif
			do pfa_tmostrar with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4
    	CASE topmenu = 4		// ULTI
    		GO BOTT
			do pfa_tmostrar with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4
    	CASE topmenu = 5		// MODIF
			do pfa_tmodif with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4

    	CASE topmenu = 6		// ELIM
			do pfa_telim with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4

    	CASE topmenu = 7		// SALIR
    		SETCOLOR(ACTUAL)
    		RESTORE SCREEN
    		RETURN
    END CASE
ENDDO

SETCOLOR(ACTUAL)
RESTORE SCREEN

RETURN

procedure pfa_pantaller
SET COLOR TO
@ 5,5 CLEAR TO 18,78
SET COLOR TO I
@ 5,5 TO 18,78 DOUBLE
SET COLOR TO


@  7,  8 say "C¢digo     :"
@  8,  8 say "Art¡culo   :"
@  9,  8 say "Descripci¢n:"
@ 11,  8 say "Descrip.Ta.:"
@ 12,  8 say "Linea 1    :"
@ 13,  8 say "Linea 2    :"
@ 14,  8 say "Linea 3    :"
@ 15,  8 say "Linea 4    :"
return

procedure pfa_tmostrar
param tcod, tnom, tart,tlinea0,tlinea1,tlinea2,tlinea3,tlinea4
@ 7, 21 say tcod
@ 8, 21 say tart
@ 9, 21 say tnom
@ 11, 21 say tlinea0
@ 12, 21 say tlinea1
@ 13, 21 say tlinea2
@ 14, 21 say tlinea3
@ 15, 21 say tlinea4
Return

procedure pfa_tmodif
param tcod, tnom, tart,tlinea0,tlinea1,tlinea2,tlinea3,tlinea4
do while .t.
	@ 17,7 say space(70)
	@ 7, 21 say tcod
	@ 8, 21 say tart
	@ 9, 21 say tnom
	@ 11, 21 get tlinea0
	@ 12, 21 get tlinea1
	@ 13, 21 get tlinea2
	@ 14, 21 get tlinea3
	@ 15, 21 get tlinea4
	read

	@ 17,  7 prompt " Graba "
	@ 17, 14 prompt " Cancela "
	@ 17, 23 prompt " Reingresa "
	menu to optmodif
	do case
	case optmodif = 0
		do pfa_tmostrar with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4
		exit
	case optmodif = 1
		Replace taller->linea0 with tlinea0, taller->linea1 with tlinea1, taller->linea2 with tlinea2
		Replace	taller->linea3 with tlinea3, taller->linea4 with tlinea4
		do pfa_tmostrar with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4
		exit
	case optmodif = 2
		do pfa_tmostrar with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4
		exit
	endcase
enddo
Return

procedure pfa_telim
param tcod, tnom, tart,tlinea0,tlinea1,tlinea2,tlinea3,tlinea4
do while .t.
	@ 17,7 say space(70)
	@ 17,  7 prompt " Elimina "
	@ 17, 17 prompt " Cancela "
	menu to optelim
	do case
	case optelim = 0
		do pfa_tmostrar with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4
		exit
	case optelim = 1
		xcurreg = recno()
		delete
		pack
		if xcurreg > 1
		    xcurreg = xcurreg - 1
		else
		    xcurreg = xcurreg + 1
		endif
		goto xcurreg
		do pfa_tmostrar with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4
		exit
	case optelim = 2
		do pfa_tmostrar with taller->cod, taller->nom, taller->art,taller->linea0,taller->linea1, ;
				taller->linea2,taller->linea3,taller->linea4
		exit
	endcase
enddo
Return

procedure pfa_dtmodif
********
* modifiacion de textos en la facturacion
*********
param tcod, tnom, tart,tlinea0,tlinea1,tlinea2,tlinea3,tlinea4
do while .t.
	@ 17,7 say space(70)
	@ 7, 21 say tcod
	@ 8, 21 say tart
	@ 9, 21 say tnom
	@ 11, 21 get tlinea0
	@ 12, 21 get tlinea1
	@ 13, 21 get tlinea2
	@ 14, 21 get tlinea3
	@ 15, 21 get tlinea4
	read

	@ 17,  7 prompt " Aceptar "
	@ 17, 16 prompt " Cancelar "
	@ 17, 26 prompt " Reingresar "
	menu to optmodif2
	do case
	case optmodif2 = 0
		exit
	case optmodif2 = 1
		dtlinea0 = tlinea0
		dtlinea1 = tlinea1
		dtlinea2 = tlinea2
		dtlinea3 = tlinea3
		dtlinea4 = tlinea4
		exit
	case optmodif2 = 2
		exit
	endcase
enddo
Return


***************************************
procedure pfa_DescTaller
***************************************
* arma la descripcion en función de un texto en la tabla taller.dbf
LOCAL AREAACTUAL
public Mdescripcion,Mdescadi1,Mdescadi2,Mdescadi3,Mdescadi4

SAVE SCREEN

AREAACTUAL=ALIAS()
SELECT 59
USE TALLER INDEX TALLER02
SEEK mcodprod
if alltrim(taller->art) == alltrim(mcodprod)
* solicitar confirmacion de datos OK
    dtcod=taller->cod
    dtart=taller->art
    dtnom=taller->nom
    dtlinea0=taller->linea0
    dtlinea1=taller->linea1
    dtlinea2=taller->linea2
    dtlinea3=taller->linea3
    dtlinea4=taller->linea4


	do pfa_pantaller

	do pfa_tmostrar with dtcod, dtnom, dtart,dtlinea0,dtlinea1,dtlinea2,dtlinea3,dtlinea4

	@ 17,  7 prompt " Aceptar "
	@ 17, 16 prompt " Modif "
    MENU TO dtmenu
	do case
		case dtmenu = 0
		case dtmenu = 1
		case dtmenu = 2  // modifica el texto
			do pfa_dtmodif with dtcod, dtnom, dtart,dtlinea0,dtlinea1,dtlinea2,dtlinea3,dtlinea4
	endcase

	MDescripcion = alltrim(Mcodprod)+" "+alltrim(dtlinea0)
	if len(alltrim(dtlinea1)) > 0
		MDescAdi1=alltrim(dtlinea1)
	end if
	if len(alltrim(dtlinea2)) > 0
		MDescAdi2=alltrim(dtlinea2)
	end if
	if len(alltrim(dtlinea3)) > 0
		MDescAdi3=alltrim(dtlinea3)
	end if
	if len(alltrim(dtlinea4)) > 0
		MDescAdi4=alltrim(dtlinea4)
	end if
else
endif

restore screen

USE
IF LEN(ALLTRIM(AREAACTUAL)) > 0
    SELECT &AreaActual
ENDIF

Return

********************************************************
PROCEDURE PFA_ImpTaller
********************************************************
* 8888
*Imprime un listado de comprobantes marcando aquellos que son de taller
*
local fdesde,  fhasta, optimpta, imatriz, tindi, xlinea,x10cpi_on, x10cpi_off
local xlf, xsubt, xiva, xtot, ysubt, yiva, ytot, zsubt, ziva, ztot, xmulti, xtipo

imatriz := {}
tindi = 0
xlf = chr(10)+chr(13)
x10cpi_on = chr(15)
x10cpi_off = chr(18)
xsubt = 0
xiva  = 0
xtot  = 0
ysubt = 0
yiva  = 0
ytot  = 0
zsubt = 0
ziva  = 0
ztot  = 0
xind  = ' '
xmulti = 1
xtipo = 'T'

* Carga la matriz con los productos de taller
SELE 57
USE taller
INDEX ON ART TO TALLER02
GO TOP
DO WHILE .NOT. EOF()
	AADD(imatriz,taller->cod)
	skip
ENDDO

** PREPARAR PANTALLA
ACTUAL = SETCOLOR()
SAVE SCREEN


fdesde = date() - 30
fhasta = date()
optimpta = 0
DO pfa_panimpta

do while .t.
	@ 17,7 say space(70)
	@ 9, 21 get fdesde
	@ 10, 21 get fhasta
	@ 11, 48 get xtipo valid  xtipo$'TtRrOo'
	read


	@ 17,  7 prompt " Aceptar "
	@ 17, 16 prompt " Cancelar "
	@ 17, 26 prompt " Reingresar "
	menu to optimpta
	do case
	case optimpta = 0
		exit
	case optimpta = 1
		exit
	case optimpta = 2
		exit
	endcase
enddo

if optimpta = 1

    abrprn(15,15,44,IMPPRSM)
	if bandloop=1
	    SETCOLOR(ACTUAL)
		RESTORE SCREEN
		return
    endif

	select 57
    USE  CTCALF INDEX FCTCALF ALIAS CAB

	SELE 58
	USE FAVALF INDEX cfavalf alias DET

	* CABECERA
	xlinea = x10cpi_on+'ALFA REPUESTOS'+replicate(' ',18)+'LISTADO DE COMPROBANTES'+REPLICATE(' ',23)+ ;
		pfa_fecha(date())+ ' '+ pfa_hora(time())
	? xlinea
	xlinea = replicate(' ',23)+'COMPROBANTES ENTRE '+PFA_FECHA(fdesde)+ ' - '+PFA_FECHA(fhasta)
	? xlinea
	? replicate('-',94)
    ?'Fecha        Comprobante   Cliente                              Subtotal      IVA       Total'
	? replicate('-',94)

	SELE CAB
	SET SOFTSEEK ON
	SEEK DTOS(fdesde)
    DO WHILE .t.
    	if eof()
    		exit
   		endif
    	if cab->fec > fhasta
    		exit
    	endif
		if .not. cab->com$"NC.ND.FC"
			skip
			loop
		endif
		if cab->com$"ND.FC"
			xmulti = 1
		else
			xmulti = -1
		endif
    	xlinea = ''

    	sele det
    	seek str(cab->cod)+str(cab->nro)
    	xind = ' '
    	do while .t.
	    	if cab->cod <> det->cod .or. cab->nro <> det->nro
    			exit
    		endif
			if .not. ASCAN( imatriz,det->art) = 0
				xind = '*'
				exit
			end if
    		skip
    	enddo
    	xtipo = upper(xtipo)
		if ( xtipo = 'T' .and. xind = '*' ) .or. ( xtipo = 'O' .and. xind = '*' ) .or. ;
			( xtipo = 'R' .and. xind = ' ' ) .or. ( xtipo = 'R' .and. xind = ' ' ) .or. xtipo = 'O'
    		xlinea = xind + ' '+ pfa_fecha(cab->fec)+ ' ' + cab->com + ' '+str(cab->suc,2)+'-'+ ;
    				padl(alltrim(str(cab->nro)),8,'0')+ ;
    				padr(padl(str(cab->cod),5,' '),32,' ') + ;
    				transform(cab->gra,'###,##9.99') + '  ' + ;
    				 transform(cab->iv1,'###,##9.99') + '  ' + ;
    				 transform(cab->imp,'###,##9.99')

    		if xind = '*'
    			xsubt = xsubt + ( cab->gra * xmulti )
    			xiva  = xiva  + ( cab->iv1 * xmulti )
    			xtot  = xtot  + ( cab->imp * xmulti )
    		else
    			ysubt = ysubt + ( cab->gra * xmulti )
    			yiva  = yiva  + ( cab->iv1 * xmulti )
    			ytot  = ytot  + ( cab->imp * xmulti )
    		endif
    		zsubt = zsubt + ( cab->gra * xmulti )
    		ziva  = ziva  + ( cab->iv1 * xmulti )
    		ztot  = ztot  + ( cab->imp * xmulti )

    		?xlinea
    	endif
		sele cab
    	SKIP
    ENDDO
	? replicate('-',94)
    ?'*: Indica taller                                 Repuestos ' + ;
			transform(ysubt,'###,##9.99') + '  ' + ;
				 transform(yiva,'###,##9.99') + '  ' + ;
				 transform(ytot,'###,##9.99')
    ?'                                                    Taller ' + ;
			transform(xsubt,'###,##9.99') + '  ' + ;
				 transform(xiva,'###,##9.99') + '  ' + ;
				 transform(xtot,'###,##9.99')
    ?'                                                   Totales ' + ;
			transform(zsubt,'###,##9.99') + '  ' + ;
				 transform(ziva,'###,##9.99') + '  ' + ;
				 transform(ztot,'###,##9.99')
    ??x10cpi_off
    EJECT
    SET PRINTER OFF
    SET CONSOLE ON

endif

SETCOLOR(ACTUAL)
RESTORE SCREEN
RETURN


procedure pfa_panimpta
SET COLOR TO
@ 5,5 CLEAR TO 18,78
SET COLOR TO I
@ 5,5 TO 18,78 DOUBLE
SET COLOR TO
@  7,   8 say "Ingrese el rango de fechas para Listado de comprobantes"
@  9,   8 say "Desde      :"
@  10,  8 say "Hasta      :"
@  11,  8 say "Tipo: [T]aller   [R]puestos   T[O]dos :"
return

function pfa_fecha
param fecha
set century on
return padl(alltrim(str(day(fecha),2)),2,'0')+'/'+padl(alltrim(str(month(fecha),2)),2,'0')+'/'+str(year(fecha),4)

function pfa_hora
param hora
return substr(hora,1,2)+':'+substr(hora,1,2)



FUNCTION PFA_GraLin(pLinea)     &&Graba una linea en la base del listado de nc por descuento
* Asume que hay una base en el área LIN para grabar
fareaActual = SELECT()
Select Lin

zlinea = plinea
ylinea = left(zlinea,132)
zlinea = substr(zlinea, 133)

append blank
replace linea with ylinea

do while .t.
    if len(zlinea) > 0
	ylinea = left(zlinea,132)
	zlinea = substr(zlinea, 133)
	append blank
	replace linea with ylinea
    else
        exit
    endif
enddo

selec &fareaActual
Return


****************************************************************************************************************
****************************************************************************************************************
PROCEDURE PFA_ImpEstad   
****************************************************************************************************************
****************************************************************************************************************
* 
*Imprime un listado de ESTADISTICAS DE VENTAS
*
*
*Bases de datos relacionadas            Area
*1) param.dbf 		    ALIAS PAR  58
*2) sucursal.dbf 	    alias suc  58
*3) ctcalf.dbf INDEX FCTCALF ALIAS CAB  57
*4) favalf.dbf INDEX XFAVALF ALIAS DET  55
*5) stoalf.dbf INDEX CSTOALF ALIAS ART  56
*6) estad.dbf                ALIAS EST  59 -> contiene los datos a partir de los cuales se genera la estadistica
*                                             Se genera con cada nueva ejecución.
*7) lineas.dbf               ALIAS LIN  60 -> contiene las lineas de impresion del listado de nc por descuento
*
local fdesde,  fhasta, optimpta, imatriz, tindi, xlinea,x10cpi_on, x10cpi_off
local xlf, zsubtP, zcosP, zsubtD, zcosD, zdifP,  zdifD, xmulti, xtipo, zncred
local xsubtP, xcosP, xdifP, xsubtD, xcosD, xdifD, xncred
local factual, hactual

PUBLIC ImpLista := { }
PUBLIC ValDol, ImpNCDto


STORE 0 to zsubtP, zcosP, zsubtD, zcosD
STORE 0 TO xsubtP, xcosP, xdifP, xsubtD, xcosD, xdifD, zncred, xncred
STORE 0 TO xsubtNC, xivaNC, xtotNC
AADD(ImpLista,'OK')
AADD(ImpLista,0)
AADD(ImpLista,0)
AADD(ImpLista,0)
AADD(ImpLista,0)

ValDol=3

imatriz := {}
tindi = 0
xlf = chr(10)+chr(13)
x10cpi_on = chr(15)
x10cpi_off = chr(18)
xFormFeed = chr(12)
xsubt = 0
xcos  = 0
xdif  = 0
zsubt = 0
zcos  = 0
zdif  = 0
xind  = ' '
xmulti = 1
xtipo = 'T'

** PREPARAR PANTALLA
ACTUAL = SETCOLOR()
SAVE SCREEN


fdesde = date() - 30
fhasta = date()
optimpta = 0
DO pfa_panESimp
xIndListadoNC="S"
xIndMensajes="N"
do while .t.
	@ 17,7 say space(70)
	@ 9, 38 get fdesde
	@ 10, 38 get fhasta valid (fdesde<=fhasta)
        @ 11, 38 get ValDol PICTURE "@ 999.99"
        @ 12, 38 get xIndListadoNC PICTURE "@!" valid (xIndListadoNC$"SN")
        @ 13, 38 get xIndMensajes PICTURE "@!" valid (xIndMensajes$"SN")
	read


	@ 17,  7 prompt " Aceptar "
	@ 17, 16 prompt " Cancelar "
	@ 17, 26 prompt " Reingresar "
	menu to optimpta
	do case
	case optimpta = 0
		exit
	case optimpta = 1
		exit
	case optimpta = 2
		exit
	endcase
enddo
XPATH = NIL
if optimpta = 1

    abrprn(15,15,44,IMPPRSM)
    if bandloop=1
        SETCOLOR(ACTUAL)
        RESTORE SCREEN
        return
    endif

    select 58
    USE  param

    xPATH = alltrim(param->setdefa)

*	PFA_MESSAGEBOX(xPATH,'')
    use

* abre la base de las líneas para el listado de nc por descuento
    select 60
    use lineas alias LIN
    zap

    select 59
    use estadis
    zap
    use

    select 58
    USE  SUCURSAL ALIAS SUC
    GO TOP


    * CABECERA
    xlinea = x10cpi_on+'ALFA REPUESTOS'+replicate(' ',26)+'ESTADISTICA DE VENTAS'+REPLICATE(' ',32)+ ;
	pfa_fecha(date())+ ' '+ pfa_hora(time())
    ? xlinea
    xlinea = replicate(' ',32)+'COMPROBANTES ENTRE '+PFA_FECHA(fdesde)+ ' - '+PFA_FECHA(fhasta)
    ? xlinea
    ? replicate('-',109)
    ?'                                    PESOS                 |             DOLARES                | N. Credito |'
    ? replicate('-',109)
    ?'Sucursales            Venta(Neto)   Costo     Ganancia    | Venta(Neto)   Costo     Ganancia   | Sin Prod.  |'
    ? replicate('-',109)



    do while .t.
* Cabecera de listado de NC por Descuento
        ImpNCDto = x10cpi_on+'ALFA REPUESTOS'+replicate(' ',26)+'LISTA NC POR DESCUENTO'+REPLICATE(' ',31)+ ;
     	         pfa_fecha(date())+ ' '+ pfa_hora(time()) + xlf
        ImpNCDto += 'Sucursal: ' + PADR(ALLTRIM(SUC->DESCRIP),22, ' ') + 'COMPROBANTES ENTRE '+PFA_FECHA(fdesde)+ ' - '+PFA_FECHA(fhasta)+ xlf
        ImpNCDto += replicate('-',94) + xlf
        ImpNCDto += 'Fecha        Comprobante   Cliente                              Subtotal      IVA       Total' + xlf
        ImpNCDto += replicate('-',94) + xlf
	PFA_GraLin(ImpNCDto)

	zsubt = 0
	zcos  = 0
	zdif  = 0
	xPATHnew = ALLTRIM(SUC->DIRECTORIO)
	SET DEFA TO &xPATHnew

	* APERTURA DE BASES DE DATOS
	***********************************************************************
	select 55
    	USE FAVALF ALIAS DET
           INDEX ON STR(cod) + com + STR(nro) ;
           TO XFAVALF
	*          FOR (DET->Fec >= fdesde .and. DET->Fec <= fhasta)
	***********************************************************************

	select 56
    	USE STOALF INDEX CSTOALF ALIAS ART
	***********************************************************************

	select 59
	wdbestadis = xPATH + '\estadis'
    	USE &wdbestadis ALIAS EST

        factual = date()
        hactual = time()
	***********************************************************************
	select 57
    	USE CTCALF INDEX FCTCALF ALIAS CAB
	***********************************************************************
*		PFA_MESSAGEBOX(xBASE,xPATHnew)

	SELE CAB

    	SET SOFTSEEK ON
    	SEEK DTOS(fdesde)
        DO WHILE .t.
	    if eof()
        	exit
     	    endif
            if cab->fec > fhasta
        	exit
            endif
    	    if .not. cab->com$"NC.ND.FC"
    		skip
    		loop
    	    endif

* crea el registro en la base de estadística
            sele est
            append blank
            replace est->fechaemi with factual, est->horaemi with hactual
            sele cab

    	    if cab->com$"ND.FC"
    	 	xmulti = 1
    	    else
    	 	xmulti = -1
    	    endif
            xlinea = ''


* Actualiza la estadistica con los datos del comprobante
            replace est->suc with SUC->nro_suc, ;
                    est->nomsuc with SUC->DESCRIP, ;
                    est->cod with cab->cod, ;
                    est->com with cab->com, ;
                    est->nro with cab->nro, ;
                    est->fec with cab->fec
            sele cab
           

           IF cab->com = 'NC' .and. ( cab->mot = 'A' .or. cab->mot = 'F')
		zncred = ROUND(zncred + cab->gra, 2)

* Actualiza la estadistica para Notas de crédito por descuento
                replace est->ncrcbte with cab->gra,;
                        est->ncracum with zncred
                sele cab

* Arma la línea de impresión de Listado de nc por descuento
               ImpNCDto = ' '+ pfa_fecha(cab->fec)+ ' ' + cab->com + ' '+str(cab->suc,2)+'-'+ ;
                           padl(alltrim(str(cab->nro)),8,'0')+ ;
            		   padr(padl(str(cab->cod),5,' '),32,' ') + ;
            		   transform(cab->gra,'###,##9.99') + '  ' + ;
            		   transform(cab->iv1,'###,##9.99') + '  ' + ;
            		   transform(cab->imp,'###,##9.99') + xlf 
               PFA_GraLin(ImpNCDto)

               xsubtNC += cab->gra
               xivaNC += cab->iv1
               xtotNC += cab->imp

           ELSE
                Respuesta = PFA_Importes(cab->cod, cab->com, cab->nro, xIndMensajes)
                IF ImpLista[1] <> 'OK'
     *              pfa_messagebox('Error','Al volver de la funci¢n')

* Actualiza la estadistica con el comentario por el error de no encontrar detalle
                    replace est->coment with 'Comprobante sin detalle'
                    SELECT CAB
                    SKIP
     	            LOOP
     	       ENDIF

               zsubtP = ROUND(zsubtP + ( ImpLista[2] * xmulti ), 2)
               zcosP  = ROUND(zcosP  + ( ImpLista[3] * xmulti ), 2)
               zsubtD = ROUND(zsubtD + ( ImpLista[4] * xmulti ), 2)
               zcosD  = ROUND(zcosD  + ( ImpLista[5] * xmulti ), 2)
	       zdifP =  zsubtP - zcosP
               zdifD =  zsubtD - zcosD

* Carga la base de datos de datos de la estadística
               replace est->stpcbte with ImpLista[2] * xmulti, ;
                       est->copcbte with ImpLista[3] * xmulti, ;
                       est->stdcbte with ImpLista[4] * xmulti, ;
                       est->codcbte with ImpLista[5] * xmulti, ;
                       est->gapcbte with (est->stpcbte - est->copcbte), ;
                       est->gadcbte with (est->stdcbte - est->codcbte), ;
                       est->stpacum with zsubtp, ;
                       est->copacum with zcosP, ;
                       est->stdacum with zsubtD, ;
                       est->codacum with zcosD,;
                       est->gapacum with zdifP,;
                       est->gadacum with zdifD
               
           ENDIF

       	   sele CAB
           SKIP
        ENDDO

*PFA_MESSAGEBOX('va a entrar en la línea con error','si, error')

*        PFA_MESSAGEBOX(VALTYPE(zsubtP),TYPE('zsubtP'))



        if VALTYPE(zsubtP) <> 'N'
		store 0 to zsubtP
	endif
        if VALTYPE(zcosP) <> 'N'
		store 0 to zcosP
	endif
        if VALTYPE(zdifP) <> 'N'
		store 0 to zdifP
	endif
        if VALTYPE(zsubtD) <> 'N'
		store 0 to zsubtD
	endif
        if VALTYPE(zcosD) <> 'N'
		store 0 to zcosD
	endif
        if VALTYPE(zdifD) <> 'N'
		store 0 to zdifD
	endif
        if VALTYPE(zncred) <> 'N'
		store 0 to zncred
	endif

    	xlinea = PADR(ALLTRIM(SUC->DESCRIP),22, ' ') + ;
    		transform(zsubtP,'###,##9.99') + replicate(' ',2) + ;
    		transform(zcosP,'###,##9.99') + replicate(' ',2) + ;
    		transform(zdifP,'###,##9.99') + '  | ' + ;
    		transform(zsubtD,'###,##9.99') + replicate(' ',2) + ;
    		transform(zcosD,'###,##9.99') + replicate(' ',2) + ;
    		transform(zdifD,'###,##9.99') + ' | ' + ;
    		transform(zncred,'###,##9.99') + ' |'
* PFA_MESSAGEBOX('va a salir de la línea con error','si, error')
    	? xlinea

* Arma la linea de totales por sucursal del listado de NC
       ImpNCDto = replicate('-',94) + xlf
       PFA_GraLin(ImpNCDto)
       ImpNCDto = '                                                Tot. Suc. ' + ;
		transform(xsubtNC,'###,##9.99') + '  ' + ;
                transform(xivaNC,'###,##9.99') + '  ' + ;
		transform(xtotNC,'###,##9.99') + xlf + xlf
       PFA_GraLin(ImpNCDto)

*       ImpNCDto = replicate('-',94) + xlf
*       ImpNCDto += 'Fecha        Comprobante   Cliente                              Subtotal      IVA       Total' + xlf
*       ImpNCDto += replicate('-',94) + xlf
*       PFA_GraLin(ImpNCDto)

       Store 0 to xsubtNC, xivaNC, xtotNC


        xsubtP += zsubtP
        xcosP  += zcosP
        xdifP  += zdifP
        xsubtD += zsubtD
        xcosD  += zcosD
        xdifD  += zdifD
        xncred += zncred

        replace est->stptot with xsubtp, ;
                est->stdtot with xsubtd, ;
                est->coptot with xcosP, ;
                est->codtot with xcosD, ;
                est->gaptot with xdifP, ;
                est->gadtot with xdifD, ;
                est->ncrtot with xncred


        STORE 0 TO zsubtP, zcosP, zdifP, zsubtD, zcosD, zdifD, zncred
    	SELE SUC
    	SKIP
    	IF EOF()
    		EXIT
    	ENDIF
    	SELE CAB
    	USE
    ENDDO

    ? replicate('-',109)
    xlinea = PADR(ALLTRIM(SUC->DESCRIP),22, ' ') + ;
    transform(xsubtP,'###,##9.99') + replicate(' ',2) + ;
    transform(xcosP,'###,##9.99') + replicate(' ',2) + ;
    transform(xdifP,'###,##9.99') + '  | ' + ;
    transform(xsubtD,'###,##9.99') + replicate(' ',2) + ;
    transform(xcosD,'###,##9.99') + replicate(' ',2) + ;
    transform(xdifD,'###,##9.99') + replicate(' ',3) + ;
    transform(xncred,'###,##9.99') 
    ?xlinea

* Imprime el listado de NC por descuento si fue indicado
    IF xIndListadoNC = "S" 
        select lin
        go top
        ?xFormFeed
        do while .not. eof()
           ??rtrim(lin->linea)
           skip
        enddo
    ENDIF

    ??x10cpi_off
    EJECT
    SET PRINTER OFF
    SET CONSOLE ON

endif
if XPATH <> NIL
        SET DEFA TO &xPATH
ENDIF
sele 55
use
sele 56
use
sele 57 
use
sele 58
use
sele 59
use
sele 60
use

SETCOLOR(ACTUAL)
RESTORE SCREEN
RETURN

************************************************************************
FUNCTION PFA_Importes( codPF, comPF, nroPF, xMje)
************************************************************************
*Entrada:  cab->cod: nro cliente
*            cab->com: tipo cbte
*            cab->nro: nro cbte
*  Salida: un vector con vec(1) = DsubtP  Subtotal en pesos
*			vec(2) = DcosP   Costo en pesos
*			vec(3) = DsubtD  Subtotal en U$S
*			vec(4) = Dzcos   Costo en U$S
*
* Devuelve el importe en pesos y dolares del subtotal y costo de un 
* comprobante.
************************************************************************

LOCAL nPos, tipoF
// Definir una matriz vacía
*LOCAL aLista := { }
*PRIVATE aLista := { }
*aLista := { }
ImpLista[1] = 'OK'
ImpLista[2] = 0
ImpLista[3] = 0
ImpLista[4] = 0
ImpLista[5] = 0

nAreadeTrabajo:= SELECT()


SELECT DET

fClave = STR(codPF) + comPF + STR(nroPF)

*pfa_messagebox('va a buscar:',fClave)

SEEK fclave
if .not. FOUND() 
    IF xMje = 'S'
       PFA_Messagebox('Error '+fclave+cab->mot,'Registro de detalle no encontrado. Posible corrupci¢n de datos.')
    ENDIF
       ImpLista[1] = 'ERROR'
ELSE
   DO WHILE .T.
     	if eof()
*                pfa_messagebox('Dio EOF',fClave)

      	exit
  		endif
      SELECT ART
      SEEK DET->art
      if .NOT. FOUND()
          IF xMje = 'S'
              PFA_Messagebox('Error','Registro de detalle no encontrado. Posible corrupci¢n de datos.')
          ENDIF
          ImpLista[1] = 'ERROR'
          EXIT
      END IF

*     pfa_messagebox('Encontro el articulo',ART->nom+'-'+ART->art+'-'+str(ART->cod)+'-'+ART->ori)

      IF ART->tip <> 1
          tipoF='DOL'
*      pfa_messagebox('Encontro ART->tip <> 01 DOLAR',ART->nom)

      ELSE
         xuno=""
         if len(alltrim(ART->ori)) > 0
             xdos=alltrim(ART->ori)
         else
             xdos="* EN BLANCO *"
         endif
         xpos = AT("US",UPPER(xdos))
         if AT("U$S",UPPER(xdos)) > 0 .OR. ( xpos > 0 .and. len(alltrim(right(xdos,xpos))) = 2 )
             xdos="* EN U$S *"
             tipoF='DOL'
*                 pfa_messagebox('Encontro US/U$S DOLAR',ART->nom)
       	 endif
*         if !len(alltrim(ART->ori)) > 0
             xdos="* EN PESOS *"
	     tipoF='PES'
*             pfa_messagebox('Encontro PESOS',ART->nom)
*         endif
      ENDIF
      
      IF tipoF='PES'
                        ImpLista[2] += DET->can * DET->pre
                        ImpLista[3] += DET->can * ART->cpr

*    pfa_messagebox('SUMA A PESOS',ART->nom)

		ELSE
                        ImpLista[4] += DET->can * DET->pre
                        ImpLista[5] += DET->can * ART->cpr * ValDol
*      pfa_messagebox('SUMA A DOLAR',Str(ValDol))
		ENDIF

      SELECT DET
      SKIP
      *fClave = STR(codPF) + comPF + STR(nroPF)
      IF  DET->cod <> codPF .OR. DET->com <> comPF .OR. DET->nro <> nroPF
*     pfa_messagebox('NO HAY MAS ART DEL CBTE',ART->nom)
        
          EXIT
      ENDIF
   END DO
END IF
SELECT (nAreadeTrabajo)
//
// Devolver la matriz
*pfa_messagebox(str(ImpLista[2]),str(ImpLista[3]))
*pfa_messagebox(str(ImpLista[4]),str(ImpLista[5]))

RETURN ImpLista

************************************************************************
FUNCTION PFA_type(campo)
************************************************************************
*Entrada:  campo
*
* Devuelve un valor 0 si el campo es de tipo U
* comprobante.
************************************************************************

if TYPE('campo') $ 'U'
	campo = 0
end if
RETURN campo



*********************************************
procedure pfa_panESimp
*********************************************
* Pantalla del listado de estadística de ventas
SET COLOR TO
@ 5,5 CLEAR TO 18,78
SET COLOR TO I
@ 5,5 TO 18,78 DOUBLE
SET COLOR TO
@  7,   8 say "Ingrese el rango de fechas para Listado de estad¡sticas"
@  9,   8 say "Desde.......................:"
@  10,  8 say "Hasta.......................:"
@  11,  8 say "Valor dolar.................:"
@  12,  8 say "Listado de NC por descuento?:"
@  13,  8 say "Mensajes de error?..........:"
return

*********************************
*--------------------------------
PROCEDURE PFA_GENCIERRE
*--------------------------------
* Funci¢n   : Genera el cierre de stock. Actualiza CIERRECA Y CIERREDE
* 787878
LOCAL ACTUAL && color actual antes de ingresar a este proceso
LOCAL topmenu, PANTA01, tmatriz


** APERTURA DE ARCHIVOS
SELE 57
USE cierreca
INDEX ON codcierre TO CIECA01 UNIQUE
USE CIERRECA INDEX CIECA01

SELE 58
USE cierreDE
*INDEX ON STR(codcierre)+STR(COD) TO CIEDE01 UNIQUE
*INDEX ON STR(codcierre)+NOM TO CIEDE02 
*INDEX art+str(cod,6)+STR(codcierre,6) TO CIEDE03 
*
* VER EN GENCIE.PRG LA DEFINICION DE LOS INDICES
*
USE CIERREDE INDEX CIEDE01,CIEDE03


SELE CIERRECA

** PREPARAR PANTALLA
ACTUAL = SETCOLOR()
SAVE SCREEN

do pfa_panCIERRE   

do pfa_CIEmostrar with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora


DO WHILE .T.
	@ 17,  7 prompt " Prim "
	@ 17, 13 prompt " Ante "
	@ 17, 19 prompt " Sigui "
	@ 17, 26 prompt " Ulti "
	@ 17, 32 prompt " Modif "
	@ 17, 39 prompt " Nuevo "
	@ 17, 46 prompt " Salir "
    MENU TO topmenu
*    pfa_messagebox(str(topmenu),'')
    DO CASE
    	CASE topmenu = 0		// ESCAPE
    		SETCOLOR(ACTUAL)
    		RESTORE SCREEN
    		return
    	CASE topmenu = 1		// PRIMERO
    		GO TOP
			do pfa_CIEmostrar with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora
    	CASE topmenu = 2		// ANTERIOR
    		if .not. bof()
	    		SKIP -1
				do pfa_CIEmostrar with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora
			endif
    	CASE topmenu = 3		// SIGUI
    		SKIP 1
			if eof()
				go bott
			endif
			do pfa_CIEmostrar with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora
    	CASE topmenu = 4		// ULTI
    		GO BOTT
			do pfa_CIEmostrar with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora
    	CASE topmenu = 5		// MODIF
			do pfa_CIEmodif with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora

    	CASE topmenu = 6		// NUEVO
			do pfa_CIENuevo

    	CASE topmenu = 7		// SALIR
    		SETCOLOR(ACTUAL)
    		RESTORE SCREEN
    		RETURN
    END CASE
ENDDO

sele 57
use
sele 58 
use

SETCOLOR(ACTUAL)
RESTORE SCREEN

RETURN

procedure pfa_panCIERRE
SET COLOR TO
@ 5,5 CLEAR TO 18,78
SET COLOR TO I
@ 5,5 TO 18,78 DOUBLE
SET COLOR TO


@  7,  8 say "C¢digo     :"
@  8,  8 say "Descripci¢n:"
@  9,  8 say "Fecha      :"
@ 10,  8 say "Hora       :"
return

procedure pfa_CIEMostrar
param tcod, tdesc,tfecha,thora
@ 7, 21 say tcod
@ 8, 21 say tdesc
@ 9, 21 say tfecha
@ 10, 21 say thora
Return

procedure pfa_CIEModif
param tcod, tdes, tfecha,thora
do while .t.
	@ 17,7 say space(70)
	@ 7, 21 say tcod
	@ 8, 21 get tdes
	@ 9, 21 say tfecha
	@ 10, 21 say thora 
	read

	@ 17,  7 prompt " Graba "
	@ 17, 14 prompt " Cancela "
	@ 17, 23 prompt " Reingresa "
	menu to optmodif
	do case
	case optmodif = 0
			do pfa_CIEmostrar with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora
		exit
	case optmodif = 1
		Replace CIERRECA->descrip with tdes
			do pfa_CIEmostrar with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora
		exit
	case optmodif = 2
			do pfa_CIEmostrar with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora
		exit
	endcase
enddo
Return

procedure pfa_CIENuevo
local xcodcierre, tdes, optnuevo , xcurreg 
sele cierreca
go bott
xcodcierre = cierreca->codcierre + 1
append blank
replace codcierre with xcodcierre, fecha with date(), hora with time()
replace descrip with dtoc(date())+' - '+hora
do pfa_CIEmostrar with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora
tdes = cierreca->descrip
do while .t.
	@ 17,7 say space(70)
	@ 8, 21 get tdes
	read
	
	@ 17,  7 prompt " Genera "
	@ 17, 14 prompt " Cancela "
	@ 17, 23 prompt " Reingresa "
	menu to optnuevo
	do case
	case optnuevo = 0
		xcurreg = recno()
		delete
		pack
		if xcurreg > 1
		    xcurreg = xcurreg - 1
		else
		    xcurreg = xcurreg + 1
		endif
		goto xcurreg
		do pfa_CIEmostrar with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora
		exit
	case optnuevo = 1   // Genera
		replace cierreca->descrip with tdes
		sele 59 
		use stoalf index cstoalf alias prod shared
		go top
		@ 15,8 say "Procesando:"
		do while .not. eof()
			@ 15,21 say str(prod->cod)+'  '+prod->nom
			if ( prod->sto + prod->rec - prod->dec - prod->rev ) > 0
				sele cierrede
				append blank
				replace codcierre with xcodcierre, ;
					cod with prod->cod, ;
					art with prod->art, ;
					sto with prod->sto + prod->rec - prod->dec - prod->rev, ;
					st2 with prod->st2, ;
					st3 with prod->st3, ;
					nom with prod->nom
				sele prod
			endif
			skip
		enddo
	
		do pfa_CIEmostrar with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora
		exit
	case optnuevo = 2   // Cancela
		xcurreg = recno()
		delete
		pack
		if xcurreg > 1
		    xcurreg = xcurreg - 1
		else
		    xcurreg = xcurreg + 1
		endif
		goto xcurreg
		do pfa_CIEmostrar with cierreca->codcierre, cierreca->descrip, cierreca->fecha,cierreca->hora
		exit
	endcase
enddo
sele 59
use
sele cierreca
Return

********************************************************
PROCEDURE PFA_ImpComparativo
********************************************************
* 3434
*Imprime un listado de Comparativo de stock
*
local optimpta, imatriz, tindi, xlinea,x10cpi_on, x10cpi_off
local xlf, xsubt, xcos, zsubt, zcos, zdif,  xmulti, xtipo
local xcodcie1,xfecha1,xdesc1,xhora1, xcan1
local xcodcie2,xfecha2,xdesc2,xhora2, xcan2, xdif
imatriz := {}
tindi = 0
xlf = chr(10)+chr(13)
x10cpi_on = chr(15)
x10cpi_off = chr(18)
xsubt = 0
xcos  = 0
xdif  = 0
zsubt = 0
zcos  = 0
zdif  = 0
xind  = ' '
xmulti = 1
xtipo = 'T'
xcan1 = 0
xcan2 = 0
xdif  = 0

** PREPARAR PANTALLA
ACTUAL = SETCOLOR()
SAVE SCREEN

sele 57
use cierreca index cieca01 alias cab
if .not. reccount() > 1
	pfa_messagebox('Sr. Operador','Debe generar al menos 2 cierres de stock.')
	return
endif
go bott
xcodcie2 = cab->codcierre
xfecha2  = cab->fecha
xdesc2  = cab->descrip
xhora2  = cab->hora

skip -1
xcodcie1 = cab->codcierre
xfecha1  = cab->fecha
xdesc1  = cab->descrip
xhora1  = cab->hora

optimpta = 0
DO pfa_panCI2

do while .t.
	@ 17,7 say space(70)
	@ 7,16 get xcodcie1 valid pfa_valcie(xcodcie1,16)
	@ 8,16 say left(xdesc1,30)
	@ 9,16 say right(xdesc1,10)
	@ 10,16 say xfecha1
	@ 11,16 say xhora1
	@ 7,48 get xcodcie2 valid pfa_valcie(xcodcie2,48)
	@ 8,48 say left(xdesc2,30)
	@ 9,48 say right(xdesc2,10)
	@ 10,48 say xfecha2
	@ 11,48 say xhora2
	read


	@ 17,  7 prompt " Aceptar "
	@ 17, 16 prompt " Cancelar "
	@ 17, 26 prompt " Reingresar "
	menu to optimpta
	do case
	case optimpta = 0
		exit
	case optimpta = 1
		exit
	case optimpta = 2
		exit
	endcase
enddo

if optimpta = 1

	IF .NOT. PF_SINFISCAL > 0
	    abrprn(15,15,44,IMPPRSM)
		if bandloop=1
	    	SETCOLOR(ACTUAL)
			RESTORE SCREEN
			return
	    endif
	ELSE
		SET ALTERNATE TO SALIDA.TXT
		SET ALTERNATE ON
	ENDIF
	seek xcodcie1
	xfecha1  = cab->fecha
	xdesc1  = cab->descrip
	xhora1  = cab->hora

	seek xcodcie2
	xfecha2  = cab->fecha
	xdesc2  = cab->descrip
	xhora2  = cab->hora

	* CABECERA
	xlinea = x10cpi_on+'ALFA REPUESTOS'+replicate(' ',19)+'COMPARATIVO DE STOCK '+REPLICATE(' ',24)+ ;
		pfa_fecha(date())+ ' '+ pfa_hora(time())
	? xlinea
	xlinea = 'Cierre A: '+str(xcodcie1)+' '+xdesc1+' '+dtoc(xfecha1)+' '+xhora1
	? xlinea
	xlinea = 'Cierre B: '+str(xcodcie2)+' '+xdesc2+' '+dtoc(xfecha2)+' '+xhora2
	? xlinea
	? replicate('-',94)
    ?'Art¡culo                                                  Cierre1 Cierre2 Diferen. Costo'
	?'--------------------------------------------------------- ------- ------- -------- -----------'

	sele 59
	use stoalf index cstoalf alias prod shared

	select 58
    use cierrede index ciede03 alias det
	go top
	xcan1 = 0
	xcan2 = 0
	xcod = det->cod
	xcosto = 0
	xsubtot = 0
	xtotal = 0
	do while .not. eof()
		if det->codcierre = xcodcie1 .and. xcod = det->cod
			xcan1 = det->sto		
		end if
		if det->codcierre = xcodcie2 .and. xcod = det->cod
			xcan2 = det->sto		
		end if
		if xcan1 > 0 .and. xcan2 > 0 .and. ( xcan1 <> xcan2 )
******************************
          select prod
		  Mcod_prod = det->cod
          seek Mcod_prod
          if .not. found()
            PFA_Messagebox('Error: 1 ','Articulo no encontrado: '+det->nom)
            Return .f.
          endif
          xcosto = prod->cpr
          select det
******************************

			xdif = xcan2 - xcan1
			xsubtot = xcosto * ABS(xdif)
			xtotal = xtotal + xsubtot
			linea= str(det->cod,6)+' ' +det->art+' '+det->nom+replicate(' ',1)+str(xcan1,6)+ ;
					replicate(' ',2)+str(xcan2,6)+replicate(' ',2)+str(xdif,6)+replicate(' ',3)+transform(xsubtot,"9999999.99")
			?linea
			xcan1 = 0
			xcan2 = 0
			skip
			xcod = det->cod
		else
		    wcod = det->cod
            wart = det->art
            wnom = det->nom
			skip
			if xcod <> det->cod
				xdif = xcan2 - xcan1
				if xdif <> 0
                    ******************************
                    select prod
          		    Mcod_prod = wcod
                    seek Mcod_prod
                    if .not. found()
                      PFA_Messagebox('Error: 2 ','Articulo no encontrado: '+wnom)
                      Return .f.
                    endif
                    xcosto = prod->cpr
                    select det
                    ******************************
					xsubtot = xcosto * ABS(xdif)
					xtotal = xtotal + xsubtot
					linea= str(wcod,6)+' ' +wart+' '+wnom+replicate(' ',1)+str(xcan1,6)+ ;
						replicate(' ',2)+str(xcan2,6)+replicate(' ',2)+str(xdif,6)+replicate(' ',3)+transform(xsubtot,"9999999.99")+'*'
					?linea
				endif
				xcan1 = 0 
				xcan2 = 0
				xcod = det->cod
			endif
		endif
*		if det->cod = 4198
*			linea= ' codcierre:' +str(det->codcierre,6)+' cod:' +str(det->cod,6)+'$$ codcie1:' +;
*					str(xcodcie1,6)+' xcod:' +str(xcod,6)+' codcie2:' +str(xcodcie2,6)
*			?linea
*			pfa_messagebox('asda','asd')
*		endif
	enddo
	? replicate('-',94)
	?replicate(' ',83)+transform(xtotal,"9999999.99")
    ??x10cpi_off
	IF  .not. PF_SINFISCAL > 0
	   EJECT
	   SET PRINTER OFF
	else
		SET ALTERNATE OFF
	ENDIF
    SET CONSOLE ON
endif

sele 57
use

sele 58
use


sele 59
use

SETCOLOR(ACTUAL)
RESTORE SCREEN
RETURN

function pfa_valcie(xcodcie,fila)
seek xcodcie
if .not. found()
	pfa_messagebox('Sr. Operador','Cierre inexistente. Reingrese')
	Return .f.
else
	@ 8,fila say left(cab->descrip,30)
	@ 9,fila say right(cab->descrip,10)
	@ 10,fila say cab->fecha
	@ 11,fila say cab->hora
endif
Return .t.


procedure pfa_panCI2
SET COLOR TO
@ 5,5 CLEAR TO 18,78
SET COLOR TO I
@ 5,5 TO 18,78 DOUBLE
SET COLOR TO


@  7,  8 say "C¢digo:"
@  8,  8 say "Descr.:"
@ 10,  8 say "Fecha :"
@ 11,  8 say "Hora  :"
return

PROCEDURE PFA_UtiNeg
*Elimina las facturas con utilidad negativa


IF PFA_questionbox('Sr. Operador','Confirma emisi¢n listado de stock valorizado?') = 'SI'
	sele 58
	use utialf index cutialf alias uti
	delete all
	pack
	reindex
	Pfa_messagebox('Sr. Operador','Proceso realizado.')
ENDIF
sele 58
use
RETURN

 procedure stkalfa
*:**************************************@@@@*******************************
*:        Procedure: stkalfa  Nuevo programa para carga de stock
*:*********************************************************************
*
* Agregado por Miguel Rossi el 06/06/06
*
*: INICIALIZACION *****************************************************
local nArcLog, Arclog, contador, nroLog, dFecPre

nroLog = 1
ArcLog="C:\LogSto.txt"

if file(ArcLog)
	   nArcLog = FOPEN(ArcLog,FO_READWRITE+FO_EXCLUSIVE)
else
	   nArcLog = FCREATE(ArcLog,FC_NORMAL)
endif
FSEEK(nArcLog, 0, 2)   &&se posiciona al final del archivo

save screen to panterior
set confirm off
set color to

@  1, 0 clear to 22, 79 

xarea=SELECT()
select 91           && se define el area de trabajo 92 para la tabla 
                    && de articulos
use stoalf index astoalf ALIAS a SHARED

* cada vez que se hace referencia a una de estas tablas se selecciona 
* un area de trabajo
* area 91 -->  (a->)   

@  2,  1  to 18, 78
subtitu = '  Actualizacion de Movimientos de Stock  '
@  2, 40-len(subtitu)/2  say subtitu
@  4, 11  say 'Fecha:'
@  8, 11  say 'Articulo          :'
@ 10, 11  say 'Descripcion       :'
@ 12, 11  say 'Exist. segun stock:'
*         Exist. segun stock:          Remito de venta: 
@ 14, 11  say 'Cantidad          :'
@ 16, 11  say 'Nueva Existencia  :'
save screen to pzona          && se guarda en memoria la pantalla

mo_fec = date()
cod_art = '                   '
cant_art = 0
*: FIN  INICIALIZACION ************************************************



linea='Ingrese fecha del dia. <Enter> Acepta.'
@ 24, 0 clear to 24, 79
@ 24, 40-len(linea)/2 say linea

@  4, 18  get  mo_fec               && pide fecha  en todos los casos se 
                                    && trabaja con variables transitorias
read                                  

if readkey() = 12 			   && .or. read() = 268 
   USE
   set confirm on
   set escape on
   restore screen from panterior             && restaura pantalla inicial
   return
endif

save screen to pzona1

do while .t.

     linea='Ingrese Articulo. <Enter> Acepta.'
     @ 24, 0 clear to 24, 79 
     @ 24, 40-len(linea)/2 say linea

     @  8, 31  get  cod_art  picture "@K"      && pide cod. articulo
     read

     if readkey() = 12 .or. cod_art = '                   ' 
        restore screen from panterior  && en caso de escape se restaura 
                                   && la pantalla de memoria
        exit
     endif

     go top
     seek cod_art 

     if .not. found() 
        @ 22,  0 clear to 22, 79 
      error = PFA_Messagebox('NO EXISTE ARTICULO. VERIFICAR !!!','','')
		 @ 22,  0 clear to 22, 79 
        loop
     endif

     set color to i
     @  10, 31 say a->nom                           && escribe en pantalla
                                                    && la descripcion
     @ 12, 31 say a->sto picture '999'              && escribe en pantalla

		   			            && el stock actual
     set color to
     nroop = 4

     cantidadOK = .F.
     do while .not. cantidadOK
         ayuda = 'Ingrese cantidad. <Enter> Acepta.'
         @ 24, 0 clear to 24, 79 
         @ 24, 40-len(ayuda)/2 say ayuda
         @ 14, 31  get  cant_art && picture '99,999.999'
         read

         if readkey() = 12                              && .or. readkey() = 268
            restore screen from pzona1            && restaura la pantalla para hacer otro mvto.
            cant_art = 0
            exit
         endif

         if cant_art = 0
            @ 22,  0 clear to 22, 79 
            error = PFA_Messagebox('CANTIDAD = 0. VERIFICAR !!! ','','')
            @ 22,  0 clear to 22, 79 
            loop
         else
            cantidadOK = .t.
         endif
     enddo
     if .not. cantidadOK
          loop
     endif

     @ 24, 0 clear to 24, 79 

     ar_exi = a->sto + cant_art

     set color to i
     @ 16, 31 say ar_exi picture '99,999.999'
     set color to 

*     if cant_art < 0 
*        cant_art = cant_art * (-1)
*     endif

     ultili = 'Confirma ? (S/N)  '
     @ 24,  0 clear to 24, 79 
     @ 21, 40-len(ultili)/2 say ultili

     confi = ' '
     do while confi <> 'S' .and. confi <> 'N' .and. confi <> 's' .and. confi <> 'n'
        confi = 'S'
        @ 21, 49 get confi 
        read
     enddo           


     if readkey() = 12 .or. confi = 'N' .or. confi = 'n'
        @ 21,  0 clear to 21, 79 
        restore screen from pzona1            && restaura la pantalla para hacer otro mvto.
        loop
     endif                        


     
     select 91                             && abre el area 91 (articulo) y reemplaza
     
     IF RLOCK()
	replace a->sto with ar_exi            && las existencias
        replace a->fec with mo_fec            && guardo la fecha

        FWRITE(nArcLog,dtoc(date())+'-'+TIME()+' '+ ;
            ' '+ padr(cod_art,15,' ')+ ;
            ' '+ padr(a->nom,30,' ') +;
            ' '+ padr(alltrim(str(a->sto)),15,' ') +;
            ' '+ padr(alltrim(str(cant_art)),15,' ') +;
            ' '+ padr(alltrim(str(ar_exi)),15,' ') +;
            chr(13)+chr(10))
     else
  	 Pfa_messagebox('Sr. Operador','El articulo está siendo actualizado por otro operador. La operación no fue realizada')
         loop
     end if

     restore screen from pzona1            && restaura la pantalla para hacer otro mvto.
     cant_art = 0

     @ 21,  0 clear to 21, 79 

enddo .t.

*: Cerrar bases de datos y archivos ****************************************************
FCLOSE(nArcLog)
select 91
use
SELECT(xarea)

*: EOF: stkalfa.PRG


**********************************************************
*Programa mellizo de ACPALF para que
*la actualización del costo promedio y la fecha de
*actualización.
**********************************************************

procedure ACPALF2

   linv:= 5
   long:= 0
   cdis:= 3
   set color to i
   @  5, cdis - 2 clear to 22, cdis + 41
   @  5, cdis - 2 to 22, cdis + 41
   @  7, cdis - 1 to  7, cdis + 40
   set color to W+/R
   @  6, cdis - 1 say cedeiz("ACTUALIZACION DE PRECIOS", 0, 42)
   set color to I
   @  8, cdis + 5 to 13, cdis + 34 double
   @ 15, cdis say "Art¡culo :"
   @ 16, cdis say "Nombre   :"
   @ 17, cdis say "Precio   :"
   @ 20, cdis - 1 to 20, cdis + 40
   
   @ 21, cdis + 10 say "Productos Procesados: "
   tipm:= 0
   prom:= 0
   porm:= 0
   conf:= "N"
   @  9, cdis + 6 say "Tipo de Producto   :        "
   @ 10, cdis + 6 say "Proveedor          :        "
   @ 11, cdis + 6 say "Porcentaje         :        "
   @ 12, cdis + 6 say "Confirma el Proceso:        "
   @  9, cdis + 27 get tipm picture "@z 99"
   @ 10, cdis + 27 get prom picture "@z 9999"
   @ 11, cdis + 27 get porm picture "@z 9999.99"
   @ 12, cdis + 27 get conf picture "@!"
   read
   if ( conf != "S" .OR. LastKey() = 27 )
   else
      set color to I
      @ 11, cdis + 6 say cedeiz("P R O C E S A N D O", 0, 28)
      set color to i
      a:= 0
      use (bsto) shared
      do while ( !EOF() )
         if ( prom != 0 .AND. prom != pro )
            skip 
            loop
         endif
         if ( tipm != 0 .AND. tipm != tip )
            skip 
            loop
         endif
         a:= a + 1
         bloqueareg(0)
         CostoPromedio = 0
         PrecioNuevo = 0
         PrecioNuevo = pre + pre * porm / 100

   CostoPromedio:= PrecioNuevo - PrecioNuevo * De1 / 100
   CostoPromedio:= CostoPromedio - CostoPromedio * De2 / 100
   CostoPromedio:= CostoPromedio - CostoPromedio * fi1 / 100
   CostoPromedio:= CostoPromedio - CostoPromedio * fi2 / 100         

         replace pre with PrecioNuevo,  cpr with CostoPromedio, fec with date()
         @ 15, cdis + 11 say art
         @ 16, cdis + 11 say nom
         @ 17, cdis + 11 say Transform(pre, "999999.99")
         @ 21, cdis + 32 say a picture "999999"
         skip 
      enddo
      set color to 
   endif

**********************************************************^
*Programa mellizo de ELESTO para que en vez de llamar
*ACPALF llame a ACPALF2 que tiene la incorporación de 
*la actualización del costo promedio y la fecha de
*actualización.
**********************************************************
procedure ELESTO2

   listad:= 1
   do case
   case f = 1
      busclave:= Nil
      lcla:= 19
      band5:= 30
      basem:= " PRODUCTOS "
      ultauxi:= "PROSTO"
      use (bsto) shared index (icsto), (insto), (ipsto), (itsto), (iasto)
      mantenimie("MAPALF", "LIPALF")
   case f = 2
      ultauxi:= "PROCED"
      basem:= "PROCEDENCIA"
      use (bced) shared index (icced), (inced)
      mantenimie("MAXCTA", "LIXCTA")
   case f = 3
      basem:= "TIP.PRODUC."
      ultauxi:= "PROTIP"
      use (btip) shared index (ictip), (intip)
      mantenimie("MAXCTA", "LIXCTA")
   case f = 4
      disalf()
   case f = 5
      acpalf2()
   case f = 6
      band5:= 1
      stoalf()
   case f = 7
      band5:= 2
      stoalf()
   case f = 8
      lcla:= 19
      busclave:= Nil
      band5:= 30
      ultauxi:= "PROSTO"
      select 1
      use (bsto) shared index (icsto), (insto), (ipsto), (itsto), (iasto)
      mantenimie("MANSTO", "LISSTO")
   case f = 9
      rinsto()
   case f = 10
**      pfa_messagebox(palcla,PFA_Decript(palcla,"funcion")) &&   VERIFICACION PALABRA CLAVE 20111023

**********************************************************************
**      23/10/2011 - ciesto() Se reemplazó la llamada a esta función para ser reemplazada por ciesto2()
**      que controla la palabra clave de balance desencriptada
**********************************************************************

      ciesto2()
   case f = 11
      revsto()
   case f = 12
      regsto()
   endcase
   return


********************************
Procedure CIESTO2
* Reemplaza a ciesto por el control de la palabra clave de Balance
********************************

   cola:= 14
   Set Color To I
   @  5, cola - 2 Clear To 15, cola + 30
   @  5, cola - 2 To 15, cola + 30
   @  7, cola - 1 To  7, cola + 29
   @ 12, cola - 1 To 12, cola + 29
   Set Color To W+/R
   @  6, cola - 1 Say "        CIERRE DE STOCK        "
   Set Color To I
   @  9, cola Say "Reg. Stock en Cero  (S/N) :   "
   @ 10, cola Say "Confirma el Proceso (S/N) :   "
   @ 14, cola Say "Productos Procesados :        "
   Set Color To 
   stbl:= "N"
   conf:= "N"
   If (PFA_Decript(palcla,"funcion") = PF_CLABAL)       && controla si se ha ingresado con la clave de balance que se encuentra en la constante PF_CLABAL
      @  9, cola + 28 Get STBL Picture "@!" Valid stbl $ "SN"
   EndIf
   @ 10, cola + 28 Get CONF Picture "@!" Valid conf $ "SN"
   Read
   Set Color To I
   If (conf = "N" .OR. LastKey() = K_ESC)
   Else
      abredbf(bpre, .T., 0)
      Zap
      abredbf(bmst, .T., 0)
      Zap
      Index On STR(ART)+DTOS(FEC)+TIP To (iamst)
      Index On MAR+TIP+STR(COD)+COM+STR(NRO) To (icmst)
      Close Databases
      Select 1
      Use (bsto) Shared Index (icsto)
      Do While (!EOF())
         bloqueareg(0)
         Replace ven With 0
         Replace com With 0
         Replace rev With 0
         Replace dev With 0
         Replace rec With 0
         Replace dec With 0
         Replace res With 0
         Skip 
      EndDo
      temsto(brev, "REV")
      temsto(brec, "REC")
      If (stbl = "N")
         Select 2
         Use (bmst) Shared Index (iamst), (icmst)
      EndIf
      Select 1
      Goto Top
      r:= 0
      fecm:= Date()
      Do While (!EOF())
         r:= r + 1
         @ 14, cola + 24 Say r Picture "99999"
         bloqueareg(0)
         If (stbl = "S")
            Replace sto With 0
         Else
            artm:= cod
            canm:= sto
            cprm:= cpr
            If (cprm = 0 .AND. canm > 0)
               cprm:= pre - pre * de1 / 100
               cprm:= cprm - cprm * de2 / 100
            EndIf
            Select 2
            insertareg(0)
            Replace art With artm
            Replace can With canm
            Replace cpr With cprm
            Replace cos With cprm
            Replace tip With "I"
            Replace fec With Date()
            Select 1
         EndIf
         @ 13, cola + 0 Say nom
         Skip 
      EndDo
      temsto(bdev, "DEV")
      temsto(bdec, "DEC")
      temsto(bpev, "RES")
      Use (baux) Shared
      bloqueareg(0)
      Replace fecsto With Date()
      fecstom:= fecsto
   EndIf
***********************************************************


**********************************************************************
* 13/12/10 Función de encriptación de datos
* Origen: Bajada de http://www.elguille.info/Clipper/
* ********************************************************************
Function PFA_Cript(__xValue, __xPassWord)
Local __xUsePass, __xUseValue, nILoop, __nPos

Do Case
  Case ValType(__xPassWord) = "L"
    If __xPassWord
      __xUsePass := 1
    Else
      __xUsePass := 0
    EndI
  Case ValType(__xPassWord) = "N"
    __xUsePass := __xPassWord
  Case ValType(__xPassWord) = "C"
    __xUsePass := PFA_SumChars(__xPassWord)
  Case ValType(__xPassWord) = "D"
    __xUsePass := PFA_SumChars(dToc(__xPassWord))
  Othe
EndC
Do Case
  Case ValType(__xValue) = "L"
    If Par(__xUsePass)
      If __xValue
        __xUseValue = .T.
      Else
        __xUseValue = .F.
      EndI
    Else
      If __xValue
        __xUseValue = .F.
      Else
        __xUseValue = .T.
      EndI
    EndI
  Case ValType(__xValue) = "N"
    __xUseValue := __xValue - Int(__xUsePass / 2)
  Case ValType(__xValue) = "D"
    __xUseValue := __xValue + __xUsePass
  Case ValType(__xValue) = "C"
    __xUseValue := ""
    For nILoop = 1 to Len(__xValue)
      __nPos := Iif(Par(nILoop), 1, -1)
      __xUseValue += Chr(Asc(SubStr(__xValue, nILoop, 1)) + __xUsePass;
          * __nPos)
    Next nILoop

EndC
Return __xUseValue

***********************************************************************
* Utilizado para la encriptación y desencriptación
***********************************************************************
Func PFA_SumChars(__cCharStr)
Local nILoop, __nRespValue := 0, ___nPos := 1
For nILoop := 1 to Len(__cCharStr)
  ___nPos++
  If ___nPos >256
    ___nPos := 1
  EndI
  __nRespValue += Asc(SubStr(__cCharStr, nILoop, 1)) * ___nPos
  If __nRespValue > 4294967296 // 32-Bits
    __nRespValue -= 4294967296
  EndI
Next
Retu __nRespValue

Func Par(__nValue)
If Int(__nValue / 2) = __nValue / 2
  Retu .T.
Else
  Retu .F.
EndI
Retu .F.

***********************************************************************
* Desencripta
***********************************************************************
Func PFA_DeCript(__xValue, __xPassWord)
Local __xUsePass, __xUseValue, nILoop, __nPos
Do Case
  Case ValType(__xPassWord) = "L"
    If __xPassWord
      __xUsePass := 1
    Else
      __xUsePass := 0
    EndI
  Case ValType(__xPassWord) = "N"
    __xUsePass := __xPassWord
  Case ValType(__xPassWord) = "C"
    __xUsePass := PFA_SumChars(__xPassWord)
  Case ValType(__xPassWord) = "D"
    __xUsePass := PFA_SumChars(dToc(__xPassWord))
EndC
Do Case
  Case ValType(__xValue) = "L"
    If Par(__xUsePass)
      If __xValue
        __xUseValue = .T.
      Else
        __xUseValue = .F.
      EndI
    Else
      If __xValue
        __xUseValue = .F.
      Else
        __xUseValue = .T.
      EndI
    EndI
  Case ValType(__xValue) = "N"
    __xUseValue := __xValue + Int(__xUsePass / 2)
  Case ValType(__xValue) = "D"
    __xUseValue := __xValue - __xUsePass
  Case ValType(__xValue) = "C"
    __xUseValue := ""
    For nILoop = 1 to Len(__xValue)
      __nPos := Iif(Par(nILoop), 1, -1)
      __xUseValue += Chr(Asc(SubStr(__xValue, nILoop, 1));
         - __xUsePass / __nPos)
    Next nILoop
EndC
Retu __xUseValue

******************************************************************
* 17/12/2010 - Pasos para implementar este cambio
* 1) Generar los 2 ejecutables con y sin fiscal
* 2) Llevar claalf.dbf, cclaalf.ntx, xclaalf.ntx, nclaalf.ntx
* 3) Backupear los mismos archivos
* 4) Remplazar ejecutabales, base e indices
* 5) Probar que todo funcione en la PC de adelante
* 6) Reemplazar los ejecutables en el directorios WKS
******************************************************************
PROCEDURE PFA_ACTCLAVE
LOCAL ACTUAL
PUBLIC xUSU, xCLA, xCOD

SAVE SCREEN
ACTUAL = SETCOLOR()


SELE 57
use
USE claalf index cclaalf, nclaalf, xclaalf

GO TOP

DO PFA_PanClave

DO pfa_MueveClave

DO PFA_MostClave

DO WHILE .T.
    @ 13,  7 prompt " Prim "
    @ 13, 13 prompt " Ante "
    @ 13, 19 prompt " Sigui "
    @ 13, 26 prompt " Ulti "
    @ 13, 32 prompt " Modif "
    @ 13, 38 prompt " Salir "
    MENU TO topmenu
    DO CASE
    	CASE topmenu = 0		// ESCAPE
    		SETCOLOR(ACTUAL)
    		RESTORE SCREEN
    		return
    	CASE topmenu = 1		// PRIMERO
    		GO TOP

		DO pfa_MueveClave

            DO PFA_MostClave
    	CASE topmenu = 2		// ANTERIOR
    		if .not. bof()
	    		SKIP -1

			DO pfa_MueveClave

                  DO PFA_MostClave
		endif
    	CASE topmenu = 3		// SIGUI
    		SKIP 1
			if eof()
				go bott
			endif

			DO pfa_MueveClave

                  DO PFA_MostClave
    	CASE topmenu = 4		// ULTI
    		GO BOTT

		DO pfa_MueveClave

            DO PFA_MostClave
    	CASE topmenu = 5		// MODIF
            DO pfa_ModiClave

    	CASE topmenu = 6		// SALIR
    		SETCOLOR(ACTUAL)
    		RESTORE SCREEN
    		RETURN
    END CASE
ENDDO

SELE 57
use

RETURN

******************************************************************
******************************************************************
procedure pfa_panClave
SET COLOR TO
@ 5,5 CLEAR TO 18,78
SET COLOR TO I
@ 5,5 TO 18,78 DOUBLE
SET COLOR TO


@  7,  16 say "Código        :"
@  8,  16 say "Usuario       :"
@  9,  16 say "Clave         :"
@ 10,  16 say "Confimar clave:"

return

procedure pfa_MostClave
@  7,  32 say xCod
@  8,  32 say xUsu
@  9,  32 say xCla
Return

procedure pfa_MueveClave
xCod = claalf->cod
xUsu = claalf->nom
xCla = PFA_Decript(claalf->cla,"funcion")
Return


******************************************************************
******************************************************************
procedure pfa_ModiClave
do while .t.
	@ 13,7 say space(70)
      @  7,  32 say xCod
      @  8,  32 get xUsu
      @  9,  32 get xCla
	read

	@ 13,  7 prompt " Graba "
	@ 13, 14 prompt " Cancela "
	@ 13, 23 prompt " Reingresa "
	menu to optmodif
	do case
	case optmodif = 0
		do pfa_MostClave
		exit
	case optmodif = 1
		Replace claalf->cod with xCod, claalf->nom with xUsu, claalf->cla with PFA_Cript(xCla,"funcion") 

		do pfa_MostClave
		exit
	case optmodif = 2
		do pfa_MostClave
		exit
	endcase
enddo
Return

***************************************
procedure ELECTA2
***************************************
* Reemplaza a ELECTA para poder ver de que manera se ejecuta esta función
***************************************
*PFA_messagebox("ELECTA2","Pasa por aqui")
   impdocm:= "IMPREC"
   urm:= 0
   colprom:= 43
   larprom:= 36
   docpag:= "OP"
   nompag:= " O.PAGO N§   "
   docnro:= "ULTOPAM"
   blacta:= 0
   bandpar:= 0
   busclave:= "BUSQCN"
   if ( bandera = 1 )
      copcomm:= coprecm
      if ( abrclim = "ROS" )
         impdocm:= "IREROS"
      endif
      lco:= 4
      abrbandera:= "CLI"
      docpag:= "RE"
      nompag:= " RECIBO N§   "
      docnro:= "ULTRECM"
      bcta:= bctc
      iccta:= icctc
      iocta:= ioctc
      ifcta:= ifctc
      ipcta:= ipctc
      ixcta:= ixctc
      bsal:= basc
      idsal:= idasc
      icsal:= icasc
      irsal:= irasc
      linprom:= 11
*PFA_messagebox("ELECTA2","f vale->"+str(f))

      do case
      case f = 1
         busclave:= "CLACUI"
         basem:= "   CLIENTES"
         band5:= 29
         ultauxi:= "ULTCLI"
         use (bcli) shared index (iccli), (incli), (ircli), (ivcli), (izcli), (ihcli)
         mantenimie("MANSIS", "LIXCTA")
      case f = 2
         listado("LISCTA")
      case f = 3
*         PFA_messagebox("ELECTA2","entra con f=3 y man_CTA->"+man_cta)

         band6:= 1
         mantenimie(man_cta, "LISCTA", 1, "CTACON")
      case f = 4
         pagcta()
      case f = 5
         linprom:= 15
         bandera:= 7
         paggas()
         bandera:= 1
      case f = 6
         basem:= "RUBROS CLI."
         ultauxi:= "ULTRUB"
         use (brub) shared index (icrub), (inrub)
         mantenimie("MAXCTA", "LIXCTA")
      case f = 7
         basem:= "      ZONAS"
         ultauxi:= "ULTZON"
         use (bzon) shared index (iczon), (inzon)
         mantenimie("MAXCTA", "LIXCTA")
      case f = 8
         basem:= "PROVINCIAS "
         band5:= 23
         ultauxi:= "ULTPRV"
         use (bprv) shared index (icprv), (inprv)
         mantenimie("MAXCTA", "LIXCTA")
      case f = 9 .AND. abrclim = "ROS"
         basem:= "REGIONES"
         ultauxi:= "ULTREG"
         use (breg) shared index (icreg), (inreg)
         mantenimie("MAXCTA", "LIXCTA")
      endcase
   elseif ( bandera = 2 )
      copcomm:= coppagm
      lco:= 4
      abrbandera:= "PRO"
      bcta:= bctp
      iccta:= icctp
      iocta:= ioctp
      ifcta:= ifctp
      ipcta:= ipctp
      ixcta:= ixctp
      bsal:= basp
      idsal:= idasp
      icsal:= icasp
      irsal:= irasp
      linprom:= 12 - ( aretgan + aretibr + aretiva + adesobt )
      do case
      case f = 1
         busclave:= "CLACUI"
         band5:= 28
         basem:= "PROVEEDORES"
         ultauxi:= "ULTPRO"
         use (bpro) shared index (icpro), (inpro), (irpro), (ihpro)
         mantenimie("MANSIS", "LIXCTA")
      case f = 2
         listado("LISCTA")
      case f = 3
         band6:= 1
         mantenimie(man_cta, "LISCTA", 1, "CTACON")
      case f = 4
         pagcta()
      case f = 5
         linprom:= 13
         bandera:= 4
         paggas()
         bandera:= 2
      case f = 6
         basem:= "RUBROS PROV"
         ultauxi:= "ULTRUP"
         use (brup) shared index (icrup), (inrup)
         mantenimie("MAXCTA", "LIXCTA")
      case f = 7
         select 2
         use (bpro) shared index (icpro)
         select 1
         use (bret) shared index (ifret)
         listado("LISRET")
      case f = 8
         parret()
      case f = 9
         select 2
         use (bpro) shared index (icpro)
         select 1
         use (briv) shared index (ifriv)
         listado("LISRIV")
      endcase
   elseif ( bandera = 3 )
      copcomm:= coppagm
      lco:= 2
      abrbandera:= "VEN"
      bcta:= bctv
      iccta:= icctv
      iocta:= ioctv
      ifcta:= ifctv
      ipcta:= ipctv
      bsal:= basv
      idsal:= idasv
      icsal:= icasv
      irsal:= irasv
      linprom:= 13
      do case
      case f = 1
         band5:= 27
         basem:= " VENDEDORES"
         ultauxi:= "ULTVEN"
         use (bven) shared index (icven), (inven)
         mantenimie("MAXCTA", "LIXCTA")
      case f = 2
         listado("LISCTA")
      case f = 3
         band6:= 1
         mantenimie(man_cta, "LISCTA", 1, "CTACON")
      case f = 4
         if ( abrclim = "ROS" )
            rutpag1:= "RUPAG1"
            rutpag2:= "RUPAG2"
            rutpag3:= "RUPAG3"
         endif
         pagcta()
      endcase
   endif
   rutpag1:= Nil
   rutpag2:= Nil
   rutpag3:= Nil

Return

***************************************
Procedure BUSVEN2
***************************************
* Reemplaza a BUSVEN para controlar la llamada a BUXFAC2 en vez de BUXFAC
***************************************


   If (comm = "FC" .OR. comm = "NC" .OR. comm = "ND")
      Use (bctc) Shared Index (ioctc)
      If (LastKey() == K_PGDN)
         Seek Str(codm, 4) + comm
         If (!Found())
            aviso("NO SE REGISTRAN " + comm, 1, 19, pol + 7)
            nohalla:= 1
            Close Databases
            Return
         Else
            cant:= 0
            pedi:= 1
            ren_nom:= ;
               "strzero(nro,8)+'  '+MUEFEC(fec)+' '+trans(imp,'@e 99999999.99')+'  '+mot+' '"
            tit_nom:= "N£mero    Fec.Emi.     Importe Obs"
            busnom(" ", "BUXFAC2", ren_nom, tit_nom, Nil, 1) 
            If (cant == 0)
               aviso("NO SE REGISTRAN " + comm, 1, 19, pol + 7)
               nohalla:= 1
               Close Databases
               Return
            EndIf
         EndIf
      Else
         Set Color To 
         nrom:= 0
         @  1, 70 Get NROM Picture "@Z 99999999" Valid nrom > 0
         Read
         If (LastKey() == K_ESC)
            nohalla:= 1
            Close Databases
            Return
         EndIf
         Seek Str(codm, 4) + comm + Str(nrosucm, 4) + Str(nrom, 8)
         If (!Found())
            aviso("NO EXISTE ESTE COMPROBANTE", 1, 22, 10)
            nohalla:= 1
            Close Databases
            Return
         EndIf
      EndIf
      pedi:= 3
      motm:= mot
      If (motm = "F")
         motm:= "A"
      EndIf
      desm:= des
      fopm:= fop
      opem:= ope
      horm:= hor
      sucm:= suc
      nrom:= nro
      comm:= com
      fecm:= fec
      fvem:= fve
      venm:= ven
      imvm:= imv
      pcom:= pco
      pibm:= pib
      impm:= imp
      gram:= gra
      ivfm:= iv1
      cosm:= cos
      flem:= fle
      tarm:= tar
      cuom:= cuo
      vt2m:= vt2
      dv2m:= dv2
      vt3m:= vt3
      dv3m:= dv3
      vtom:= fve - fec
      nroasim:= asi
      If (motm = "R")
         tmox:= "PR"
      EndIf
      Set Color To w+
      @  1, 65 Say strzero(sucm, 4)
      @  1, 70 Say strzero(nrom, 8)
      @  2, 55 Say muefec(fecm)
      @  2, 70 Say muefec(fvem)
      Do Case
      Case comm = "FC"
         buscre()
      Case motm = "A"
         busnov()
         Return
      Case motm = "H"
         buschr()
         Return
      Case motm = "H"
         buschr()
      EndCase
      tarn:= Space(10)
      If (tarm != 0)
         Use (btar) Shared Index (ictar)
         Seek tarm
         If (Found())
            tarn:= SubStr(nom, 1, 10)
         EndIf
      EndIf
      Set Color To w+
      @  4, 26 Say cuom Picture "@z 99"
      @  4, 34 Say tarn
      @  5, 16 Say vtom Picture "999"
      @  5, 20 Say vt2m Picture "999"
      @  5, 26 Say dv2m Picture "99.99"
      @  5, 33 Say vt3m Picture "999"
      @  5, 38 Say dv3m Picture "99.99"
      Set Color To 
      abrven()
   Else
      abrven()
      Seek Str(codm, 4)
      If (!Found())
         aviso("NO SE REGISTRAN " + comm, 1, 19, pol + 7)
         nohalla:= 1
         Return
      EndIf
      ren_nom:= "strzero(nro,8)+'   '+MUEFEC(fec)+' '"
      tit_nom:= " N£mero       Fecha "
      busnom(" ", "BUXCOM", ren_nom, tit_nom, Nil, 1)
      pedi:= 3
      penm:= 0
      fopm:= fop
      opem:= ope
      horm:= hor
      nrom:= nro
      fecm:= fec
      Set Color To w+
      @  1, 70 Say strzero(nrom, 8)
      @  2, 54 Say muefec(fecm)
      If (comm != "PR")
         retm:= ret
      EndIf
      If (comm = "RV")
         tmox:= tmo
         desm:= des
      ElseIf (comm = "PV")
         tmox:= tmo
         desm:= des
      ElseIf (comm = "PR")
         npev:= npv
      EndIf
   EndIf
   Set Color To 
   leeven(Str(codm, 4) + Str(nrom, 8), "STR(COD)+STR(NRO)", comm)
   If (mueconm != Nil)
      &mueconm()
   EndIf
   Close Databases

***************************************
Procedure BUXFAC2
***************************************
* Reemplaza a BUXFAC2 para controlar el tamaño del vector tabla
***************************************
LOCAL BindOFlow, Bpuntero, BmaxLen

BindOFlow = 0      // indicador de overflow de tabla 0 => OK    1 => overflow
Bpuntero = 1       // puntero al último elemento
BmaxLen = 1000     // cantidad de elementos máxim

*pfa_messagebox("Enta en buxfac","tabla mide: "+str(Len(tabla)))

   Do While (cod = codm .AND. com = comm .AND. !EOF())
      If (fec < fecctam)
         Skip 
         Loop
      ElseIf (comm = "NC" .AND. motm = " " .AND. mot != " ")
         Skip 
         Loop
      ElseIf (comm = "NC" .AND. motm != " " .AND. mot = " ")
         Skip 
         Loop
      EndIf

************25/03/2011************************************************************
      if len(TABLA) >= BmaxLen
          if BindOFlow = 0
	        BindOFlow = 1   // hay overflow     
    	        Bpuntero = 1
              PFA_Messagebox("Atenci¢n! Solo se mostrar n los £ltimos " + alltrim(str(BmaxLen)) + " registros.", "")
    	        TABLA[Bpuntero, 1] = &ren_nom
    	        TABLA[Bpuntero, 2] = RECNO()
    	    else
    	        TABLA[Bpuntero, 1] = &ren_nom
    	        TABLA[Bpuntero, 2] = RECNO()
    	        Bpuntero = Bpuntero + 1
    	        if Bpuntero = BmaxLen + 1
    	            Bpuntero = 1
    	        endif
          endif
      else
          AAdd(tabla, {&ren_nom, RecNo()})
   	endif      
      Skip
   EndDo
   cant:= Len(tabla)



***********************************************************************************
* Las siguientes líneas fueron reemplazadas
*****    AAdd(tabla, {&ren_nom, RecNo()})
*****    Skip 
*****    EndDo
*****    cant:= Len(tabla)


*pfa_messagebox("Sale en buxfac","tabla mide: "+str(Len(tabla)))

   Return