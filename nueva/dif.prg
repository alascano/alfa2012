Comparando archivos pfiscal2.prg y pfiscal.prg
****** pfiscal2.prg
* IMPRESION DEL PAGO
select 56
use cargos alias xcargos
go top
DO WHILE .not. eof()
    do PF_IniciarSalida
    xcon=alltrim(xcargos->concepto)
    ximp=xcargos->importe
    ximp=ximp * 100
    ximp2=alltrim(str(ximp,8))
    nada = PF_AgregaCampoSalida(xcon)
    nada = PF_AgregaCampoSalida(ximp2)
    nada = PF_AgregaCampoSalida("R")
    if .not. PF_EnviarComando( PF_FCPago )
        * Si el error es por desbordamiento de items
        *    cierra el formulario
        *    abre un nuevo formulario
        *    intenta de imprimir el item que dio el error
        if isbit(PF_DatoRecibido[2], 7) .and. isbit(PF_DatoRecibido[2], 12)
            do PF_IniciarSalida
            nada = PF_AgregaCampoSalida( 'D' )
            nada = PF_AgregaCampoSalida( 'P' )
            nada = PF_AgregaCampoSalida( 'T' )
            nada = PF_AgregaCampoSalida( 'T' )
            if .not. PF_EnviarComando( PF_SysCommand )
                do PFA_mostrar_datos
                PFA_MueMen(fcodigo+' Error: 12 en impresi¢n de recargo',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ 
PF_MensajeEstado( PF_ModuloFiscal ))
                PFA_CerrDB(xarea)
                Return .f.
            endif
            *
            do PF_IniciarSalida
            nada = PF_AgregaCampoSalida( 'D' )
            nada = PF_AgregaCampoSalida( 'P' )
            nada = PF_AgregaCampoSalida( 'T' )
            nada = PF_AgregaCampoSalida( 'O' )
            if .not. PF_EnviarComando( PF_SysCommand )
                do PFA_mostrar_datos
                PFA_MueMen(fcodigo+' Error: 13 en impresi¢n de recargo',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ 
PF_MensajeEstado( PF_ModuloFiscal ))
                PFA_CerrDB(xarea)
                Return .f.
            endif

            * Vuelve a intentar imprimir el item que dio el error
            loop
        ELSE
            do PFA_mostrar_datos
            PFA_MueMen(fcodigo+' Error: 14 en impresi¢n de recargos',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_
MensajeEstado( PF_ModuloFiscal ))
            PFA_CerrDB(xarea)
            Return .f.
        endif
    ENDIF
    skip
ENDDO

****** pfiscal.prg
* IMPRESION DEL PAGO
if es_nd_cheque = .T.
    select 56
    use cargos alias xcargos
    go top
    Mdescadi1=PF_DEL
    MdescAdi2 = PF_DEL
        Mdescripcion=alltrim(xcargos->concepto)
    Mprecio=int(xcargos->importe * 100)/100
    Mprecio=Mprecio*100
        Mprecio2=ltrim(rtrim(str(Mprecio,8)))
    mcanti2='1000'
        Mtas_iva=param->tas_iva
        
        do PF_IniciarSalida
        nada = PF_AgregaCampoSalida( PFA_Limpiar(Mdescripcion) )  && Descripci¢n
        nada = PF_AgregaCampoSalida( mcanti2 )       && Cantidad  3,000
    nada = PF_AgregaCampoSalida( Mprecio2 )      && Precio   12,50
    nada = PF_AgregaCampoSalida( Mtas_iva)       && Tasa IVA 21,00%
    nada = PF_AgregaCampoSalida( Mtip_monto )    && Monto vendido de mercaderia
    nada = PF_AgregaCampoSalida( Mbultos )       && Bultos
    nada = PF_AgregaCampoSalida( Mtas_impint )   && Tasa de Ajuste Imp.Int.
    nada = PF_AgregaCampoSalida( MdescAdi1 )     && DEscrip adicional 1
    nada = PF_AgregaCampoSalida( MdescAdi2 )     && DEscrip adicional 2
    nada = PF_AgregaCampoSalida( PF_DEL )        && DEscrip adicional 3
    nada = PF_AgregaCampoSalida( Mtas_ivani )    && IVA Incremento no inscripto
    nada = PF_AgregaCampoSalida( Mmfi_impint  )  && Imp.Int. monto fijo

    if .not. PF_EnviarComando( PF_FCItemDeLinea )
                do PFA_mostrar_datos
        PFA_MueMen(fcodigo+' Error: 39 en impresi¢n de item',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ PF_MensajeE
stado( PF_ModuloFiscal ))
        PFA_CerrDB(xarea)
        Return .f.
    endif
ELSE
    select 56
    use cargos alias xcargos
    go top
    DO WHILE .not. eof()
       do PF_IniciarSalida
       xcon=alltrim(xcargos->concepto)
       ximp=xcargos->importe
       ximp=ximp * 100
       ximp2=alltrim(str(ximp,8))
       nada = PF_AgregaCampoSalida(xcon)
       nada = PF_AgregaCampoSalida(ximp2)
       nada = PF_AgregaCampoSalida("R")
       if .not. PF_EnviarComando( PF_FCPago )
           * Si el error es por desbordamiento de items
           *    cierra el formulario
           *    abre un nuevo formulario
           *    intenta de imprimir el item que dio el error
           if isbit(PF_DatoRecibido[2], 7) .and. isbit(PF_DatoRecibido[2], 12)
               do PF_IniciarSalida
               nada = PF_AgregaCampoSalida( 'D' )
               nada = PF_AgregaCampoSalida( 'P' )
               nada = PF_AgregaCampoSalida( 'T' )
               nada = PF_AgregaCampoSalida( 'T' )
               if .not. PF_EnviarComando( PF_SysCommand )
                   do PFA_mostrar_datos
                   PFA_MueMen(fcodigo+' Error: 12 en impresi¢n de recargo',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10
)+ PF_MensajeEstado( PF_ModuloFiscal ))
                   PFA_CerrDB(xarea)
                   Return .f.
               endif
               *
               do PF_IniciarSalida
               nada = PF_AgregaCampoSalida( 'D' )
               nada = PF_AgregaCampoSalida( 'P' )
               nada = PF_AgregaCampoSalida( 'T' )
               nada = PF_AgregaCampoSalida( 'O' )
               if .not. PF_EnviarComando( PF_SysCommand )
                   do PFA_mostrar_datos
                   PFA_MueMen(fcodigo+' Error: 13 en impresi¢n de recargo',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10
)+ PF_MensajeEstado( PF_ModuloFiscal ))
                   PFA_CerrDB(xarea)
                   Return .f.
               endif

               * Vuelve a intentar imprimir el item que dio el error
               loop
           ELSE
               do PFA_mostrar_datos
               PFA_MueMen(fcodigo+' Error: 14 en impresi¢n de recargos',PF_MensajeEstado( PF_ModuloImpresor )+chr(13)+chr(10)+ 
PF_MensajeEstado( PF_ModuloFiscal ))
               PFA_CerrDB(xarea)
               Return .f.
           endif
       ENDIF
       skip
    ENDDO
ENDIF

******


