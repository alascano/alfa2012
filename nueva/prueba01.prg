*
* Este programa realiza un ticket y una factura A con los impresores
* fiscales de Compa¤ia HASAR.
* Linkear este programa con la libreria fiscal.lib
*
* Compa¤ia HASAR
* 03/12/98
*

#define PF_DEBUG
#define fiscal_log
#undef PF_DEBUG

#include "pfiscal.ch"
#include "Fileio.ch"



PUBLIC Handler, Port, n, Se, Com

	* Separador de campos
	Se = CHR(28)
	Com = "1"

	n = VAL(Com)

	if ( n < 0 .OR. n > 4 )
		return
	endif

	* Abro el Port de Comunicaciones
	Handler = OpenPort (Com)

	pfa_messagebox(str(Handler),'')
	
	* SearchPr:
	* Busca al controlador. No es necesario llamar a esta
	* funcion toda vez que se arranque el programa. Una vez
	* establecida la velocidad de trabajo, utilizar la funcion
	* SetBaud para fijar la velocidad en la PC.

	BaudRate = SearchPr (Handler)
	? "Controlador Fiscal detectado a " + alltrim(str(BaudRate))

    * Si el controlador esta a 19200, esta funcion fija la velocidad
	* de la PC en 19200.
	* SetBaud (Handler, 19200)

	* Nuevo Protocolo.
	* NO utilizar en impresores PR4, 262, 614, 615 y 950.
	NProtocol(1)

	* Inicializo el Printer
	InitFiscal (Handler)

	* respuesta
	resp = Respuesta(Handler)
    pfa_messagebox('Respuesta llamando a la función RESPUESTA()',resp)

	* Cancela un ticket que haya quedado por la mitad
	CancelTicket ()

	* Cierra el ticket que no pudo ser cerrado
	CloseTicket ()


	RETURN


	*
	* Establece el texto de encabezamiento y el texto de cola del
	* ticket
	*

	* SetHeaderTrailer ()

    * Imprime una factura
    ImprimirFactura ()

	* Cierra el Port
	ClosePort (Handler)


* ****
* ** Funcion: CancelTicket
* ****

FUNCTION CancelTicket

	s = "D" + Se + " " + Se + "0.00" + Se + "C" + Se + "0"
	Enviar (s)

RETURN 0

* ****
* ** Funcion: CloseTicket
* ****

FUNCTION CloseTicket

	s = "E"
	Enviar (s)

RETURN 0


* ****
* ** Funcion: SetHeaderTrailer
* ****

FUNCTION SetHeaderTrailer

*
* Establece las lineas a imprimir en el header y en el trailer.
* Esto NO hace falta hacerlo para cada ticket.
*

for i=1 to 10

	linea = alltrim(str(i))

	* s = "]" + Se + linea + Se + "Linea " + linea + " de Header"

	* Borra la linea
	s = "]" + Se + linea + Se + CHR(127)

	Enviar (s)

next

for i=11 to 20

	linea = alltrim(str(i))

	* s = "]" + Se + linea + Se + "Linea " + linea + " de Trailer"

	* Borra la linea
	s = "]" + Se + linea + Se + CHR(127)

	Enviar (s)

next

RETURN 0

* ****
* ** Funcion: ImprimirFactura
* ****

FUNCTION ImprimirFactura

	*
	* Programa un codigo de barras.
	*

	s = "Z" + Se + "1" + Se + "779123456789" + Se + "N" + Se + "P"
	Enviar (s)

	*
	* Datos del comprador
	*

	s = "b" + Se + "Emilio Soft" + Se + "20183697308" + Se + "N" + Se + "C" + Se + "Tapalqu‚ 761"
	Enviar (s)

	*
	* Abre un comprobante fiscal
	*

	s = "@" + Se + "A"
	Enviar (s)

	*
	* Venta de los articulos
	*

	s = "B" + Se + "Articulo 1" + Se + "10.0" + Se + "10.11" + Se + "21.00" + Se + "M" + Se + "0" + Se + "" + Se + "T"
	Enviar (s)

	*
	* Percepciones a aplicar
	*

	s = "`" + Se + "**.**" + Se + "Percepcion General" + Se + "23.00"
	Enviar (s)

	*
	* Cierra el ticket
	*

	s = "E"
	Enviar (s)

RETURN 0

FUNCTION HacerZ

	// Lee informacion caracteristica del impresor
	Enviar ("s")

	// Hace la Z
	Enviar ("9" + Se + "Z")

return 0





