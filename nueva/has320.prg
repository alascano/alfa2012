*
* Este programa realiza un ticket y una factura A con los impresores
* fiscales de Compa¤ia HASAR.
* Linkear este programa con la libreria fiscal.lib
*
* Compa¤ia HASAR
* 03/12/98
*

PARAMETERS Com



PUBLIC Handler, Port, n, Se

	* Separador de campos
	Se = CHR(28)

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
    pfa_messagebox()

	* Cancela un ticket que haya quedado por la mitad
	CancelTicket ()

	* Cierra el ticket que no pudo ser cerrado
	CloseTicket ()

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

*****
* Funcion: Enviar
*
* Envia un comando al impresor fiscal y analiza la respuesta.
*****

FUNCTION Enviar

PARAMETERS String

PRIVATE Result, StatPrn

? "Comm: " + String

* Si la funcion MandaPaq retorna un numero menor que cero, retorna.
* Esto puede ser por un problema de comunicaciones con el impresor.

StatPrn = MandaPaq (Handler, String)

* ? " StatPrn = " + alltrim (str(StatPrn))

IF ( StatPrn < 0 )
	DO WHILE StatPrn = -9
		? "Impresora Ocupada"
		String = "¡"	// consulta de estado intermedio
		StatPrn = MandaPaq (Handler, String)
		IF LASTKEY() = 27
			? "Fin de la consulta intermedia"
			RETURN -9
		ENDIF
		inkey (1)
	ENDDO
	? "Error enviando el comando"
	RETURN -1
ENDIF

* Levanta la respuesta.
Result = Respuesta (Handler)

? "Resp: " + Result

* Analiza si existe algun error.
GetErrors (Result)

? " "

RETURN 0

****
* FUNCTION GetErrors
*
* Esta funcion levanta la respuesta del printer e imprime en
* el mensaje de error si es que existe.
****

FUNCTION GetErrors

PARAMETERS Resp

PRIVATE Origen, OffsetSep, i, c

DECLARE FiscalErrors [16]
DECLARE PrinterErrors[16]

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
	RETURN -1
ENDIF

Origen = OffsetSep

* Analiza los bits comenzando del menos significativo
FOR i = 1 TO 16
	IF ( INT (PrinterStatus % 2) == 1 )
		IF ( LEN (PrinterErrors[i]) > 0 )
			? "PrinterStatus: " + PrinterErrors[i]
		ENDIF
	ENDIF
	PrinterStatus = PrinterStatus / 2
NEXT

OffsetSep = AT ( CHR(28), SUBSTR (Resp, Origen + 1) )

IF OffsetSep == 0
	OffsetSep = LEN(Resp)
ENDIF

* Convierte en hexa el status fiscal
FiscalStatus = HexaToInt (SUBSTR (Resp, Origen + 1, OffsetSep - 1))

IF FiscalStatus < 0
	RETURN -1
ENDIF

* Analiza los bits comenzando del menos significativo
FOR i = 1 TO 16
	IF ( INT (FiscalStatus % 2) == 1 )
		IF ( LEN (FiscalErrors[i]) > 0 )
			? "FiscalStatus: " + FiscalErrors[i]
		ENDIF
	ENDIF
	FiscalStatus = FiscalStatus / 2
NEXT

RETURN 0

****
* FUNCTION HexaToInt
*
* Esta funcion convierte un numero hexadecimal en su equivalente
* en binario.
****

FUNCTION HexaToInt

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
function PFA_messagebox(mensaje1, mensaje2)
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
men1=alltrim(mensaje1)
men2=alltrim(mensaje2)
if len(men1) > 60
   men11 = left(men1,60)
   men12 = left(substr(men1,61),60)
else
   men11 = men1
   men12 = ""
endif
if len(men2) > 60
   men21 = left(men2,60)
   men22 = left(substr(men2,61),60)
else
   men21 = men2
   men22 = ""
endif
@ 12, 11 say men11
@ 13, 11 say men12
@ 14, 11 say men21
@ 15, 11 say men22
@ 17, 11 get nada
READ
SETCOLOR(ACTUAL)
RESTORE SCREEN
RETURN


