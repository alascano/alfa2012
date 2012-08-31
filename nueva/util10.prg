*valor = 0
*valor_ant = 0
*use param
*valor_ant = param->tiempomax
*valor = valor_ant
*?'Valor del tiempo maximo de espera en la comunicacion'
*?valor_ant
*?'Desea cambiarlo? S/N'
*a=0
*do while a=0
* a=inkey()
*enddo
*?''
*if a=asc('S') .or. a=asc('s')
*   @row(),1 get valor
*   read
*   replace tiempomax with valor
*  ?
*   @row(),1 say 'Valor actual: '+str(valor)+'  Valor anterior: '+str(valor_ant)
*endif
* LASTKEY() devueve
*	27 -> Esc
*	 3 -> PageDown
*	18 -> PageUp
*	13 -> Enter

VALOR = 0
VALOR2 = '  '
DO WHILE VALOR <> 27
	@row(),1 get valor2
	read

	VALOR = LASTKEY()
	? VALOR
ENDDO
