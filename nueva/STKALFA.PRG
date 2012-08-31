*:*********************************************************************
*:        Program: stkalfa.PRG
*:*********************************************************************
set confirm off
set color to

@  1, 0 to 22, 79 clear

CLOSE TABLES all

select 1            && se define el area de trabajo 92 para la tabla 
                    && de articulos
use stoalf &&index astoalf,cstoalf,nstoalf

* cada vez que se hace referencia a una de estas tablas se selecciona 
* un area de trabajo
* area 91 -->  (a->)   area 92 -->  (b->)    

@  2,  1  to 18, 78

subtitu = '  Actualizacion de Movimientos de Stock  '
@  2, 40-len(subtitu)/2  say subtitu
@  4, 11  say 'Fecha:'
@  8, 11  say 'Producto          :'
@ 10, 11  say 'Articulo          :'
@ 12, 11  say 'Existencia Actual :'
@ 14, 11  say 'Cantidad          :'
@ 16, 11  say 'Nueva Existencia  :'

save screen to pzona          && se guarda en memoria la pantalla

mo_fec = date()

do while .t.

   linea='Ingrese fecha del dia. <Enter> Acepta.'
   @ 24, 0 to 24, 79 clear
   @ 24, 40-len(linea)/2 say linea

   @  4, 18  get  mo_fec               && pide fecha  en todos los casos se 
                                       && trabaja con variables transitorias
   read                                  
   
   if read() = 12 .or. read() = 268 
      close all
      set confirm on
      set escape on
      return
   endif

   save screen to pzona1

   cod_art = 0
   cant_art = 0

   do while .t.

      linea='Ingrese codigo del producto. <Enter> Acepta.'
      @ 24, 0 to 24, 79 clear
      @ 24, 40-len(linea)/2 say linea

      @  8, 31  get  cod_art picture '99999999'  && pide cod. articulo
      read
   
      if read() = 12 .or. read() = 268 .or. cod_art = 0 then
         restore screen from pzona  && en caso de escape se restaura 
                                    && la pantalla de memoria
         exit
      endif

      select 1
      go top
      locate for a->cod = cod_art

      if .not. found() then
         @ 22,  0 to 22, 79 clear
		 @ 22, 30 say ' NO EXISTE PRODUCTO. VERIFICAR !!! '
		 @ 22,  0 to 22, 79 clear
         loop
      endif

      set color to i
      @  8, 45 say a->nom function '@!'              && escribe en pantalla
                                                     && la descripcion
      @ 10, 31 say a->art                            && escribe en pantalla
                                                     && el articulo 
      @ 12, 31 say a->sto picture '99,999.99'        && escribe en pantalla
		   											 && el stock actual
      set color to
      nroop = 4

      ayuda = 'Ingrese cantidad. <Enter> Acepta.'
      @ 24, 0 to 24, 79 clear
      @ 24, 40-len(ayuda)/2 say ayuda
      @ 14, 31  get  cant_art && picture '99,999.999'
      read

      if read() = 12 .or. read() = 268
         loop
      endif

      if cant_art = 0
         @ 22,  0 to 22, 79 clear
		 @ 22, 30 say ' CANTIDAD = 0. VERIFICAR !!! '
		 @ 22,  0 to 22, 79 clear
         loop
      endif

      @ 24, 0 to 24, 79 clear

      ar_exi = a->sto + cant_art

      set color to i
      @ 16, 31 say ar_exi picture '99,999.999'
      set color to 

      if cant_art < 0 then
         cant_art = cant_art * (-1)
      endif

      ultili = 'Confirma ? (S/N)  '
      @ 24,  0 to 24, 79 clear
      @ 21, 40-len(ultili)/2 say ultili

      confi = ' '
      do while confi <> 'S' .and. confi <> 'N'
         confi = 'S'
         @ 21, 49 get confi function '!'
         read
      enddo           

      if read() = 12 .or. read() = 268 .or. confi = 'N'
         @ 21,  0 to 21, 79 clear
         loop
      endif                        

      select 1                              && abre el area 1 (articulo) y reemplaza
      replace a->sto with ar_exi            && las existencias

      restore screen from pzona1            && restaura la pantalla para hacer otro mvto.
 
      cod_art = 0
      cant_art = 0

      @ 21,  0 to 21, 79 clear

 enddo .t.

enddo .t.

*: EOF: stkalfa.PRG
