****Y2K****
IF CONSULT=0
   CLOSE DATA
   ABREDBF(BAUX,.F.,0)
   BLOQUEADBF(0)
   NROM=&NROXXX
   REPL &NROXXX WITH &NROXXX+1
   PRONROM=NROXXX+'M'
   &PRONROM=&NROXXX
ENDIF

* pfa_messagebox("pasa ","por DOCVYC  CONSULT:"+ALLTRIM(STR(CONSULT))+ ;
* 		" COP:"+ALLTRIM(STR(COP))+ ;
* 		" PF_SINFIS:"+ALLTRIM(STR(PF_SINFISCAL))+ ;
* 		" MOTM:"+MOTM + ;
* 		" C:"+ALLTRIM(STR(C))+ ;
* 		" COMM:"+comm )
IF COP#0
    TIPDOCM=TIPXXX
    IF MOTM='F'.AND.C#1
 	    IF PF_SINFISCAL > 0
 		    bandloop = 0
 	    ELSE
            ABRPRN(LINPROM,COLPROM,LARPROM,LPTI)
        ENDIF
        if bandloop=1
             return
        endif
        AVISO('FORMULARIO ('+TDOC+') N§ '+STRZERO(NROM,8),1,22,10)
    ELSE
*  	    IF MOTM=' ' .and. C=1 .AND. ( comm='RV' .OR. comm='PR') 
* Esta línea fue reemplazada por la siguiente para que abra la impresora cuando va a imprimir formularios alternativos
        IF (MOTM=' ' .and. C=1 .AND. ( comm='RV' .OR. comm='PR')) .OR. (MOTM=' ' .and. C=1 .AND. comm='FC' .AND. MARC="X")
*       *    ABRPRN(LINPROM,COLPROM,LARPROM,LPTI)
  	        ABRPRN(LINPROM,01,44,LPTI)

* 		    pfa_messagebox("pasa ","por abrprn con IF MOTM=' ' .and. C=1 .AND. comm='RV'")

        ENDIF
    ENDIF


*    pfa_messagebox("cabxxx: "+CABXXX+" cuexxx: "+CUEXXX+" renxxx:"+STR(RENXXX)," piexxx: "+PIEXXX+" finxx: "+FINXXX+" nroxxx: "+NROXXX)

    DOCVEN(CABXXX,CUEXXX,RENXXX,PIEXXX,FINXXX)

    ****** IMPRESION FISCAL
    if MOTM='F' .and. C#1 .AND. comm='NC'
    *    PFA_messagebox('DOCVYC codm: '+str(codm)+' comm: '+comm,'nrom: '+str(nrom)+' nrosucm: '+str(nrosucm))
         PFA_ImpFiscal(codm,comm,nrom,nrosucm)
         nada = PFA_CtrNum()
 	endif
    ******

    IF CONSULT=0.AND.TRAXXX=1.AND.TRAM>0
       CLOSE DATA
       ABREDBF(BAUX,.F.,0)
       BLOQUEADBF(0)
       REPL &NROXXX WITH &NROXXX+TRAM
       &PRONROM=&NROXXX
    ENDIF
ENDIF
******************************************************************************

