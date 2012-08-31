*+께께께께께께께께께께께께께께께께께께께께께께께께께께께께께께께께께께
*+
*+    Source Module => C:\INSTALAR\DOWNLO~1\LENGUA~1\CLIPPER\CLICK203\DOCVYC.PRG
*+
*+    Click! is a Clipper/Xbase++ source code reformatter.
*+    
*+    Copyright(C) 1996-1999 by Phil Barnett.
*+       
*+    This program is free software; you can redistribute it and/or modify it
*+    under the terms of the GNU General Public License as published by the
*+    Free Software Foundation; either version 2 of the License, or (at your
*+    option) any later version.
*+    
*+    This program is distributed in the hope that it will be useful, but
*+    WITHOUT ANY WARRANTY; without even the implied warranty of
*+    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*+    General Public License for more details.
*+    
*+    You should have received a copy of the GNU General Public License along
*+    with this program; if not, write to the Free Software Foundation, Inc.,
*+    675 Mass Ave, Cambridge, MA 02139, USA.
*+    
*+    You can contact me at:
*+    
*+    Phil Barnett
*+    Box 944
*+    Plymouth, Florida  32768
*+    
*+    or
*+    
*+    philb@iag.net
*+
*+    Reformatted by Click! 2.03 on Mar-31-2010 at  4:05 pm
*+
*+께께께께께께께께께께께께께께께께께께께께께께께께께께께께께께께께께께

// ***Y2K****
if CONSULT = 0
   close DATA
   ABREDBF(BAUX,.F.,0)
   BLOQUEADBF(0)
   NROM           := &NROXXX
   field->&NROXXX := &NROXXX+1
   PRONROM        := NROXXX+'M'
   &PRONROM       := &NROXXX
endif

pfa_messagebox("pasa ","por DOCVYC  CONSULT:"+alltrim(str(CONSULT))+;
                      " COP:"+alltrim(str(COP))+;
                      " PF_SINFIS:"+alltrim(str(PF_SINFISCAL))+;
                      " MOTM:"+MOTM+;
                      " C:"+alltrim(str(C))+;
                      " COMM:"+comm)
if COP # 0
   TIPDOCM := TIPXXX
   if MOTM = 'F' .and. C # 1
      if PF_SINFISCAL > 0
         bandloop := 0
         if MOTM = ' ' .and. C = 1 .and. comm = 'RV'
            ABRPRN(LINPROM,COLPROM,LARPROM,LPTI)

            pfa_messagebox("pasa ","por abrprn ")

         endif
      else
         ABRPRN(LINPROM,COLPROM,LARPROM,LPTI)
      endif
      if bandloop = 1
         return
      endif
      AVISO('FORMULARIO ('+TDOC+') N� '+strzero(NROM,8),1,22,10)
   endif

   pfa_messagebox("cabxxx: "+CABXXX+" cuexxx: "+CUEXXX+" renxxx:"+str(RENXXX)," piexxx: "+PIEXXX+" finxx: "+FINXXX+" nroxxx: "+NROXXX)

   DOCVEN(CABXXX,CUEXXX,RENXXX,PIEXXX,FINXXX)

   // ***** IMPRESION FISCAL
   if MOTM = 'F' .and. C # 1 .and. comm = 'NC'
      // 	    PFA_messagebox('DOCVYC codm: '+str(codm)+' comm: '+comm,'nrom: '+str(nrom)+' nrosucm: '+str(nrosucm))
      PFA_ImpFiscal(codm,comm,nrom,nrosucm)
      nada := PFA_CtrNum()
   endif
   // *****

   if CONSULT = 0 .and. TRAXXX = 1 .and. TRAM > 0
      close DATA
      ABREDBF(BAUX,.F.,0)
      BLOQUEADBF(0)
      field->&NROXXX := &NROXXX+TRAM
      &PRONROM       := &NROXXX
   endif
endif
// *****************************************************************************

*+ EOF: DOCVYC.PRG
