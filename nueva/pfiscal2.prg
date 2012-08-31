***pfiscal2.prg
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
   If (PFA_Decript(palcla,"funcion") = "QQWWE")
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

