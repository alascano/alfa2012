****Y2K****
/**********************
* Surround Versioning
**********************
* $Author: admin$
* $Date: S墎ado, 05 de Noviembre de 2005 7:26:06$
* $File: TABVYC.PRG$
* $Revision: 1.0$
* $Log$
* admin - S墎ado, 05 de Noviembre de 2005 7:26:06
* Alfredo - s墎ado, 29 de noviembre de 2003 11:01:31
* Mostrar fecha (stoalf.fec)+ moneda (stoalf.ori)+ tipo de articulo (stoalf.tip) al facturar.
*****************************************************/

PROCEDURE BLAVYC
if len(tcod) > LT
   asize(tcod,lt)
   asize(tnom,lt)
   asize(tpre,lt)
   asize(tcan,lt)
   asize(tdes,lt)
   asize(tuni,lt)
   asize(tunl,lt)
   asize(ttot,lt)
   asize(tiva,lt)
   asize(tde1,lt)
   asize(tde2,lt)
   asize(tsto,lt)
   asize(tcos,lt)
   asize(tcpr,lt)
   asize(tbaj,lt)
   asize(tfi1,lt)
   asize(tfi2,lt)
   asize(tpco,lt)
   asize(trem,lt)
   asize(tnpe,lt)
   asize(tadi,lt)
   asize(tres,lt)
   asize(tfpe,lt)
   asize(tvpe,lt)
   asize(tren,lt)
   asize(tmd1,lt)
   asize(tmd2,lt)
   asize(tmd3,lt)
   asize(tmd4,lt)
   asize(tobs,lt)
   asize(tart,lt)
   asize(tmon,lt)
   asize(tcer,lt)
   asize(talt,lt)
   asize(tgan,lt)
   asize(tvto,lt)
   asize(tlot,lt)
   asize(tmst,lt)
   asize(tlyv,lt)
endif
afill(tcod,0)
afill(tnom,space(lnom))
afill(tpre,0)
afill(tdes,space(lnom))
afill(tuni,1)
afill(tunl,1)
afill(tcan,blacan)
afill(ttot,0)
afill(tiva,space(01))
afill(tde1,0)
afill(tde2,0)
afill(tsto,0)
afill(tcos,0)
afill(tcpr,0)
afill(tbaj,0)
afill(tfi1,0)
afill(tfi2,0)
afill(tpco,0)
afill(trem,0)
afill(tnpe,space(30))
afill(tadi,space(60))
afill(tfpe,blafpe)
afill(tvpe,0)
afill(tren,0)
afill(tmd1,0)
afill(tmd2,0)
afill(tmd3,0)
afill(tmd4,0)
afill(tres,0)
afill(tobs,space(01))
afill(tart,space(19))
afill(tmon,space(30))
afill(tcer,0)
afill(talt,space(01))
afill(tgan,space(01))
afill(tvto,space(07))
afill(tlot,space(10))
afill(tmst,0)
afill(tlyv,space(01))
RETURN

****/agrega una posicion mas en las tablas************************************

PROCEDURE MASVYC
aadd(tcod,0)
aadd(tnom,space(lnom))
aadd(tpre,0)
aadd(tdes,space(lnom))
aadd(tuni,1)
aadd(tunl,1)
aadd(tcan,blacan)
aadd(ttot,0)
aadd(tiva,space(01))
aadd(tde1,0)
aadd(tde2,0)
aadd(tsto,0)
aadd(tcos,0)
aadd(tcpr,0)
aadd(tbaj,0)
aadd(tfi1,0)
aadd(tfi2,0)
aadd(tpco,0)
aadd(trem,0)
aadd(tnpe,space(30))
aadd(tadi,space(60))
aadd(tres,0)
aadd(tfpe,blafpe)
aadd(tvpe,0)
aadd(tren,0)
aadd(tmd1,0)
aadd(tmd2,0)
aadd(tmd3,0)
aadd(tmd4,0)
aadd(tobs,space(01))
aadd(tart,space(19))
aadd(tmon,space(30))
aadd(tcer,0)
aadd(talt,space(01))
aadd(tgan,space(01))
aadd(tvto,space(07))
aadd(tlot,space(10))
aadd(tmst,0)
aadd(tlyv,space(01))
RETURN

****/remplaza una posicion en las tablas**************************************

PROCEDURE REPVYC(zz,yy)
tcod[zz]=tcod[yy]
tnom[zz]=tnom[yy]
tpre[zz]=tpre[yy]
tcan[zz]=tcan[yy]
tdes[zz]=tdes[yy]
tuni[zz]=tuni[yy]
tunl[zz]=tunl[yy]
ttot[zz]=ttot[yy]
tiva[zz]=tiva[yy]
tde1[zz]=tde1[yy]
tde2[zz]=tde2[yy]
tsto[zz]=tsto[yy]
tcos[zz]=tcos[yy]
tcpr[zz]=tcpr[yy]
tbaj[zz]=tbaj[yy]
tfi1[zz]=tfi1[yy]
tfi2[zz]=tfi2[yy]
tpco[zz]=tpco[yy]
trem[zz]=trem[yy]
tnpe[zz]=tnpe[yy]
tadi[zz]=tadi[yy]
tres[zz]=tres[yy]
tfpe[zz]=tfpe[yy]
tvpe[zz]=tvpe[yy]
tren[zz]=tren[yy]
tmd1[zz]=tmd1[yy]
tmd2[zz]=tmd2[yy]
tmd3[zz]=tmd3[yy]
tmd4[zz]=tmd4[yy]
tobs[zz]=tobs[yy]
tart[zz]=tart[yy]
tmon[zz]=tmon[yy]
tcer[zz]=tcer[yy]
talt[zz]=talt[yy]
tgan[zz]=tgan[yy]
tvto[zz]=tvto[yy]
tlot[zz]=tlot[yy]
tmst[zz]=tmst[yy]
tlyv[zz]=tlyv[yy]
RETURN

****/inserta una posicion mas en las tablas***********************************

PROCEDURE AINVYC(zz)
ains(tcod,zz,0)
ains(tnom,zz,space(lnom))
ains(tpre,zz,0)
ains(tdes,zz,space(lnom))
ains(tuni,zz,1)
ains(tunl,zz,1)
ains(tcan,zz,blacan)
ains(ttot,zz,0)
ains(tiva,zz,space(01))
ains(tde1,zz,0)
ains(tde2,zz,0)
ains(tsto,zz,0)
ains(tcos,zz,0)
ains(tcpr,zz,0)
ains(tbaj,zz,0)
ains(tfi1,zz,0)
ains(tfi2,zz,0)
ains(tpco,zz,0)
ains(trem,zz,0)
ains(tnpe,zz,space(30))
ains(tadi,zz,space(60))
ains(tres,zz,0)
ains(tfpe,zz,blafpe)
ains(tvpe,zz,0)
ains(tren,zz,0)
ains(tmd1,zz,0)
ains(tmd2,zz,0)
ains(tmd3,zz,0)
ains(tmd4,zz,0)
ains(tobs,zz,space(01))
ains(tart,zz,space(19))
ains(tmon,zz,space(30))
ains(tcer,zz,0)
ains(talt,zz,space(01))
ains(tgan,zz,space(01))
ains(tvto,zz,space(07))
ains(tlot,zz,space(10))
ains(tmst,zz,0)
ains(tlyv,zz,space(01))
RETURN


****/borra una posicion en las tablas*****************************************

PROCEDURE BORVYC
scroll(w,4,tothoj,78,1)
adel(tcod,q)
adel(tnom,q)
adel(tpre,q)
adel(tcan,q)
adel(tdes,q)
adel(tuni,q)
adel(tunl,q)
adel(ttot,q)
adel(tiva,q)
adel(tde1,q)
adel(tde2,q)
adel(tsto,q)
adel(tcos,q)
adel(tcpr,q)
adel(tbaj,q)
adel(tfi1,q)
adel(tfi2,q)
adel(tpco,q)
adel(trem,q)
adel(tnpe,q)
adel(tadi,q)
adel(tres,q)
adel(tfpe,q)
adel(tvpe,q)
adel(tren,q)
adel(tmd1,q)
adel(tmd2,q)
adel(tmd3,q)
adel(tmd4,q)
adel(tobs,q)
adel(tart,q)
adel(tmon,q)
adel(tcer,q)
adel(talt,q)
adel(tgan,q)
adel(tvto,q)
adel(tlot,q)
adel(tmst,q)
adel(tlyv,q)

tcod[len(tcod)]=0

tnom[len(tcod)]=space(lnom)

tpre[len(tcod)]=0

tdes[len(tcod)]=space(lnom)

tuni[len(tcod)]=1

tunl[len(tcod)]=1

tcan[len(tcod)]=blacan

ttot[len(tcod)]=0

tiva[len(tcod)]=space(01)

tde1[len(tcod)]=0

tde2[len(tcod)]=0

tsto[len(tcod)]=0

tcos[len(tcod)]=0

tcpr[len(tcod)]=0

tbaj[len(tcod)]=0

tfi1[len(tcod)]=0

tfi2[len(tcod)]=0

tpco[len(tcod)]=0

trem[len(tcod)]=0

tnpe[len(tcod)]=space(30)

tadi[len(tcod)]=space(60)

tres[len(tcod)]=0

tfpe[len(tcod)]=blafpe

tvpe[len(tcod)]=0

tren[len(tcod)]=0

tmd1[len(tcod)]=0

tmd2[len(tcod)]=0

tmd3[len(tcod)]=0

tmd4[len(tcod)]=0

tobs[len(tcod)]=space(01)

tart[len(tcod)]=space(19)

tmon[len(tcod)]=space(30)

tcer[len(tcod)]=0

talt[len(tcod)]=space(01)

tgan[len(tcod)]=space(01)

tvto[len(tcod)]=space(07)

tlot[len(tcod)]=space(10)

tmst[len(tcod)]=0

tlyv[len(tcod)]=space(01)

*tx01[len(tcod)]=space(01)

@ 23,c1-1 say [設
@ 23,c2-1 say [設
@ 23,c3-1 say [設
@ 23,c4-1 say [設
@ 23,c5-1 say [設
@ 23,c6-1 say [設
@ 23,c7-1 say [設
@ 23,c8-1 say [設
if q+tothoj+1-w <= len(tcod)
   if tcod[q+tothoj+1-w] # 0
      antq = q
      antw = w
      q = q+tothoj-w
      w = tothoj
      DO &RENGLON
      q = antq
      w = antw
   endif
endif
w-=1
band7 = 0
RETURN
