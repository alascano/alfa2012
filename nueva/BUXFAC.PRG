procedure BUXFAC
   Do while (cod = codm .and. com = comm .and. !EOF())
       if (fec < fecctam)
           skip
           loop
       elseif (comm = "NC" .and. motm = " " .and. mot != " ")
           skip
           loop
       elseif (comm = "NC" .and. motm != " " .AND. mot = " ")
           skip
           loop
       endif
       AADD(tabla, {&ren_nom, RecNo()})
       skip
   enddo
   cant:= len(tabla)
