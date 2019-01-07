       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC13.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION:
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

      **************FILE STATUS
       01 FS-ALUMNOS PIC XX.
               88 FS-ALUMNOS-OK     VALUE '00'.
               88 FS-ALUMNOS-EOF    VALUE '10'.
       01 FS-NOTAS PIC XX.
               88 FS-NOTAS-OK       VALUE '00'.
               88 FS-NOTAS-EOF      VALUE '10'.

       01 WK-SEPARADOR  PIC X(80)    VALUE ALL '*'.
       01 WK-SEPARADOR2 PIC X(80)    VALUE ALL '-'.

       01 OPCION PIC X.

       01 CT-CONTADORES.
           05 CT-ALUMNOS-TOTALES   PIC 99.
           05 CT-NOTAS-TOTALES     PIC 99.

      ************** ALUMNOS *******************************************
      *    CALL A MODULO "MODALUMN" PARA LECTURA DE FICHERO "ALUMNOS"
      ******************************************************************
           COPY C-ALUMNOS.

       01 WK-TITULO-ALUMNOS.
           05 FILLER               PIC X(3)    VALUE ' | '.
           05 WK-nif               pic x(9)    VALUE 'NIF'.
           05 FILLER               PIC X(3)    VALUE ' | '.
           05 WK-nombre            pic x(15)   VALUE 'NOMBRE '.
           05 FILLER               PIC X(3)    VALUE ' | '.
           05 WK-apellidos         pic x(30)   VALUE 'APELLIDOS'.
           05 FILLER               PIC X(3)    VALUE ' | '.
           05 WK-colegio           pic XXX     VALUE 'COL'.
           05 FILLER               PIC X(3)    VALUE ' | '.

       01 WK-reg-alumnos-gral.
           05 FILLER               PIC X(3)    VALUE ' | '.
           05 WK-nif               pic x(9).
           05 FILLER               PIC X(3)    VALUE ' | '.
           05 WK-nombre            pic x(15).
           05 FILLER               PIC X(3)    VALUE ' | '.
           05 WK-apellidos         pic x(30).
           05 FILLER               PIC X(3)    VALUE ' | '.
           05 WK-colegio           pic 99.
           05 FILLER               PIC X(4)    VALUE '  | '.

      ************** NOTAS *********************************************
      *    CALL A MODULO "MODNOTAS" PARA LECTURA DE FICHERO "NOTAS"
      ******************************************************************
           COPY C-NOTAS.

       01 WK-TITULO-NOTAS.
           05 FILLER               PIC X(3)    VALUE ' | '.
           05 WK-nif               pic x(9)    VALUE 'NIF'.
           05 FILLER               PIC X(3)    VALUE ' | '.
           05 WK-asignatura        pic X(4)    VALUE 'ASIG'.
           05 FILLER               PIC X(3)    VALUE ' | '.
           05 WK-nota              pic X(4)    VALUE 'NOTA'.
           05 FILLER               PIC X(3)    VALUE ' | '.

       01  WK-reg-notas.
           05 FILLER               PIC X(3)    VALUE ' | '.
           05 WK-nif               pic x(9).
           05 FILLER               PIC X(5)    VALUE ' |   '.
           05 WK-asignatura        pic 9.
           05 FILLER               PIC X(5)    VALUE '  |  '.
           05 WK-nota              pic 99.
           05 FILLER               PIC X(4)    VALUE '  | '.

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM 1000-INICIO
           PERFORM 2000-CALCULO-ALUMNOS
           PERFORM 3000-CALCULO-NOTAS
      *     PERFORM 4000-CALCULO-FINAL
           PERFORM 5000-FIN
           .

       1000-INICIO.
           INITIALIZE WK-reg-alumnos-gral
           INITIALIZE FS-ALUMNOS
           INITIALIZE FS-NOTAS
           INITIALIZE CT-CONTADORES
           .

      ******************************************************************
      *    2000-CALCULO-ALUMNOS                                        *
      *    MOSTRAR DATOS DE LOS ALUMNOS Y CONTADOR DE CUANTOS HAY      *
      ******************************************************************
       2000-CALCULO-ALUMNOS.
           DISPLAY WK-SEPARADOR
           DISPLAY WK-TITULO-ALUMNOS
      *OPEN FILE
           MOVE 'O' TO OPCION
           CALL "MODALUMN" USING
                 reg-alumnos-gral OPCION FS-ALUMNOS
           END-CALL
           IF FS-ALUMNOS-OK THEN
               MOVE 'F' TO OPCION
           ELSE
               DISPLAY '*FS-ALUMNOS*' FS-ALUMNOS
           END-IF
      *READ LINES UNTIL ERROR
           PERFORM UNTIL not FS-ALUMNOS-OK
               CALL "MODALUMN" USING
                 reg-alumnos-gral OPCION FS-ALUMNOS
               END-CALL

               IF FS-ALUMNOS-OK THEN
                   MOVE NIF OF reg-alumnos-gral
                     TO WK-nif OF WK-reg-alumnos-gral
                   MOVE NOMBRE TO WK-nombre OF WK-reg-alumnos-gral
                   MOVE APELLIDOS TO WK-apellidos OF WK-reg-alumnos-gral
                   MOVE COLEGIO TO WK-colegio OF WK-reg-alumnos-gral

                   DISPLAY WK-reg-alumnos-gral
                   ADD 1 TO CT-ALUMNOS-TOTALES
               ELSE
                   IF NOT FS-ALUMNOS-EOF THEN
                       DISPLAY '05*FS-ALUMNOS*' FS-ALUMNOS
                   END-IF
               END-IF
           END-PERFORM
      *CLOSE FILE
           MOVE 'C' TO OPCION
               CALL "MODALUMN" USING
                   reg-alumnos-gral OPCION FS-ALUMNOS
           END-CALL
           DISPLAY WK-SEPARADOR2
           DISPLAY 'ALUMNOS TOTALES: ' CT-ALUMNOS-TOTALES
           .
      ******************************************************************
      *    3000-CALCULO-NOTAS                                          *
      *    MOSTRAR DATOS DE NOTAS Y CONTADOR DE CUANTAS HAY            *
      ******************************************************************
       3000-CALCULO-NOTAS.
           DISPLAY WK-SEPARADOR
           DISPLAY WK-TITULO-NOTAS
      *OPEN FILE
           MOVE 'O' TO OPCION
           CALL "MODNOTAS" USING
                 reg-notas OPCION FS-NOTAS
           END-CALL
           IF FS-NOTAS-OK THEN
               MOVE 'F' TO OPCION
           ELSE
               IF NOT FS-NOTAS-EOF THEN
                   DISPLAY '*FS-NOTAS*' FS-NOTAS
               END-IF
           END-IF
      *READ LINES UNTIL ERROR
           PERFORM UNTIL not FS-NOTAS-OK
               CALL "MODNOTAS" USING
                 reg-notas OPCION FS-NOTAS
               IF FS-NOTAS-OK THEN
                   MOVE NIF            OF reg-notas
                     TO WK-nif         OF WK-reg-notas
                   MOVE asignatura     OF reg-notas
                     TO WK-asignatura  OF WK-reg-notas
                   MOVE NOTA           OF reg-notas
                     TO WK-nota        OF WK-reg-notas

                   DISPLAY WK-reg-notas
                   ADD 1 TO CT-NOTAS-TOTALES
               ELSE
                   IF NOT FS-NOTAS-EOF THEN
                       DISPLAY '*FS-NOTAS*: ' FS-NOTAS
                   END-IF
               END-IF

           END-PERFORM
      *CLOSE FILE
           MOVE 'C' TO OPCION
               CALL "MODNOTAS" USING
                 reg-notas OPCION FS-NOTAS
           END-CALL
           DISPLAY WK-SEPARADOR2
           DISPLAY 'NOTAS TOTALES: ' CT-NOTAS-TOTALES
           .
      ******************************************************************
      *    4000-CALCULO-FINAL                                          *
      *    MOSTRAR ALUMNOS CON NOTA MEDIA Y NOTAS DE CADA ASIGNATURA   *
      ******************************************************************
       4000-CALCULO-FINAL.

           .
      ******************************************************************
      *    5000-FIN                                                    *
      ******************************************************************
       5000-FIN.
           PERFORM 9000-END-PROGRAM
           .
      ******************************************************************
      *    9000-END-PROGRAM                                            *
      ******************************************************************
       9000-END-PROGRAM.
           STOP RUN.
