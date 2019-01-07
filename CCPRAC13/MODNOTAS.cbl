       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODNOTAS.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION:
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-NOTAS ASSIGN './NOTAS'
               FILE STATUS IS FS-NOTAS.
       DATA DIVISION.
       FILE SECTION.
       FD F-NOTAS
           RECORDING MODE IS F.

           COPY C-NOTAS.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  L-reg-notas.
           05 nif                 pic x(9).
           05 asignatura          pic 9.
           05 nota                pic 99.

       01 FS-NOTAS PIC XX.
           88 FS-NOTAS-OK     VALUE '00'.
           88 FS-NOTAS-EOF    VALUE '10'.

       01 OPCION PIC X.

       PROCEDURE DIVISION USING L-reg-notas OPCION FS-NOTAS.
      *     PERFORM 1000-INICIO
           PERFORM 2000-OPCION
           PERFORM 3000-FIN
           .

       1000-INICIO.
           MOVE nif            of L-reg-notas
             to nif            of reg-notas
           MOVE asignatura     of L-reg-notas
             to asignatura     of reg-notas
           MOVE nota           of L-reg-notas
             to nota           of reg-notas
           .

       2000-OPCION.
           EVALUATE OPCION
               WHEN 'O' PERFORM 2100-OPEN
               WHEN 'F' PERFORM 2200-READ
               WHEN 'C' PERFORM 2200-CLOSE
               WHEN OTHER DISPLAY '*MOD*NOTAS*OPCION*INVALIDA*'
           END-EVALUATE
           .

       2100-OPEN.
           OPEN INPUT F-NOTAS
           .

       2200-READ.
           READ F-NOTAS
           IF FS-NOTAS-OK THEN

               MOVE nif            of reg-notas
                 to nif            of L-reg-notas
               MOVE asignatura     of reg-notas
                 to asignatura     of L-reg-notas
               MOVE nota           of reg-notas
                 to nota           of L-reg-notas

           ELSE
               IF NOT FS-NOTAS-EOF
                   DISPLAY '*MOD*NOTAS*FS-NOTAS*' FS-NOTAS
               END-IF
           END-IF
.          .

       2200-CLOSE.
           CLOSE F-NOTAS
           .
       3000-FIN.
           GOBACK
           .
