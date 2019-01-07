       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODALUMN.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION:
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-ALUMNOS ASSIGN './ALUMNOS'
               FILE STATUS IS FS-ALUMNOS.
       DATA DIVISION.
       FILE SECTION.
       FD F-ALUMNOS
           RECORDING MODE IS F.

           COPY C-ALUMNOS.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  L-reg-alumnos-gral.
           05 nif                 pic x(9).
           05 nombre              pic x(15).
           05 apellidos           pic x(30).
           05 colegio             pic 99.

       01 FS-ALUMNOS PIC XX.
           88 FS-ALUMNOS-OK     VALUE '00'.
           88 FS-ALUMNOS-EOF    VALUE '10'.

       01 OPCION PIC X.

       PROCEDURE DIVISION USING L-reg-alumnos-gral OPCION FS-ALUMNOS.
      *     PERFORM 1000-INICIO
           PERFORM 2000-OPCION
           PERFORM 3000-FIN
           .

       1000-INICIO.
           MOVE nif        of L-reg-alumnos-gral
             to nif        of reg-alumnos-gral
           MOVE nombre     of L-reg-alumnos-gral
             to nombre     of reg-alumnos-gral
           MOVE apellidos  of L-reg-alumnos-gral
             to apellidos  of reg-alumnos-gral
           MOVE colegio    of L-reg-alumnos-gral
             to colegio    of reg-alumnos-gral
           .

       2000-OPCION.
           EVALUATE OPCION
               WHEN 'O' PERFORM 2100-OPEN
               WHEN 'F' PERFORM 2200-READ
               WHEN 'C' PERFORM 2200-CLOSE
               WHEN OTHER DISPLAY '*MOD*ALUMN*OPCION*INVALIDA*'
           END-EVALUATE
           .

       2100-OPEN.
           OPEN INPUT F-ALUMNOS
           .

       2200-READ.
           READ F-ALUMNOS
           IF FS-ALUMNOS-OK THEN

               MOVE nif        of reg-alumnos-gral
                 to nif        of L-reg-alumnos-gral
               MOVE nombre     of reg-alumnos-gral
                 to nombre     of L-reg-alumnos-gral
               MOVE apellidos  of reg-alumnos-gral
                 to apellidos  of L-reg-alumnos-gral

               MOVE colegio    of reg-alumnos-gral
                 to colegio    of L-reg-alumnos-gral
           ELSE
               IF NOT FS-ALUMNOS-EOF THEN
                   DISPLAY '*MOD*ALUMN*FS-ALUMNOS*' FS-ALUMNOS
               END-IF
           END-IF
.          .

       2200-CLOSE.
           CLOSE F-ALUMNOS
           .
       3000-FIN.
           GOBACK
           .
