       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC12.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION: LECTURA DE DATOS DE FICHERO INDEXADO CON       *
      *                 CON ACCESO RANDOM                              *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-ENTRADA ASSIGN ENTRADA
               organization IS indexed
               ACCESS MODE  IS RANDOM
               RECORD KEY   IS cod-colegio OF reg-fcolegio
               FILE STATUS  IS FS-ENTRADA.

       DATA DIVISION.
       FILE SECTION.
       FD F-ENTRADA.
           COPY RCURCOLE.

       WORKING-STORAGE SECTION.
       01 FILE-STATUS.
           05 FS-ENTRADA  PIC XX.
               88 FS-ENTRADA-OK     VALUE '00'.
               88 FS-ENTRADA-EOF    VALUE '10'.
               88 FS-ENTRADA-NF     VALUE '23'.

      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO-DATOS
           PERFORM 4000-FIN
           .
      ******************************************************************
       1000-INICIO.
           PERFORM 1100-ABRIR-FICHEROS
           .
      ******************************************************************
       1100-ABRIR-FICHEROS.
           OPEN INPUT F-ENTRADA
           IF NOT FS-ENTRADA-OK THEN
               DISPLAY 'ERROR LEER FICHERO ENTRADA'
               DISPLAY FS-ENTRADA
               PERFORM 5000-END-PROGRAM
           END-IF

           .
      ******************************************************************
       2000-PROCESO-DATOS.
           MOVE 'A' TO cod-colegio
           START F-ENTRADA KEY = cod-colegio
           IF FS-ENTRADA-OK THEN
             display 'COD-COLEGIO1: ' cod-colegio
             DISPLAY 'nomb-colegio1:' nomb-colegio
           ELSE
             DISPLAY 'FS-ENTRADA:' FS-ENTRADA

           END-IF
           READ F-ENTRADA NEXT
           PERFORM UNTIL NOT FS-ENTRADA-OK

             display 'COD-COLEGIOA:  ' cod-colegio
             DISPLAY 'nomb-colegioA: ' nomb-colegio

             READ F-ENTRADA NEXT

           end-perform

      ******PARTE*2****
           MOVE 'Z' TO cod-colegio
           START F-ENTRADA KEY = cod-colegio
           IF FS-ENTRADA-OK THEN
             display 'COD-COLEGIO2: ' cod-colegio
             DISPLAY 'nomb-colegio2:' nomb-colegio
           ELSE
             DISPLAY 'FS-ENTRADA:' FS-ENTRADA

           END-IF
           READ F-ENTRADA NEXT
           PERFORM UNTIL NOT FS-ENTRADA-OK
             display 'COD-COLEGIOZ:  ' cod-colegio
             DISPLAY 'nomb-colegioZ: ' nomb-colegio

             READ F-ENTRADA NEXT

           end-perform
           .
      ******************************************************************
       4000-FIN.
           PERFORM 4200-CERRAR-FICHEROS
           PERFORM 5000-END-PROGRAM
           .
      ******************************************************************
       4200-CERRAR-FICHEROS.
           CLOSE F-ENTRADA
           .
      ******************************************************************
       5000-END-PROGRAM.
           STOP RUN.


