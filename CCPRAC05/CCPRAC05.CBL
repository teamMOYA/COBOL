       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC05.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION: USO DE LLAMADAS SQL A DB2.                     *
      *                 LECTURA DE CODIGO DE COLEGIO DE FICHERO Y      *
      *                 BUSQUEDA DE NOMBRE EN LA BASE DE DATOS         *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-COLEGIOS ASSIGN COLEGIOS
               FILE STATUS IS FS-COLEGIOS.

       DATA DIVISION.
       FILE SECTION.
       FD F-COLEGIOS
           RECORDING MODE IS F
           DATA RECORD F-REGISTRO-COLEGIO.
       01 F-REGISTRO-COLEGIO.
           05 F-COD-COLEGIO PIC 99.
           05 FILLER        PIC X(78).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS.
           05 FS-COLEGIOS PIC XX.
               88 FS-COLEGIOS-OK     VALUE '00'.
               88 FS-COLEGIOS-EOF    VALUE '10'.
       01 SQL-ERROR       PIC -9(9).
           EXEC SQL
             INCLUDE CCCOLEG
           END-EXEC.

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

      *******************************************************
       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO-F-COLEGIOS UNTIL FS-COLEGIOS-EOF
           PERFORM 4000-FIN
           .
      *******************************************************
       1000-INICIO.
           PERFORM 1100-ABRIR-FICHEROS
           PERFORM 1200-INICIALIZAR-DATOS
           .
      *******************************************************
       1100-ABRIR-FICHEROS.
           OPEN INPUT F-COLEGIOS
           IF NOT FS-COLEGIOS-OK THEN
               DISPLAY 'ERROR LEER FICHERO COLEGIOS'
               DISPLAY FS-COLEGIOS
               PERFORM 5000-END-PROGRAM
           END-IF

           .
      *******************************************************
       1200-INICIALIZAR-DATOS.
           INITIALIZE DCLCOLEGIOS
           .
      *******************************************************
       2000-PROCESO-F-COLEGIOS.
           READ F-COLEGIOS
           IF FS-COLEGIOS-OK THEN
             MOVE F-COD-COLEGIO TO COD-COLEGIO
             PERFORM 2100-PROCESO-DB-COLEGIOS
             IF SQLCODE = 0 THEN
               DISPLAY 'COD:    ' COD-COLEGIO
               DISPLAY 'NOMBRE: ' NOMBRE
             ELSE
               IF SQLCODE = 100
                 DISPLAY 'COD:    ' COD-COLEGIO
                 DISPLAY 'NOMBRE: NO SE ENCUENTRA '
               END-IF
               DISPLAY 'SQL-ERROR' SQL-ERROR
             END-IF
           END-IF
           .
      *******************************************************
       2100-PROCESO-DB-COLEGIOS.
           EXEC SQL
             SELECT NOMBRE INTO :NOMBRE
              FROM COLEGIOS
              WHERE COD_COLEGIO = :COD-COLEGIO
           END-EXEC
           MOVE SQLCODE TO SQL-ERROR

           .
      *******************************************************
       4000-FIN.
           PERFORM 4200-CERRAR-FICHEROS

           PERFORM 5000-END-PROGRAM
           .
      *******************************************************
       4200-CERRAR-FICHEROS.
           CLOSE F-COLEGIOS
           .
      *******************************************************
       5000-END-PROGRAM.
           STOP RUN.
