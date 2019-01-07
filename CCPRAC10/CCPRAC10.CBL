       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC10.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION: USO DE LLAMADAS SQL A DB2.                     *
      *                 LECTURA DE DATOS DE FICHERO Y REALIZACION DE   *
      *                 INSERCION,MODIFICACION,BORRADO DE DATOS        *
      *                 DEPENDIENDO DE OPCION                          *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-ENTRADA ASSIGN ENTRADA
               FILE STATUS IS FS-ENTRADA.

       DATA DIVISION.
       FILE SECTION.
       FD F-ENTRADA
           RECORDING MODE IS F
           DATA RECORD F-ENTRADA.
       01 FD-ENTRADA.
           05 F-OPCION   PIC X.
           05 FILLER     PIC X.
           05 F-COD-ASIG PIC XXX.
           05 FILLER     PIC X.
           05 F-NOMBRE   PIC X(15).
           05 FILLER     PIC X(59).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS.
           05 FS-ENTRADA  PIC XX.
               88 FS-ENTRADA-OK     VALUE '00'.
               88 FS-ENTRADA-EOF    VALUE '10'.

       01 SQL-ERROR       PIC -9(9).

           EXEC SQL
             INCLUDE CCASIG
           END-EXEC.

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.


      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO-DATOS UNTIL FS-ENTRADA-EOF
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
           READ F-ENTRADA
           IF FS-ENTRADA-OK THEN
             MOVE F-COD-ASIG TO COD-ASIG
             MOVE F-NOMBRE   TO NOMBRE
             EVALUATE F-OPCION
               WHEN 'A' PERFORM 2100-ALTA
               WHEN 'B' PERFORM 2200-BAJA
               WHEN 'M' PERFORM 2300-MODIFICAR
             END-EVALUATE

             MOVE SQLCODE TO SQL-ERROR
             IF SQLCODE = 0 THEN
               EVALUATE F-OPCION
                 WHEN 'A' DISPLAY 'ALTA CORRECTA DE         ' F-COD-ASIG
                 WHEN 'B' DISPLAY 'BORRADO CORRECTO DE      ' F-COD-ASIG
                 WHEN 'M' DISPLAY 'MODIFICACION CORRECTA DE ' F-COD-ASIG
               END-EVALUATE
             ELSE
               EVALUATE F-OPCION
                 WHEN 'A' DISPLAY '*ALTA**ERR************** ' F-COD-ASIG
                 WHEN 'B' DISPLAY '*BORRA*ERR************** ' F-COD-ASIG
                 WHEN 'M' DISPLAY '*MODIF*ERR************** ' F-COD-ASIG
               END-EVALUATE
               DISPLAY 'SQL-ERROR ' SQL-ERROR
             END-IF
           END-IF
           .
      ******************************************************************
       2100-ALTA.
           EXEC SQL
             INSERT INTO ASIGNATURAS
              VALUES (:COD-ASIG,:NOMBRE)
           END-EXEC
           .
      ******************************************************************
       2200-BAJA.
           EXEC SQL
             DELETE FROM ASIGNATURAS
              WHERE COD_ASIG = :COD-ASIG
           END-EXEC
           .
      ******************************************************************
       2300-MODIFICAR.
           EXEC SQL
             UPDATE ASIGNATURAS
              SET NOMBRE = :NOMBRE
              WHERE COD_ASIG = :COD-ASIG
           END-EXEC
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
