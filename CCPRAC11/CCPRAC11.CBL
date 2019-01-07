       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC11.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION: USO DE LLAMADAS SQL A DB2.                     *
      *                 LECTURA DE DATOS DE FICHERO Y REALIZACION DE   *
      *                 INSERCION,MODIFICACION,BORRADO DE DATOS        *
      *                 DEPENDIENDO DE OPCION. EN CASO DE BORRADO EN   *
      *                 "CASCADE" BORRANDO NOTAS                       *
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
             INCLUDE CCNOTAS
           END-EXEC.

           EXEC SQL
             INCLUDE CCALUMN
           END-EXEC.
      *****  CURSOR  ******
           EXEC SQL
             DECLARE C-NOTAS CURSOR FOR
               SELECT N.NIF,N.NOTA
                FROM NOTAS N
                JOIN ALUMNOS A
                ON N.NIF = A.NIF
                WHERE COD_ASIG = :F-COD-ASIG
           END-EXEC.
      ***** END-CURSOR ******
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
                 WHEN 'M' DISPLAY 'MODIFICACION CORRECTA DE ' F-COD-ASIG
               END-EVALUATE
               EXEC SQL
                COMMIT
               END-EXEC

             ELSE
               EVALUATE F-OPCION
                 WHEN 'A' DISPLAY '*ALTA**ERR************** ' F-COD-ASIG
                 WHEN 'M' DISPLAY '*MODIF*ERR************** ' F-COD-ASIG
               END-EVALUATE
               DISPLAY 'SQL-ERROR ' SQL-ERROR
               EXEC
                 ROLLBACK
               END-EXEC
             END-IF
           END-IF
           .
      ******************************************************************
       2100-ALTA.
           EXEC SQL
             INSERT INTO ASIGNATURAS
              VALUES (:COD-ASIG,:DCLASIGNATURAS.NOMBRE)
           END-EXEC
           .
      ******************************************************************
       2200-BAJA.
           EXEC SQL
             DELETE FROM ASIGNATURAS
              WHERE COD_ASIG = :COD-ASIG
           END-EXEC
           IF SQLCODE = 0 THEN
             WHEN 'B' DISPLAY 'BORRADO CORRECTO DE      ' F-COD-ASIG
             EXEC SQL
               OPEN C-NOTAS
             END-EXEC
             PERFORM UNTIL NOT SQLCODE = 0
               EXEC SQL
                 FETCH C-NOTAS
                 INTO :DCLNOTAS.NIF,:NOTA
               END-EXEC
               IF SQLCODE = 0 THEN
                 DISPLAY 'NIF:   ' NIF
                 DISPLAY 'NOTAS: ' NOTA
                 DISPLAY '******************'
               END-IF
             END-PERFORM
             EXEC SQL
               CLOSE C-NOTAS
             END-EXEC
             EXEC SQL
               DELETE FROM NOTAS
                WHERE NIF IN (  SELECT N.NIF
                                  FROM NOTAS N
                                  JOIN ALUMNOS A
                                    ON N.NIF = A.NIF
                                 WHERE COD_ASIG = :F-COD-ASIG
                             )
             END-EXEC
           ELSE
             WHEN 'B' DISPLAY '*BORRA*ERR************** ' F-COD-ASIG
           END-IF
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
