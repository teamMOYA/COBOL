       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC06.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION: USO DE LLAMADAS SQL A DB2.                     *
      *                 LECTURA DE CODIGO DE ALUMNO DE FICHERO Y       *
      *                 CALCULO DE MEDIA DE DATOS EN BASE DE DATOS     *
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
           05 F-NIF   PIC X(9).
           05 FILLER  PIC X(71).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS.
           05 FS-ENTRADA  PIC XX.
               88 FS-ENTRADA-OK     VALUE '00'.
               88 FS-ENTRADA-EOF    VALUE '10'.
       01 SQL-ERROR       PIC -9(9).
       01 W-MEDIA         PIC S99V99 COMP-3.
       01 W-MEDIA-ED      PIC -99.99.
           EXEC SQL
             INCLUDE CCALUMN
           END-EXEC.

           EXEC SQL
             INCLUDE CCNOTAS
           END-EXEC.

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

      *******************************************************
       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO-F-COLEGIOS UNTIL FS-ENTRADA-EOF
           PERFORM 4000-FIN
           .
      *******************************************************
       1000-INICIO.
           PERFORM 1100-ABRIR-FICHEROS
           PERFORM 1200-INICIALIZAR-DATOS
           .
      *******************************************************
       1100-ABRIR-FICHEROS.
           OPEN INPUT F-ENTRADA
           IF NOT FS-ENTRADA-OK THEN
               DISPLAY 'ERROR LEER FICHERO ENTRADA'
               DISPLAY FS-ENTRADA
               PERFORM 5000-END-PROGRAM
           END-IF

           .
      *******************************************************
       1200-INICIALIZAR-DATOS.
           INITIALIZE DCLALUMNOS
           INITIALIZE DCLNOTAS
           INITIALIZE W-MEDIA
           .
      *******************************************************
       2000-PROCESO-F-COLEGIOS.
           READ F-ENTRADA
           IF FS-ENTRADA-OK THEN
             MOVE F-NIF TO NIF OF DCLALUMNOS
             PERFORM 2100-PROCESO-DB-COLEGIOS
             IF SQLCODE = 0 THEN
               MOVE W-MEDIA TO W-MEDIA-ED

               DISPLAY 'NIF:    ' NIF OF DCLALUMNOS
               DISPLAY 'NOMBRE: ' NOMBRE OF DCLALUMNOS
               DISPLAY 'MEDIA: ' W-MEDIA-ED
             ELSE
               IF SQLCODE = 100
                 DISPLAY 'NIF:    ' NIF OF DCLALUMNOS
                 DISPLAY 'NOMBRE: NO SE ENCUENTRA '
               END-IF
               DISPLAY 'SQL-ERROR' SQL-ERROR
             END-IF
           END-IF
           .
      *******************************************************
       2100-PROCESO-DB-COLEGIOS.
           EXEC SQL
             SELECT A.NIF,A.NOMBRE,AVG(N.NOTA)
               INTO :DCLALUMNOS.NIF,:DCLALUMNOS.NOMBRE,:W-MEDIA
               FROM ALUMNOS A JOIN NOTAS N
                 ON A.NIF = N.NIF
              WHERE A.NIF = :DCLALUMNOS.NIF
              GROUP BY A.NIF,A.NOMBRE
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
           CLOSE F-ENTRADA
           .
      *******************************************************
       5000-END-PROGRAM.
           STOP RUN.


