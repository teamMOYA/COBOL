       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC07.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION: USO DE LLAMADAS SQL A DB2.                     *
      *                 LECTURA DE CODIGO DE COLEGIO DE FICHERO Y      *
      *                 USO DE CURSORES PARA EXTRAER DATOS DE ALUMNOS  *
      *                 Y MEDIA DE SUS NOTAS                           *
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
           05 F-COD-COL PIC 99.
           05 FILLER    PIC X(78).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS.
           05 FS-ENTRADA  PIC XX.
               88 FS-ENTRADA-OK     VALUE '00'.
               88 FS-ENTRADA-EOF    VALUE '10'.

       01 SQL-ERROR       PIC -9(9).

       01 C-CONTADORES.
           05 C-TOTAL-ALUMNOS    PIC 999.
           05 C-TOTAL-ALUMNOS-ED PIC ZZ9.

       01 W-MEDIAS.
           05 W-MEDIA PIC S99V99 COMP-3.
           05 W-MEDIA-ED  PIC -99.99.

           EXEC SQL
             INCLUDE CCALUMN
           END-EXEC.

           EXEC SQL
             INCLUDE CCCOLEG
           END-EXEC.

           EXEC SQL
             INCLUDE CCNOTAS
           END-EXEC.

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

      *****  CURSOR  ******
           EXEC SQL
             DECLARE ALUMNOS-BY-COLEGIO CURSOR FOR
               SELECT A.NIF,A.NOMBRE,AVG(N.NOTA)
                FROM ALUMNOS A
                JOIN NOTAS N
                 ON A.NIF=N.NIF
                WHERE COD_COLEGIO = :DCLALUMNOS.COD-COLEGIO
                GROUP BY A.NIF,NOMBRE
           END-EXEC.
      ***** END-CURSOR ******

      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO-F-COLEGIOS UNTIL FS-ENTRADA-EOF
           PERFORM 4000-FIN
           .
      ******************************************************************
       1000-INICIO.
           PERFORM 1100-ABRIR-FICHEROS
           PERFORM 1200-INICIALIZAR-DATOS
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
       1200-INICIALIZAR-DATOS.
           INITIALIZE DCLALUMNOS
           INITIALIZE DCLCOLEGIOS
           INITIALIZE C-TOTAL-ALUMNOS
           INITIALIZE C-TOTAL-ALUMNOS-ED
           INITIALIZE W-MEDIAS
           .
      ******************************************************************
       2000-PROCESO-F-COLEGIOS.
           READ F-ENTRADA
           IF FS-ENTRADA-OK THEN
             MOVE F-COD-COL TO COD-COLEGIO OF DCLCOLEGIOS
             MOVE F-COD-COL TO COD-COLEGIO OF DCLALUMNOS

             PERFORM 2100-PROCESO-DB-COLEGIOS

             PERFORM 2200-PROCESO-DB-ALUMNOS
           END-IF
           .
      ******************************************************************
       2100-PROCESO-DB-COLEGIOS.
           EXEC SQL
             SELECT NOMBRE
              INTO :DCLCOLEGIOS.NOMBRE
              FROM COLEGIOS
              WHERE COD_COLEGIO = :DCLCOLEGIOS.COD-COLEGIO
           END-EXEC
           MOVE SQLCODE TO SQL-ERROR
           IF SQLCODE = 0 THEN

             DISPLAY 'ALUMNOS DEL COLEGIO ' NOMBRE OF DCLCOLEGIOS
           ELSE
             IF SQLCODE = 100
               DISPLAY 'ALUMNOS DEL COLEGIO *ERR*NOMBRE*NO*ENCONTRADO*'
             END-IF
             DISPLAY 'SQL-ERROR COLEGIO' SQL-ERROR
           END-IF
           .
      ******************************************************************
       2200-PROCESO-DB-ALUMNOS.
           MOVE 0 TO C-TOTAL-ALUMNOS
            EXEC SQL
                OPEN ALUMNOS-BY-COLEGIO
            END-EXEC
           PERFORM UNTIL NOT SQLCODE = 0
             EXEC SQL
             FETCH ALUMNOS-BY-COLEGIO
             INTO :DCLALUMNOS.NIF,:DCLALUMNOS.NOMBRE,:W-MEDIA
             END-EXEC
             IF SQLCODE = 0 THEN
               MOVE W-MEDIA TO W-MEDIA-ED
               DISPLAY 'NIF: ' NIF OF DCLALUMNOS
               DISPLAY 'NOMBRE: ' NOMBRE OF DCLALUMNOS
               DISPLAY 'MEDIA: ' W-MEDIA-ED
               DISPLAY '******************************'
               ADD 1 TO C-TOTAL-ALUMNOS
             END-IF
           END-PERFORM
           MOVE SQLCODE TO SQL-ERROR
           MOVE C-TOTAL-ALUMNOS TO C-TOTAL-ALUMNOS-ED

           DISPLAY 'TOTAL ALUMNOS: ' C-TOTAL-ALUMNOS-ED
           DISPLAY 'SQL-ERROR: ' SQL-ERROR

           EXEC SQL
               CLOSE ALUMNOS-BY-COLEGIO
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
