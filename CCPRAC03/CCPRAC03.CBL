       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC03.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION: USO DE LLAMADAS A MODULOS CON "CALL"           *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUMNOS ASSIGN ALUMNOS
               FILE STATUS IS FS-ALUMNOS.

           SELECT SALIDA       ASSIGN SALIDA
               FILE STATUS IS FS-SALIDA-TABLA.

       DATA DIVISION.
       FILE SECTION.
       FD ALUMNOS
           RECORDING MODE IS F
           DATA RECORD REGISTRO-ALUMNO.
           COPY RALUMN1B.

       FD SALIDA
           RECORDING MODE IS F
           BLOCK CONTAINS 0.
           COPY RCOLEGIO.

       WORKING-STORAGE SECTION.
       01 FILE-STATUS.
           05 FS-ALUMNOS PIC XX.
               88 FS-ALUMNOS-OK     VALUE '00'.
               88 FS-ALUMNOS-EOF    VALUE '10'.
           05 FS-SALIDA-TABLA PIC XX.
               88 FS-SALIDA-OK      VALUE '00'.
               88 FS-SALIDA-EOF     VALUE '10'.

       01 C-CONTADORES.
           05 C-ASIG PIC 9.
           05 C-COLEGIO PIC 999.

       01 T-TABLAS.
           05 T-COLEGIO  OCCURS 99.
               10 T-ASIG OCCURS 6.
                   15 CT-ALUMNOS PIC 9(5).
                   15 AC-NOTAS   PIC 9(6).
           COPY CPRACT04.
      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO-ALUMNOS
           PERFORM 3000-PROCESO-COLEGIOS
           PERFORM 4000-FIN
           .
      ******************************************************************
       1000-INICIO.
           PERFORM 1100-ABRIR-FICHEROS
           PERFORM 1200-INICIALIZAR-DATOS
           .
      ******************************************************************
       1100-ABRIR-FICHEROS.
           OPEN INPUT ALUMNOS
           IF NOT FS-ALUMNOS-OK THEN
               DISPLAY 'ERROR LEER FICHERO ALUMNOS'
               DISPLAY FS-ALUMNOS
               PERFORM 5000-END-PROGRAM
           END-IF

           OPEN OUTPUT SALIDA
           IF NOT FS-SALIDA-OK THEN
               DISPLAY 'ERROR LEER FICHERO SALIDA'
               DISPLAY FS-SALIDA-TABLA
               PERFORM 5000-END-PROGRAM
           END-IF
           .
      ******************************************************************
       1200-INICIALIZAR-DATOS.
           INITIALIZE T-TABLAS
           INITIALIZE C-CONTADORES
           INITIALIZE REG-COLEGIOS
           .
      ******************************************************************
       2000-PROCESO-ALUMNOS.
           READ ALUMNOS

           PERFORM UNTIL FS-ALUMNOS-EOF


              MOVE 1 TO C-ASIG
               PERFORM UNTIL C-ASIG > 6
                 IF NOT NOTA OF ASIGNATURAS (C-ASIG) = ZEROS THEN
                   ADD NOTA OF ASIGNATURAS (C-ASIG)
                     TO AC-NOTAS (COLEGIO, C-ASIG)

                   ADD 1 TO CT-ALUMNOS (COLEGIO, C-ASIG)

                 END-IF

                 ADD 1 TO C-ASIG
               END-PERFORM

               READ ALUMNOS

           END-PERFORM
           .
      ******************************************************************
       3000-PROCESO-COLEGIOS.
           MOVE 1 TO C-COLEGIO

           PERFORM UNTIL C-COLEGIO > 99

              INITIALIZE AREA-COMUNICACIONES-PRACTICA-4
              MOVE 1 TO C-ASIG
              PERFORM UNTIL C-ASIG > 6
                MOVE T-ASIG(C-COLEGIO, C-ASIG)
                  TO DATOS-ASIGNATURAS
                   OF AREA-COMUNICACIONES-PRACTICA-4 (C-ASIG)
                ADD 1 TO C-ASIG
              END-PERFORM

              CALL "CCPRAC04" USING AREA-COMUNICACIONES-PRACTICA-4

              IF TODO-BIEN THEN
                PERFORM 3100-MOVER-DATOS
                WRITE REG-COLEGIOS
              ELSE
                DISPLAY '*ERROR*' COD-RETORNO-PRACTICA-4
              END-IF

              ADD 1 TO C-COLEGIO
           END-PERFORM
           .
      ******************************************************************
       3100-MOVER-DATOS.

           MOVE C-COLEGIO TO COD-COLEGIO
           MOVE NOTA-MEDIA-TOTAL OF DATOS-SALIDA-PRACTICA-4
             TO NOTA-MEDIA-TOTAL OF REG-COLEGIOS

           MOVE 1 TO C-ASIG
           PERFORM UNTIL C-ASIG > 6

              MOVE CT-ALUMNOS-ASIG
                  OF DATOS-ENTRADA-PRACTICA-4 (C-ASIG)
                TO CT-ALUMNOS-ASIG OF REG-COLEGIOS (C-ASIG)
              MOVE AC-NOTAS-ASIG
                  OF DATOS-ENTRADA-PRACTICA-4 (C-ASIG)
                TO AC-NOTAS-ASIG   OF REG-COLEGIOS (C-ASIG)
              MOVE NOTA-MEDIA-ASIG OF MEDIA-ASIGNATURAS (C-ASIG)
                TO NOTA-MEDIA-ASIG OF REG-COLEGIOS (C-ASIG)
              ADD 1 TO C-ASIG
           END-PERFORM
           .
      ******************************************************************
       4000-FIN.
           PERFORM 4200-CERRAR-FICHEROS

           PERFORM 5000-END-PROGRAM
           .
      ******************************************************************
       4200-CERRAR-FICHEROS.
           CLOSE ALUMNOS
           CLOSE SALIDA
           .
      ******************************************************************
       5000-END-PROGRAM.
           STOP RUN.
