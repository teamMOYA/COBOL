       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC02.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION: UNION DE DATOS DE DISTINTOS FICHEROS,          *
      *                 LOS DOS FICHEROS TIENEN QUE ESTAR ORDENADOS.   *
      *                 MUESTRA CONTADORES DE REGISTROS Y LAS LINEAS   *
      *                 DE SALIDA                                      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA
            C12 IS CANAL-12.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUMNOS ASSIGN ALUMNOS
               FILE STATUS IS FS-ALUMNOS.

           SELECT SALIDA ASSIGN SALIDA
               FILE STATUS IS FS-SALIDA.

           SELECT NOTAS ASSIGN NOTAS
               FILE STATUS IS FS-NOTAS.

       DATA DIVISION.
       FILE SECTION.
       FD ALUMNOS
           RECORDING MODE IS F.
           COPY RALUMN02.

       FD SALIDA
           RECORDING MODE IS F.
           COPY RALUMN1B.

       FD NOTAS
           RECORDING MODE IS F.
           COPY RNOTAS02.

       WORKING-STORAGE SECTION.
       01 FILE-STATUS.
           05 FS-ALUMNOS PIC XX.
               88 FS-ALUMNOS-OK     VALUE '00'.
               88 FS-ALUMNOS-EOF    VALUE '10'.
           05 FS-SALIDA PIC XX.
               88 FS-SALIDA-OK      VALUE '00'.
               88 FS-SALIDA-EOF     VALUE '10'.
           05 FS-NOTAS PIC XX.
               88 FS-NOTAS-OK    VALUE '00'.
               88 FS-NOTAS-EOF   VALUE '10'.

       01 C-CONTADORES.
           05 C-ALUMNOS PIC 999.
           05 C-NOTAS-ALUMNO PIC 999.
           05 C-NOTAS PIC 999.
           05 C-AL-SIN-NOTA PIC 999.
           05 C-NOTA-SIN-AL PIC 999.
           05 C-SALIDA PIC 999.

       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO-DATOS
           PERFORM 3000-FIN
           .
      *****************************************************************
       1000-INICIO.
           PERFORM 1100-ABRIR-FICHEROS
           .

      *****************************************************************
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
               DISPLAY FS-SALIDA
               PERFORM 5000-END-PROGRAM
           END-IF

           OPEN INPUT NOTAS
           IF NOT FS-NOTAS-OK THEN
               DISPLAY 'ERROR LEER FICHERO NOTAS'
               DISPLAY FS-NOTAS
               PERFORM 5000-END-PROGRAM
           END-IF
           .

      *****************************************************************
       2000-PROCESO-DATOS.
           PERFORM 2100-LEER-ALUMNO
           PERFORM 2200-LEER-NOTA


           PERFORM  UNTIL FS-ALUMNOS-EOF AND FS-NOTAS-EOF
               IF NIF OF REG-ALUMNOS-GRAL = NIF OF REG-NOTAS THEN
                   ADD 1 TO C-NOTAS-ALUMNO
                   MOVE NOTA OF REG-NOTAS
                     TO NOTA OF ASIGNATURAS (ASIGNATURA OF REG-NOTAS)
                   PERFORM 2200-LEER-NOTA
               ELSE
                   IF NIF OF REG-ALUMNOS-GRAL > NIF OF REG-NOTAS THEN
                       ADD 1 TO C-NOTA-SIN-AL
                       PERFORM 2200-LEER-NOTA
                   ELSE
                       IF C-NOTAS-ALUMNO = 0 THEN
                           ADD 1    TO C-AL-SIN-NOTA
                       ELSE
                           WRITE REG-ALUMNOS
                           MOVE 0   TO C-NOTAS-ALUMNO
                           ADD  1   TO C-SALIDA

                       END-IF
                       PERFORM 2100-LEER-ALUMNO

                   END-IF
               END-IF
           END-PERFORM
           .
      *****************************************************************
       2100-LEER-ALUMNO.
           READ ALUMNOS
           DISPLAY reg-alumnos-gral
           IF FS-ALUMNOS-EOF THEN
               MOVE HIGH-VALUES TO NIF OF REG-ALUMNOS-GRAL
           ELSE
               IF NOT FS-ALUMNOS-OK THEN
                   DISPLAY 'ERROR READ ALUMNO'
                   DISPLAY FS-ALUMNOS
                   PERFORM 5000-END-PROGRAM
               END-IF
               ADD 1 TO C-ALUMNOS
               INITIALIZE REG-ALUMNOS
               PERFORM 2110-MOVER-DATOS
           END-IF
           .
      *****************************************************************
       2110-MOVER-DATOS.
           MOVE NIF         OF REG-ALUMNOS-GRAL
             TO NIF         OF REG-ALUMNOS

           MOVE NOMBRE      OF REG-ALUMNOS-GRAL
             TO NOMBRE      OF REG-ALUMNOS

           MOVE APELLIDOS   OF REG-ALUMNOS-GRAL
             TO APELLIDOS   OF REG-ALUMNOS

           MOVE COLEGIO     OF REG-ALUMNOS-GRAL
             TO COLEGIO     OF REG-ALUMNOS
           .

      *****************************************************************
       2200-LEER-NOTA.
           READ NOTAS
           DISPLAY reg-notas
           IF FS-NOTAS-EOF THEN
             MOVE HIGH-VALUES TO NIF OF REG-NOTAS
           ELSE
             IF NOT FS-NOTAS-OK THEN
                 DISPLAY 'ERROR READ NOTAS'

                 DISPLAY FS-NOTAS
                 PERFORM 5000-END-PROGRAM
             END-IF
             ADD 1 TO C-NOTAS
           END-IF

           .
      *****************************************************************

       3000-FIN.
           PERFORM 3100-DISPLAY-DATOS
           PERFORM 3200-CERRAR-FICHEROS
           PERFORM 5000-END-PROGRAM
           .
      *****************************************************************
       3100-DISPLAY-DATOS.
           DISPLAY 'ALUMNOS: ' C-ALUMNOS
           DISPLAY 'NOTAS: ' C-NOTAS
           DISPLAY 'C-AL-SIN-NOTA: ' C-AL-SIN-NOTA
           DISPLAY 'C-NOTA-SIN-AL: ' C-NOTA-SIN-AL

           DISPLAY 'C-SALIDA: ' C-SALIDA

           .
      *****************************************************************
       3200-CERRAR-FICHEROS.
           CLOSE ALUMNOS
           CLOSE SALIDA
           CLOSE NOTAS
           .
      *****************************************************************
       5000-END-PROGRAM.
           STOP RUN.
