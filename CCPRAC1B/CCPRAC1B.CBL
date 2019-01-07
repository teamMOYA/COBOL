       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC1B.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION: PROGRAMA ANTERIOR UTILIZANDO TABLA OCCURS      *
      *                 PARA NOTAS ALUMNO Y MEDIAS ASIGNATURAS TOTAL   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           C12 IS CANAL-12.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-ALUMNOS ASSIGN ALUMNOS
               FILE STATUS IS FS-ALUMNOS.

           SELECT SALIDA-TABLA ASSIGN IMPRESO
               FILE STATUS IS FS-SALIDA-TABLA.

       DATA DIVISION.
       FILE SECTION.
       FD F-ALUMNOS
           RECORDING MODE IS F
           DATA RECORD REGISTRO-ALUMNO.
           COPY RALUMN1B.

       FD SALIDA-TABLA
           RECORDING MODE IS F
           DATA RECORD LINEA-SALIDA.
       01 LINEA-SALIDA PIC X(132).

       WORKING-STORAGE SECTION.
       77 FS-ALUMNOS PIC XX.
       77 FS-SALIDA-TABLA PIC XX.
       01 C-CONTADORES.
           05 C-ALUMNOS-TOTALES PIC 999.
           05 C-ASIGNATURAS  PIC 999.
           05 C-LINEAS       PIC 999.
           05 C-NUM-PAGINAS  PIC 999.
           05 C-MEDIA-ASIGNATURA PIC 999 OCCURS 6.
           05 C-CONTADOR PIC 99.

       01 W-MEDIAS.
           05 W-MEDIA-ALUMNO PIC 99V99.
           05 W-MEDIA-ASIGNATURA PIC 99V99 OCCURS 6.

       01 W-CABECERA.
             05 W-LINEA1.
               10 FILLER  PIC X(7)  VALUE 'FECHA: '.
               10 W-FECHA PIC X(10).
               10 FILLER  PIC X(23) VALUE SPACES.
               10 FILLER  PIC X(18) VALUE 'LISTADO DE ALUMNOS'.
               10 FILLER  PIC X(62) VALUE SPACES.
               10 FILLER  PIC X(6)  VALUE 'PAG.:'.
               10 W-NUMPAG-FIN PIC ZZ9.
             05 W-LINEA2.
               10 FILLER  PIC X(40) VALUE SPACES.
               10 FILLER  PIC X(18) VALUE ALL '='.
             05 W-LINEA5.
               10 FILLER  PIC X(9)  VALUE '   NIF   '.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(30) VALUE  'APELLIDOS'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(15) VALUE  'NOMBRE'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(15) VALUE 'COLEGIO'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE  'NOTA1'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE  'NOTA2'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE  'NOTA3'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE  'NOTA4'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE  'NOTA5'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE  'NOTA6'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE  'MEDIA'.
             05 LINEA-SEPARADOR.
               10 FILLER  PIC X(9)  VALUE ALL '-'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(30) VALUE ALL '-'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(15) VALUE ALL '-'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(15) VALUE ALL '-'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE ALL '-'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE ALL '-'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE ALL '-'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE ALL '-'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE ALL '-'.
               10 FILLER  PIC X     VALUE SPACES.
               10 FILLER  PIC X(5)  VALUE ALL '-'.

       01 LINEA-FINAL.
           05 FILLER   PIC X(10) VALUE SPACES.
           05 FILLER   PIC X(15) VALUE 'TOTAL ALUMNOS:'.
           05 W-ALUMNOS-TOTALES PIC 9(4).
           05 FILLER   PIC X(46) VALUE SPACES.
           05 ASIGNATURAS OCCURS 6.
             10 W-TOTAL PIC Z9,99.
             10 W-TOTAL-R REDEFINES W-TOTAL PIC XXB(3).

       01 LINEA-DATOS.
             05 NIF     PIC X(9).
             05 FILLER  PIC X     VALUE SPACES.
             05 APELLIDOS PIC X(30)B.
             05 NOMBRE  PIC X(15)B.
             05 NOMB-COLEGIO  PIC X(15)BB VALUE SPACES.
             05 ASIGNATURAS OCCURS 6.
               10 NOTA   PIC Z9B(4).
               10 NOTA-R REDEFINES NOTA PIC XXB(4).
             05 MEDIA   PIC Z9,99.
             05 MEDIA-R REDEFINES MEDIA PIC XXB(3).
       01 LINEA-SYSIN.
           05 FECHA-SYSIN PIC X(10).
           05 FILLER PIC X(72).

      ******************************************************************
       PROCEDURE DIVISION.
           ACCEPT FECHA-SYSIN
           MOVE FECHA-SYSIN TO W-FECHA

           OPEN INPUT F-ALUMNOS
           IF FS-ALUMNOS = '00' THEN

             OPEN OUTPUT SALIDA-TABLA
             IF FS-SALIDA-TABLA = '00' THEN

               WRITE LINEA-SALIDA FROM W-LINEA1 AFTER 1
               WRITE LINEA-SALIDA FROM W-LINEA2 AFTER 3
               WRITE LINEA-SALIDA FROM W-LINEA5 AFTER 1
               WRITE LINEA-SALIDA FROM LINEA-SEPARADOR AFTER 1

               MOVE 6 TO C-LINEAS OF C-CONTADORES

               READ F-ALUMNOS

               PERFORM UNTIL FS-ALUMNOS = '10'

                 ADD 1 TO C-ALUMNOS-TOTALES
                 PERFORM 100-CALCULO-1
                 PERFORM 200-COLOCACION-DATOS

                 WRITE LINEA-SALIDA FROM LINEA-DATOS AFTER 1
                 READ F-ALUMNOS

               END-PERFORM
               PERFORM 300-CALCULO-2
               WRITE LINEA-SALIDA FROM LINEA-SEPARADOR AFTER 1
               WRITE LINEA-SALIDA FROM LINEA-FINAL AFTER 1


             ELSE
               DISPLAY 'ERROR SALIDA'
               DISPLAY FS-SALIDA-TABLA
             END-IF
           ELSE
             DISPLAY 'ERROR LEER FICHERO ALUMNOS'
             DISPLAY FS-ALUMNOS

           END-IF

           CLOSE F-ALUMNOS
           CLOSE SALIDA-TABLA
           .

       100-CALCULO-1.
           MOVE 0 TO W-MEDIA-ALUMNO
           MOVE 0 TO C-ASIGNATURAS
           MOVE 0 TO C-CONTADOR
           PERFORM
             VARYING C-CONTADOR FROM 1 BY 1
             UNTIL C-CONTADOR > 6

             IF NOTA  OF REG-ALUMNOS (C-CONTADOR) > 00
               ADD NOTA OF REG-ALUMNOS (C-CONTADOR)
                 TO W-MEDIA-ASIGNATURA (C-CONTADOR)
               ADD NOTA OF REG-ALUMNOS (C-CONTADOR)
                 TO W-MEDIA-ALUMNO
               MOVE NOTA OF REG-ALUMNOS (C-CONTADOR)
                 TO NOTA OF LINEA-DATOS (C-CONTADOR)

               ADD 1 TO C-MEDIA-ASIGNATURA (C-CONTADOR)
               ADD 1 TO C-ASIGNATURAS
             ELSE
               MOVE 'NP' TO NOTA-R (C-CONTADOR)
             END-IF
           END-PERFORM

           MOVE W-MEDIA-ALUMNO TO MEDIA


           IF C-ASIGNATURAS > 0 THEN
             DIVIDE C-ASIGNATURAS INTO W-MEDIA-ALUMNO GIVING MEDIA
           ELSE
            MOVE 'NP' TO MEDIA-R
           END-IF
           .

       200-COLOCACION-DATOS.
           MOVE NIF OF REG-ALUMNOS TO NIF OF   LINEA-DATOS
           MOVE NOMBRE OF REG-ALUMNOS TO NOMBRE OF   LINEA-DATOS
           MOVE APELLIDOS OF REG-ALUMNOS TO APELLIDOS OF   LINEA-DATOS
           MOVE COLEGIO OF REG-ALUMNOS TO NOMB-COLEGIO OF   LINEA-DATOS
           .

       300-CALCULO-2.

           PERFORM
             VARYING C-CONTADOR FROM 1 BY 1
             UNTIL C-CONTADOR > 6

             IF C-MEDIA-ASIGNATURA (C-CONTADOR) > 0 THEN
               DIVIDE C-MEDIA-ASIGNATURA (C-CONTADOR)
                 INTO W-MEDIA-ASIGNATURA (C-CONTADOR)
                 GIVING W-TOTAL (C-CONTADOR)
             ELSE
              MOVE 'NP' TO W-TOTAL-R (C-CONTADOR)
             END-IF
           END-PERFORM
           MOVE C-ALUMNOS-TOTALES TO  W-ALUMNOS-TOTALES
           .

       500-END-PROGRAM.
           STOP RUN.
