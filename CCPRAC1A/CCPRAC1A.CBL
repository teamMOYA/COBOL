       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC1A.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION: PROGRAMA QUE LEE DE FICHERO 'ALUMNOS',         *
      *                 HACE MEDIA DE NOTAS DEL ALUMNO Y MEDIA         *
      *                 TOTAL DE CADA ASIGNATURA.                      *
      *                 RESULTADO GUARDADO EN FICHERO SALIDA 'IMPRESO' *
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
           COPY RALUMNOS.

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
           05 C-MEDIA-TOTAL1 PIC 999.
           05 C-MEDIA-TOTAL2 PIC 999.
           05 C-MEDIA-TOTAL3 PIC 999.
           05 C-MEDIA-TOTAL4 PIC 999.
           05 C-MEDIA-TOTAL5 PIC 999.
           05 C-MEDIA-TOTAL6 PIC 999.

       01 W-MEDIAS.
           05 W-MEDIA-ALUMNO PIC 99V99.
           05 W-MEDIA-TOTAL1 PIC 99V99.
           05 W-MEDIA-TOTAL2 PIC 99V99.
           05 W-MEDIA-TOTAL3 PIC 99V99.
           05 W-MEDIA-TOTAL4 PIC 99V99.
           05 W-MEDIA-TOTAL5 PIC 99V99.
           05 W-MEDIA-TOTAL6 PIC 99V99.

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
           05 W-TOTAL1 PIC Z9,99.
           05 W-TOTAL1-R REDEFINES W-TOTAL1 PIC XXB(3).
           05 W-TOTAL2 PIC Z9,99.
           05 W-TOTAL2-R REDEFINES W-TOTAL2 PIC XXB(3).
           05 W-TOTAL3 PIC Z9,99.
           05 W-TOTAL3-R REDEFINES W-TOTAL3 PIC XXB(3).
           05 W-TOTAL4 PIC Z9,99.
           05 W-TOTAL4-R REDEFINES W-TOTAL4 PIC XXB(3).
           05 W-TOTAL5 PIC Z9,99.
           05 W-TOTAL5-R REDEFINES W-TOTAL5 PIC XXB(3).
           05 W-TOTAL6 PIC Z9,99.
           05 W-TOTAL6-R REDEFINES W-TOTAL6 PIC XXB(3).

       01 LINEA-DATOS.
             05 NIF     PIC X(9).
             05 FILLER  PIC X     VALUE SPACES.
             05 APELLIDOS PIC X(30)B.
             05 NOMBRE  PIC X(15)B.
             05 NOMB-COLEGIO  PIC X(15)BB VALUE SPACES.
             05 NOTA1   PIC Z9B(4).
             05 NOTA1-R REDEFINES NOTA1 PIC XXB(4).
             05 NOTA2   PIC Z9B(4).
             05 NOTA2-R REDEFINES NOTA2 PIC XXB(4).
             05 NOTA3   PIC Z9B(4).
             05 NOTA3-R REDEFINES NOTA3 PIC XXB(4).
             05 NOTA4   PIC Z9B(4).
             05 NOTA4-R REDEFINES NOTA4 PIC XXB(4).
             05 NOTA5   PIC Z9B(4).
             05 NOTA5-R REDEFINES NOTA5 PIC XXB(4).
             05 NOTA6   PIC Z9B(4).
             05 NOTA6-R REDEFINES NOTA6 PIC XXB(4).
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
               WRITE LINEA-SALIDA FROM W-LINEA2 AFTER 1
               WRITE LINEA-SALIDA FROM W-LINEA5 AFTER 3
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
             DISPLAY W-FECHA

           END-IF
           CLOSE F-ALUMNOS
           CLOSE SALIDA-TABLA
           .

       100-CALCULO-1.
           MOVE 0 TO W-MEDIA-ALUMNO
           MOVE 0 TO C-ASIGNATURAS
           IF NOTA1 OF REG-ALUMNOS > 00
             ADD NOTA1 OF REG-ALUMNOS TO W-MEDIA-TOTAL1
             ADD NOTA1 OF REG-ALUMNOS TO W-MEDIA-ALUMNO
            MOVE NOTA1 OF REG-ALUMNOS TO NOTA1 OF LINEA-DATOS

             ADD 1 TO C-MEDIA-TOTAL1
             ADD 1 TO C-ASIGNATURAS
           ELSE
             MOVE 'NP' TO NOTA1-R
           END-IF

           IF NOTA2 OF REG-ALUMNOS > 00
             ADD NOTA2 OF REG-ALUMNOS TO W-MEDIA-TOTAL2
             ADD NOTA2 OF REG-ALUMNOS TO W-MEDIA-ALUMNO
            MOVE NOTA2 OF REG-ALUMNOS TO NOTA2 OF LINEA-DATOS

             ADD 1 TO C-MEDIA-TOTAL2
             ADD 1 TO C-ASIGNATURAS
           ELSE
             MOVE 'NP' TO NOTA2-R
           END-IF

           IF NOTA3 OF REG-ALUMNOS > 00
             ADD NOTA3 OF REG-ALUMNOS TO W-MEDIA-TOTAL3
             ADD NOTA3 OF REG-ALUMNOS TO W-MEDIA-ALUMNO
            MOVE NOTA3 OF REG-ALUMNOS TO NOTA3 OF LINEA-DATOS

             ADD 1 TO C-MEDIA-TOTAL3
             ADD 1 TO C-ASIGNATURAS
           ELSE
            MOVE 'NP' TO NOTA3-R
           END-IF

           IF NOTA4 OF REG-ALUMNOS > 00
             ADD NOTA4 OF REG-ALUMNOS TO W-MEDIA-TOTAL4
             ADD NOTA4 OF REG-ALUMNOS TO W-MEDIA-ALUMNO
            MOVE NOTA4 OF REG-ALUMNOS TO NOTA4 OF LINEA-DATOS

             ADD 1 TO C-MEDIA-TOTAL4
             ADD 1 TO C-ASIGNATURAS
           ELSE
            MOVE 'NP' TO NOTA4-R
           END-IF

           IF NOTA5 OF REG-ALUMNOS > 00
             ADD NOTA5 OF REG-ALUMNOS TO W-MEDIA-TOTAL5
             ADD NOTA5 OF REG-ALUMNOS TO W-MEDIA-ALUMNO
            MOVE NOTA5 OF REG-ALUMNOS TO NOTA5 OF LINEA-DATOS

             ADD 1 TO C-MEDIA-TOTAL5
             ADD 1 TO C-ASIGNATURAS
           ELSE
            MOVE 'NP' TO NOTA5-R
           END-IF

           IF NOTA6 OF REG-ALUMNOS > 00
             ADD NOTA6 OF REG-ALUMNOS TO W-MEDIA-TOTAL6
             ADD NOTA6 OF REG-ALUMNOS TO W-MEDIA-ALUMNO
            MOVE NOTA6 OF REG-ALUMNOS TO NOTA6 OF LINEA-DATOS

             ADD 1 TO C-MEDIA-TOTAL6
             ADD 1 TO C-ASIGNATURAS
           ELSE
            MOVE 'NP' TO NOTA6-R
           END-IF

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

           IF C-MEDIA-TOTAL1 > 0 THEN
             DIVIDE C-MEDIA-TOTAL1 INTO W-MEDIA-TOTAL1 GIVING W-TOTAL1
           ELSE
            MOVE 'NP' TO W-TOTAL1-R
           END-IF
           IF C-MEDIA-TOTAL2 > 0 THEN
             DIVIDE C-MEDIA-TOTAL2 INTO W-MEDIA-TOTAL2 GIVING W-TOTAL2
           ELSE
            MOVE 'NP' TO W-TOTAL2-R
           END-IF
           IF C-MEDIA-TOTAL3 > 0 THEN
             DIVIDE C-MEDIA-TOTAL3 INTO W-MEDIA-TOTAL3 GIVING W-TOTAL3
           ELSE
            MOVE 'NP' TO W-TOTAL3-R
           END-IF
           IF C-MEDIA-TOTAL4 > 0 THEN
             DIVIDE C-MEDIA-TOTAL4 INTO W-MEDIA-TOTAL4 GIVING W-TOTAL4
           ELSE
            MOVE 'NP' TO W-TOTAL4-R
           END-IF
           IF C-MEDIA-TOTAL5 > 0 THEN
             DIVIDE C-MEDIA-TOTAL5 INTO W-MEDIA-TOTAL5 GIVING W-TOTAL5
           ELSE
            MOVE 'NP' TO W-TOTAL5-R
           END-IF
           IF C-MEDIA-TOTAL6 > 0 THEN
             DIVIDE C-MEDIA-TOTAL6 INTO W-MEDIA-TOTAL6 GIVING W-TOTAL6
           ELSE
            MOVE 'NP' TO W-TOTAL6-R
           END-IF
           MOVE C-ALUMNOS-TOTALES TO  W-ALUMNOS-TOTALES
           .

       500-END-PROGRAM.
           STOP RUN.
