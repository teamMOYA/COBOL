       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC1E.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION: USO DE TABLAS OCCURS PARA NOTAS ALUMNO Y       *
      *                 MEDIAS ASIGNATURAS TOTAL,                      *
      *                 LECTURA DE FICHERO INDEXADO CON ACCESO         *
      *                   SECUENCIAL.                                  *
      *                 ACCESO A TABLA OCURS CON SEARCH                *
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

           SELECT SALIDA-TABLA ASSIGN IMPRESO
               FILE STATUS IS FS-SALIDA-TABLA.

           SELECT COLEGIOS ASSIGN COLEGIO
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY  IS COD-COLEGIO OF REG-FCOLEGIO
               FILE STATUS IS FS-COLEGIOS.

       DATA DIVISION.
       FILE SECTION.
       FD ALUMNOS
           RECORDING MODE IS F
           DATA RECORD REGISTRO-ALUMNO.
           COPY RALUMN1B.

       FD SALIDA-TABLA
           RECORDING MODE IS F
           DATA RECORD LINEA-SALIDA.
       01 LINEA-SALIDA PIC X(132).

       FD COLEGIOS.
           COPY RCURCOLE.

       WORKING-STORAGE SECTION.
       01 FILE-STATUS.
           05 FS-ALUMNOS PIC XX.
               88 FS-ALUMNOS-OK     VALUE '00'.
               88 FS-ALUMNOS-EOF    VALUE '10'.
           05 FS-SALIDA-TABLA PIC XX.
               88 FS-SALIDA-OK      VALUE '00'.
               88 FS-SALIDA-EOF     VALUE '10'.
           05 FS-COLEGIOS PIC XX.
               88 FS-COLEGIOS-OK    VALUE '00'.
               88 FS-COLEGIOS-EOF   VALUE '10'.

       01 C-CONTADORES.
           05 C-ALUMNOS-TOTALES PIC 999.
           05 C-ASIGNATURAS  PIC 999.
           05 C-LINEAS       PIC 999.
           05 C-NUM-PAGINAS  PIC 999.
           05 C-MEDIA-ASIGNATURA PIC 999 OCCURS 6.
           05 C-CONTADOR PIC 99.
           05 C-COLEGIOS PIC 999 VALUE 0.

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

       01 T-TABLAS.
           05 T-COLEGIO OCCURS 1 TO 100 DEPENDING C-COLEGIOS
                        ASCENDING COD-COLEGIO
                        INDEXED BY IND-COLEGIO.
               10 COD-COLEGIO   PIC XX.
               10 NOMB-COLEGIO  PIC X(15).
               10 FILLER        PIC X(13).

      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO-COLEGIOS UNTIL NOT FS-COLEGIOS-OK
           PERFORM 3000-PROCESO-ALUMNOS
           PERFORM 4000-FIN
           .

       1000-INICIO.
           PERFORM 1100-ABRIR-FICHEROS
           PERFORM 1200-LEER-FECHA
           PERFORM 1300-ESCRIBIR-CABECERA
           .

       1100-ABRIR-FICHEROS.
           OPEN INPUT ALUMNOS
           IF NOT FS-ALUMNOS-OK THEN
               DISPLAY 'ERROR LEER FICHERO ALUMNOS'
               DISPLAY FS-ALUMNOS
               PERFORM 5000-END-PROGRAM
           END-IF

           OPEN OUTPUT SALIDA-TABLA
           IF NOT FS-SALIDA-OK THEN
               DISPLAY 'ERROR LEER FICHERO SALIDA'
               DISPLAY FS-SALIDA-TABLA
               PERFORM 5000-END-PROGRAM
           END-IF

           OPEN INPUT COLEGIOS
      *********IF COMENTADO POR TEMA DE PROBLEMAS CON FICHERO QSAM******
      *     IF NOT FS-COLEGIOS-OK THEN
      *         DISPLAY 'ERROR LEER FICHERO COLEGIOS'
      *         DISPLAY FS-COLEGIOS
      *         PERFORM 5000-END-PROGRAM
      *     END-IF
           .

       1200-LEER-FECHA.
           ACCEPT FECHA-SYSIN
           MOVE FECHA-SYSIN TO W-FECHA
           .

       1300-ESCRIBIR-CABECERA.
           WRITE LINEA-SALIDA FROM W-LINEA1 AFTER 1
           WRITE LINEA-SALIDA FROM W-LINEA2 AFTER 3
           WRITE LINEA-SALIDA FROM W-LINEA5 AFTER 1
           WRITE LINEA-SALIDA FROM LINEA-SEPARADOR AFTER 1

           MOVE 6 TO C-LINEAS OF C-CONTADORES
           .

       2000-PROCESO-COLEGIOS.
           READ COLEGIOS
           IF FS-COLEGIOS-OK THEN
               ADD 1 TO C-COLEGIOS
               MOVE COD-COLEGIO  OF REG-FCOLEGIO
                 TO COD-COLEGIO  OF T-COLEGIO (C-COLEGIOS)
               MOVE NOMB-COLEGIO OF REG-FCOLEGIO
                 TO NOMB-COLEGIO OF T-COLEGIO (C-COLEGIOS)
           ELSE
               DISPLAY '2000-PROCESO-COLEGIOS'
               DISPLAY FS-COLEGIOS
           END-IF
           .

       3000-PROCESO-ALUMNOS.
           READ ALUMNOS
           PERFORM UNTIL FS-ALUMNOS-EOF
               ADD 1 TO C-ALUMNOS-TOTALES
               PERFORM 3100-CALCULO-1
               PERFORM 3200-COLOCACION-DATOS

               WRITE LINEA-SALIDA FROM LINEA-DATOS AFTER 1
               READ ALUMNOS

           END-PERFORM

           .

       3100-CALCULO-1.
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

       3200-COLOCACION-DATOS.
           MOVE NIF OF REG-ALUMNOS TO NIF OF   LINEA-DATOS
           MOVE NOMBRE OF REG-ALUMNOS TO NOMBRE OF   LINEA-DATOS
           MOVE APELLIDOS OF REG-ALUMNOS TO APELLIDOS OF   LINEA-DATOS
           PERFORM 3210-BUSCAR-COLEGIO
           .

       3210-BUSCAR-COLEGIO.

           SEARCH ALL T-COLEGIO
             AT END DISPLAY 'ERROR colegio no encontrado'
                    MOVE COLEGIO OF REG-ALUMNOS
                      TO NOMB-COLEGIO OF  LINEA-DATOS

             WHEN COD-COLEGIO OF T-COLEGIO (IND-COLEGIO)
                 = COLEGIO OF REG-ALUMNOS
              MOVE NOMB-COLEGIO OF T-COLEGIO (IND-COLEGIO)
              TO NOMB-COLEGIO OF  LINEA-DATOS

           END-SEARCH
           .

       4000-FIN.
           PERFORM 4100-CALCULO-2
           WRITE LINEA-SALIDA FROM LINEA-SEPARADOR AFTER 1
           WRITE LINEA-SALIDA FROM LINEA-FINAL AFTER 1

           PERFORM 4200-CERRAR-FICHEROS
           PERFORM 5000-END-PROGRAM
           .

       4100-CALCULO-2.

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

       4200-CERRAR-FICHEROS.
           CLOSE ALUMNOS
           CLOSE SALIDA-TABLA
           CLOSE COLEGIOS
           .

       5000-END-PROGRAM.
           STOP RUN.
