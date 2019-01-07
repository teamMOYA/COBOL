       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCPRAC04.
      ******************************************************************
      *    AUTOR:       DAVID MOYA                                     *
      *    DESCRIPCION: PRIMERA INTERACCION CON MODULOS                *
      *                 MEDIA TOTAL DE CADA COLEGIO                    *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 C-CONTADORES.
           05 C-CONTADOR PIC 9.
           05 C-ASIG-CON-NOTA PIC 9.
           05 C-TOTAL-ALUMNOS PIC 9(5).
       01 AC-SUMA-NOTAS PIC 999V99.
       LINKAGE SECTION.
           COPY CPRACT04.
      ******************************************************************
       PROCEDURE DIVISION USING AREA-COMUNICACIONES-PRACTICA-4.
           PERFORM 1000-INICIO
           PERFORM 2000-CALCULOS
           PERFORM 3000-FIN
           .
      *****************************************************************
       1000-INICIO.
           INITIALIZE C-CONTADORES
           MOVE 1 TO  C-CONTADOR
           INITIALIZE DATOS-SALIDA-PRACTICA-4
           INITIALIZE AC-SUMA-NOTAS
           .
      *****************************************************************
       2000-CALCULOS.
           PERFORM UNTIL C-CONTADOR > 6
             IF NOT  AC-NOTAS-ASIG (C-CONTADOR) = ZERO
               ADD 1 TO C-ASIG-CON-NOTA

               COMPUTE NOTA-MEDIA-ASIG (C-CONTADOR) =
               AC-NOTAS-ASIG (C-CONTADOR) / CT-ALUMNOS-ASIG (C-CONTADOR)
               END-COMPUTE

               ADD CT-ALUMNOS-ASIG (C-CONTADOR) TO C-TOTAL-ALUMNOS
               ADD NOTA-MEDIA-ASIG (C-CONTADOR) TO AC-SUMA-NOTAS
             END-IF
             ADD 1 TO C-CONTADOR
           END-PERFORM
           IF C-ASIG-CON-NOTA = 0 THEN
             MOVE 1 TO COD-RETORNO-PRACTICA-4
           ELSE
             MOVE 0 TO COD-RETORNO-PRACTICA-4

             COMPUTE NOTA-MEDIA-TOTAL = AC-SUMA-NOTAS / C-TOTAL-ALUMNOS
           END-IF
           .
      *****************************************************************
       3000-FIN.
           GOBACK
           .
