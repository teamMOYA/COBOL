//P390CJCL JOB TBSIS,'XXXXXXXXXXX',
//*            TYPRUN=SCAN,
//             NOTIFY=&SYSUID,
//             MSGCLASS=V,
//             TIME=(0,05),
//             CLASS=A
//JOBLIB  DD  DSN=CURSO.COBOL.LOAD,DISP=SHR
//CCPRAC1B EXEC PGM=CCPRAC1B
//*=====================================================
//*===FICHEROS I-O
//*=====================================================
//*   FICHERO DE ENTRADA ALUMNOS
//ALUMNOS  DD  DSN=P390C.ALUMNOS,
//         DISP=(OLD,KEEP,KEEP),
//         UNIT=3390,VOL=SER=WRK001
//*   FICHERO DE SALIDA IMPRESO
//IMPRESO  DD  SYSOUT=L
//SYSOUT   DD  SYSOUT=*
//*   DATOS LECTURA ACCEPT
//SYSIN    DD *
18/12/2007
/*
//
