//P390CP02 JOB CLASS=A,
//*            TYPRUN=SCAN,
//             NOTIFY=&SYSUID,
//             MSGCLASS=V,
//             TIME=(0,05)
//JOBLIB  DD  DSN=CURSO.COBOL.LOAD,DISP=SHR
//*******************************************************************
//BORRA02  EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
  DELETE P390C.PRAC02
  SET MAXCC=0
/*
//********************************************************************
//CCPRAC02 EXEC PGM=CCPRAC02
//ALUMNOS  DD  DSN=P390C.ALUMNOS,DISP=SHR
//NOTAS    DD  DSN=P390C.NOTAS,DISP=SHR
//SALIDA   DD  DSN=P390C.SALIDA,
//         DISP=(NEW,CATLG,DELETE),
//         UNIT=3390,VOL=SER=WRK001,
//         SPACE=(TRK,(1,1)),
//         DCB=(RECFM=FB,BLKSIZE=0,LRECL=68)
//SYSOUT   DD  SYSOUT=*
