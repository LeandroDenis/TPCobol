      ******************************************************************
      * Author: Leandro Denis
      * Purpose: Clientes para TP 2 Algoritmos 4
      ******************************************************************
       PROGRAM-ID. SUBPGR.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES     ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                               ACCESS MODE IS RANDOM
                               RECORD KEY IS CLI-NUMERO
                               ALTERNATE KEY IS CLI-DOCUMENTO
                               FILE STATUS IS CLI-ESTADO.
       DATA DIVISION.
       FILE SECTION.
       FD  CLIENTES LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../CLIENTES.TXT".

       01  CLI.
           03 CLI-NUMERO       PIC X(8).
           03 CLI-FEC-ALTA     PIC 9(8).
           03 CLI-TELEFONO     PIC X(20).
           03 CLI-DIRECCION    PIC X(30).
           03 CLI-DOCUMENTO    PIC X(20).
       WORKING-STORAGE SECTION.
       77  CLI-EOF             PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  CLI-ESTADO          PIC XX.
       LINKAGE SECTION.
       01  OPER                PIC X.
       01  DOC                 PIC X(20).
       01  DIRECCION           PIC X(30).
       01  NUMERO              PIC X(8).
       01  RES                 PIC X.
       PROCEDURE DIVISION USING OPER, DOC, RES, NUMERO, DIRECCION.
           MOVE SPACES TO RES.
           IF OPER EQUALS 'A'
               PERFORM CLIENTE-ABRIR-ARCHIVO
               EXIT PROGRAM.
           IF OPER EQUALS 'P'
               PERFORM CLIENTE-BUSCAR
               EXIT PROGRAM.
           IF OPER EQUALS 'C'
               PERFORM CLIENTE-CERRAR-ARCHIVO
               EXIT PROGRAM.
           MOVE 'E' TO RES.
           EXIT PROGRAM.

       CLIENTE-ABRIR-ARCHIVO.
           OPEN INPUT CLIENTES.
           IF CLI-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN CLIENTES FS: " CLI-ESTADO
               STOP RUN.
       CLIENTE-CERRAR-ARCHIVO.
           CLOSE CLIENTES.

       CLIENTE-BUSCAR.
           MOVE DOC TO CLI-DOCUMENTO.
           READ CLIENTES RECORD KEY IS CLI-DOCUMENTO.
           IF CLI-ESTADO NOT = ZERO
               MOVE "E" TO RES
           ELSE
               MOVE CLI-NUMERO TO NUMERO
               MOVE CLI-DIRECCION TO DIRECCION.
       END PROGRAM SUBPGR.
