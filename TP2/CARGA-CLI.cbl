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
       01  CLI-ESTADO          PIC XXX.
       01  CLI2.
           03 CLI2-NUMERO       PIC X(8).
           03 CLI2-FEC-ALTA     PIC 9(8).
           03 CLI2-TELEFONO     PIC X(20).
           03 CLI2-DIRECCION    PIC X(30).
           03 CLI2-DOCUMENTO    PIC X(20).
       PROCEDURE DIVISION.
               PERFORM CLIENTE-ABRIR-ARCHIVO.
               MOVE "A100135" TO CLI2-NUMERO.
               MOVE 20170105 TO CLI2-FEC-ALTA.
               MOVE "TELEFONO 1" TO CLI2-TELEFONO.
               MOVE "DIRECCION 1" TO CLI2-DIRECCION.
               MOVE "35363296" TO CLI2-DOCUMENTO.
               WRITE CLI FROM CLI2.

               MOVE "X100132" TO CLI2-NUMERO.
               MOVE 20170105 TO CLI2-FEC-ALTA.
               MOVE "TELEFONO 2" TO CLI2-TELEFONO.
               MOVE "DIRECCION 2" TO CLI2-DIRECCION.
               MOVE "33333333" TO CLI2-DOCUMENTO.
               WRITE CLI FROM CLI2.

               MOVE "X983279" TO CLI2-NUMERO.
               MOVE 20170105 TO CLI2-FEC-ALTA.
               MOVE "TELEFONO 3" TO CLI2-TELEFONO.
               MOVE "DIRECCION 3" TO CLI2-DIRECCION.
               MOVE "11111111" TO CLI2-DOCUMENTO.
               WRITE CLI FROM CLI2.

               MOVE "X657432" TO CLI2-NUMERO.
               MOVE 20170105 TO CLI2-FEC-ALTA.
               MOVE "TELEFONO 4" TO CLI2-TELEFONO.
               MOVE "DIRECCION 4" TO CLI2-DIRECCION.
               MOVE "55555555" TO CLI2-DOCUMENTO.
               WRITE CLI FROM CLI2.

               MOVE "X879555" TO CLI2-NUMERO.
               MOVE 20170105 TO CLI2-FEC-ALTA.
               MOVE "TELEFONO 5" TO CLI2-TELEFONO.
               MOVE "DIRECCION 5" TO CLI2-DIRECCION.
               MOVE "99999999" TO CLI2-DOCUMENTO.
               WRITE CLI FROM CLI2.

               PERFORM CLIENTE-CERRAR-ARCHIVO.
           STOP RUN.

       CLIENTE-ABRIR-ARCHIVO.
           OPEN OUTPUT CLIENTES.
           IF CLI-ESTADO NOT = 00
               DISPLAY "ERROR EN OPEN CLIENTES FS: " CLI-ESTADO
               STOP RUN.
       CLIENTE-CERRAR-ARCHIVO.
           CLOSE CLIENTES.
