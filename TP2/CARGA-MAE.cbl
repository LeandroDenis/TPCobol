      ******************************************************************
      * Author: Leandro Denis
      * Purpose: TP 2 Algoritmos 4
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT M            ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                               ACCESS MODE IS SEQUENTIAL
                               RECORD KEY IS ALQ-PATENTE
      *>                          ALTERNATE KEY IS ALQ-FECHA
                               FILE STATUS IS M-ESTADO.

       DATA DIVISION.
       FILE SECTION.
       FD  M       LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../MAESTRO.DAT".
       01  ALQ.
           03  ALQ-PATENTE         PIC X(6).
           03  ALQ-FECHA           PIC 9(8).
           03  ALQ-TIPO-DOC        PIC X.
           03  ALQ-NRO-DOC         PIC X(20).
           03  ALQ-IMPORTE         PIC 9(4)V99.
           03  ALQ-CHOFER          PIC X(7).
           03  ALQ-ESTADO          PIC X.

       WORKING-STORAGE SECTION.
       77  M-EOF               PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       01  M-ESTADO            PIC XX.
       01  ALQ2.
           03  ALQ2-PATENTE         PIC X(6).
           03  ALQ2-FECHA           PIC 9(8).
           03  ALQ2-TIPO-DOC        PIC X.
           03  ALQ2-NRO-DOC         PIC X(20).
           03  ALQ2-IMPORTE         PIC 9(4)V99.
           03  ALQ2-CHOFER          PIC X(7).
           03  ALQ2-ESTADO          PIC X.

       PROCEDURE DIVISION.
       COMIENZO.
            PERFORM 010-ABRIR-ARCHIVOS.
            PERFORM 050-PROCESAR.
            PERFORM 070-CERRAR-ARCHIVOS.
            STOP RUN.
      *-----------------------------------------------------------------
      *******
       010-ABRIR-ARCHIVOS.
      *******
           OPEN OUTPUT M.
           IF M-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN MAESTRO FS: " M-ESTADO
               STOP RUN.
      *-----------------------------------------------------------------
      *******
       050-PROCESAR.
           MOVE "000001" TO ALQ2-PATENTE.
           MOVE 20170318 TO ALQ2-FECHA.
           MOVE "D" TO ALQ2-TIPO-DOC.
           MOVE "35363296" TO ALQ2-NRO-DOC.
           MOVE 5029 TO ALQ2-IMPORTE.
           MOVE "1234567" TO ALQ2-CHOFER.
           MOVE "P" TO ALQ2-ESTADO.
           WRITE ALQ FROM ALQ2.

           MOVE "000002" TO ALQ2-PATENTE.
           MOVE "33333333" TO ALQ2-NRO-DOC.
           MOVE 20170811 TO ALQ2-FECHA.
           MOVE "1234567" TO ALQ2-CHOFER.
           WRITE ALQ FROM ALQ2.

           MOVE "000003" TO ALQ2-PATENTE.
           MOVE "11111111" TO ALQ2-NRO-DOC.
           MOVE 20171111 TO ALQ2-FECHA.
           MOVE "1234567" TO ALQ2-CHOFER.
           WRITE ALQ FROM ALQ2.

           MOVE "000004" TO ALQ2-PATENTE.
           MOVE "55555555" TO ALQ2-NRO-DOC.
           MOVE 20171201 TO ALQ2-FECHA.
           MOVE "1234567" TO ALQ2-CHOFER.
           WRITE ALQ FROM ALQ2.

           MOVE "000005" TO ALQ2-PATENTE.
           MOVE "99999999" TO ALQ2-NRO-DOC.
           MOVE 20171101 TO ALQ2-FECHA.
           MOVE "1234567" TO ALQ2-CHOFER.
           WRITE ALQ FROM ALQ2.

           MOVE "000006" TO ALQ2-PATENTE.
           MOVE "55555555" TO ALQ2-NRO-DOC.
           MOVE 20171201 TO ALQ2-FECHA.
           MOVE "1234567" TO ALQ2-CHOFER.
           WRITE ALQ FROM ALQ2.

           MOVE "000007" TO ALQ2-PATENTE.
           MOVE "99999999" TO ALQ2-NRO-DOC.
           MOVE 20171201 TO ALQ2-FECHA.
           MOVE "1634567" TO ALQ2-CHOFER.
           WRITE ALQ FROM ALQ2.

           MOVE "000008" TO ALQ2-PATENTE.
           MOVE "11111111" TO ALQ2-NRO-DOC.
           MOVE 20171201 TO ALQ2-FECHA.
           MOVE "1234567" TO ALQ2-CHOFER.
           WRITE ALQ FROM ALQ2.


      *******
      *-----------------------------------------------------------------
      *******
       070-CERRAR-ARCHIVOS.
      *******
           CLOSE
               M.
      *******
      *-----------------------------------------------------------------
      *******
       080-LEER-MAESTRO.
      ******
           READ M
               AT END MOVE "SI" TO M-EOF.
           IF M-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR EN READ MAESTRO  FS: " M-ESTADO
               STOP RUN.
