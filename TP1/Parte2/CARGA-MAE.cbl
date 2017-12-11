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
           OPEN I-O M.
           IF M-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN MAESTRO FS: " M-ESTADO
               STOP RUN.
      *-----------------------------------------------------------------
      *******
       050-PROCESAR.
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
