      ******************************************************************
      * Author: Leandro Denis
      * Purpose: TP Algoritmos 4 Parte 1
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP-PARTE-1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT M            ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS M-ESTADO.

           SELECT LISTADO ASSIGN TO PRINTER.

       DATA DIVISION.
       FILE SECTION.
       FD  M      LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "MAESTRO.DAT".
       01  X.
           03  XXX-PROP1       PIC X(5).

       FD LISTADO  LABEL RECORD IS OMITTED.

       01  LINEA               PIC X(80).

       WORKING-STORAGE SECTION.
       77  M-EOF               PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  N1-EOF              PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  N2-EOF              PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  N3-EOF              PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  M-ESTADO            PIC XX.
       77  N1-ESTADO           PIC XX.
       77  N2-ESTADO           PIC XX.
       77  N3-ESTADO           PIC XX.
       77  TOTAL-GENERAL       PIC 9(1).

       PROCEDURE DIVISION.
       COMIENZO.
            PERFORM 010-ABRIR-ARCHIVOS.
            PERFORM 020-LEER-ARCHIVOS.
            MOVE 0 TO TOTAL-GENERAL.
            PERFORM 030-ESCRIBIR-CABECERA-LISTADO.
            PERFORM 040-CARGA-TABLA.
            PERFORM 050-PROCESAR UNTIL M-EOF = "SI" AND
            N1-EOF = "SI" AND N2-EOF = "SI" AND N3-EOF = "SI".
            PERFORM 060-ESCRIBIR-TOTAL-GENERAL.
            PERFORM 070-CERRAR-ARCHIVOS.
            STOP RUN.

      *-----------------------------------------------------------------
      *******
       010-ABRIR-ARCHIVOS.
      *******
           OPEN INPUT M.
           IF M-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN  FS: " M-ESTADO
               STOP RUN.
           OPEN OUTPUT LISTADO.
      *-----------------------------------------------------------------
      *******
       020-LEER-ARCHIVOS.
      *******

      *-----------------------------------------------------------------
      *******
       030-ESCRIBIR-CABECERA-LISTADO.
      *******

      *-----------------------------------------------------------------
      *******
       040-CARGA-TABLA.
      *******

      *-----------------------------------------------------------------
      *******
       050-PROCESAR.
      *******

      *-----------------------------------------------------------------
      *******
       060-ESCRIBIR-TOTAL-GENERAL.
      *******

      *-----------------------------------------------------------------
      *******
       070-CERRAR-ARCHIVOS.
      *******

      *-----------------------------------------------------------------
       END PROGRAM TP-PARTE-1.
