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

           SELECT N1           ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS N1-ESTADO.

           SELECT N2           ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS N2-ESTADO.

           SELECT N3           ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS N3-ESTADO.

           SELECT MAE-ACT      ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS MAE-ACT-ESTADO.

           SELECT RECHAZOS     ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS RECHAZOS-ESTADO.

           SELECT AUTOS        ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS AUTOS-ESTADO.

           SELECT LISTADO      ASSIGN TO PRINTER.

       DATA DIVISION.
       FILE SECTION.
       FD  M       LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../MAESTRO.DAT".
       01  MAE.
           03  MAE-PATENTE         PIC X(6).
           03  MAE-FECHA           PIC 9(6).
           03  MAE-TIPO-DOC        PIC X.
           03  MAE-NRO-DOC         PIC X(20).
           03  MAE-IMPORTE         PIC 9(4)V99.

       FD  N1      LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../NOVEDADES1.DAT".
       01  NOV1.
           03  NOV1-PATENTE        PIC X(6).
           03  NOV1-FECHA          PIC 9(8).
           03  NOV1-TIPO-DOC       PIC X.
           03  NOV1-NRO-DOC        PIC X(20).

       FD  N2      LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../NOVEDADES2.DAT".
       01  NOV2.
           03  NOV2-PATENTE        PIC X(6).
           03  NOV2-FECHA          PIC 9(8).
           03  NOV2-TIPO-DOC       PIC X.
           03  NOV2-NRO-DOC        PIC X(20).

       FD  N3      LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../NOVEDADES3.DAT".
       01  NOV3.
           03  NOV3-PATENTE        PIC X(6).
           03  NOV3-FECHA          PIC 9(8).
           03  NOV3-TIPO-DOC       PIC X.
           03  NOV3-NRO-DOC        PIC X(20).

       FD  MAE-ACT LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../MAESTRO-ACT.DAT".
       01  ACT.
           03  ACT-PATENTE         PIC X(6).
           03  ACT-FECHA           PIC 9(6).
           03  ACT-TIPO-DOC        PIC X.
           03  ACT-NRO-DOC         PIC X(20).
           03  ACT-IMPORTE         PIC 9(4)V99.

       FD  RECHAZOS LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../RECHAZOS.DAT".
       01  RECH.
           03  RECH-PATENTE        PIC X(6).
           03  RECH-FECHA          PIC 9(8).
           03  RECH-TIPO-DOC       PIC X.
           03  RECH-NRO-DOC        PIC X(20).
           03  RECH-MOTIVO         PIC 9.
           03  RECH-AGENCIA        PIC 9.

       FD  AUTOS   LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../AUTOS.DAT".

       01  AUT.
           03  AUT-PATENTE     PIC X(6).
           03  AUT-DESC        PIC X(30).
           03  AUT-MARCA       PIC X(20).
           03  AUT-COLOR       PIC X(10).
           03  AUT-TAMAÑO      PIC X.
           03  AUT-IMPORTE     PIC 9(4)V99.

       FD  LISTADO  LABEL RECORD IS OMITTED.

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
       77  AUTOS-EOF           PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  M-ESTADO            PIC XX.
       77  N1-ESTADO           PIC XX.
       77  N2-ESTADO           PIC XX.
       77  N3-ESTADO           PIC XX.
       77  MAE-ACT-ESTADO      PIC XX.
       77  RECHAZOS-ESTADO     PIC XX.
       77  AUTOS-ESTADO        PIC XX.
       77  WS-TOTAL-GENERAL    PIC 9(1).
       01  WS-SUB              PIC 9(3).

       01  WS-TABLE.
           03  WS-AUTO OCCURS 300 TIMES.
               05  WS-AUTO-PATENTE     PIC X(6).
               05  WS-AUTO-DESC        PIC X(30).
               05  WS-AUTO-MARCA       PIC X(20).
               05  WS-AUTO-COLOR       PIC X(10).
               05  WS-AUTO-TAMAÑO      PIC X.
               05  WS-AUTO-IMPORTE     PIC 9(4)V99.

       PROCEDURE DIVISION.
       COMIENZO.
            PERFORM 010-ABRIR-ARCHIVOS.
            PERFORM 020-LEER-ARCHIVOS.
            MOVE 0 TO WS-TOTAL-GENERAL.
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
               DISPLAY "ERROR EN OPEN MAESTRO FS: " M-ESTADO
               STOP RUN.
           OPEN INPUT N1.
           IF N1-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN NOVEDADES1 FS: " N1-ESTADO
               STOP RUN.
           OPEN INPUT N2.
           IF N2-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN NOVEDADES2 FS: " N2-ESTADO
               STOP RUN.
           OPEN INPUT N3.
           IF N3-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN NOVEDADES3 FS: " N3-ESTADO
               STOP RUN.
           OPEN INPUT AUTOS.
           IF AUTOS-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN AUTOS FS: " AUTOS-ESTADO
               STOP RUN.
           OPEN OUTPUT MAE-ACT.
           IF N1-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN  FS: " MAE-ACT-ESTADO
               STOP RUN.
           OPEN OUTPUT RECHAZOS.
           IF RECHAZOS-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN  FS: " RECHAZOS-ESTADO
               STOP RUN.
           OPEN OUTPUT LISTADO.
      *-----------------------------------------------------------------
      *******
       020-LEER-ARCHIVOS.
      *******
           PERFORM 080-LEER-MAESTRO.
           PERFORM 080-LEER-NOV1.
           PERFORM 080-LEER-NOV2.
           PERFORM 080-LEER-NOV3.
      *-----------------------------------------------------------------
      *******
       030-ESCRIBIR-CABECERA-LISTADO.
      *******

      *-----------------------------------------------------------------
      *******
       040-CARGA-TABLA.
      *******
           PERFORM 080-LEER-AUTOS.
           MOVE 1 TO WS-SUB.
           PERFORM 090-CARGAR-AUTOS VARYING WS-SUB FROM 1 BY 1 UNTIL
               AUTOS-ESTADO = "10" OR WS-SUB > 300.

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
           CLOSE
               M
               N1
               N2
               N3
               MAE-ACT
               RECHAZOS
               LISTADO.
      *******
      *-----------------------------------------------------------------
      *******
       080-LEER-MAESTRO.
           READ M
               AT END MOVE "SI" TO M-EOF.
           IF M-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR EN READ MAESTRO  FS: " M-ESTADO
               STOP RUN.
      *******
      *-----------------------------------------------------------------
      *******
       080-LEER-NOV1.
           READ N1
               AT END MOVE "SI" TO N1-EOF.
           IF N1-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR EN READ NOV 1  FS: " N1-ESTADO
               STOP RUN.
      *******
      *-----------------------------------------------------------------
      *******
       080-LEER-NOV2.
           READ N2
               AT END MOVE "SI" TO N2-EOF.
           IF N2-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR EN READ NOV 2  FS: " N2-ESTADO
               STOP RUN.
      *******
      *-----------------------------------------------------------------
      *******
       080-LEER-NOV3.
           READ N3
               AT END MOVE "SI" TO N3-EOF.
           IF N3-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR EN READ MAESTRO  FS: " N3-ESTADO
               STOP RUN.
      *******
      *-----------------------------------------------------------------
      *******
       080-LEER-AUTOS.
           READ AUTOS
               AT END MOVE "SI" TO AUTOS-EOF.
           IF AUTOS-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR EN READ AUTOS  FS: " AUTOS-ESTADO
               STOP RUN.
      *******
      *-----------------------------------------------------------------
      *******
       090-CARGAR-AUTOS.
           MOVE AUT TO WS-AUTO(WS-SUB).
           ADD 1 TO WS-SUB.
           PERFORM 080-LEER-AUTOS.
      *******
      *-----------------------------------------------------------------
       END PROGRAM TP-PARTE-1.
