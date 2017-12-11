      ******************************************************************
      * Author: Leandro Denis
      * Purpose: TP 1 Algoritmos 4 Parte 2
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP-PARTE-2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT M            ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS M-ESTADO.

           SELECT AUTOS        ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS AUTOS-ESTADO.

           SELECT LISTADO      ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  M       LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../../Parte1/MAESTRO-ACT.DAT".
       01  MAE.
           03  MAE-PATENTE         PIC X(6).
           03  MAE-FECHA           PIC 9(8).
           03  MAE-FECHA-MES REDEFINES MAE-FECHA.
               05 MAE-FECHA-AAAA           PIC 9(4).
               05 MAE-FECHA-MM             PIC 9(2).
               05 MAE-FECHA-DD             PIC 9(2).
           03  MAE-TIPO-DOC        PIC X.
           03  MAE-NRO-DOC         PIC X(20).
           03  MAE-IMPORTE         PIC 9(4)V99.

       FD  AUTOS   LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../../Parte1/AUTOS.DAT".

       01  AUT.
           03  AUT-PATENTE     PIC X(6).
           03  AUT-DESC        PIC X(30).
           03  AUT-MARCA       PIC X(20).
           03  AUT-COLOR       PIC X(10).
           03  AUT-TAMAÑO      PIC X.
           03  AUT-IMPORTE     PIC 9(4)V99.

       FD  LISTADO  LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../LISTADO.DAT".

       01  LINEA               PIC X(80).
       WORKING-STORAGE SECTION.
       77  M-EOF               PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  AUTOS-EOF           PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  M-ESTADO            PIC XX.
       77  AUTOS-ESTADO        PIC XX.
       77  WS-TOTAL-GENERAL    PIC 9(4).
       01  WS-SUB              PIC 9(3).
       01  WS-EXISTE-MARCA     PIC X(2).
       01  FECHA.
           03 FECHA-AA         PIC 9(4).
           03 FECHA-MM         PIC 9(2).
           03 FECHA-DD         PIC 9(2).
       01  PE1-ENCABE.
           03 FILLER           PIC X(7) VALUE 'Fecha: '.
           03 PE1-FECHA-DD     PIC 99.
           03 FILLER           PIC X   VALUE '/'.
           03 PE1-FECHA-MM     PIC 99.
           03 FILLER           PIC X   VALUE '/'.
           03 PE1-FECHA-AA     PIC 9999.
           03 FILLER           PIC X(54) VALUE ' '.
           03 FILLER           PIC X(6) VALUE 'Hoja: '.
           03 PE1-HOJA         PIC 999.
       01  PE2-ENCABE.
           03 FILLER           PIC X(20) VALUE ' '.
           03 FILLER           PIC X(60) VALUE 'Listado de Estadistico
      -    'de Alquileres por mes'.
       01  PE3-ENCABE          PIC X(60) VALUE ' '.
       01  PE4-ENCABE.
           03 FILLER           PIC X(20) VALUE 'Marca '.
           03 FILLER           PIC X(52) VALUE 'Ene Feb Mar Abr May Jun
      -    'Jul Ago Sep Oct Nov Dic     '.
           03 FILLER           PIC X(6) VALUE 'Total'.
       01  PE5-ENCABE.
           03 FILLER           PIC X(80) VALUE ALL '_'.
       01  PTR-ROW.
           03 ROW-MARCA        PIC X(20).
           03 ROW-ENE          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 ROW-FEB          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 ROW-MAR          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 ROW-ABR          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 ROW-MAY          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 ROW-JUN          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 ROW-JUL          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 ROW-AGO          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 ROW-SEP          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 ROW-OCT          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 ROW-NOV          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 ROW-DIC          PIC 999.
           03 FILLER           PIC X(5) VALUE ' '.
           03 ROW-TOT          PIC 9999.
           03 FILLER           PIC X(4) VALUE ' '.
           03 TOT-MARCA        PIC X(20).
           03 TOT-ENE          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 TOT-FEB          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 TOT-MAR          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 TOT-ABR          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 TOT-MAY          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 TOT-JUN          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 TOT-JUL          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 TOT-AGO          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 TOT-SEP          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 TOT-OCT          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 TOT-NOV          PIC 999.
           03 FILLER           PIC X(1) VALUE ' '.
           03 TOT-DIC          PIC 999.
           03 FILLER           PIC X(5) VALUE ' '.
           03 TOT-TOT          PIC 9999.
       01  WS-TABLE.
           03  WS-AUTO OCCURS 300 TIMES
               INDEXED BY IND.
               05 WS-AUTO-MARCA                PIC X(20).
               05 WS-AUTO-MES OCCURS 13 TIMES  PIC 9(3).
       01  WS-TABLE2.
           03  WS-AUX OCCURS 300 TIMES
               INDEXED BY IND-AUX.
               05  WS-AUX-PATENTE     PIC X(6).
               05  WS-AUX-DESC        PIC X(30).
               05  WS-AUX-MARCA       PIC X(20).
               05  WS-AUX-COLOR       PIC X(10).
               05  WS-AUX-TAMAÑO      PIC X.
               05  WS-AUX-IMPORTE     PIC 9(4)V99.
       01  WS-MARCA                   PIC X(20).
       01  WS-INDICE                  PIC 9(2).
       01  WS-I                       PIC 9(2).

       PROCEDURE DIVISION.
       COMIENZO.
            PERFORM 010-ABRIR-ARCHIVOS.
            PERFORM 020-LEER-ARCHIVOS.
            PERFORM 030-ESCRIBIR-CABECERA-LISTADO.
            MOVE 0 TO WS-TOTAL-GENERAL.
            PERFORM 040-CARGA-TABLA.
            PERFORM 050-PROCESAR UNTIL M-EOF = "SI".
            PERFORM 060-ESCRIBIR-LISTADO.
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
               OPEN INPUT AUTOS.
           IF AUTOS-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN AUTOS FS: " AUTOS-ESTADO
               STOP RUN.
           OPEN OUTPUT LISTADO.
      *-----------------------------------------------------------------
      *******
       020-LEER-ARCHIVOS.
      *******
           PERFORM 080-LEER-MAESTRO.
      *-----------------------------------------------------------------
      *******
       030-ESCRIBIR-CABECERA-LISTADO.
      *******
           MOVE FUNCTION CURRENT-DATE TO FECHA.
           MOVE FECHA-DD TO PE1-FECHA-DD.
           MOVE FECHA-MM TO PE1-FECHA-MM.
           MOVE FECHA-AA TO PE1-FECHA-AA.
           ADD 1 TO PE1-HOJA.
           WRITE LINEA FROM PE1-ENCABE.
           WRITE LINEA FROM PE2-ENCABE.
           WRITE LINEA FROM PE3-ENCABE.
           WRITE LINEA FROM PE4-ENCABE.
           WRITE LINEA FROM PE5-ENCABE.
      *-----------------------------------------------------------------
      *******
       040-CARGA-TABLA.
      *******
           PERFORM 080-LEER-AUTOS.
           MOVE 1 TO WS-SUB.
           MOVE 1 TO WS-I.
           PERFORM 090-CARGAR-AUTOS UNTIL
               AUTOS-ESTADO = "10" OR WS-I > 300.
           MOVE 'Totales' TO WS-AUTO-MARCA(WS-SUB).
      *-----------------------------------------------------------------
      *******
       050-PROCESAR.
      *******
           PERFORM 100-OBTENGO-MARCA.
           PERFORM 110-OBTENGO-INDICE.
           PERFORM 120-SUMO-FECHA.
           PERFORM 080-LEER-MAESTRO.
      *-----------------------------------------------------------------
      *******
       060-ESCRIBIR-LISTADO.
      *******
           MOVE 1 TO WS-I.
           PERFORM ESCRIBIR-LINEA UNTIL WS-I > WS-SUB.
      *-----------------------------------------------------------------
      *******
       ESCRIBIR-LINEA.
      *******
           PERFORM CARGAR-LINEA.
           WRITE LINEA FROM PTR-ROW.
           ADD 1 TO WS-I.
      *-----------------------------------------------------------------
      *******
      *******
       CARGAR-LINEA.
      *******
           MOVE WS-AUTO-MARCA(WS-I) TO ROW-MARCA.
           MOVE WS-AUTO-MES(WS-I, 1) TO ROW-ENE.
           MOVE WS-AUTO-MES(WS-I, 2) TO ROW-FEB.
           MOVE WS-AUTO-MES(WS-I, 3) TO ROW-MAR.
           MOVE WS-AUTO-MES(WS-I, 4) TO ROW-ABR.
           MOVE WS-AUTO-MES(WS-I, 5) TO ROW-MAY.
           MOVE WS-AUTO-MES(WS-I, 6) TO ROW-JUN.
           MOVE WS-AUTO-MES(WS-I, 7) TO ROW-JUL.
           MOVE WS-AUTO-MES(WS-I, 8) TO ROW-AGO.
           MOVE WS-AUTO-MES(WS-I, 9) TO ROW-SEP.
           MOVE WS-AUTO-MES(WS-I, 10) TO ROW-OCT.
           MOVE WS-AUTO-MES(WS-I, 11) TO ROW-NOV.
           MOVE WS-AUTO-MES(WS-I, 12) TO ROW-DIC.
           MOVE WS-AUTO-MES(WS-I, 13) TO ROW-TOT.
      *-----------------------------------------------------------------
      *******
       070-CERRAR-ARCHIVOS.
      *******
           CLOSE
               M
               AUTOS
               LISTADO.
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
      *******
      *-----------------------------------------------------------------
      *******
       080-LEER-AUTOS.
      ******
           READ AUTOS
               AT END MOVE "SI" TO AUTOS-EOF.
           IF AUTOS-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR EN READ AUTOS  FS: " AUTOS-ESTADO
               STOP RUN.
      *******
      *-----------------------------------------------------------------
      *******
       090-CARGAR-AUTOS.
      ******
           PERFORM 130-BUSCO-TABLA.
           PERFORM 080-LEER-AUTOS.
      *******
      *-----------------------------------------------------------------
      *******
       130-BUSCO-TABLA.
      *******
           MOVE AUT TO WS-AUX(WS-I).
           ADD 1 TO WS-I.
           MOVE 'NO' TO WS-EXISTE-MARCA.
           MOVE 1 TO IND.
           SEARCH WS-AUTO
               WHEN AUT-MARCA EQUALS WS-AUTO-MARCA(IND)
               MOVE 'SI' TO WS-EXISTE-MARCA.
           IF WS-EXISTE-MARCA EQUALS 'NO'
               MOVE AUT-MARCA TO WS-AUTO-MARCA(WS-SUB)
               ADD 1 TO WS-SUB.
      *******
      *-----------------------------------------------------------------
      *******
       100-OBTENGO-MARCA.
      *******
           MOVE 1 TO IND-AUX.
           SEARCH WS-AUX
               WHEN WS-AUX-PATENTE(IND-AUX)EQUALS MAE-PATENTE
               MOVE WS-AUX-MARCA(IND-AUX) TO WS-MARCA.
      *******
      *-----------------------------------------------------------------
      *******
       110-OBTENGO-INDICE.
      *******
           MOVE 1 TO IND.
           SEARCH WS-AUTO
               WHEN WS-AUTO-MARCA(IND)EQUALS WS-MARCA
               MOVE IND TO WS-INDICE.
      *******
      *-----------------------------------------------------------------
      *******
       120-SUMO-FECHA.
      *******
           ADD 1 TO WS-AUTO-MES(WS-INDICE, MAE-FECHA-MM).
           ADD 1 TO WS-AUTO-MES(WS-INDICE, 13).
           ADD 1 TO WS-AUTO-MES(WS-SUB, MAE-FECHA-MM).
           ADD 1 TO WS-AUTO-MES(WS-SUB, 13).
      *******
      *-----------------------------------------------------------------
       END PROGRAM TP-PARTE-2.
