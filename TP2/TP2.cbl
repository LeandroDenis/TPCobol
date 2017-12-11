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

           SELECT CHOFERES     ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                               ACCESS MODE IS DYNAMIC
                               RECORD KEY IS CHO-NRO-LEGAJO
      *>                          ALTERNATE KEY IS CHO-FECHA-DESDE
                               FILE STATUS IS CHO-ESTADO.

           SELECT RECHAZOS     ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                               ACCESS MODE IS RANDOM
                               RECORD KEY IS RECH-PATENTE
                               ALTERNATE KEY IS RECH-FECHA
                               FILE STATUS IS RECH-ESTADO.

           SELECT ARCH-AUX     ASSIGN TO DISK
                               SORT STATUS IS FS-AUX.

           SELECT LISTADO      ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL.
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

       FD  LISTADO  LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../LISTADO.TXT".

       01  LINEA               PIC X(80).

       FD  CHOFERES LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../CHOFERES.TXT".

       01  CHO.
           03 CHO-NRO-LEGAJO       PIC X(7).
           03 CHO-FECHA-DESDE      PIC 9(8).
           03 CHO-FECHA-HASTA      PIC 9(8).
           03 CHO-TURNO            PIC X.

       FD  RECHAZOS LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "./RECHAZOS.TXT".

       01  RECH.
           03 RECH-PATENTE         PIC X(6).
           03 RECH-FECHA           PIC 9(8).
           03 RECH-TIPO-DOC        PIC X.
           03 RECH-NRO-DOC         PIC X(20).
           03 RECH-IMPORTE         PIC 9(4)V99.

       SD  ARCH-AUX    DATA RECORD IS REG-AUX.
       01  REG-AUX.
           03  AUX-PATENTE         PIC X(6).
           03  AUX-FECHA           PIC 9(8).
           03  AUX-TIPO-DOC        PIC X.
           03  AUX-NRO-DOC         PIC X(20).
           03  AUX-IMPORTE         PIC 9(4)V99.
           03  AUX-CHOFER          PIC X(7).
           03  AUX-ESTADO          PIC X.

       WORKING-STORAGE SECTION.
       77  M-EOF               PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  CHO-EOF             PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  RECH-EOF             PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  RECH-ESTADO          PIC XX.
       77  FS-AUX              PIC XX.
       77  M-ESTADO            PIC XX.
       77  CHO-ESTADO          PIC XX.
       77  AUX-EOF             PIC XX.
       77  WS-TOTAL-GENERAL    PIC 9(4).
       01  WS-SUB              PIC 9(3).
       01  WS-RECHAZADO        PIC XX.
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
           03 FILLER           PIC X(30) VALUE ' '.
           03 FILLER           PIC X(50) VALUE 'Listado alquileres aprob
      -    'ados'.
       01  PE3-ENCABE          PIC X(80) VALUE ' '.
       01  PE4-ENCABE.
           03 FILLER           PIC X(7) VALUE 'Fecha: '.
           03 PE4-FECHA        PIC 9(8).
       01  PE5-ENCABE.
           03 FILLER           PIC X(80) VALUE ALL '_'.
       01  PE6-ENCABE.
           03 FILLER           PIC X(8) VALUE 'Chofer: '.
           03 PE6-CHOFER       PIC X(7).
           03 FILLER           PIC X VALUE ' '.
           03 FILLER           PIC X(7) VALUE 'Turno: '.
           03 PE6-TURNO        PIC X.
       01  PE7-ENCABE.
           03 FILLER           PIC X(15) VALUE ' '.
           03 FILLER           PIC X(7) VALUE 'Cliente'.
           03 FILLER           PIC X(6) VALUE ' '.
           03 FILLER           PIC X(8) VALUE 'Tipo Doc'.
           03 FILLER           PIC X(6) VALUE ' '.
           03 FILLER           PIC X(13) VALUE 'Nro Documento'.
           03 FILLER           PIC X(10) VALUE ' '.
           03 FILLER           PIC X(10) VALUE 'Direccion'.

       01  WS-TOTAL            PIC 999.
       01  WS-TOTAL-FECHA      PIC 999.
       01  WS-TOTAL-CHOFER     PIC 999.
       01  WS-FECHA            PIC 9(8).
       01  WS-CHOFER           PIC X(7).
       01  WS-LINEA            PIC 999.

       01  PTR-ROW.
           03 FILLER           PIC X(15) VALUE ' '.
           03 ROW-CLIENTE      PIC X(8).
           03 FILLER           PIC X(5) VALUE ' '.
           03 ROW-TIPO-DOC     PIC X.
           03 FILLER           PIC X(15) VALUE ' '.
           03 ROW-DOC          PIC X(20).
           03 FILLER           PIC X(1) VALUE ' '.
           03 ROW-DIRECCION    PIC X(30) VALUE ' '.

       01  PTR-TOTAL-CHOFER.
           03 FILLER           PIC X(20) VALUE 'Total por chofer: '.
           03 PTR-CHOFER       PIC 999.
       01  PTR-TOTAL-FECHA.
           03 FILLER           PIC X(20) VALUE 'Total por fecha: '.
           03 PTR-FECHA       PIC 999.
       01  PTR-TOTAL.
           03 FILLER           PIC X(20) VALUE 'Totales generales: '.
           03 PTR-TOTALGRAL    PIC 999.

       01  LK-TIPO-OP          PIC X.
       01  LK-NRO-DOC          PIC X(20).
       01  LK-DIRECCION        PIC X(20).
       01  LK-RES              PIC X.
       01  LK-CLIENTE          PIC X(8).

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
           MOVE 'A' TO LK-TIPO-OP.
           CALL 'SUBPGR' USING LK-TIPO-OP, LK-NRO-DOC, LK-RES, LK-CLI
      -    ENTE.
           OPEN I-O M.
           IF M-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN MAESTRO FS: " M-ESTADO
               STOP RUN.
           OPEN INPUT CHOFERES.
           IF CHO-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN CHOFERES FS: " CHO-ESTADO
               STOP RUN.
           OPEN OUTPUT RECHAZOS.
           IF RECH-ESTADO NOT = ZERO
               DISPLAY "ERROR EN OPEN RECHAZOS FS: " RECH-ESTADO
               STOP RUN.
           OPEN OUTPUT LISTADO.
      *-----------------------------------------------------------------
      *******
       030-ESCRIBIR-CABECERA-LISTADO.
      *******
           IF WS-LINEA >= 60
           MOVE FUNCTION CURRENT-DATE TO FECHA
           MOVE FECHA-DD TO PE1-FECHA-DD
           MOVE FECHA-MM TO PE1-FECHA-MM
           MOVE FECHA-AA TO PE1-FECHA-AA
           ADD 1 TO PE1-HOJA
           WRITE LINEA FROM PE1-ENCABE
           WRITE LINEA FROM PE2-ENCABE
           WRITE LINEA FROM PE3-ENCABE
           MOVE AUX-FECHA TO PE4-FECHA
           WRITE LINEA FROM PE4-ENCABE
           MOVE AUX-CHOFER TO PE6-CHOFER
           MOVE CHO-TURNO TO PE6-TURNO
           WRITE LINEA FROM PE6-ENCABE
           WRITE LINEA FROM PE3-ENCABE
           WRITE LINEA FROM PE7-ENCABE
           WRITE LINEA FROM PE5-ENCABE
           MOVE 9 TO WS-LINEA.
      *-----------------------------------------------------------------
      *******
       050-PROCESAR.
      *******
           SORT ARCH-AUX
               ON ASCENDING KEY AUX-FECHA
               ON ASCENDING KEY AUX-CHOFER
               INPUT PROCEDURE IS ENTRADA
               OUTPUT PROCEDURE IS SALIDA.
      *-----------------------------------------------------------------
      *******
       070-CERRAR-ARCHIVOS.
      *******
           CLOSE
               M
               CHOFERES
               RECHAZOS
               LISTADO.
           MOVE 'C' TO LK-TIPO-OP.
           CALL 'SUBPGR' USING LK-TIPO-OP, LK-NRO-DOC, LK-RES, LK-CLIEN
      -    TE.
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
      ******
       ENTRADA SECTION.
       MOVE 100 TO WS-LINEA.
       PERFORM 080-LEER-MAESTRO.
       PERFORM PROCESAR-ALQ UNTIL M-EOF = "SI".
       OTRA SECTION.
      *-----------------------------------------------------------------
      ******
       PROCESAR-ALQ.
       IF ALQ-ESTADO = "P"
           MOVE ALQ-CHOFER TO CHO-NRO-LEGAJO
           START CHOFERES KEY IS EQUAL TO CHO-NRO-LEGAJO
           IF CHO-ESTADO = 00
               PERFORM ACTUALIZAR
           ELSE
               IF CHO-ESTADO = 10
                   PERFORM RECHAZO
               ELSE
                   DISPLAY 'ERROR ABRIENDO CHOFERES 'CHO-ESTADO.
       PERFORM 080-LEER-MAESTRO.
      *******
      *-----------------------------------------------------------------
      ******
       ACTUALIZAR.
           PERFORM LEER-CHOFERES.
           MOVE "SI" TO WS-RECHAZADO.
           PERFORM PROCESO-FECHA UNTIL CHO-ESTADO NOT ZERO OR
           CHO-NRO-LEGAJO <> ALQ-CHOFER.
           IF WS-RECHAZADO = "SI"
               PERFORM RECHAZO.
      *******
      *-----------------------------------------------------------------
      ******
       PROCESO-FECHA.
           IF ALQ-FECHA <= CHO-FECHA-HASTA AND ALQ-FECHA >= CHO-FECHA-
      -    DESDE
           MOVE ALQ TO REG-AUX
           MOVE "NO" TO WS-RECHAZADO
           MOVE "T" TO ALQ-ESTADO
           REWRITE ALQ
           RELEASE REG-AUX
           PERFORM LEER-CHOFERES.
      *******
      *-----------------------------------------------------------------
      ******
       LEER-CHOFERES.
           READ CHOFERES NEXT RECORD.
      *******
      *-----------------------------------------------------------------
      ******
       RECHAZO.
           WRITE RECH FROM ALQ.
      *******
      *-----------------------------------------------------------------
      ******
       SALIDA SECTION.
      *******
           MOVE "NO" TO AUX-EOF.
           MOVE 0 TO WS-TOTAL.
           MOVE 0 TO WS-TOTAL-FECHA.
           MOVE 0 TO WS-TOTAL-CHOFER.
           RETURN ARCH-AUX RECORD AT END MOVE "SI" TO AUX-EOF.
           PERFORM 030-ESCRIBIR-CABECERA-LISTADO.
           PERFORM PROCESAR-ORDENADO UNTIL AUX-EOF = "SI".
           MOVE WS-TOTAL TO PTR-TOTALGRAL.
           WRITE LINEA FROM PTR-TOTAL.
           ADD 1 TO WS-LINEA.

       OTRA-SALIDA SECTION.
      *-----------------------------------------------------------------
      *******
       PROCESAR-ORDENADO.
               MOVE AUX-FECHA TO WS-FECHA.
               PERFORM PROCESAR-CHOFER UNTIL AUX-FECHA <> WS-FECHA OR
               AUX-EOF = "SI".
               ADD WS-TOTAL-FECHA TO WS-TOTAL.
               MOVE WS-TOTAL-FECHA TO PTR-FECHA.
               WRITE LINEA FROM PTR-TOTAL-FECHA.
               PERFORM LLENAR-HOJA.
               PERFORM 030-ESCRIBIR-CABECERA-LISTADO.
               MOVE 0 TO WS-TOTAL-FECHA.
               ADD 1 TO WS-LINEA.
      *-----------------------------------------------------------------
      *******
       LLENAR-HOJA.
           IF WS-LINEA < 60
               PERFORM UNTIL WS-LINEA = 61
                   WRITE LINEA FROM PE3-ENCABE
                   ADD 1 TO WS-LINEA
               END-PERFORM.
      *-----------------------------------------------------------------
      *******
       PROCESAR-CHOFER.
           MOVE AUX-CHOFER TO WS-CHOFER.
           PERFORM ESCRIBIR-LISTADO UNTIL (AUX-CHOFER <> WS-CHOFER OR
           AUX-EOF = "SI" OR AUX-FECHA <> WS-FECHA).
           ADD WS-TOTAL-CHOFER TO WS-TOTAL-FECHA.
           MOVE WS-TOTAL-CHOFER TO PTR-CHOFER.
           WRITE LINEA FROM PTR-TOTAL-CHOFER.
           ADD 1 TO WS-LINEA.
           IF AUX-FECHA = WS-FECHA
           PERFORM LLENAR-HOJA
           PERFORM 030-ESCRIBIR-CABECERA-LISTADO.
           MOVE 0 TO WS-TOTAL-CHOFER.
      *-----------------------------------------------------------------
      *******
       ESCRIBIR-LISTADO.
           ADD 1 TO WS-TOTAL-CHOFER.
           MOVE 'P' TO LK-TIPO-OP.
           MOVE AUX-NRO-DOC TO LK-NRO-DOC.
           CALL 'SUBPGR' USING LK-TIPO-OP, LK-NRO-DOC, LK-RES, LK-CLIEN
      -    TE, LK-DIRECCION.
           IF LK-RES <> SPACES
               DISPLAY "NO SE ENCUENTRA EL CLIENTE".
           MOVE LK-CLIENTE TO ROW-CLIENTE.
           MOVE AUX-TIPO-DOC TO ROW-TIPO-DOC.
           MOVE AUX-NRO-DOC TO ROW-DOC.
           MOVE LK-DIRECCION TO ROW-DIRECCION.
           WRITE LINEA FROM PTR-ROW.
           ADD 1 TO WS-LINEA.
           RETURN ARCH-AUX RECORD AT END MOVE "SI" TO AUX-EOF.
      *-----------------------------------------------------------------
      *******
       END PROGRAM TP2.
