      ******************************************************************
      * Author: Leandro Denis
      * Purpose: TP 1 Algoritmos 4 Parte 1
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

           SELECT LISTADO      ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  M       LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../MAESTRO.DAT".
       01  MAE.
           03  MAE-PATENTE         PIC X(6).
           03  MAE-FECHA           PIC 9(8).
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
           03  ACT-FECHA           PIC 9(8).
           03  ACT-TIPO-DOC        PIC X.
           03  ACT-NRO-DOC         PIC X(20).
           03  ACT-IMPORTE         PIC 9(4)V99.

       FD  RECHAZOS LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../RECHAZOS.DAT".
       01  RECHAZO.
           03  RECHAZO-PATENTE        PIC X(6).
           03  RECHAZO-FECHA          PIC 9(8).
           03  RECHAZO-TIPO-DOC       PIC X.
           03  RECHAZO-NRO-DOC        PIC X(20).
           03  RECHAZO-MOTIVO         PIC 9.
           03  RECHAZO-AGENCIA        PIC 9.

       FD  AUTOS   LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../AUTOS.DAT".

       01  AUT.
           03  AUT-PATENTE     PIC X(6).
           03  AUT-DESC        PIC X(30).
           03  AUT-MARCA       PIC X(20).
           03  AUT-COLOR       PIC X(10).
           03  AUT-TAMAÑO      PIC X.
           03  AUT-IMPORTE     PIC 9(4)V99.

       FD  LISTADO  LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../LISTADO.DAT".

       01  LINEA               PIC X(60).

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
       77  WS-TOTAL-GENERAL    PIC 9(7)V99.
       01  WS-SUB              PIC 9(3).
       01  WS-MENOR-PATENTE    PIC X(6).
       01  WS-NROPATENTE       PIC X(6).
       01  WS-TOTAL-PATENTE    PIC 9(3).
       01  WS-CANTIDAD-DIAS    PIC 9(3).
       01  WS-ALQ              PIC X(2).
       01  WS-EXISTE           PIC X(2).
       01  WS-EXISTE-TABLA     PIC X(2).
       01  WS-MENOR-FECHA      PIC 9(8).
       01  WS-LINEA            PIC 9(2).
       01  WS-ESCRIBE_ENCABE   PIC X(2).
       01  PTR-DETALLE.
           03 PTR-DESC         PIC X(30).
           03 PTR-MARCA        PIC X(20).
           03 PTR-COLOR        PIC X(10).
           03 PTR-TAMANIO      PIC X.
           03 PTR-IMPORTE      PIC 9(4)V99.
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
           03 FILLER           PIC X(34) VALUE ' '.
           03 FILLER           PIC X(6) VALUE 'Hoja: '.
           03 PE1-HOJA         PIC 999.
       01  PE2-ENCABE.
           03 FILLER           PIC X(15) VALUE ' '.
           03 FILLER           PIC X(45) VALUE 'Listado de autos alquila
      -    'dos aprobados'.
       01  PE3-ENCABE          PIC X(60) VALUE ' '.
       01  PE4-ENCABE.
           03 FILLER           PIC X(9) VALUE 'Patente: '.
           03 PE4-PATENTE      PIC XXX999.
           03 FILLER           PIC X(2) VALUE ' '.
           03 FILLER           PIC X(13) VALUE 'Descripcion: '.
           03 PE4-DESC         PIC X(30).
       01  PE5-ENCABE.
           03 FILLER           PIC X(17) VALUE ' '.
           03 FILLER           PIC X(7) VALUE 'Marca: '.
           03 PE5-MARCA        PIC X(20).
           03 FILLER           PIC X(6) VALUE ' '.
       01  PE6-ENCABE.
           03 FILLER           PIC X(17) VALUE ' '.
           03 FILLER           PIC X(7) VALUE 'Color: '.
           03 PE6-COLOR        PIC X(10).
           03 FILLER           PIC X(16) VALUE ' '.
       01  PE7-ENCABE.
           03 FILLER           PIC X(17) VALUE ' '.
           03 FILLER           PIC X(8) VALUE 'Tamano: '.
           03 PE7-TAMANIO      PIC X.
           03 FILLER           PIC X(26) VALUE ' '.
       01  PE8-ENCABE.
           03 FILLER           PIC X(10) VALUE ' '.
           03 FILLER           PIC X(5) VALUE 'Fecha'.
           03 FILLER           PIC X(8) VALUE ' '.
           03 FILLER           PIC X(8) VALUE 'Tipo Doc'.
           03 FILLER           PIC X(3) VALUE ' '.
           03 FILLER           PIC X(14) VALUE 'Nro. Documento'.
           03 FILLER           PIC X(3) VALUE ' '.
           03 FILLER           PIC X(7) VALUE 'Agencia'.
           03 FILLER           PIC X(2) VALUE ' '.
       01  PE9-ENCABE.
           03 FILLER           PIC X(60) VALUE ALL '_'.
       01  PTR-ROW.
           03 FILLER           PIC X(8) VALUE ' '.
           03 ROW-FECHA.
               05 ROW-FECHA-DD PIC 99.
               05 FILLER       PIC X VALUE '/'.
               05 ROW-FECHA-MM PIC 99.
               05 FILLER       PIC X VALUE '/'.
               05 ROW-FECHA-AA PIC 9999.
           03 FILLER           PIC X(7) VALUE ' '.
           03 ROW-DOC          PIC X.
           03 FILLER           PIC X(9) VALUE ' '.
           03 ROW-NRO-DOC      PIC X(20).
           03 ROW-AG           PIC 9.
           03 FILLER           PIC X(2) VALUE ' '.
       01  PTR-TOTAL.
           03 FILLER           PIC X(19) VALUE 'Totales por patente'.
           03 FILLER           PIC X(3) VALUE ' '.
           03 FILLER           PIC X(17) VALUE 'Cantidad de dias '.
           03 TOTAL-DIAS       PIC 9999.
           03 FILLER           PIC X(3) VALUE ' '.
           03 FILLER           PIC X(8) VALUE 'Importe '.
           03 TOTAL-IMP        PIC 9(4)V99.
       01  PTR-TOT-GRAL.
           03 FILLER           PIC X(42) VALUE 'Totales general'.
           03 FILLER           PIC X(8) VALUE 'Importe '.
           03 TOT-GRAL         PIC 9(7)V99.
       01  RECH.
           03  RECH-PATENTE        PIC X(6).
           03  RECH-FECHA          PIC 9(8).
           03  RECH-TIPO-DOC       PIC X.
           03  RECH-NRO-DOC        PIC X(20).
           03  RECH-MOTIVO         PIC 9.
           03  RECH-AGENCIA        PIC 9.

       01  WS-TABLE.
           03  WS-AUTO OCCURS 300 TIMES
               INDEXED BY IND.
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
       COMPLETAR-HOJA.
               WRITE LINEA FROM PE3-ENCABE.
               ADD 1 TO WS-LINEA.
      *******
       030-ESCRIBIR-CABECERA-LISTADO.
      *******
           IF WS-ESCRIBE_ENCABE = "SI"
               MOVE FUNCTION CURRENT-DATE TO FECHA
               MOVE FECHA-DD TO PE1-FECHA-DD
               MOVE FECHA-MM TO PE1-FECHA-MM
               MOVE FECHA-AA TO PE1-FECHA-AA
               ADD 1 TO PE1-HOJA
               WRITE LINEA FROM PE1-ENCABE
               WRITE LINEA FROM PE2-ENCABE
               WRITE LINEA FROM PE3-ENCABE
               MOVE PTR-DESC TO PE4-DESC
               MOVE WS-NROPATENTE TO PE4-PATENTE
               WRITE LINEA FROM PE4-ENCABE
               MOVE PTR-MARCA TO PE5-MARCA
               WRITE LINEA FROM PE5-ENCABE
               MOVE PTR-COLOR TO PE6-COLOR
               WRITE LINEA FROM PE6-ENCABE
               MOVE PTR-TAMANIO TO PE7-TAMANIO
               WRITE LINEA FROM PE7-ENCABE
               WRITE LINEA FROM PE3-ENCABE
               WRITE LINEA FROM PE8-ENCABE
               WRITE LINEA FROM PE9-ENCABE
               MOVE 11 TO WS-LINEA.
           WRITE LINEA FROM PTR-ROW.
           ADD 1 TO WS-LINEA.
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
           PERFORM 100-DETERMINO-MENOR-PATENTE.
           MOVE WS-NROPATENTE TO WS-MENOR-PATENTE.
           MOVE 0 TO WS-TOTAL-PATENTE.
           MOVE 0 TO WS-CANTIDAD-DIAS.
           MOVE "SI" TO WS-ESCRIBE_ENCABE.
           PERFORM 110-PROCESO-PATENTE UNTIL (M-EOF = "SI" AND
            N1-EOF = "SI" AND N2-EOF = "SI" AND N3-EOF = "SI") OR
            WS-MENOR-PATENTE <> WS-NROPATENTE.
           WRITE LINEA FROM PE3-ENCABE.
           IF WS-CANTIDAD-DIAS <> 0
           ADD 2 TO WS-LINEA
           PERFORM 120-ESCRIBIR-TOTAL-PATENTE
           WRITE LINEA FROM PE3-ENCABE.
           COMPUTE WS-TOTAL-GENERAL = WS-TOTAL-GENERAL +
           WS-TOTAL-PATENTE.
      *-----------------------------------------------------------------
      *******
       060-ESCRIBIR-TOTAL-GENERAL.
      *******
           MOVE FUNCTION CURRENT-DATE TO FECHA
           MOVE FECHA-DD TO PE1-FECHA-DD
           MOVE FECHA-MM TO PE1-FECHA-MM
           MOVE FECHA-AA TO PE1-FECHA-AA
           ADD 1 TO PE1-HOJA
           WRITE LINEA FROM PE1-ENCABE
           WRITE LINEA FROM PE2-ENCABE
           WRITE LINEA FROM PE3-ENCABE
           MOVE WS-TOTAL-GENERAL TO TOT-GRAL.
           WRITE LINEA FROM PTR-TOT-GRAL.
      *-----------------------------------------------------------------
      *******
       070-CERRAR-ARCHIVOS.
      *******
           CLOSE
               M
               N1
               N2
               N3
               MAE-ACT
               RECHAZOS
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
       080-LEER-NOV1.
      ******
           READ N1
               AT END MOVE "SI" TO N1-EOF.
           IF N1-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR EN READ NOV 1  FS: " N1-ESTADO
               STOP RUN.
      *******
      *-----------------------------------------------------------------
      *******
       080-LEER-NOV2.
      ******
           READ N2
               AT END MOVE "SI" TO N2-EOF.
           IF N2-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR EN READ NOV 2  FS: " N2-ESTADO
               STOP RUN.
      *******
      *-----------------------------------------------------------------
      *******
       080-LEER-NOV3.
      ******
           READ N3
               AT END MOVE "SI" TO N3-EOF.
           IF N3-ESTADO NOT = ZERO AND 10
               DISPLAY "ERROR EN READ MAESTRO  FS: " N3-ESTADO
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
           MOVE AUT TO WS-AUTO(WS-SUB).
           ADD 1 TO WS-SUB.
           PERFORM 080-LEER-AUTOS.
      *******
      *-----------------------------------------------------------------
      *******
       100-DETERMINO-MENOR-PATENTE.
      *******
           IF M-EOF = "NO"
               MOVE MAE-PATENTE TO WS-NROPATENTE
           ELSE
               IF N1-EOF = "NO"
                   MOVE NOV1-PATENTE TO WS-NROPATENTE
               ELSE
                   IF N2-EOF = "NO"
                       MOVE NOV2-PATENTE TO WS-NROPATENTE
                   ELSE
                       MOVE NOV3-PATENTE TO WS-NROPATENTE.
           IF N1-EOF = "NO" AND NOV1-PATENTE < WS-NROPATENTE
               MOVE NOV1-PATENTE TO WS-NROPATENTE.
           IF N2-EOF = "NO" AND NOV2-PATENTE < WS-NROPATENTE
               MOVE NOV2-PATENTE TO WS-NROPATENTE.
           IF N3-EOF = "NO" AND NOV3-PATENTE < WS-NROPATENTE
               MOVE NOV3-PATENTE TO WS-NROPATENTE.
      *-----------------------------------------------------------------
      *******
       110-PROCESO-PATENTE.
      *******
           MOVE "NO" TO WS-ALQ.
           MOVE "NO" TO WS-EXISTE.
           PERFORM 130-BUSCO-TABLA.
           PERFORM 140-DETERMINO-MENOR-FECHA.
           PERFORM 150-PROCESO-M UNTIL M-EOF = "SI" OR WS-NROPATENTE
           <> MAE-PATENTE OR WS-MENOR-FECHA <> MAE-FECHA.
           PERFORM 150-PROCESO-N1 UNTIL N1-EOF = "SI" OR WS-NROPATENTE
           <> NOV1-PATENTE OR WS-MENOR-FECHA <> NOV1-FECHA.
           PERFORM 150-PROCESO-N3 UNTIL N3-EOF = "SI" OR WS-NROPATENTE
           <> NOV3-PATENTE OR WS-MENOR-FECHA <> NOV3-FECHA.
           PERFORM 150-PROCESO-N2 UNTIL N2-EOF = "SI" OR WS-NROPATENTE
           <> NOV2-PATENTE OR WS-MENOR-FECHA <> NOV2-FECHA.

           PERFORM 100-DETERMINO-MENOR-PATENTE.
      *-----------------------------------------------------------------
      *******
       120-ESCRIBIR-TOTAL-PATENTE.
      *******
           MOVE WS-TOTAL-PATENTE TO TOTAL-IMP.
           MOVE WS-CANTIDAD-DIAS TO TOTAL-DIAS.
           WRITE LINEA FROM PTR-TOTAL.
           ADD 1 TO WS-LINEA.
           PERFORM COMPLETAR-HOJA UNTIL WS-LINEA = 60.
      *-----------------------------------------------------------------
      *******
       130-BUSCO-TABLA.
      *******
           MOVE "NO" TO WS-EXISTE.
           MOVE 1 TO IND.
           SEARCH WS-AUTO
               AT END MOVE "NO" TO WS-EXISTE-TABLA
               WHEN WS-AUTO-PATENTE(IND)EQUALS WS-NROPATENTE
               MOVE WS-AUTO-DESC(IND) TO PTR-DESC
               MOVE WS-AUTO-MARCA(IND) TO PTR-MARCA
               MOVE WS-AUTO-COLOR(IND) TO PTR-COLOR
               MOVE WS-AUTO-TAMAÑO(IND) TO PTR-TAMANIO
               MOVE WS-AUTO-IMPORTE(IND) TO PTR-IMPORTE
               MOVE "SI" TO WS-EXISTE.
      *******
      *-----------------------------------------------------------------
      *******
       140-DETERMINO-MENOR-FECHA.
      *******
           IF M-EOF = "NO" AND MAE-PATENTE = WS-NROPATENTE
               MOVE MAE-FECHA TO WS-MENOR-FECHA
           ELSE
               IF N1-EOF = "NO" AND NOV1-PATENTE = WS-NROPATENTE
                       MOVE NOV1-FECHA TO WS-MENOR-FECHA
                   ELSE
                       IF N2-EOF = "NO" AND NOV2-PATENTE = WS-NROPATENTE
                           MOVE NOV2-FECHA TO WS-MENOR-FECHA
                       ELSE
                           IF N3-EOF = "NO" AND
                               NOV3-PATENTE = WS-NROPATENTE
                           MOVE NOV3-FECHA TO WS-MENOR-FECHA.
           IF NOV1-FECHA < WS-MENOR-FECHA AND
               NOV1-PATENTE = WS-NROPATENTE AND N1-EOF = "NO"
               MOVE NOV1-FECHA TO WS-MENOR-FECHA.
           IF NOV2-FECHA < WS-MENOR-FECHA AND N2-EOF = "NO" AND
               NOV2-PATENTE = WS-NROPATENTE
               MOVE NOV2-FECHA TO WS-MENOR-FECHA.
           IF NOV3-FECHA < WS-MENOR-FECHA AND
               NOV3-PATENTE = WS-NROPATENTE AND N3-EOF = "NO"
               MOVE NOV3-FECHA TO WS-MENOR-FECHA.
      *-----------------------------------------------------------------
      *******
       150-PROCESO-M.
      *******
           IF WS-ALQ = "NO" AND WS-EXISTE = "SI"
               WRITE ACT FROM MAE
               MOVE MAE-FECHA TO ROW-FECHA
               MOVE MAE-TIPO-DOC TO ROW-DOC
               MOVE MAE-NRO-DOC TO ROW-NRO-DOC
               MOVE 0 TO ROW-AG
               PERFORM 030-ESCRIBIR-CABECERA-LISTADO.
               MOVE "NO" TO WS-ESCRIBE_ENCABE
               MOVE "SI" TO WS-ALQ
           PERFORM 080-LEER-MAESTRO.
      *-----------------------------------------------------------------
      *******
       150-PROCESO-N1.
      *******
           IF WS-ALQ = "NO" AND WS-EXISTE = "SI"
               COMPUTE WS-TOTAL-PATENTE = WS-TOTAL-PATENTE + PTR-IMPORTE
               COMPUTE WS-CANTIDAD-DIAS = WS-CANTIDAD-DIAS + 1
               WRITE ACT FROM NOV1
               MOVE NOV1-FECHA TO ROW-FECHA
               MOVE NOV1-TIPO-DOC TO ROW-DOC
               MOVE NOV1-NRO-DOC TO ROW-NRO-DOC
               MOVE 1 TO ROW-AG
               PERFORM 030-ESCRIBIR-CABECERA-LISTADO
               MOVE "NO" TO WS-ESCRIBE_ENCABE
               MOVE "SI" TO WS-ALQ
           ELSE
               MOVE NOV1-PATENTE TO RECH-PATENTE.
               MOVE NOV1-FECHA TO RECH-FECHA.
               MOVE NOV1-TIPO-DOC TO RECH-TIPO-DOC.
               MOVE NOV1-NRO-DOC TO RECH-NRO-DOC.
               MOVE 1 TO RECH-AGENCIA.
               PERFORM 160-GRABAR-RECHAZO.
           PERFORM 080-LEER-NOV1.
      *-----------------------------------------------------------------
      *******
       150-PROCESO-N2.
      *******
           IF WS-ALQ = "NO" AND WS-EXISTE = "SI"
               COMPUTE WS-TOTAL-PATENTE = WS-TOTAL-PATENTE + PTR-IMPORTE
               COMPUTE WS-CANTIDAD-DIAS = WS-CANTIDAD-DIAS + 1
               MOVE NOV2-FECHA TO ROW-FECHA
               MOVE NOV2-TIPO-DOC TO ROW-DOC
               MOVE NOV2-NRO-DOC TO ROW-NRO-DOC
               MOVE 2 TO ROW-AG
               PERFORM 030-ESCRIBIR-CABECERA-LISTADO
               MOVE "NO" TO WS-ESCRIBE_ENCABE
               WRITE ACT FROM NOV2
               MOVE "SI" TO WS-ALQ
           ELSE
               MOVE NOV2-PATENTE TO RECH-PATENTE.
               MOVE NOV2-FECHA TO RECH-FECHA.
               MOVE NOV2-TIPO-DOC TO RECH-TIPO-DOC.
               MOVE NOV2-NRO-DOC TO RECH-NRO-DOC.
               MOVE 2 TO RECH-AGENCIA.
               PERFORM 160-GRABAR-RECHAZO.
           PERFORM 080-LEER-NOV2.
      *-----------------------------------------------------------------
      *******
       150-PROCESO-N3.
      *******
           IF WS-ALQ = "NO" AND WS-EXISTE = "SI"
               COMPUTE WS-TOTAL-PATENTE = WS-TOTAL-PATENTE + PTR-IMPORTE
               COMPUTE WS-CANTIDAD-DIAS = WS-CANTIDAD-DIAS + 1
               MOVE NOV3-FECHA TO ROW-FECHA
               MOVE NOV3-TIPO-DOC TO ROW-DOC
               MOVE NOV3-NRO-DOC TO ROW-NRO-DOC
               MOVE 3 TO ROW-AG
               PERFORM 030-ESCRIBIR-CABECERA-LISTADO
               MOVE "NO" TO WS-ESCRIBE_ENCABE
               WRITE ACT FROM NOV3
               MOVE "SI" TO WS-ALQ
           ELSE
               MOVE NOV3-PATENTE TO RECH-PATENTE.
               MOVE NOV3-FECHA TO RECH-FECHA.
               MOVE NOV3-TIPO-DOC TO RECH-TIPO-DOC.
               MOVE NOV3-NRO-DOC TO RECH-NRO-DOC.
               MOVE 3 TO RECH-AGENCIA.
               PERFORM 160-GRABAR-RECHAZO.
           PERFORM 080-LEER-NOV3.
      *-----------------------------------------------------------------
      *******
       160-GRABAR-RECHAZO.
           IF WS-EXISTE = "NO"
               MOVE 2 TO RECH-MOTIVO
           ELSE
               MOVE 1 TO RECH-MOTIVO.
           WRITE RECHAZO FROM RECH.
      *******
      *-----------------------------------------------------------------
       END PROGRAM TP-PARTE-1.
