      ******************************************************************
      * Author: Leandro Denis
      * Purpose: TP 2 Algoritmos 4
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP-2.
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
                               ALTERNATE KEY IS ALQ-FECHA
                               FILE STATUS IS M-ESTADO.

           SELECT CHOFERES     ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                               ACCESS MODE IS DYNAMIC
                               RECORD KEY IS CHO-NRO-LEGAJO
                               ALTERNATE KEY IS CHO-FECHA-DESDE
                               FILE STATUS IS CHO-ESTADO.

           SELECT CLIENTES     ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                               ACCESS MODE IS RANDOM
                               RECORD KEY IS CLI-NUMERO
                               ALTERNATE KEY IS CLI-DOCUMENTO
                               FILE STATUS IS CLI-ESTADO.

           SELECT RECHAZOS     ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                               ACCESS MODE IS RANDOM
                               RECORD KEY IS RECH-PATENTE
                               ALTERNATE KEY IS RECH-FECHA
                               FILE STATUS IS RECH-ESTADO.

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
                   VALUE OF FILE-ID IS "../LISTADO.DAT".

       01  LINEA               PIC X(80).

       FD  CLIENTES LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../CLIENTES.DAT".

       01  CLI.
           03 CLI-NUMERO       PIC X(8).
           03 CLI-FEC-ALTA     PIC 9(8).
           03 CLI-TELEFONO     PIC X(20).
           03 CLI-DIRECCION    PIC X(30).
           03 CLI-DOCUMENTO    PIC X(20).

       FD  CHOFERES LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "../CHOFERES.DAT".

       01  CHO.
           03 CHO-NRO-LEGAJO       PIC X(7).
           03 CHO-FECHA-DESDE      PIC 9(8).
           03 CHO-FECHA-HASTA      PIC 9(8).
           03 CHO-TURNO            PIC X.

       FD  RECHAZOS LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "./RECHAZOS.DAT".

       01  RECH.
           03 RECH-PATENTE         PIC X(6).
           03 RECH-FECHA           PIC 9(8).
           03 RECH-TIPO-DOC        PIC X.
           03 RECH-NRO-DOC         PIC X(20).
           03 RECH-IMPORTE         PIC 9(4)V99.

       WORKING-STORAGE SECTION.
       77  M-EOF               PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  CHO-EOF             PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  CLI-EOF             PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  RECH-EOF             PIC XXX     VALUE "NO".
           88 EOF                          VALUE "SI".
       77  RECH-ESTADO          PIC XX.
       77  CLI-ESTADO          PIC XX.
       77  M-ESTADO            PIC XX.
       77  CHO-ESTADO          PIC XX.
       77  WS-TOTAL-GENERAL    PIC 9(4).
       01  WS-SUB              PIC 9(3).
       01  CHOF.
           03 CHOF-NRO-LEGAJO       PIC X(7).
           03 CHOF-FECHA-DESDE      PIC 9(8).
           03 CHOF-FECHA-HASTA      PIC 9(8).
           03 CHOF-TURNO            PIC X.
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

       01  PTR-ROW.
           03 ROW-MARCA        PIC X(20).
           03 ROW-ENE          PIC 999.

       PROCEDURE DIVISION.
       COMIENZO.
            PERFORM 010-ABRIR-ARCHIVOS.
            PERFORM 030-ESCRIBIR-CABECERA-LISTADO.
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
           MOVE FUNCTION CURRENT-DATE TO FECHA.
           MOVE FECHA-DD TO PE1-FECHA-DD.
           MOVE FECHA-MM TO PE1-FECHA-MM.
           MOVE FECHA-AA TO PE1-FECHA-AA.
           ADD 1 TO PE1-HOJA.
           WRITE LINEA FROM PE1-ENCABE.
           WRITE LINEA FROM PE2-ENCABE.
           WRITE LINEA FROM PE3-ENCABE.
           WRITE LINEA FROM PE4-ENCABE.
           WRITE LINEA FROM PE6-ENCABE.
           WRITE LINEA FROM PE3-ENCABE.
           WRITE LINEA FROM PE7-ENCABE.
           WRITE LINEA FROM PE5-ENCABE.
      *-----------------------------------------------------------------
      *******
       050-PROCESAR.
      *******
           PERFORM 080-LEER-MAESTRO.
      *-----------------------------------------------------------------
      *******
       070-CERRAR-ARCHIVOS.
      *******
           CLOSE
               M
               CHOFERES
               RECHAZOS
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
       END PROGRAM TP-2.
