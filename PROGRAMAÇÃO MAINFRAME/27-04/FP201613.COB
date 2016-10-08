       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP201613.
       AUTHOR. LEANDRO.
      *******************************************
      * MANUTENCAO DO CADASTRO DE FUNCIONARIOS. *
      *******************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCARGO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CHAPA
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO
                               WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADFUNC
       FD CADCARGO
       FD CADEPTO     
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADFUNC.DAT".
       01 REGCARGO.
                03 CHAPA               PIC 9(05).
                03 NOME                PIC X(35).
                03 CPF                 PIC 9(11).
      		03 RG                  PIC X(15).
                03 NASC.   
		   05 DIA              PIC 9(2).
                   05 MES              PIC 9(2).
                   05 ANO              PIC 9(4).
                03 ECIVIL              PIC X(1).
                03 DEPARTAMENTO        PIC 9(3).
                03 CC                  PIC 9(1).
                03 CCARGO              PIC 9(3).
                03 TIPOSAL             PIC X(1).
                03 SALBASE             PIC 999.999,99.
                03 DATAADMIS           PIC 99/99/9999.
                03 NFILHOS             PIC 9(1).
                03 DATADEMIS           PIC 99/99/9999.
                03 STATUS              PIC X(1).
                03 FILLER              PIC X(31).
0		
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.

       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

       INICIO.
      *
       INC-OP0.
           OPEN I-O CADFUNC
           OPEN I-O CADCARGO
           OPEN I-O CADEPTO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADFUNC
                      CLOSE CADFUNC
                      MOVE "*** ARQUIVO CADFUNC SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADFUNC" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
               NEXT SENTENCE.

        INC-001.
                MOVE ZEROS  TO CHAPA CPF NASC DEPARTAMENTO CC CCARGO SALBASE
                DATAADMIS NFILHOS DATADEMIS
                MOVE SPACES TO NOME RG ECIVIL TIPOSAL STATUS.

        INC-002.
                ACCEPT  (04, 12) CHAPA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADNOTA
                   GO TO ROT-FIM.
                IF RM  = ZEROS
                   MOVE "*** RM INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.