       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP201612.
       AUTHOR. LEANDRO.
      **************************************
      * MANUTENCAO DO CADASTRO DE CARGOS.  *
      **************************************
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
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO
                               WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCARGO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCARGO.DAT".
       01 REGCARGO.
                03 CODIGO              PIC 9(03).
                03 DENOMINACAO         PIC X(20).
                03 TIPO-SALARIO        PIC X(1).
      		03 SALARIO-BASE        PIC 9(6)V99.
                03 NIVEL-ESCOLARIDADE  PIC 9(1).
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
       01 TIPO-SALARIOT         PIC X(25) VALUE SPACES.
       01 ESCOLARIDADET         PIC X(25) VALUE SPACES.
       01 INDX         PIC 9(03) VALUE ZEROS.
       01 TABSAL. 
           03 TABSAL1      PIC X(25) VALUE "HORISTA       ".
           03 TABSAL2      PIC X(25) VALUE "DIARISTA        ".
           03 TABSAL3      PIC X(25) VALUE "MENSALISTA       ".
           03 TABSAL4      PIC X(25) VALUE "TAREFEIRO       ".
           03 TABSAL5      PIC X(25) VALUE "COMISSIONARIO    ".
       01 TABCC     REDEFINES TABSAL.
           03 TABCC1       PIC X(25) OCCURS 5 TIMES.

       01 TABESCO. 
           03 TABESCO1      PIC X(25) VALUE "ANALFABETO ".
           03 TABESCO2      PIC X(25) VALUE "FUNDAMENTAL INCOMPLETO ".
           03 TABESCO3      PIC X(25) VALUE "FUNDAMENTAL COMPLETO  ".
           03 TABESCO4      PIC X(25) VALUE "MEDIO INCOMPLETO ".
           03 TABESCO5      PIC X(25) VALUE "MEDIO COMPLETO    ".
           03 TABESCO6      PIC X(25) VALUE "SUPERIOR INCOMPLETO ".
           03 TABESCO7      PIC X(25) VALUE "SUPERIOR COMPLETO ".
           03 TABESCO8      PIC X(25) VALUE "POS-GRADUACAO INCOMPLETO ".
           03 TABESCO9      PIC X(25) VALUE "POS-GRADUACAO COMPLETO".
       01 TABCCC     REDEFINES TABESCO.
           03 TABCCC1       PIC X(25) OCCURS 9 TIMES.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADCARGO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADCARGO
                      CLOSE DEPART
                      MOVE "*** ARQUIVO CADCARGO SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADCARGO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS TO CODIGO SALARIO-BASE NIVEL-ESCOLARIDADE
                MOVE SPACES TO DENOMINACAO TIPOSALARIO.
                DISPLAY TELAFP.

       INC-002.
                ACCEPT TCODIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADCARGO
                   GO TO ROT-FIM.
                IF CODIGO  = 0
                   MOVE "*** CADCARGO  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.

       LER-CADCARGO01.
                MOVE 0 TO W-SEL
                READ CADCARGO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM INC-004A
                      DISPLAY TELAFP
                      MOVE "*** CARGO JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CADCARGO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.

       INC-003.
                ACCEPT TDENOMINACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.

       INC-004.
                ACCEPT TTIPO-SALARIO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF W-ACT = 11 ADD 1 TO TIPO-SALARIO
                       MOVE TABCC1(TIPO-SALARIO) TO TIPO-SALARIOT
                       DISPLAY TELAFP
                       GO TO INC-004.
                IF W-ACT = 10 COMPUTE TIPO-SALARIO = TIPO-SALARIO - 1
                       MOVE TABCC1(TIPO-SALARIO) TO TIPO-SALARIOT
                       DISPLAY TELAFP
                       GO TO INC-004.
       INC-004A.
                IF TIPO-SALARIO = 0 MOVE 10 TO INDX
                ELSE
                  MOVE CDC TO INDX.
                MOVE TABCC1(INDX) TO TIPO-SALARIOT

                DISPLAY TELAFP
                GO TO INC-003.
       INC-005.
                ACCEPT TSALARIO-BASE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.


       INC-006.
                ACCEPT TESCOLARIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-ACT = 11 ADD 1 TO NIVEL-ESCOLARIDADE
                       MOVE TABCCC1(NIVEL-ESCOLARIDADE) TO ESCOLARIDADET
                       DISPLAY TELAFP
                       GO TO INC-006.
                IF W-ACT = 10 COMPUTE NIVEL-ESCOLARIDADE = NIVEL-ESCOLARIDADE - 1
                       MOVE TABCCC1(NIVEL-ESCOLARIDADE) TO ESCOLARIDADET
                       DISPLAY TELAFP
                       GO TO INC-006.
       INC-006A.
                IF NIVEL-ESCOLARIDADE = 0 MOVE 10 TO INDX
                ELSE
                  MOVE NIVEL-ESCOLARIDADE TO INDX.
                MOVE TABCCC1(INDX) TO ESCOLARIDADET

                DISPLAY TELAFP.
      *
                IF W-SEL = 1
                              GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE CADCARGO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** CADCARGO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CADCARGO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCARGO RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***           " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE CADCARGO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CADCARGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 30000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

