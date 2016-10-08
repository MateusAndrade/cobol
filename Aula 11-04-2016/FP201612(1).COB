       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP201612.
       AUTHOR. MATEUS.
      **************************************
      * MANUTENCAO DO CADASTRO DE CARGOS *
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
       FD CACARGO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCARGO.DAT".
       01 REGCARGO.
                03 CODIGO        PIC 9(03).
                03 DENOMINACAO   PIC X(20).
                03 TIPOSALARIO   PIC X(01).
				03 SALARIOBASE   PIC 9(06)v99.
				03 NIVELESCOLAR  PIC 9(01).
                03 FILLER        PIC X(31).
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
       01 CDCT         PIC X(25) VALUE SPACES.
       01 INDXSAL      PIC 9(03) VALUE ZEROS.
       01 TABSAL. 
           03 TABSAL1      PIC X(25) VALUE "HORISTA                  ".
           03 TABSAL2      PIC X(25) VALUE "DIARISTA                 ".
           03 TABSAL3      PIC X(25) VALUE "MENSALISTA               ".
           03 TABSAL4      PIC X(25) VALUE "TAREFEIRO                ".
           03 TABSAL5      PIC X(25) VALUE "COMISSIONADO             ".
       01 TABSALARIO       REDEFINES TABSAL.
           03 TABSALARIO1  PIC X(25) OCCURS 5 TIMES.
       01 INDXESC      PIC 9(03) VALUE ZEROS.
       01 TABSESC. 
           03 TABESC1      PIC X(25) VALUE "ANALFABETO               ".
           03 TABESC2      PIC X(25) VALUE "FUNDAMENTAL INCOMPLETO   ".
           03 TABESC3      PIC X(25) VALUE "FUNDAMENTAL COMPLETO     ".
           03 TABESC4      PIC X(25) VALUE "MEDIO INCOMPLETO         ".
           03 TABESC5      PIC X(25) VALUE "MEDIO COMPLETO           ".
           03 TABESC6      PIC X(25) VALUE "SUPERIOR INCOMPLETO      ".
           03 TABESC7      PIC X(25) VALUE "SUPERIOR COMPLETO        ".
           03 TABESC8      PIC X(25) VALUE "POS-GRADUACAO INCOMPLETO ".
           03 TABESC9      PIC X(25) VALUE "POS-GRADUACAO COMPLETO ".
       01 TABESCOLA        REDEFINES TABSAL.
           03 TABESCOLA1   PIC X(25) OCCURS 5 TIMES.	
       SCREEN SECTION.
       01  TELACARGO.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "                            CADASTRO DE".
           05  LINE 03  COLUMN 41 
               VALUE  "CARGOS".
           05  LINE 06  COLUMN 01 
               VALUE  "           CODIGO:".
           05  LINE 08  COLUMN 01 
               VALUE  "           DENOMINACAO:".
           05  LINE 10  COLUMN 01 
               VALUE  "           TIPO SALARIO:".
           05  LINE 12  COLUMN 01 
               VALUE  "           NIVEL ESCOLARIDADE:".
           05  LINE 14  COLUMN 01 
               VALUE  "           SALARIO BASE:".
           05  TCODIGO
               LINE 06  COLUMN 19  PIC 9(03)
               USING  CODIGO
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 08  COLUMN 24  PIC X(20)
               USING  DENOMINACAO.
           05  TTIPOSALARIO
               LINE 10  COLUMN 25  PIC X(01)
               USING  TIPOSALARIO
               HIGHLIGHT.
           05  TNIVELESCOLAR
               LINE 12  COLUMN 31  PIC 9(01)
               USING  NIVELESCOLAR
               HIGHLIGHT.
           05  TSALARIOBASE
               LINE 14  COLUMN 25  PIC 999.999,99
               USING  SALARIOBASE
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADCARGO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADCARGO
                      CLOSE CADCARGO
                      MOVE "*** ARQUIVO CARGO SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CARGO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.		   
       INC-001.
                MOVE ZEROS TO CODIGO SALARIOBASE NIVELESCOLAR
                MOVE SPACES TO DENOMINACAO CDCT.
                DISPLAY TELACARGO.
       INC-002.
                ACCEPT TCODIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE REGCARGO
                   GO TO ROT-FIM.
                IF CODIGO  = 0
                   MOVE "*** CARGO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       LER-CARGO01.
                MOVE 0 TO W-SEL
                READ REGCARGO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM INC-004A
                      DISPLAY TELACARGO
                      MOVE "*** CARGOAMENTO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO REGCARGO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT TDENOMINACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.			
       LER-REGCARGO01.
                MOVE 0 TO W-SEL
                READ REGCARGO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM INC-004A
                      DISPLAY TELACARGO
                      MOVE "*** REGCARGO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO REGCARGO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-004.
                ACCEPT TTIPOSALARIO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF W-ACT = 11 ADD 1 TO CDC
                       MOVE TABCC1(CDC) TO CDCT
                       DISPLAY TELACARGO
                       GO TO INC-004.
                IF W-ACT = 10 COMPUTE CDC = CDC - 1
                       MOVE TABCC1(CDC) TO CDCT
                       DISPLAY TELACARGO
                       GO TO INC-004.
       INC-004A.
                IF CDC = 0 MOVE 10 TO INDX
                ELSE
                  MOVE CDC TO INDX.
                MOVE TABSALARIO(INDXSAL) TO CDCT

                DISPLAY TELACARGO.
 
                IF W-SEL = 1
                              GO TO INC-005.
       INC-005.
                ACCEPT TTIPOSALARIO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.	
       INC-006.
                ACCEPT TNIVELESCOLAR
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
	   INC-007.
                ACCEPT TSALARIOBASE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-OPC.							  
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE CADEPTO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** REGCARGO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE REGCARGO"
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
                DELETE REGCARGO RECORD
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
                REWRITE CADEPTO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO REGCARGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
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