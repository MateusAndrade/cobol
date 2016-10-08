       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP201612.
       AUTHOR. GILLIARD.
      **************************************
      *    MANUTENCAO CADASTRO DE CARGOS   *
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
                03 CODIGO            PIC 9(03).
                03 DENOMINACAO       PIC X(15).
                03 TIPOSALARIO       PIC X(01).
                03 SALARIOBASE       PIC 9(06)V99.
                03 NIVELESCOLARIDADE PIC 9(01).
                03 FILLER            PIC X(31).
       
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       01 OPTTIPOSALARIO PIC X(25) VALUE SPACES.
       01 OPTNE        PIC X(25) VALUE SPACES.
       01 INDX         PIC 9(03) VALUE ZEROS.
       01 TABCCA. 
           03 TABCCA1      PIC X(25) VALUE "ANALFABETO               ".
           03 TABCCA2      PIC X(25) VALUE "FUNDAMENTAL INCOMPLETO   ".
           03 TABCCA3      PIC X(25) VALUE "FUNDAMENTAL COMPLETO     ".
           03 TABCCA4      PIC X(25) VALUE "MEDIO INCOMPLETO         ".
           03 TABCCA5      PIC X(25) VALUE "MEDIO COMPLETO           ".
           03 TABCCA6      PIC X(25) VALUE "SUPERIOR INCOMPLETO      ".
           03 TABCCA7      PIC X(25) VALUE "SUPERIOR COMPLETO        ".
           03 TABCCA8      PIC X(25) VALUE "POS-GRADUACAO INCOMPLETA ".
           03 TABCCA9      PIC X(25) VALUE "POS-GRADUACAO COMPLETA   ".
       01 TABCC REDEFINES TABCCA.
           03 TABCC1       PIC X(25) OCCURS 10 TIMES.
      *-----------------------------------------------------------------
      
       SCREEN SECTION
       
       
       01  TELACADASTROCARGOS.
           05  BLANK SCREEN.
           05  FOREGROUND-COLOR  2  BACKGROUND-COLOR 0.
           05  LINE 02  COLUMN 01 
               VALUE  "                            CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  "CARGOS".
           05  LINE 05  COLUMN 01 
               VALUE  "     CODIGO:".
           05  LINE 07  COLUMN 01 
               VALUE  "     DENOMINACAO:".
           05  LINE 09  COLUMN 01 
               VALUE  "     TIPO SALARIO:".
           05  LINE 11  COLUMN 01 
               VALUE  "     SALARIO BASE:".
           05  LINE 13  COLUMN 01 
               VALUE  "     NIVEL DE ESCOLARIDADE:".
           05  TELACODIGO
               LINE 05  COLUMN 14  PIC 9(03)
               USING  CODIGO
               FOREGROUND-COLOR  2  BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  TELADENOMINACAO
               LINE 07  COLUMN 19  PIC X(20)
               USING  DENOMINACAO
               FOREGROUND-COLOR  2  BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  TELATIPOSALARIO
               LINE 09  COLUMN 20  PIC X(01)
               USING  TIPOSALARIO
               FOREGROUND-COLOR  2  BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  TELASALARIOBASE
               LINE 11  COLUMN 20  PIC 999999,99
               USING  SALARIOBASE
               FOREGROUND-COLOR  2  BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  TELANIVELESCOLARIDADE
               LINE 13  COLUMN 29  PIC 9(01)
               USING  NIVELESCOLARIDADE
               FOREGROUND-COLOR  2  BACKGROUND-COLOR 0
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
                      MOVE "***CRIANDO ARQUIVO CADCARGO***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NO ARQUIVO CADCARGO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      NEXT SENTENCE.

       INC-001.
                MOVE ZEROS TO CODIGO SALARIOBASE NIVELESCOLARIDADE.
                MOVE SPACES TO DENOMINACAO TIPOSALARIO.
                DISPLAY TELACADASTROCARGOS.

       INC-002.
                ACCEPT TELACODIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADCARGO
                   GO TO ROT-FIM.
                IF CODIGO = ZEROS
                   MOVE "*** CODIGO INVALIDO ***" TO MENS
                   MOVE ZEROS TO CODIGO
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.

       LER-CADCARGO01.
                MOVE 0 TO W-SEL
                READ CADCARGO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TELACADASTROCARGOS
                      MOVE "*** CODIGO JA CADASTRAD0 ***" TO MENS
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
                ACCEPT TELADENOMINACAO
                IF DENOMINACAO = SPACES
                   MOVE "*** DENOMINACAO INVALIDA ***" TO MENS
                   MOVE SPACES TO DENOMINACAO
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                
       INC-004.
                ACCEPT TELATIPOSALARIO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF TIPOSALARIO = "H"
                  MOVE "H - HORISTA" TO OPTTIPOSALARIO
                   DISPLAY (09, 20) OPTTIPOSALARIO
                ELSE IF TIPOSALARIO = "D"
                  MOVE "D - DIARISTA" TO OPTTIPOSALARIO
                   DISPLAY (09, 20) OPTTIPOSALARIO
                ELSE IF TIPOSALARIO = "M"
                  MOVE "M - MENSALISTA" TO OPTTIPOSALARIO
                   DISPLAY (09, 20) OPTTIPOSALARIO
                ELSE IF TIPOSALARIO = "T"
                  MOVE "T - TAREFEIRO" TO OPTTIPOSALARIO
                   DISPLAY (09, 20) OPTTIPOSALARIO
                ELSE IF TIPOSALARIO = "C"
                  MOVE "C - COMISSIONADO" TO OPTTIPOSALARIO
                   DISPLAY (09, 20) OPTTIPOSALARIO
                ELSE
                   MOVE "*** OPCAO INVALIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE SPACES TO TIPOSALARIO
                   GO TO INC-004.
       INC-005.
                ACCEPT TELASALARIOBASE
                IF SALARIOBASE = ZEROS
                   MOVE "*** SALARIOBASE INVALIDO ***" TO MENS
                   MOVE ZEROS TO SALARIOBASE
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-005.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT TELANIVELESCOLARIDADE
                DISPLAY (13, 31) TABCC1(NIVELESCOLARIDADE)
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                
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
                WRITE REGCARGO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** CEP JA EXISTE ***       " TO MENS
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
                   GO TO INC-002.
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
                REWRITE REGCARGO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CADCARGO"   TO MENS
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
                IF W-CONT < 300000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.