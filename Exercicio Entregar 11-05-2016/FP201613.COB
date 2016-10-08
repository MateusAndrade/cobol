       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP201613.
       AUTHOR. MATEUS A.
      **************************************
      *   MANUTENCAO CADASTRO DE FUNCIONARIO*
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADFUNC ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CHAPA
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                               WITH DUPLICATES.
      *

      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADFUNC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADFUNC.DAT".
       01 REGFUNC.
                03 CHAPA             PIC 9(05).
                03 FSTATUS           PIC X(01).
                03 NOME              PIC X(35).
                03 CPF               PIC 9(11).
                03 RG                PIC X(15).
                03 DATANASC          PIC 9(08).
                03 ESTCIVIL          PIC X(01).
                03 FDEPART           PIC 9(03).
                03 FCARGO            PIC 9(03).
                03 TIPOSAL           PIC X(02).
                03 FSALBASE          PIC 9(09)v99.
                03 DTADMISSAO        PIC 9(08).
                03 DTDEMISSAO        PIC 9(08).
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
       01 OPTNE        PIC X(25) VALUE SPACES.
       01 OPTTIPOSALARIO PIC X(25) VALUE SPACES.
       01 OPTTIPOSTATUS PIC X(25) VALUE SPACES.
       01 OPTESTCIVIL  PIC X(25) VALUE SPACES.
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

       01  TELAFUNCIONARIO.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                         CADASTRO DE FUN".
           05  LINE 02  COLUMN 41 
               VALUE  "CIONARIOS".
           05  LINE 05  COLUMN 01 
               VALUE  "               CHAPA:".
           05  LINE 05  COLUMN 41 
               VALUE  "     STATUS:".
           05  LINE 07  COLUMN 01 
               VALUE  "               NOME:".
           05  LINE 09  COLUMN 01 
               VALUE  "               CPF:".
           05  LINE 11  COLUMN 01 
               VALUE  "               RG:".
           05  LINE 13  COLUMN 01 
               VALUE  "               DATA DE NASCIMENTO:".
           05  LINE 13  COLUMN 41 
               VALUE  "      ESTADO CIVIL:".
           05  LINE 15  COLUMN 01 
               VALUE  "               DEPARTAMENTO:".
           05  LINE 15  COLUMN 41 
               VALUE  "              C.C:".
           05  LINE 17  COLUMN 01 
               VALUE  "               CARGO:".
           05  LINE 19  COLUMN 01 
               VALUE  "               TP.SAL:".
           05  LINE 19  COLUMN 41 
               VALUE  " SAL.BASE:".
           05  LINE 21  COLUMN 01 
               VALUE  "               DATA ADMISSAO:".
           05  LINE 21  COLUMN 41 
               VALUE  " DATA DEMISSAO:".
           05  TCHAPA
               LINE 05  COLUMN 23  PIC 9(05)
               USING  CHAPA
               HIGHLIGHT.
           05  TSTATUS
               LINE 05  COLUMN 54  PIC X(01)
               USING  FSTATUS.
           05  TNOME
               LINE 07  COLUMN 23  PIC X(35)
               USING  NOME
               HIGHLIGHT.
           05  TCPF
               LINE 09  COLUMN 23  PIC 9(11)
               USING  CPF
               HIGHLIGHT.
           05  TRG
               LINE 11  COLUMN 23  PIC X(15)
               USING  RG
               HIGHLIGHT.
           05  TDATANASC
               LINE 13  COLUMN 36  PIC 99/99/9999
               USING  DATANASC
               HIGHLIGHT.
           05  TESTCIVIL
               LINE 13  COLUMN 61  PIC X(01)
               USING  ESTCIVIL
               HIGHLIGHT.
           05  TDEPART
               LINE 15  COLUMN 30  PIC 9(03)
               USING  FDEPART
               HIGHLIGHT.
           05  TCARGO
               LINE 17  COLUMN 23  PIC 9(03)
               USING  FCARGO.
           05  TTIPOSAL
               LINE 19  COLUMN 24  PIC X(02)
               USING  TIPOSAL
               HIGHLIGHT.
           05  TSALBASE
               LINE 19  COLUMN 52  PIC 999.999,99
               USING  FSALBASE
               HIGHLIGHT.
           05  TDTADMISSAO
               LINE 21  COLUMN 31  PIC 99/99/9999
               USING  DTADMISSAO
               HIGHLIGHT.
           05  TDTDEMISSAO
               LINE 21  COLUMN 57  PIC 99/99/9999
               USING  DTDEMISSAO
               HIGHLIGHT.

      *-----------------------------------------------------------------

       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADFUNC
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADFUNC
                      CLOSE CADFUNC
                      MOVE "***CRIANDO ARQUIVO CADFUNC***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NO ARQUIVO CADFUNC" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      NEXT SENTENCE.

       INC-001.
                MOVE ZEROS TO CHAPA CPF DATANASC FDEPART FCARGO.
                MOVE ZEROS TO FSALBASE DTADMISSAO DTDEMISSAO.
                MOVE SPACES TO FSTATUS NOME RG ESTCIVIL TIPOSAL.
                DISPLAY TELAFUNCIONARIO.

       INC-002.
                ACCEPT TCHAPA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADFUNC
                   GO TO ROT-FIM.
                IF CHAPA = ZEROS
                   MOVE "*** CHAPA INVALIDO ***" TO MENS
                   MOVE ZEROS TO CHAPA
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.

       LER-CADFUNC01.
                MOVE 0 TO W-SEL
                READ CADFUNC
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TELAFUNCIONARIO
                      MOVE "*** CHAPA JA CADASTRADA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CADFUNC"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.

       INC-003.
                ACCEPT TNOME
                IF NOME = SPACES
                   MOVE "*** NOME INVALIDA ***" TO MENS
                   MOVE SPACES TO NOME
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       
       INC-004.
                ACCEPT TSTATUS
                ACCEPT W-ACT FROM ESCAPE KEY
                IF FSTATUS = "A"
                  MOVE "A - ATIVO" TO OPTTIPOSTATUS
                   DISPLAY (5, 54) OPTTIPOSTATUS
                ELSE IF FSTATUS = "D"
                  MOVE "D - DEMITIDO" TO OPTTIPOSTATUS
                   DISPLAY (5, 54) OPTTIPOSTATUS
                ELSE IF TIPOSAL = "I"
                  MOVE "I - INATIVO" TO OPTTIPOSTATUS
                   DISPLAY (5, 54) OPTTIPOSTATUS
                ELSE
                   MOVE "*** OPCAO INVALIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE SPACES TO FSTATUS
                   GO TO INC-004.
                IF W-ACT = 02 GO TO INC-003.

       INC-005.
                ACCEPT TCPF
                IF CPF = ZEROS
                   MOVE "*** CPF INVALIDO ***" TO MENS
                   MOVE ZEROS TO CPF
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-005.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       
       INC-006.
                ACCEPT TRG
                IF RG = SPACES
                   MOVE "*** RG INVALIDO ***" TO MENS
                   MOVE SPACES TO RG
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-006.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT TDATANASC
                IF DATANASC = ZEROS
                   MOVE "*** DATA INVALIDA ***" TO MENS
                   MOVE ZEROS TO DATANASC
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-007.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT TESTCIVIL
                IF ESTCIVIL = "S"
                  MOVE "S - SOLTEIRO" TO OPTESTCIVIL
                   DISPLAY (13, 61) OPTESTCIVIL
                ELSE IF ESTCIVIL = "C"
                  MOVE "C - CASADO" TO OPTESTCIVIL
                   DISPLAY (13, 61) OPTESTCIVIL
                ELSE IF ESTCIVIL = "D"
                  MOVE "D - DIVORCIADO" TO OPTESTCIVIL
                   DISPLAY (13, 61) OPTESTCIVIL
                ELSE IF ESTCIVIL = "V"
                  MOVE "V - VIUVO" TO OPTESTCIVIL
                   DISPLAY (13, 61) OPTESTCIVIL
                ELSE 
                   MOVE "*** OPCAO NVLIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE SPACES TO ESTCIVIL
                   GO TO INC-008.
                IF W-ACT = 02 GO TO INC-007.
       INC-009.
                ACCEPT TDEPART
                IF FDEPART = ZEROS
                   MOVE "*** DEPART INVALIDA ***" TO MENS
                   MOVE ZEROS TO FDEPART
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-009.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
       INC-010.
                ACCEPT TCARGO
                IF FCARGO = ZEROS
                   MOVE "*** CARGO INVALIDA ***" TO MENS
                   MOVE ZEROS TO FCARGO
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-010.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
       INC-011.
                ACCEPT TTIPOSAL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF TIPOSAL = "H"
                  MOVE "H - HORISTA" TO OPTTIPOSALARIO
                   DISPLAY (19, 24) OPTTIPOSALARIO
                ELSE IF TIPOSAL = "D"
                  MOVE "D - DIARISTA" TO OPTTIPOSALARIO
                   DISPLAY (19, 24) OPTTIPOSALARIO
                ELSE IF TIPOSAL = "M"
                  MOVE "M - MENSALISTA" TO OPTTIPOSALARIO
                   DISPLAY (19, 24) OPTTIPOSALARIO
                ELSE IF TIPOSAL = "T"
                  MOVE "T - TAREFEIRO" TO OPTTIPOSALARIO
                   DISPLAY (19, 24) OPTTIPOSALARIO
                ELSE IF TIPOSAL = "C"
                  MOVE "C - COMISSIONADO" TO OPTTIPOSALARIO
                   DISPLAY (19, 24) OPTTIPOSALARIO
                ELSE
                   MOVE "*** OPCAO INVALIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE SPACES TO TIPOSAL
                   GO TO INC-011.
       INC-012.
                ACCEPT TSALBASE
                IF FSALBASE = ZEROS
                   MOVE "*** SALARIOBASE INVALIDO ***" TO MENS
                   MOVE ZEROS TO FSALBASE
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-012.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-011.
       INC-013.
                ACCEPT TDTADMISSAO
                IF DTADMISSAO = ZEROS
                   MOVE "*** DATA INVALIDA ***" TO MENS
                   MOVE ZEROS TO DTADMISSAO
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-012.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-012.
       INC-014.
                ACCEPT TDTDEMISSAO
                IF DTDEMISSAO = ZEROS
                   MOVE "*** DATA INVALIDA ***" TO MENS
                   MOVE ZEROS TO DTDEMISSAO
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-014.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-013.
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
                WRITE REGFUNC
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** CEP JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CADFUNC"
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
                DELETE CADFUNC RECORD
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
                REWRITE REGFUNC
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CADFUNC"   TO MENS
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