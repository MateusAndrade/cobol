       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP201613.
       AUTHOR. LEANDRO VIEIRA SENA.
      **************************************
      *      CADASTRO DE FUNCIONARIO       *
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
                               
           SELECT CADEPTO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGODEPTO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CENTRODECUSTO
                               WITH DUPLICATES.
                               
           SELECT CADCARGO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO
                               WITH DUPLICATES.

      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADFUNC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADFUNC.DAT".
       01 REGFUNC.
                03 CHAPA             PIC 9(05).
                03 NOME              PIC X(35).
                03 CPF               PIC 9(11).
                03 RG                PIC X(15).
                03 DATANASC.
                   05 DIA     PIC 9(02).
                   05 MES     PIC 9(02).
                   05 ANO     PIC 9(02).
                03 ESTCIVIL   PIC X(01).
                03 NUMFIL     PIC 9(01).
                03 DATAADM.
                   05 DIA     PIC 9(02).
                   05 MES     PIC 9(02).
                   05 ANO     PIC 9(02).
                03 DATADEM.
                   05 DIA     PIC 9(02).
                   05 MES     PIC 9(02).
                   05 ANO     PIC 9(02).
                03 ESTATUS    PIC X(01).
                03 FILLER     PIC X(31).
                
       FD CADEPTO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADEPTO.DAT".
       01 REGDEPTO.
                03 CODIGODEPTO    PIC 9(03).
                03 DENOMINACAODEPTO PIC X(15).
                03 CENTRODECUSTO  PIC 9(01).
                03 FILLER   PIC X(45).
                
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
       01 OPTESTCIVIL  PIC X(15) VALUE SPACES.
       01 OPTESTATUS   PIC X(15) VALUE SPACES.
      *-----------------------------------------------------------------
      
       SCREEN SECTION
       
       01  TELACADFUNC.
           05  BLANK SCREEN.
           05  FOREGROUND-COLOR  2  BACKGROUND-COLOR 0.
           05  LINE 02  COLUMN 01 
               VALUE  "                          CADASTRO DE FU".
           05  LINE 02  COLUMN 41 
               VALUE  "NCIONARIOS".
           05  LINE 04  COLUMN 01 
               VALUE  "     CHAPA:        NOME:".
           05  LINE 06  COLUMN 01 
               VALUE  "     CPF:              RG:".
           05  LINE 06  COLUMN 41 
               VALUE  "   DATA DE NASCIMENTO:".
           05  LINE 08  COLUMN 01 
               VALUE  "     ESTADO CIVIL:".
           05  LINE 08  COLUMN 41 
               VALUE  " NUMERO DE FILHOS:".
           05  LINE 10  COLUMN 01 
               VALUE  "     DEPARTAMENTO:".
           05  LINE 10  COLUMN 41 
               VALUE  " CENTRO DE CUSTO:".
           05  LINE 12  COLUMN 01 
               VALUE  "     CARGO:                   TIPO SALAR".
           05  LINE 12  COLUMN 41 
               VALUE  "IO:           SALARIO BASE:".
           05  LINE 14  COLUMN 01 
               VALUE  "     DATA ADMISSAO:                   DA".
           05  LINE 14  COLUMN 41 
               VALUE  "TA DEMISSAO:".
           05  LINE 16  COLUMN 01 
               VALUE  "     STATUS:".
           05  TELACHAPA
               LINE 04  COLUMN 13  PIC 9(05)
               USING  CHAPA
               HIGHLIGHT.
           05  TELANOME
               LINE 04  COLUMN 26  PIC X(35)
               USING  NOME
               FOREGROUND-COLOR  2  BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  TELACPF
               LINE 06  COLUMN 11  PIC 9(11)
               USING  CPF
               HIGHLIGHT.
           05  TELARG
               LINE 06  COLUMN 28  PIC X(15)
               USING  RG
               HIGHLIGHT.
           05  TELADATANASC
               LINE 06  COLUMN 64  PIC 99/99/99
               USING  DATANASC
               HIGHLIGHT.
           05  TELAESTCIVIL
               LINE 08  COLUMN 20  PIC X(01)
               USING  ESTCIVIL
               HIGHLIGHT.
           05  TELANUMFIL
               LINE 08  COLUMN 60  PIC 9(01)
               USING  NUMFIL
               HIGHLIGHT.
           05  TELADATAADM
               LINE 14  COLUMN 21  PIC 99/99/99
               USING  DATAADM
               HIGHLIGHT.
           05  TELADATADEM
               LINE 14  COLUMN 54  PIC 99/99/99
               USING  DATADEM
               HIGHLIGHT.
           05  TELASTATUS
               LINE 16  COLUMN 14  PIC X(01)
               USING  ESTATUS
               HIGHLIGHT.

      *-----------------------------------------------------------------

       PROCEDURE DIVISION.
       INICIO.
        INC-OP1.
           OPEN INPUT CADEPTO
           IF ST-ERRO NOT = "00"
                 MOVE "ERRO NA ABERTURA CADEPTO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
               NEXT SENTENCE.

   
        INC-OP2.
           OPEN INPUT  CADCARGO
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "* ARQUIVO CADCARGO NAO ENCONTRADO*" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
              
                 MOVE "ERRO NA ABERTURA CADCARGO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
               NEXT SENTENCE.

       INC-OP3.
           OPEN I-O CADFUNC
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADFUNC
                      CLOSE CADFUNC
                      MOVE "***CRIANDO ARQUIVO CADFUNC***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP3
                   ELSE
                      MOVE "ERRO NO ARQUIVO CADFUNC" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      NEXT SENTENCE.

       INC-X.
           ACCEPT CODIGODEPTO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02 GO TO INC-OP3
           IF CODIGODEPTO = ZEROS
                 MOVE "*** DEPTO  INVALIDA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-X.
           PERFORM LER-CADEPTO THRU LER-CADEPTO-FIM
           IF ST-ERRO = "23"
                         GO TO INC-X
           ELSE
                        DISPLAY TELACADFUNC.



       LER-CADEPTO.
           MOVE CODIGODEPTO  TO CODIGODEPTO.
           READ CADEPTO
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "DEPTO NAO CADASTRADO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADEPTO"   TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       LER-CADEPTO-FIM.
           EXIT.

       LER-CADCARGO.
           MOVE CODIGO TO CODIGO.
           READ CADCARGO
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "CARGO NAO CADASTRADO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCARGO"   TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       LER-CADCARGO-FIM.
           EXIT.

       INC-001.
                MOVE ZEROS TO CHAPA CPF DATANASC NUMFIL DATAADM.
                MOVE ZEROS TO DATADEM.
                MOVE SPACES TO NOME RG ESTCIVIL ESTATUS.
                DISPLAY TELACADFUNC.

       INC-002.
                ACCEPT TELACHAPA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADFUNC
                   GO TO ROT-FIM.
                IF CHAPA = ZEROS
                   MOVE "*** CHAPA INVALIDA ***" TO MENS
                   MOVE ZEROS TO CHAPA
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.

       LER-CADFUNC01.
                MOVE 0 TO W-SEL
                READ CADFUNC
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TELACADFUNC
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
                ACCEPT TELANOME
                IF NOME = SPACES
                   MOVE "*** NOME INVALIDO ***" TO MENS
                   MOVE SPACES TO NOME
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                
       INC-004.
                ACCEPT TELACPF
                IF CPF = ZEROS
                   MOVE "*** CPF INVALIDO ***" TO MENS
                   MOVE ZEROS TO CPF
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-004.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT TELARG
                IF RG = SPACES
                   MOVE "*** RG INVALIDO ***" TO MENS
                   MOVE SPACES TO RG
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-005.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT TELADATANASC
                IF DATANASC = ZEROS
                   MOVE "*** DATA INVALIDA ***" TO MENS
                   MOVE ZEROS TO DATANASC
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-006.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT TELAESTCIVIL
                IF ESTCIVIL = "S"
                  MOVE "S - SOLTEIRX" TO OPTESTCIVIL
                   DISPLAY (08, 20) OPTESTCIVIL
                ELSE IF ESTCIVIL = "C"
                  MOVE "C - CASADX" TO OPTESTCIVIL
                   DISPLAY (08, 20) OPTESTCIVIL
                ELSE IF ESTCIVIL = "D"
                  MOVE "D - DIVORCIADX" TO OPTESTCIVIL
                   DISPLAY (08, 20) OPTESTCIVIL
                ELSE IF ESTCIVIL = "V"
                  MOVE "V - VIUVX" TO OPTESTCIVIL
                   DISPLAY (08, 20) OPTESTCIVIL
                ELSE IF ESTCIVIL = "E"
                  MOVE "E - SEPARADX" TO OPTESTCIVIL
                   DISPLAY (08, 20) OPTESTCIVIL
                ELSE IF ESTCIVIL = "O"
                  MOVE "O - COMPANHEIRX" TO OPTESTCIVIL
                   DISPLAY (08, 20) OPTESTCIVIL
                ELSE
                   MOVE "*** OPCAO INVALIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE SPACES TO ESTCIVIL
                   GO TO INC-007.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT TELANUMFIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
       INC-009.
                ACCEPT TELADATAADM
                IF DATAADM = ZEROS
                   MOVE "*** DATA INVALIDA ***" TO MENS
                   MOVE ZEROS TO DATAADM
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-009.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
       INC-010.
                ACCEPT TELADATADEM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
       INC-011.
                ACCEPT TELASTATUS
				CLOSE CADEPTO
				CLOSE CADCARGO
                IF ESTATUS = "A"
                  MOVE "A - ATIVX" TO OPTESTATUS
                   DISPLAY (16, 14) OPTESTATUS
                ELSE IF ESTATUS = "I"
                  MOVE "I - INATIVX" TO OPTESTATUS
                   DISPLAY (16, 14) OPTESTATUS
                ELSE IF ESTATUS = "D"
                  MOVE "D - DEMITIDX" TO OPTESTATUS
                   DISPLAY (16, 14) OPTESTATUS
                ELSE
                   MOVE "*** OPCAO INVALIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE SPACES TO ESTATUS
                   GO TO INC-011.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-010.
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