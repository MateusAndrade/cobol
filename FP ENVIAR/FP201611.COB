       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP201611.
       AUTHOR. GILLIARD.
      **************************************
      *MANUTENCAO CADASTRO  DE DEPARTAMENTO*
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADEPTO ASSIGN TO DISK
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
       FD CADEPTO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADEPTO.DAT".
       01 REGDEPTO.
                03 CODIGO         PIC 9(03).
                03 DENOMINACAO    PIC X(15).
                03 CENTRODECUSTO  PIC 9(01).
                03 FILLER   PIC X(45).
       
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       01 CCE          PIC X(30).
      *-----------------------------------------------------------------
      
       SCREEN SECTION
       
       01  TELACADDEPTO.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                          CADASTRO DE DE".
           05  LINE 02  COLUMN 41 
               VALUE  "PARTAMENTO".
           05  LINE 04  COLUMN 01 
               VALUE  "      CODIGO:".
           05  LINE 06  COLUMN 01 
               VALUE  "      DENOMINACAO:".
           05  LINE 08  COLUMN 01 
               VALUE  "      CENTRO DE CUSTO:".
           05  LINE 11  COLUMN 41 
               VALUE  "     1".
           05  LINE 12  COLUMN 41 
               VALUE  "     2".
           05  LINE 13  COLUMN 41 
               VALUE  "     3".
           05  LINE 14  COLUMN 41 
               VALUE  "     4".
           05  LINE 15  COLUMN 41 
               VALUE  "     5".
           05  LINE 16  COLUMN 41 
               VALUE  "     6".
           05  LINE 17  COLUMN 41 
               VALUE  "     7".
           05  LINE 18  COLUMN 41 
               VALUE  "     8".
           05  LINE 19  COLUMN 41 
               VALUE  "     9".
           05  LINE 20  COLUMN 41 
               VALUE  "     0".
           05  TELACODIGO
               LINE 04  COLUMN 15  PIC 9(03)
               USING  CODIGO
               FOREGROUND-COLOR  2  BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  TELADENOMINACAO
               LINE 06  COLUMN 20  PIC X(15)
               USING  DENOMINACAO
               FOREGROUND-COLOR  6  BACKGROUND-COLOR 0
               HIGHLIGHT.
           05  TELACENTRODECUSTO
               LINE 08  COLUMN 24  PIC 9(01)
               USING  CENTRODECUSTO
               FOREGROUND-COLOR  4  BACKGROUND-COLOR 0
               HIGHLIGHT.

      *-----------------------------------------------------------------

       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADEPTO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADEPTO
                      CLOSE CADEPTO
                      MOVE "*** ARQUIVO CADEPTO SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADEPTO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      NEXT SENTENCE.

       INC-001.
                MOVE ZEROS TO CODIGO CENTRODECUSTO.
                MOVE SPACES TO DENOMINACAO.
                DISPLAY TELACADDEPTO.

       INC-002.
                ACCEPT TELACODIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADEPTO
                   GO TO ROT-FIM.
                IF CODIGO = ZEROS
                   MOVE "*** CODIGO INVALIDO ***" TO MENS
                   MOVE ZEROS TO CODIGO
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.

       LER-CADEPTO01.
                MOVE 0 TO W-SEL
                READ CADEPTO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY  TELACODIGO
                      DISPLAY  TELADENOMINACAO
                      DISPLAY  TELACENTRODECUSTO
                      MOVE "*** CODIGO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CADEPTO"   TO MENS
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
                ACCEPT TELACENTRODECUSTO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF CENTRODECUSTO = 1
                  MOVE "1 - PRESIDENCIA" TO CCE
                  DISPLAY (08, 24) CCE
                ELSE IF CENTRODECUSTO = 2
                  MOVE "2 - VICE-PRESIDENCIA" TO CCE
                   DISPLAY (08, 24) CCE
                ELSE IF CENTRODECUSTO = 3
                  MOVE "3 - DIRETORIA ADMINISTRATIVA" TO CCE
                   DISPLAY (08, 24) CCE
                ELSE IF CENTRODECUSTO = 4
                  MOVE "4 - DIRETORIA COMERCIAL" TO CCE
                   DISPLAY (08, 24) CCE
                ELSE IF CENTRODECUSTO = 5
                  MOVE "5 - DIRETORIA INDUSTRIAL" TO CCE
                   DISPLAY (08, 24) CCE
                ELSE IF CENTRODECUSTO = 6
                  MOVE "6 - DIRETORIA DESENVOLVIMENTO" TO CCE
                   DISPLAY (08, 24) CCE
                ELSE IF CENTRODECUSTO = 7
                  MOVE "7 - FILIAL A" TO CCE
                   DISPLAY (08, 24) CCE
                ELSE IF CENTRODECUSTO = 8
                  MOVE "8 - FILIAL B" TO CCE
                   DISPLAY (08, 24) CCE
                ELSE IF CENTRODECUSTO = 9
                  MOVE "9 - FILIAL C" TO CCE
                   DISPLAY (08, 24) CCE
                ELSE IF CENTRODECUSTO = 0
                  MOVE "0 - REPRESENTANTES" TO CCE
                   DISPLAY (08, 24) CCE
                ELSE
                   MOVE "*** OPCAO INVALIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE SPACES TO CENTRODECUSTO
                   GO TO INC-004.
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
                WRITE REGDEPTO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** CEP JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CADEPTO"
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
                DELETE CADEPTO RECORD
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
                REWRITE REGDEPTO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CADEPTO"   TO MENS
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