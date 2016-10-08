       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP201611.
       AUTHOR. MATEUS.
      **************************************
      * MANUTENCAO DO CADASTRO DE DEPTOS *
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
                03 CODIGO        PIC 9(03).
                03 DENOMINACAO   PIC X(15).
                03 CENTROCUSTO   PIC 9(01).
                03 FILLER        PIC X(45).
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

       SCREEN SECTION
       
       01  TELADEPTO.
           05  BLANK SCREEN.
           05  LINE 04  COLUMN 01 
               VALUE  "                         CADASTRO DE DEP".
           05  LINE 04  COLUMN 41 
               VALUE  "ARTAMENTO".
           05  LINE 07  COLUMN 01 
               VALUE  "            CODIGO:".
           05  LINE 09  COLUMN 01 
               VALUE  "            DENOMINACAO:".
           05  LINE 11  COLUMN 01 
               VALUE  "            CENTRO DE CUSTO:".
           05  TCODIGO
               LINE 07  COLUMN 20  PIC 9(03)
               USING  CODIGO
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 09  COLUMN 25  PIC X(15)
               USING  DENOMINACAO
               HIGHLIGHT.
           05  TCENTROCUSTO
               LINE 11  COLUMN 29  PIC 9(01)
               USING  CENTROCUSTO
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
                      MOVE "*** ARQUIVO CADDETO SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
               NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO CODIGO CENTROCUSTO
                MOVE SPACES TO DENOMINACAO.
                DISPLAY TELADEPTO.
       INC-002.
                ACCEPT TCODIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADEPTO
                   GO TO ROT-FIM.
                IF CODIGO  = ZEROS
                   MOVE "*** CODIGO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-CLIENTE01.
                MOVE 0 TO W-SEL
                READ CADEPTO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY  TCODIGO
                      DISPLAY  TDENOMINACAO
                      DISPLAY  TCENTROCUSTO
                      MOVE "*** CODIGO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CADDEP"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       
       INC-004.
                ACCEPT  TDENOMINACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-005.
                ACCEPT  TCENTROCUSTO
                IF CENTROCUSTO = 1
                     DISPLAY(13, 11) " - PRESIDENCIA".
                IF CENTROCUSTO = 2
                     DISPLAY(13, 11) " - VICE-PRESIDENCIA".
                IF CENTROCUSTO = 3
                     DISPLAY(13, 11) " - DIRETORIA ADMINISTRATIVA".
                IF CENTROCUSTO = 4
                     DISPLAY(13, 11) " - DIRETORIA COMERCIAL".
                IF CENTROCUSTO = 5
                     DISPLAY(13, 11) " - DIRETORIA INDUSTRIAL".
                IF CENTROCUSTO = 6
                     DISPLAY(13, 11) " - DIRETORIA DESENVOLVIMENTO".
                IF CENTROCUSTO = 7
                     DISPLAY(13, 11) " - FILIAL A".
                IF CENTROCUSTO = 8
                     DISPLAY(13, 11) " - FILIAL B".
                IF CENTROCUSTO = 9
                     DISPLAY(13, 11) " - FILIAL C".
                IF CENTROCUSTO = 0
                     DISPLAY(13, 11) " - REPRESENTANTES".
                IF W-ACT = 02 GO TO INC-004.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-SEL = 1  GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
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
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CADDEP"
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
                IF W-ACT = 02 GO TO INC-005.
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
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CADDEP"   TO MENS
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