       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP201611.
	   AUTHOR. LEONARDO.
      **************************************
      * MANUTENCAO DO CADASTRO DE DEPARTAMENTO *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DEPART ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD DEPART
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADEPTO.DAT".
       01 CADEPTO.
                03 CODIGO            PIC 9(03).
                03 DENOMI            PIC X(15).
                03 CDC               PIC 9(01).
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
       01 INDX         PIC 9(03) VALUE ZEROS.
       01 TABCCA. 
           03 TABCCA1      PIC X(25) VALUE "PRESIDENCIA              ".
           03 TABCCA2      PIC X(25) VALUE "VICE PRESIDENCIA         ".
           03 TABCCA3      PIC X(25) VALUE "DIRETORIA ADEMINISTRATIVA".
           03 TABCCA4      PIC X(25) VALUE "DIRETORIA COMERCIAL      ".
           03 TABCCA5      PIC X(25) VALUE "DIRETORIA INDUSTRIAOL    ".
           03 TABCCA6      PIC X(25) VALUE "DIRETORIA DESENVOLVIMENTO".
           03 TABCCA7      PIC X(25) VALUE "FILIAL A                 ".
           03 TABCCA8      PIC X(25) VALUE "FILIAL B                 ".
           03 TABCCA9      PIC X(25) VALUE "FILIAL C                 ".
           03 TABCCA10     PIC X(25) VALUE "REPRESENTANTES           ".
       01 TABCC     REDEFINES TABCCA.
           03 TABCC1       PIC X(25) OCCURS 10 TIMES.
       SCREEN SECTION.
       01  TELAFP.
           05  LINE 02  COLUMN 01 
               VALUE  "                           CADASTRO DE D".
           05  LINE 02  COLUMN 41 
               VALUE  "EPARTAMENTO".
           05  LINE 04  COLUMN 01 
               VALUE  "  CODIGO:".
           05  LINE 06  COLUMN 01 
               VALUE  "  DENOMINACAO:".
           05  LINE 08  COLUMN 01 
               VALUE  "  CENTRO DE CUSTO:".
           05  TCODIGO
               LINE 04  COLUMN 11  PIC 9(03)
               USING  CODIGO.
           05  TDENOMI
               LINE 06  COLUMN 16  PIC X(15)
               USING  DENOMI.
           05  TCDC
               LINE 08  COLUMN 20  PIC 9(01)
               USING  CDC.
           05  TCDCT
               LINE 08  COLUMN 22  PIC X(25)
               USING  CDCT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O DEPART
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT DEPART
                      CLOSE DEPART
                      MOVE "*** ARQUIVO DEPART SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO DEPART" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS TO CODIGO CDC
                MOVE SPACES TO DENOMI CDCT.
                DISPLAY TELAFP.
       INC-002.
                ACCEPT TCODIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE DEPART
                   GO TO ROT-FIM.
                IF CODIGO  = 0
                   MOVE "*** DEPART  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       LER-DEPART01.
                MOVE 0 TO W-SEL
                READ DEPART
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM INC-004A
                      DISPLAY TELAFP
                      MOVE "*** DEPARTAMENTO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO DEPART"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT TDENOMI
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT TCDC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF W-ACT = 11 ADD 1 TO CDC
                       MOVE TABCC1(CDC) TO CDCT
                       DISPLAY TELAFP
                       GO TO INC-004.
                IF W-ACT = 10 COMPUTE CDC = CDC - 1
                       MOVE TABCC1(CDC) TO CDCT
                       DISPLAY TELAFP
                       GO TO INC-004.
       INC-004A.
                IF CDC = 0 MOVE 10 TO INDX
                ELSE
                  MOVE CDC TO INDX.
                MOVE TABCC1(INDX) TO CDCT

                DISPLAY TELAFP.
      *
                IF W-SEL = 1
                              GO TO ALT-OPC.
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
                      MOVE "*** DEPART JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE DEPART"
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
                DELETE DEPART RECORD
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
                MOVE "ERRO NA EXCLUSAO DO REGISTRO DEPART"   TO MENS
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