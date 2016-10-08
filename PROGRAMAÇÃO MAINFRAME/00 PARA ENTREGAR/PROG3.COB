       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP001.
      **************************************
      * MANUTENCAO DO CADASTRO DE CLIENTES *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT AMIGOS ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS APELIDO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE3 = APELIDO
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD AMIGOS
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "AMIGOS.DAT".
       01 REGCLI.
                03 CHAVE1.
                   05 APELIDO    PIC X(12).
                03 NOME          PIC X(30).
                03 BAIRRO        PIC X(30).
                03 CIDADE        PIC X(20).
		03 UF          	 PIC X(2).
		03 TELEFONE      PIC 9(12).
0		03 EMAIL         PIC X(30).
                03 TIPO-AMIGO    PIC X(25).
                03 ENDERECO      PIC X(25).
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
       
       
       
       01  TELA1.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 01  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
           05  LINE 02  COLUMN 01 
               VALUE  "³                         ³   CADASTRO D".
           05  LINE 02  COLUMN 41 
               VALUE  "E AMIGOS  ³                            ³".
           05  LINE 03  COLUMN 01 
               VALUE  "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 03  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
           05  LINE 04  COLUMN 01 
               VALUE  "³".
           05  LINE 04  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 05  COLUMN 01 
               VALUE  "³".
           05  LINE 05  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 06  COLUMN 01 
               VALUE  "³  APELIDO:".
           05  LINE 06  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 07  COLUMN 01 
               VALUE  "³".
           05  LINE 07  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 08  COLUMN 01 
               VALUE  "³  NOME:".
           05  LINE 08  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 09  COLUMN 01 
               VALUE  "³".
           05  LINE 09  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 10  COLUMN 01 
               VALUE  "³  BAIRRO:".
           05  LINE 10  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 11  COLUMN 01 
               VALUE  "³".
           05  LINE 11  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 12  COLUMN 01 
               VALUE  "³  CIDADE:                        UF:".
           05  LINE 12  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 13  COLUMN 01 
               VALUE  "³".
           05  LINE 13  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 14  COLUMN 01 
               VALUE  "³  TELEFONE:".
           05  LINE 14  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 15  COLUMN 01 
               VALUE  "³".
           05  LINE 15  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 16  COLUMN 01 
               VALUE  "³  EMAIL:".
           05  LINE 16  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 17  COLUMN 01 
               VALUE  "³".
           05  LINE 17  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 18  COLUMN 01 
               VALUE  "³  TIPO DE AMIGO:".
           05  LINE 18  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 19  COLUMN 01 
               VALUE  "³".
           05  LINE 19  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 20  COLUMN 01 
               VALUE  "³  ENDERECO:".
           05  LINE 20  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 21  COLUMN 01 
               VALUE  "³".
           05  LINE 21  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 22  COLUMN 01 
               VALUE  "³".
           05  LINE 22  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 23  COLUMN 01 
               VALUE  "³".
           05  LINE 23  COLUMN 41 
               VALUE  "                                       ³".
           05  LINE 24  COLUMN 01 
               VALUE  "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 24  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
           05  TAPELIDO
               LINE 06  COLUMN 12  PIC X(12)
               USING  APELIDO
               HIGHLIGHT.
           05  TNOME
               LINE 08  COLUMN 09  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TBAIRRO
               LINE 10  COLUMN 11  PIC X(30)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 12  COLUMN 11  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 12  COLUMN 38  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TTELEFONE
               LINE 14  COLUMN 13  PIC 99.99999.9999
               USING  TELEFONE
               HIGHLIGHT.
           05  TEMAIL
               LINE 16  COLUMN 10  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TTIPO-AMIGO
               LINE 18  COLUMN 18  PIC X(25)
               USING  TIPO-AMIGO
               HIGHLIGHT.
           05  TENDERECO
               LINE 20  COLUMN 13  PIC X(25)
               USING  ENDERECO
               HIGHLIGHT.



      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O AMIGOS
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT AMIGOS
                      CLOSE AMIGOS
                      MOVE "*** ARQUIVO AMIGOS SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO AMIGOS" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO TELEFONE
                MOVE SPACES TO  APELIDO NOME BAIRRO CIDADE UF TIPO-AMIGO
				EMAIL ENDERECO.
                DISPLAY TELA1.
       INC-002.
                ACCEPT  TAPELIDO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE AMIGOS
                   GO TO ROT-FIM.
                IF APELIDO  = SPACE
                   MOVE "*** APELIDO  INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       LER-CLIENTE01.
                MOVE 0 TO W-SEL
                READ AMIGOS
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
		      DISPLAY  TAPELIDO
                      DISPLAY  TNOME
                      DISPLAY  TBAIRRO
                      DISPLAY  TCIDADE
		      DISPLAY  TUF
                      DISPLAY  TTELEFONE
		      DISPLAY  TEMAIL
                      DISPLAY  TTIPO-AMIGO
                      DISPLAY  TENDERECO
                      MOVE "*** AMIGO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO AMIGO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT TNOME
                ACCEPT W-ACT FROM ESCAPE KEY
		IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT TBAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT TCIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT TUF
                ACCEPT W-ACT FROM ESCAPE KEY
		IF W-ACT = 02 GO TO INC-005.
		IF UF NOT = "SP" AND "RJ" AND "MG"
                      MOVE "*** UF INCORRETA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE SPACES TO UF
		      DISPLAY TUF
                      GO TO INC-006.
       INC-007.
                ACCEPT TTELEFONE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT TEMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
       INC-009.
                ACCEPT TTIPO-AMIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
       INC-010.
                ACCEPT TENDERECO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.

      *
                IF W-SEL = 1
                              GO TO ALT-OPC.
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
                WRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** AMIGO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE AMIGOS"
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
                DELETE AMIGOS RECORD
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
                REWRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGOS"   TO MENS
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