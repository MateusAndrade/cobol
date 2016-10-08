       IDENTIFICATION DIVISION.
       PROGRAM-ID. P201612.
       AUTHOR. LEANDRO.
      **************************************
      * MANUTENCAO DO CADASTRO DE ENDERECOS *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS LOGRAD
                               WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
                03 CEP.
                   05 CEP1       PIC 9(05).
                   05 CEP2       PIC 9(03).
                03 LOGRAD        PIC X(35).
                03 BAIRRO        PIC X(20).
                03 CIDADE        PIC X(20).
		03 UF          	 PIC X(02).
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
       01 CEPX         PIC 9(08) VALUE ZEROS.
       01 MASC1        PIC 99999.999.
       01 MASC2.
          03 MASC2A    PIC 9(05).
          03 MASC2B    PIC X(01) VALUE "-".
          03 MASC2C    PIC 9(03).
       
       SCREEN SECTION.
       
       
       01  TELA1.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                              CADASTRO D".
           05  LINE 02  COLUMN 41 
               VALUE  "E CEP".
           05  LINE 03  COLUMN 01 
               VALUE  "ษอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 03  COLUMN 41 
               VALUE  "ออออออออออออออออออออออออออออออออออออออป".
           05  LINE 04  COLUMN 01 
               VALUE  "บ".
           05  LINE 04  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 05  COLUMN 01 
               VALUE  "บ  CEP:".
           05  LINE 05  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 06  COLUMN 01 
               VALUE  "บ".
           05  LINE 06  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 07  COLUMN 01 
               VALUE  "บ  LOGRADOURO:".
           05  LINE 07  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 08  COLUMN 01 
               VALUE  "บ".
           05  LINE 08  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 09  COLUMN 01 
               VALUE  "บ  BAIRRO:".
           05  LINE 09  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 10  COLUMN 01 
               VALUE  "บ".
           05  LINE 10  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 11  COLUMN 01 
               VALUE  "บ  CIDADE:".
           05  LINE 11  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 12  COLUMN 01 
               VALUE  "บ".
           05  LINE 12  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 13  COLUMN 01 
               VALUE  "บ  UF:".
           05  LINE 13  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 14  COLUMN 01 
               VALUE  "บ".
           05  LINE 14  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 15  COLUMN 01 
               VALUE  "บ".
           05  LINE 15  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 16  COLUMN 01 
               VALUE  "บ".
           05  LINE 16  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 17  COLUMN 01 
               VALUE  "บ".
           05  LINE 17  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 18  COLUMN 01 
               VALUE  "บ".
           05  LINE 18  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 19  COLUMN 01 
               VALUE  "บ".
           05  LINE 19  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 20  COLUMN 01 
               VALUE  "บ".
           05  LINE 20  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 21  COLUMN 01 
               VALUE  "บ".
           05  LINE 21  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 22  COLUMN 01 
               VALUE  "บ".
           05  LINE 22  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 23  COLUMN 01 
               VALUE  "บ".
           05  LINE 23  COLUMN 41 
               VALUE  "                                      บ".
           05  LINE 24  COLUMN 01 
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 24  COLUMN 41 
               VALUE  "ออออออออออออออออออออออออออออออออออออออผ".
           05  TCEP
               LINE 05  COLUMN 08  PIC 99999.999
               USING  CEP
               HIGHLIGHT.
           05  TLOGRAD
               LINE 07  COLUMN 15  PIC X(35)
               USING  LOGRAD
               HIGHLIGHT.
           05  TBAIRRO
               LINE 09  COLUMN 11  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 11  COLUMN 11  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 13  COLUMN 07  PIC X(02)
               USING  UF
               HIGHLIGHT.
 

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADCEP
                      CLOSE CADCEP
                      MOVE "*** ARQUIVO CADCEP SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADCEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
               NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO CEP
                MOVE SPACES TO  LOGRAD BAIRRO CIDADE UF.
                DISPLAY TELA1.
                
       INC-002.
                ACCEPT TCEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADCEP
                   GO TO ROT-FIM.
                IF CEP  = ZEROS
                   MOVE "*** CEP INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.

       LER-CLIENTE01.
                MOVE 0 TO W-SEL
                READ CADCEP
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
		      DISPLAY  TCEP
                      DISPLAY  TLOGRAD
                      DISPLAY  TBAIRRO
                      DISPLAY  TCIDADE
		      DISPLAY  TUF
                      MOVE "*** CEP JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CADCEP"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       
       INC-004.
                ACCEPT TLOGRAD
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-005.
                ACCEPT TBAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT TCIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT TUF
                ACCEPT W-ACT FROM ESCAPE KEY
		IF W-ACT = 02 GO TO INC-005.
		IF UF NOT = "SP" AND "RJ" AND "MG"
                      MOVE "*** UF INCORRETA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE SPACES TO UF
       		      DISPLAY TUF
                      GO TO INC-007.

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
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** CEP JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CADCEP"
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
                DELETE CADCEP RECORD
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
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CADCEP"   TO MENS
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