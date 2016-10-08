       IDENTIFICATION DIVISION.
       PROGRAM-ID. P201611.
       AUTHOR. MATEUS.	
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
                   05 CEP1    PIC 9(05).
                   05 CEP2    PIC 9(03).
                03 LOGRAD    PIC X(35).
                03 BAIRRO        PIC X(20).
                03 CIDADE        PIC X(20).
		03 UF          	 PIC X(02).
                03 FILLER        PIC X(45).
       
		
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
           03 MASC2A   PIC 9(05).
           03 MASC2Y   PIC X(01) VALUE "-".
           03 MASC2B   PIC 9(03).
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
                      MOVE "*** ARQUIVO CADCEP CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADCEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      NEXT SENTENCE.

       INC-001.
                MOVE ZEROS  TO CEP.
                MOVE SPACES TO  LOGRAD BAIRRO CIDADE UF.
                DISPLAY  (01, 01) ERASE
                DISPLAY  (01, 20) "CADASTRO DE ENDERECOS"
                DISPLAY  (04, 01) "CEP: "
                DISPLAY  (05, 01) "LOGRADOURO: "
                DISPLAY  (06, 01) "BAIRRO: "
                DISPLAY  (07, 01) "CIDADE: " 
		DISPLAY  (08, 01) "UF: ".

       INC-002.
                ACCEPT  (04, 12) CEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADCEP
                   GO TO ROT-FIM.
                IF CEP  = ZEROS
                   MOVE "*** CEP INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
                MOVE CEP TO CEPX
                MOVE CEPX TO MASC1
                DISPLAY(04, 12) MASC1.

                MOVE CEP1 TO MASC2A
                MOVE CEP2 TO MASC2B
                DISPLAY (04, 30)MASC2.
 

       LER-CEP01.
                MOVE 0 TO W-SEL
                READ CADCEP
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
		      DISPLAY  (04, 12) CEP
                      DISPLAY  (05, 12)	LOGRAD
                      DISPLAY  (06, 12)	BAIRRO
                      DISPLAY  (07, 12) CIDADE
		      DISPLAY  (08, 12)	UF

                      MOVE "*** CEP JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CEP"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.

       INC-003.
                ACCEPT (05, 12) LOGRAD
                ACCEPT W-ACT FROM ESCAPE KEY
		IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT (06, 12) BAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT (07, 12) CIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT (08, 12) UF
                ACCEPT W-ACT FROM ESCAPE KEY
		IF W-ACT = 02 GO TO INC-005.
		IF UF NOT = "SP" AND "RJ" AND "MG"
                      MOVE "*** UF INCORRETA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE SPACES TO UF
		      DISPLAY (08, 12) UF
                      GO TO INC-006.
      
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