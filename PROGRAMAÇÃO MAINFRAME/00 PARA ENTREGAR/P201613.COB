       IDENTIFICATION DIVISION.
       PROGRAM-ID. P201613.
       AUTHOR. LEANDRO.
      **************************************
      * MANUTENCAO DO CADASTRO DE NOTAS    *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADNOTA ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS RM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                               WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADNOTA
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADNOTA.DAT".
       01 REGNOTA.
                03 RM            PIC 9(05).
                03 NOME          PIC X(35).
                03 SEXO          PIC X(10).
                03 NASCIMENTO.   
		   05 DIA        PIC 9(2).
                   05 MES        PIC 9(2).
                   05 ANO        PIC 9(4).
                03 NOTAS.
                   05 NOTA1      PIC 9(2)V9.
                   05 NOTA2      PIC 9(2)V9.
		03 FALTAS.
                   05 FALTA1     PIC 9(2).
                   05 FALTA2     PIC 9(2).
      		03 FILLER        PIC X(40).
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
       01 MEDIA        PIC 9(2)V9.
       01 MEDIAMASC    PIC Z9,9.
       01 SOMAFALTAS   PIC 9(02).
       01 SITUACAO     PIC X(16).
       01 MASC1        PIC 99999.999.
       01 MASCNASC.
          03 MASCNASC1    PIC 9(02).
          03 MASCNASC2    PIC X(01) VALUE "/".
          03 MASCNASC3    PIC 9(02).
          03 MASCNASC4    PIC X(01) VALUE "/".
          03 MASCNASC5    PIC 9(04).

       SCREEN SECTION

        
       01  TELA1.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  " ษออออออออออออออออออออออออออออออออออออออ".
           05  LINE 01  COLUMN 41 
               VALUE  "อออออออออออออออออออออออออออออออออออออออป".
           05  LINE 02  COLUMN 01 
               VALUE  " บ                       CADASTRO DE NOT".
           05  LINE 02  COLUMN 41 
               VALUE  "AS FATEC                               บ".
           05  LINE 03  COLUMN 01 
               VALUE  " บ".
           05  LINE 03  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 04  COLUMN 01 
               VALUE  " บ".
           05  LINE 04  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 05  COLUMN 01 
               VALUE  " บ RM:".
           05  LINE 05  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 06  COLUMN 01 
               VALUE  " บ".
           05  LINE 06  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 07  COLUMN 01 
               VALUE  " บ NOME:".
           05  LINE 07  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 08  COLUMN 01 
               VALUE  " บ".
           05  LINE 08  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 09  COLUMN 01 
               VALUE  " บ SEXO:".
           05  LINE 09  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 10  COLUMN 01 
               VALUE  " บ".
           05  LINE 10  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 11  COLUMN 01 
               VALUE  " บ DATA DE NASCIMENTO:".
           05  LINE 11  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 12  COLUMN 01 
               VALUE  " บ".
           05  LINE 12  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 13  COLUMN 01 
               VALUE  " บ NOTA1:      NOTA2:".
           05  LINE 13  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 14  COLUMN 01 
               VALUE  " บ".
           05  LINE 14  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 15  COLUMN 01 
               VALUE  " บ FALTA1:     FALTA2:".
           05  LINE 15  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 16  COLUMN 01 
               VALUE  " บ".
           05  LINE 16  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 17  COLUMN 01 
               VALUE  " บ".
           05  LINE 17  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 18  COLUMN 01 
               VALUE  " บ".
           05  LINE 18  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 19  COLUMN 01 
               VALUE  " บ".
           05  LINE 19  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 20  COLUMN 01 
               VALUE  " บ".
           05  LINE 20  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 21  COLUMN 01 
               VALUE  " บ".
           05  LINE 21  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 22  COLUMN 01 
               VALUE  " บ".
           05  LINE 22  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 23  COLUMN 01 
               VALUE  " บ MENSAGEM:".
           05  LINE 23  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 24  COLUMN 01 
               VALUE  " ศออออออออออออออออออออออออออออออออออออออ".
           05  LINE 24  COLUMN 41 
               VALUE  "อออออออออออออออออออออออออออออออออออออออผ".
           05  PRM
               LINE 05  COLUMN 10  PIC 9(05)
               USING  RM
               HIGHLIGHT.
           05  TNOME
               LINE 07  COLUMN 10  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TSEXO
               LINE 09  COLUMN 10  PIC X(10)
               USING  SEXO
               HIGHLIGHT.
           05  TNASCIMENTO
               LINE 11  COLUMN 24  PIC 99/99/9999
               USING  NASCIMENTO
               HIGHLIGHT.
           05  TNOTA1
               LINE 13  COLUMN 10  PIC 99,9
               USING  NOTA1
               HIGHLIGHT.
           05  TNOTA2
               LINE 13  COLUMN 22  PIC 99,9
               USING  NOTA2
               HIGHLIGHT.
           05  TFALTA1
               LINE 15  COLUMN 11  PIC 9(02)
               USING  FALTA1
               HIGHLIGHT.
           05  TFALTA2
               LINE 15  COLUMN 23  PIC 9(02)
               USING  FALTA2
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

       INICIO.
      *
       INC-OP0.
           OPEN I-O CADNOTA
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADNOTA
                      CLOSE CADNOTA
                      MOVE "*** ARQUIVO CADNOTA SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADNOTA" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
               NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO RM NOTAS NASCIMENTO FALTAS MEDIA
                MOVE SPACES TO  NOME SEXO SITUACAO.
                DISPLAY TELA1.  
                
       INC-002.
                ACCEPT  PRM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADNOTA
                   GO TO ROT-FIM.
                IF RM  = ZEROS
                   MOVE "*** RM INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
                
       LER-CLIENTE01.
                MOVE 0 TO W-SEL
                READ CADNOTA
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY  PRM
                      DISPLAY   TNOME
                      DISPLAY   TSEXO
                      DISPLAY   TNASCIMENTO
                      DISPLAY   TNOTA1
                      DISPLAY   TNOTA2
                      DISPLAY   TFALTA1
                      DISPLAY   TFALTA2

                      MOVE "*** RM JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CADNOTA" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       
       INC-003.
                ACCEPT TNOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT TSEXO
                IF SEXO NOT = "M" AND "F"
                   MOVE "***OPCAO INCORRETA***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE SPACES TO SEXO
                   DISPLAY (06, 12) SEXO
                   GO TO INC-004.               
                IF SEXO = "M"
                   MOVE "MASCULINO" TO SEXO
                   DISPLAY TSEXO.
                IF SEXO = "F"
                   MOVE "FEMININO" TO SEXO.
                   DISPLAY TSEXO.
                ACCEPT W-ACT FROM ESCAPE KEY              
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT TNASCIMENTO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.

    
       INC-006.
                ACCEPT TNOTA1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF NOTA1 > 10,0
                   MOVE "***NOTA INVALIDA***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE ZEROS TO NOTA1
                   DISPLAY TNOTA1
                   GO TO INC-006.
		IF W-ACT = 02 GO TO INC-005.

       INC-007.
                ACCEPT TNOTA2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF NOTA2 > 10,0
                   MOVE "***NOTA INVALIDA***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE ZEROS TO NOTA2
                   DISPLAY TNOTA2
                   GO TO INC-007.
                COMPUTE MEDIA = (NOTA1 + NOTA2) / 2.
                DISPLAY (09, 50) "MEDIA:"
                MOVE MEDIA TO MEDIAMASC
                DISPLAY (09, 57) MEDIAMASC

                IF W-ACT = 02 GO TO INC-006.
                

       INC-008.
                ACCEPT TFALTA1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF FALTA1 > 20
                MOVE "***Nro DE FALTAS INVALIDA***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE ZEROS TO FALTA1
                   DISPLAY TFALTA1
                   GO TO INC-008.
		IF W-ACT = 02 GO TO INC-007.

       INC-009.
                ACCEPT TFALTA2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF FALTA2 > 20
                   MOVE "***Nro DE FALTAS INVALIDA***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE ZEROS TO FALTA2
                   DISPLAY TFALTA2
                   GO TO INC-009.
                COMPUTE SOMAFALTAS = FALTA1 + FALTA2.
                DISPLAY (10, 50) "TOTAL DE FALTAS:"
                DISPLAY (10, 67) SOMAFALTAS
		IF W-ACT = 02 GO TO INC-008.

                                              
		

       
       INC-010.
               DISPLAY (11, 50) "SITUACAO:"
               IF MEDIA < 5
                  IF SOMAFALTAS > 19
                       MOVE "REPROVADO" TO SITUACAO
                       DISPLAY (11, 60) SITUACAO
                  ELSE
                        MOVE "RECUPERACAO" TO SITUACAO
                        DISPLAY (11, 60) SITUACAO
               ELSE
                  IF SOMAFALTAS > 19
                       MOVE "REPOSICAO" TO SITUACAO
                     DISPLAY (11, 60) SITUACAO
                  ELSE
                        MOVE "APROVADO" TO SITUACAO
                        DISPLAY (11, 60) SITUACAO.


               

             

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
                WRITE REGNOTA
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** RM JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CADNOTA"
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
                DELETE CADNOTA RECORD
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
                REWRITE REGNOTA
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CADNOTA"   TO MENS
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