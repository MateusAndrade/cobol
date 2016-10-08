       IDENTIFICATION DIVISION.
       PROGRAM-ID. P201612.
       AUTHOR. MATEUS.	
      **************************************
      * MANUTENCAO DO CADASTRO DE NOTAS DE ALUNOS *
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
                03 RM             PIC 9(05).
                03 NOME           PIC X(35).
                03 SEXO           PIC X(01).
                03 DATANASCIMENTO.
                   05 DIA     PIC 9(02).
                   05 MES     PIC 9(02).
                   05 ANO     PIC 9(02).
                03 NOTAS.
                   05 NOTA1     PIC 9(02)V99.
                   05 NOTA2     PIC 9(02)V99.
                03 FALTAS.
                   05 FALTAS1     PIC 9(02).
                   05 FALTAS2     PIC 9(02).
                03 FILLER         PIC X(40).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL            PIC 9(01) VALUE ZEROS.
       77 W-CONT           PIC 9(06) VALUE ZEROS.
       77 W-OPCAO          PIC X(01) VALUE SPACES.
       77 ST-ERRO          PIC X(02) VALUE "00".
       77 W-ACT            PIC 9(02) VALUE ZEROS.
       77 MENS             PIC X(50) VALUE SPACES.
       77 LIMPA            PIC X(50) VALUE SPACES.
       01 DATAMASC.
           03 DIAMASC      PIC 9(02).
           03 BARRADDMM    PIC X(01) VALUE "/".
           03 MESMASC      PIC 9(02).
           03 BARRAMMAA    PIC X(01) VALUE "/".
           03 ANOMASC      PIC 9(02).
       01 MEDIA            PIC Z9,99.
       01 VALIDACAO499     PIC Z9,99.
       01 TOTALFALTAS      PIC 9(02).
       01 SEXOINTEIRO      PIC X(09).
       01 SITUACAOFINAL    PIC X(20).
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
                      MOVE "*** ARQUIVO CADNOTA CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADNOTA" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO RM.
                MOVE SPACES TO NOME SEXO.
                DISPLAY  (01, 01) ERASE
                DISPLAY  (01, 20) "CADASTRO DE NOTAS"
                DISPLAY  (04, 01) "RM: "
                DISPLAY  (05, 01) "NOME: "
                DISPLAY  (06, 01) "SEXO: "
                DISPLAY  (07, 01) "DATA DE NASCIMENTO: " 
                DISPLAY  (08, 01) "NOTA1: "
                DISPLAY  (09, 01) "NOTA2: "
                DISPLAY  (09, 20) "MEDIA: "
                DISPLAY  (10, 01) "FALTAS1: "
                DISPLAY  (11, 01) "FALTAS2: "
                DISPLAY  (11, 20) "TOTAL FALTAS: "
                DISPLAY  (13, 30) "SITUACAOFINAL: ".
       INC-002.
                ACCEPT  (04, 05) RM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADNOTA
                   GO TO ROT-FIM.
                IF RM = ZEROS
                   MOVE "*** RM INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE ZEROS TO RM
                   GO TO INC-002.
       LER-NOTAS.
                MOVE 0 TO W-SEL
                READ CADNOTA
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY  (04, 05) RM
                      DISPLAY  (05, 07) NOME
                      DISPLAY  (06, 07) SEXO
                      DISPLAY  (07, 21) DATANASCIMENTO
                      DISPLAY  (08, 08) NOTA1
                      DISPLAY  (09, 08) NOTA2
                      DISPLAY  (09, 28) MEDIA
                      DISPLAY  (10, 15) FALTAS1
                      DISPLAY  (11, 15) FALTAS2
                      DISPLAY  (11, 35) TOTALFALTAS
                      DISPLAY  (13, 51) SITUACAOFINAL
                      MOVE "*** RM JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO NOTAS"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT (05, 07) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF NOME = SPACES
                   MOVE "*** NOME INVALIDO ***" TO MENS
                   MOVE SPACES TO NOME
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
       INC-004.
                ACCEPT (06, 07) SEXO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF SEXO = "M"
                   MOVE "MASCULINO" TO SEXOINTEIRO
                   DISPLAY (06, 07) SEXOINTEIRO
                ELSE IF SEXO = "F"
                   MOVE "FEMININO" TO SEXOINTEIRO
                   DISPLAY (06, 07) SEXOINTEIRO
                ELSE
                   MOVE "*** SEXO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   MOVE SPACES TO SEXO
                   DISPLAY (06, 07) SEXO
                   GO TO INC-004.
       INC-005.
                ACCEPT (07, 21) DATANASCIMENTO
                MOVE DIA TO DIAMASC
                MOVE MES TO MESMASC
                MOVE ANO TO ANOMASC
                DISPLAY (07, 21) DATAMASC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT (08, 08) NOTA1
                IF NOTA1 > 10
                   MOVE "*** NOTA INVALIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-006.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT (09, 08) NOTA2
                IF NOTA2 > 10
                   MOVE "*** NOTA INVALIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-007.
                COMPUTE MEDIA = (NOTA1 + NOTA2) / 2.
                DISPLAY  (09, 28) MEDIA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT (10, 15) FALTAS1
                IF FALTAS1 > 40
                   MOVE "*** FALTAS INVALIDAS ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-008.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
       INC-009.
                ACCEPT (11, 15) FALTAS2
                IF FALTAS2 > 40
                   MOVE "*** FALTAS INVALIDAS ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-009.
                COMPUTE TOTALFALTAS = FALTAS1 + FALTAS2.
                DISPLAY  (11, 35) TOTALFALTAS
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
       INC-010.
               COMPUTE VALIDACAO499 = (4,99 + 4,99) / 2.
               IF MEDIA > VALIDACAO499 
                  IF TOTALFALTAS < 20
                    MOVE "APROVADO" TO SITUACAOFINAL
                    DISPLAY (13, 51) SITUACAOFINAL
                  ELSE
                    MOVE "REPOSICAO AULAS" TO SITUACAOFINAL
                    DISPLAY (13, 51) SITUACAOFINAL
               ELSE
                  IF TOTALFALTAS < 20
                    MOVE "RECUP. NOTAS" TO SITUACAOFINAL
                    DISPLAY (13, 51) SITUACAOFINAL
                  ELSE
                    MOVE "REPROVADO" TO SITUACAOFINAL
                    DISPLAY (13, 51) SITUACAOFINAL.
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
                      MOVE "*** CEP JA EXISTE ***       " TO MENS
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