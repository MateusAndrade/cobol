
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
               LINE 11  COLUMN 23  PIC 9(15)
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
