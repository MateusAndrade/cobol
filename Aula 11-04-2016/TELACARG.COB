
       01  TELACARGO.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "                            CADASTRO DE".
           05  LINE 03  COLUMN 41 
               VALUE  "CARGOS".
           05  LINE 06  COLUMN 01 
               VALUE  "           CODIGO:".
           05  LINE 08  COLUMN 01 
               VALUE  "           DENOMINACAO:".
           05  LINE 10  COLUMN 01 
               VALUE  "           TIPO SALARIO:".
           05  LINE 12  COLUMN 01 
               VALUE  "           NIVEL ESCOLARIDADE:".
           05  LINE 14  COLUMN 01 
               VALUE  "           SALARIO BASE:".
           05  TCODIGO
               LINE 06  COLUMN 19  PIC 9(03)
               USING  CODIGO
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 08  COLUMN 24  PIC X(20)
               USING  DENOMINACAO.
           05  TTIPOSALARIO
               LINE 10  COLUMN 25  PIC X(01)
               USING  TIPOSALARIO
               HIGHLIGHT.
           05  TNIVELESCOLAR
               LINE 12  COLUMN 31  PIC 9(01)
               USING  NIVELESCOLAR
               HIGHLIGHT.
           05  TSALARIOBASE
               LINE 14  COLUMN 25  PIC 999.999,99
               USING  SALARIOBASE
               HIGHLIGHT.
