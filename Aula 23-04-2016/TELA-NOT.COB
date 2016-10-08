
       01  TELAPRINCIPAL.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  " NOTA".
           05  LINE 07  COLUMN 01 
               VALUE  "    RM:".
           05  LINE 09  COLUMN 01 
               VALUE  "    NOME:".
           05  LINE 11  COLUMN 01 
               VALUE  "    SEXO:".
           05  LINE 13  COLUMN 01 
               VALUE  "    NASCIMENTO:".
           05  LINE 15  COLUMN 01 
               VALUE  "    NOTA1:       NOTA2:".
           05  LINE 16  COLUMN 01 
               VALUE  "    FALTA1:      FALTA2:".
           05  TRM
               LINE 07  COLUMN 08  PIC 9(05)
               USING  RM
               HIGHLIGHT.
           05  TNOME
               LINE 09  COLUMN 10  PIC X(35)
               USING  NOME
               HIGHLIGHT.
           05  TSEXO
               LINE 11  COLUMN 10  PIC X(10)
               USING  SEXO
               HIGHLIGHT.
           05  TNASC
               LINE 13  COLUMN 16  PIC 99/99/9999
               USING  NASCIMENTO
               HIGHLIGHT.
           05  TNOTA1
               LINE 15  COLUMN 11  PIC 99.9
               USING  NOTA1.
           05  TNOTA2
               LINE 15  COLUMN 24  PIC 99.9
               USING  NOTA2
               HIGHLIGHT.
           05  TFALTA1
               LINE 16  COLUMN 12  PIC 9(02)
               USING  FALTA1
               HIGHLIGHT.
           05  TFALTA2
               LINE 16  COLUMN 25  PIC 9(02)
               USING  FALTA2
               HIGHLIGHT.
