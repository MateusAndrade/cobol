
       01  TELA1.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                                CADASTRO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE CEP".
           05  LINE 05  COLUMN 01 
               VALUE  "    CEP:".
           05  LINE 07  COLUMN 01 
               VALUE  "    LOGRADOURO:".
           05  LINE 09  COLUMN 01 
               VALUE  "    BAIRRO:".
           05  LINE 11  COLUMN 01 
               VALUE  "    CIDADE:".
           05  LINE 13  COLUMN 01 
               VALUE  "    UF:".
           05  TCEP
               LINE 05  COLUMN 09  PIC 99999.999
               USING  CEP
               HIGHLIGHT.
           05  TLOGRAD
               LINE 07  COLUMN 16  PIC X(35)
               USING  LOGRAD
               HIGHLIGHT.
           05  TBAIRRO
               LINE 09  COLUMN 12  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 11  COLUMN 12  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 13  COLUMN 08  PIC X(02)
               USING  UF
               HIGHLIGHT.
