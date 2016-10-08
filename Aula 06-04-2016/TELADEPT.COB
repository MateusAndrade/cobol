
       01  TELADEPTO.
           05  BLANK SCREEN.
           05  LINE 04  COLUMN 01 
               VALUE  "                         CADASTRO DE DEP".
           05  LINE 04  COLUMN 41 
               VALUE  "ARTAMENTO".
           05  LINE 07  COLUMN 01 
               VALUE  "            CODIGO:".
           05  LINE 09  COLUMN 01 
               VALUE  "            DENOMINACAO:".
           05  LINE 11  COLUMN 01 
               VALUE  "            CENTRO DE CUSTO:".
           05  TCODIGO
               LINE 07  COLUMN 20  PIC 9(03)
               USING  CODIGO
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 09  COLUMN 25  PIC X(15)
               USING  DENOMINACAO
               HIGHLIGHT.
           05  TCENTROCUSTO
               LINE 11  COLUMN 29  PIC 9(01)
               USING  CENTROCUSTO
               HIGHLIGHT.
