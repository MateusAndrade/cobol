
       01  TELASHOW.
           05  LINE 02  COLUMN 01 
               VALUE  "    컴컴컴컴컴컴컴컴컴 CADASTRO SHOW FAT".
           05  LINE 02  COLUMN 41 
               VALUE  "EC ZL컴컴컴컴컴컴컴컴컴컴컴컴컴�".
           05  LINE 04  COLUMN 01 
               VALUE  "              RM:".
           05  LINE 06  COLUMN 01 
               VALUE  "              NOME:".
           05  LINE 08  COLUMN 01 
               VALUE  "              SEXO:".
           05  LINE 10  COLUMN 01 
               VALUE  "              DATA DE NASCIMENTO:".
           05  LINE 12  COLUMN 01 
               VALUE  "              NOTAS         FALTAS".
           05  LINE 14  COLUMN 01 
               VALUE  "              NOTA1:        FALTAS1:".
           05  LINE 16  COLUMN 01 
               VALUE  "              NOTA2:        FALTAS2:".
           05  LINE 19  COLUMN 01 
               VALUE  "              SITUACAO FINAL:".
           05  TRM
               LINE 04  COLUMN 18  PIC 9(05
               USING  RM
               HIGHLIGHT.
           05  TNOME
               LINE 06  COLUMN 20  PIC X(35)
               USING  NOME
               HIGHLIGHT.
           05  TSEXO
               LINE 08  COLUMN 20  PIC X(10)
               USING  SEXO
               HIGHLIGHT.
           05  TNASCIMENTO
               LINE 10  COLUMN 34  PIC 99/99/9999
               USING  NASCIMENTO
               HIGHLIGHT.
           05  TNOTA1
               LINE 14  COLUMN 21  PIC 99,99
               USING  NOTA1
               HIGHLIGHT.
           05  TFALTA1
               LINE 14  COLUMN 37  PIC 9(02)
               USING  FALTA1
               HIGHLIGHT.
           05  TNOTA2
               LINE 16  COLUMN 21  PIC 99,99
               USING  NOTA2
               HIGHLIGHT.
           05  TFALTA2
               LINE 16  COLUMN 37  PIC 9(02)
               USING  FALTA2
               HIGHLIGHT.
