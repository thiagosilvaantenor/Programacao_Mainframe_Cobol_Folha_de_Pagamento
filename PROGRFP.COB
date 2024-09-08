		IDENTIFICATION DIVISION.
		PROGRAM-ID. PROGRFP.
      *AUTOR. THIAGO SILVA ANTENOR.
      ****************************************************
      * MANUTENCAO DO CADASTRO DE FUNCIONARIOS   *
      ********************************************************
*-----------------------------------------
		ENVIRONMENT DIVISION.
		CONFIGURATION SECTION.
		SPECIAL-NAMES.
				DECIMAL-POINT IS COMMA.
		INPUT-OUTPUT SECTION.
		FILE-CONTROL.
		SELECT CADFOLHA ASSIGN TO DISK
					ORGANIZATION IS INDEXED
					ACCESS MODE IS DYNAMIC
					RECORD KEY IS CODIGO
					ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
					FILE STATUS IS ST-ERRO.
      
*
*-----------------------------------------------------------------
		DATA DIVISION.
		FILE SECTION.
		FD CADFOLHA
				LABEL RECORD IS STANDARD
				VALUE OF FILE-ID IS "REGFOLHA.DAT".
		01 REGFOLHA.
		   03 CODIGO         PIC 9(6).
		   03 NOME           PIC X(30).
		   03 TIPOSALARIO    PIC X(1).
		   03 SALARIOBASE    PIC 9(6)V99.
		   03 NUMEROFILHOS   PIC 9(2).
		   03 DEPARTAMENTO   PIC 9(2).
		   03 FUNCAO         PIC X(1).
		   03 SALARIOBRUTO   PIC 9(6)V99.
		   03 INSS			 PIC 9(6)V99.
		   03 IMPOSTORENDA   PIC 9(6)V99.
		   03 SALARIOFAMILIA PIC 9(6)V99.
		   03 SALARIOLIQ     PIC 9(6)V99.
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
	   77 W-CONT 		    PIC 9(06) VALUE ZEROS.
	   77 W-OPCAO 		    PIC X(01) VALUE SPACES.
	   77 W-ACT 		    PIC 9 (02) VALUE ZEROS.
	   77 MENS			    PIC X(50) VALUE SPACES.
	   77 LIMPA             PIC X(50) VALUE SPACES. 
       01 ST-ERRO           PIC X(02) VALUE "00".
       01 W-SEL             PIC 9(01) VALUE ZEROS.
	   01 TXTTIPOSAL		PIC 9(3)  VALUE ZEROS.
	   01 TXTDEPART       	PIC X(30) VALUE SPACES.
	   01 TXTNOMEFUNC       PIC X(30) VALUE SPACES
	   01 IND			    PIC 9(02) VALUE ZEROS.
	   
	   
	   01 TABTIPOSAL.
	      03 TBTIPOSAL PIC 9(3) OCCURS 3 TIMES.
	  
	   01 TABDEPART.
		  03 TBDEP	PIC X(15) OCCURS 10 TIMES.
	
	   01 TABFUNC.
		  03 TBFUNCAO PIC X(30) OCCURS 8 TIMES.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAFP.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                              CADASTRO D".
           05  LINE 01  COLUMN 41 
               VALUE  "E FUNCIONARIO".
           05  LINE 02  COLUMN 01 
               VALUE  "________________________________________".
           05  LINE 02  COLUMN 41 
               VALUE  "________________________________________".
           05  LINE 04  COLUMN 01 
               VALUE  "    CODIGO:".
           05  LINE 05  COLUMN 01 
               VALUE  "    NOME:".
           05  LINE 06  COLUMN 01 
               VALUE  "    TIPO SALARIO:".
           05  LINE 07  COLUMN 01 
               VALUE  "    SALARIO BASE:".
           05  LINE 08  COLUMN 01 
               VALUE  "    NUMERO FILHOS:".
           05  LINE 09  COLUMN 01 
               VALUE  "    DEPARTAMENTO:".
           05  LINE 10  COLUMN 01 
               VALUE  "    FUN€AO:".
           05  LINE 11  COLUMN 01 
               VALUE  "    SALARIO BRUTO:".
           05  LINE 12  COLUMN 01 
               VALUE  "    INSS:".
           05  LINE 13  COLUMN 01 
               VALUE  "    IMPOSTO RENDA:".
           05  LINE 14  COLUMN 01 
               VALUE  "    SALARIO FAMILIA:".
           05  LINE 15  COLUMN 01 
               VALUE  "    SALARIO LIQUIDO:".
           05  LINE 22  COLUMN 01 
               VALUE  "________________________________________".
           05  LINE 22  COLUMN 41 
               VALUE  "________________________________________".
           05  LINE 23  COLUMN 01 
               VALUE  "MENSAGEM:".
           05  TCODIGO
               LINE 04  COLUMN 13  PIC 9(6)
               USING  CODIGO.
           05  TNOME
               LINE 05  COLUMN 13  PIC X(29)
               USING  NOME
               HIGHLIGHT.
           05  TTIPOSALARIO
               LINE 06  COLUMN 18  PIC X(01)
               USING  TIPOSALARIO.
           05  TTXTTIPOSAL
               LINE 06  COLUMN 20  PIC X(03)
               USING  TXTTIPOSAL.
           05  TSALARIOBASE
               LINE 07  COLUMN 18  PIC ZZZ.ZZ9,99
               USING  SALARIOBASE.
           05  TNUMEROFILHOS
               LINE 08  COLUMN 19  PIC 9(02)
               USING  NUMEROFILHOS.
           05  TDEPARTAMENTO
               LINE 09  COLUMN 19  PIC 9(02)
               USING  DEPARTAMENTO.
           05  TNOMEDEPART
               LINE 09  COLUMN 22  PIC X(30)
               USING  TXTDEPART
               HIGHLIGHT.
           05  TFUNCAO
               LINE 10  COLUMN 13  PIC X(01)
               USING  FUNCAO.
           05  TNOMEFUNCAO
               LINE 10  COLUMN 15  PIC X(30)
               USING  TXTNOMEFUNC.
           05  TSALARIOBRUTO
               LINE 11  COLUMN 19  PIC ZZZ.ZZ9,99
               USING  SALARIOBRUTO.
           05  TINSS
               LINE 12  COLUMN 11  PIC ZZZ.ZZ9,99
               USING  INSS.
           05  TIMPOSTORENDA
               LINE 13  COLUMN 19  PIC ZZZ.ZZ9,99
               USING  IMPOSTORENDA.
           05  TSALARIOFAMILIA
               LINE 14  COLUMN 21  PIC ZZZ.ZZ9,99
               USING  SALARIOFAMILIA.
           05  TSALARIOLIQ
               LINE 15  COLUMN 21  PIC ZZZ.ZZ9,99
               USING  SALARIOLIQ.
			   
	   01  TELATIPOSAL.
		   05  LINE 06  COLUMN 41 VALUE  "H-HORISTA".
		   05  LINE 07  COLUMN 41 VALUE  "D-DIARISTA".
		   05  LINE 08  COLUMN 41 VALUE  "M-MENSALISTA".					
      *   
	   01  TELADEP.
		   05  LINE 09  COLUMN 41 VALUE  "1-CONTABILIDADE".
		   05  LINE 10  COLUMN 41 VALUE  "2-FINANCEIRO".
		   05  LINE 11  COLUMN 41 VALUE  "3-INFORMATICA".
		   05  LINE 12  COLUMN 41 
			   VALUE  "4-ARQUIVO".
		   05  LINE 13  COLUMN 41 
			   VALUE  "5-RECURSOS HUMANOS".
		   05  LINE 14  COLUMN 41 
			   VALUE  "6-SEGURANÇA DO TRABALHO".
		   05  LINE 15  COLUMN 41 
			   VALUE  "7-ADMNISTRATIVO".
		   05  LINE 16  COLUMN 41 
			   VALUE  "8-OPERACIONAL".
		   05  LINE 17  COLUMN 41 
			   VALUE  "9-GESTÃO".
		   05  LINE 18 COLUMN 41 VALUE "10- COMPRAS".
      *   	
       01  TELAFUNC.
		   05  LINE 10  COLUMN 41 VALUE  "A-ANALISTA".
		   05  LINE 11  COLUMN 41 VALUE  "B-ASSISTENTE".
		   05  LINE 12  COLUMN 41 VALUE  "G-GERENTE".
		   05  LINE 13  COLUMN 41 
			   VALUE  "D-DIRETOR".
		   05  LINE 14  COLUMN 41 
			   VALUE  "E-AUXILIAR".
		   05  LINE 15  COLUMN 41 
			   VALUE  "S-SECRETARIA".
		   05  LINE 16  COLUMN 41 
			   VALUE  "C-CONTADOR".
		   05  LINE 17  COLUMN 41 
			   VALUE  "V-VENDEDOR".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.    
	   INICIO.
      *------------[ PREENCHENDO OS VETORES - TIPO SALARIO]---------------------               	   
			MOVE 220 TO TBTIPOSAL(1)
			MOVE 30 TO TBTIPOSAL(2)
			MOVE 1 TO TBTIPOSAL(3)
      
      *------------[ PREENCHENDO OS VETORES - DEPARTAMENTO]---------------------               	   	  
			MOVE "CONTABILIDADE" TO TBDEP(1)
			MOVE "FINANCEIRO" TO TBDEP(2)
			MOVE "INFORMATICA" TO TBDEP(3)
			MOVE "ARQUIVO" TO TBDEP(4)
			MOVE "RECURSOS HUMANOS" TO TBDEP(5)
			MOVE "SEGURANÇA DO TRABALHO" TO TBDEP(6)
			MOVE "ADMINISTRATIVO" TO TBDEP(7)
			MOVE "OPERACIONAL" TO TBDEP(8)
			MOVE "GESTÃO" TO TBDEP(9)
         	
      *------------[ PREENCHENDO OS VETORES - FUNCAO]---------------------               	   			
			MOVE "ANALISTA" TO TBFUNCAO(1)
			MOVE "ASSISTENTE" TO TBFUNCAO(2)
			MOVE "GERENTE" TO TBFUNCAO(3)
			MOVE "DIRETOR" TO TBFUNCAO(4)
			MOVE "AUXILIAR" TO TBFUNCAO(5)
			MOVE "SECRETARIA" TO TBFUNCAO(6)
			MOVE "CONTADOR" TO TBFUNCAO(7)
			MOVE "VENDEDOR" TO TBFUNCAO(8).
	  
		R0.
			OPEN I-O CADFOLHA
			IF ST-ERRO NOT = "00"
				IF ST-ERRO = "30"
					OPEN OUTPUT CADFOLHA
					CLOSE CADFOLHA
					MOVE "*** ARQUIVO CADFOLHA FOI CRIADO ***" TO MENS
					PERFORM ROT-MENS THRU ROT-MENS-FIM
					GO TO R0
				ELSE
					MOVE "ERRO NA ABERTURA DO ARQUIVO CADFOHLA" TO MENS
					PERFORM ROT-MENS THRU ROT-MENS-FIM
					GO TO ROT-FIM
				ELSE
					NEXT SENTENCE.
      * 
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------               
       R1.
			MOVE ZERO TO W-SEL
			MOVE SPACES TO NOME TIPOSALARIO FUNCAO TXTDEPART 
			MOVE SPACES TO TXTNOMEFUNC 
			MOVE ZERO TO CODIGO SALARIOBASE NUMEROFILHOS DEPARTAMENTO SALARIOBRUTO TXTTIPOSAL INSS IMPOSTORENDA SALARIOFAMILIA SALARIOLIQ.
      *-------------[VISUALIZACAO DA TELA]--------------------------------
           DISPLAY TELAFP.
       R2.
           ACCEPT TCODIGO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
	   LER-CADFOLHA.
           READ CADFOLHA
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R4A
                PERFORM R7A
                PERFORM R8A
                DISPLAY TELAFP
                MOVE "*** FUNCIONARIO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
              ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADFOLHA" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** FUNCIONARIO NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
	   R3.       
           ACCEPT TNOME.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
	   R4.
           DISPLAY TELATIPOSAL
		   MOVE 1 TO IND
           ACCEPT TTIPOSALARIO.
		   ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAFP
                   GO TO R3.
	   R4A.
		   IF TTIPOSALARIO = "H"
				MOVE TBTIPOSAL(1) TO TXTTIPOSAL
		   IF TTIPOSALARIO = "D"
		        MOVE TBTIPOSAL(2) TO TXTTIPOSAL
		   IF TTIPOSALARIO = "M"
		        MOVE TBTIPOSAL(3) TO TXTTIPOSAL.
           DISPLAY TTXTTIPOSAL.
		   DISPLAY TELAFP.
	   R5.
		   ACCEPT TSALARIOBASE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAFP
                   GO TO R4.
	   R6.
		   ACCEPT TNUMEROFILHOS.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAFP
                   GO TO R5.		   
	   R7.
		   DISPLAY TELADEP.
		   MOVE 1 TO IND
		   ACCEPT TDEPARTAMENTO.
		   ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAFP
                   GO TO R6.
	   R7A.
		   MOVE TBDEP(IND) TO TXTDEPART
		   IF IND NOT = DEPARTAMENTO
			  ADD 1 TO IND
			  IF IND < 11
                 GO TO R7A
              ELSE
                 MOVE "*** DEPARTAMENTO INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R7
           ELSE
               DISPLAY TXTDEPART.
			   DISPLAY TELAFP.

       R8.
	       MOVE 1 TO IND
		   DISPLAY TELAFUNC
		   ACCEPT TFUNCAO.
		   IF W-ACT = 01
              DISPLAY TELAFP
              GO TO R7.
	   R8A.
		   MOVE TBFUNCAO(IND) TO TXTNOMEFUNC
		   IF TXTNOMEFUNC NOT = FUNCAO
		      ADD 1 TO IND
			  IF IND < 9
				 GO TO R7
				 ELSE
					MOVE "*** FUNCAO INCORRETA***" TO MENS
					PERFORM ROT-MENS THRU ROT-MENS-FIM
					GO TO R7
           ELSE
               DISPLAY TXTNOMEFUNC.
			   DISPLAY TELAFP.	      
	   R9.
           MULTIPLY SALARIOBASE BY TIPOSALARIO GIVING TSALARIOBRUTO
		   ACCEPT W-ACT FROM ESCAPE KEY
		   DISPLAY TSALARIOBASE
           IF W-ACT = 01
               DISPLAY TELAFP
               GO TO R8.
	   R10.
	       IF SALARIOBASE <= 2500
		       MULTIPLY SALARIOBRUTO BY 0.08 GIVING TINSS
		   END-IF.
		   IF SALARIOBASE > 2500 AND SALARIOBSE <= 6300
		       MULTIPLY SALARIOBRUTO BY 0.09 GIVING TINSS
		   END-IF.
           IF SALARIOBASE > 6300
		       MULTIPLY SALARIOBRUTO BY 0.10 GIVING TINSS.
		   END-IF.
		   DISPLAY TINSS.
           IF W-ACT = 01
               DISPLAY TELAFP
               GO TO R9.
		   END-IF.
       R11.
		   MOVE ZEROS TO IND
           SUBTRACT SALARIOBRUTO FROM INSS GIVING IND
		   IF IND <= 5000
			   MOVE 0 TO TIMPOSTORENDA
			   MULTIPLY IND BY TIMPOSTORENDA
		   END-IF.
		   IF IND > 5000 AND IND <= 12000
			    MOVE 5 TO TIMPOSTORENDA
				MULTIPLY IND BY TIMPOSTORENDA
		   END-IF.	
		   IF IND > 12000
			    MOVE 10 TO TIMPOSTORENDA
				MULTIPLY IND BY TIMPOSTORENDA.
		   END-IF.
		   DISPLAY TIMPOSTORENDA.
	   R12.
           MULTIPLY NUMEROFILHOS BY 20 GIVING TSALARIOFAMILIA
           DISPLAY TSALARIOFAMILIA.
       R13.
	       SUBTRACT SALARIOBRUTO BY INSS BY IMPOSTORENDA GIVING SALARIOLIQ
		   SUM SALARIOLIQ BY SALARIOFAMILIA
		   DISPLAY SALARIOFAMILIA.
            			
      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
	   INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R9.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGAMIGO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* AMIGO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADFOLHA RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO FUNCIONARIO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
				
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R9.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGFOLHA
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADFOLHA.
           STOP RUN.
      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (24, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (24, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
				STOP RUN.
	   
      *    FILE STATUS
      *    00 = OPERA플O REALIZADO COM SUCESSO
      *    22 = REGISTRO J?CADASTRADO
      *    23 = REGISTRO N? ENCONTRADO
      *    30 = ARQUIVO N? ENCONTRADO