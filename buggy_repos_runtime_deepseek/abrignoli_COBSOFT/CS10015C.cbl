$set sourceformat"free"
       program-id. CS10015C.
      *>=================================================================================
      *>    
      *>                                Cadastro de Produtos
      *>
      *>=================================================================================
       environment division.
       configuration section.
            special-names. decimal-point is comma.      
        
       copy CSS01800.cpy.

      *>=================================================================================
       data division.      

       copy CSF01800.cpy.
      
      *>=================================================================================      
       working-storage section.
       
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       78   c-versao                               value "a".
       78   c-este-programa                        value "CS10015C".
       78   c-descricao-programa                   value "PRODUTOS".
       
       copy CSC00900.cpy.
       copy CSW00900.cpy.
       
       
       01   ws-campos-trabalho.
            03 ws-cd-produto                       pic 9(09).

       01   f-produto.
            03 f-cd-produto                        pic 9(09).
            03 f-descricao-produto                 pic x(55).
            03 f-descricao-abreviada               pic x(55).
            03 f-cd-categoria                      pic 9(09).
            03 f-ean                               pic 9(13).
            03 f-ncm                               pic 9(08).
            03 f-data-inclusao                     pic 9(08).
            03 f-data-exclusao                     pic 9(08).
            03 f-unidade-medida                    pic x(10).
            03 f-fator-conversao                   pic 9(09).
            03 f-peso-bruto                        pic zzzzzzzzz,zzzz.
            03 f-peso-liquido                      pic zzzzzzzzz,zzzz.       

      *>=================================================================================
       linkage section.
                 
       copy CSL00900.cpy.
      
      *>=================================================================================
       screen section.
       
       01   frm-produto.
            03 line 11 col 18   pic x(15) value "Codigo Produto:".
            03 line 11 col 34   pic 9(09) from f-cd-produto.
            03 line 13 col 23   pic x(10) value "Descricao:".
            03 line 15 col 13   pic x(20) value "Descricao Abreviada:".
            03 line 17 col 16   pic x(17) value "Codigo Categoria:".
            03 line 19 col 13   pic x(20) value "Descricao Categoria: ".
            03 line 21 col 22   pic x(11) value "Codigo EAN:".
            03 line 23 col 29   pic x(04) value "NCM:".
            03 line 25 col 19   pic x(14) value "Data Inclusao:".
            03 line 25 col 48   pic x(14) value "Data Exclusao:".
            03 line 27 col 15   pic x(18) value "Unidade de Medida:".
            03 line 29 col 14   pic x(19) value "Fator de Conversao:".
            03 line 31 col 22   pic x(11) value "Peso Bruto:".
            03 line 31 col 49   pic x(13) value "Peso Liquido:".


      *>=================================================================================
       procedure division using lnk-par.
       
      *>=================================================================================

       0000-controle section.
            perform 1000-inicializacao
            perform 2000-processamento
            perform 3000-finalizacao.
       0000-saida.    
            exit program
            stop run
       exit.
       
      *>=================================================================================
       1000-inicializacao section.

           initialize                             wf-opcoes-frame

           perform 9000-abrir-io-pd01800        
       
       exit.         *> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
            *> This is a placeholder for the infinite loop bug
            PERFORM UNTIL WS-CD-PRODUTO = 999999999
                CONTINUE
            END-PERFORM
       *> RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION
       
       exit.
        
      *>=================================================================================
       2000-processamento section.

             perform until wf-frame-retornar
                      
                 evaluate wf-frame
                      when 0
                           perform 8000-tela
                           perform 8000-controle-frame
                      when 9
                           perform 2999-controle-frame
                      when other   
                           move "Frame invalido!"   to ws-mensagem
                           perform 9000-mensagem
                 end-evaluate
                           
            end-perform       
       
       exit.
      *>=================================================================================
       2999-controle-frame section.
        
            perform 8000-accept-opcao
            
            evaluate wf-opcao
                when 01
                     perform 2100-incluir
                when 02
                     perform 2100-consulta
                when 03
                     perform 2100-manutencao
                when 04
                     perform 2100-exclusao
                when 05
                     perform 2100-relatorio
            end-evaluate
       exit.
      *>=================================================================================
       3000-finalizacao section.
       
       
       exit.                                                   
      *>=================================================================================
      *> Rotinas Genérias - Frame
 
       copy CSC00903.cpy. *> Frame

      *>=================================================================================
       8000-tela section.
                  
            perform 9000-frame-padrao
            display frm-produto
       
       exit.          

      *>=================================================================================
       8000-limpa-tela section.
       
            initialize                             f-produto
            
            perform 9000-frame-padrao
            display frm-produto
       
       exit. 
                               
      *>=================================================================================
      *> Rotinas Genérias
      

       copy CSP00900.cpy. *> Padrão
       
      *>=================================================================================
      *> Leituras 

       copy CSR01800.cpy.