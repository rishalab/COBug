$set sourceformat"free"
        program-id. CS00002M.
      *>=================================================================================
      *>    
      *>                           Menu Principal - ESTOQUE
      *>
      *>=================================================================================
       environment division.
       configuration section.
            special-names. decimal-point is comma.
            
       copy CSS00100.cpy. *> Módulos
       copy CSS01300.cpy. *> Usuário x Módulos           

      *>=================================================================================
       data division.
       
       copy CSF00100.cpy. *> Módulos
       copy CSF01300.cpy. *> Usuário x Módulos       
      
      *>=================================================================================      
       working-storage section.
       
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       78   c-versao                                value "a".
       78   c-este-programa                         value "CS00002M". 
       78   c-descricao-programa                    value "CONTROLE DE ESTOQUE".        
       
       copy CSC00900.cpy.
       *> RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
       copy CSW00900.cpy.
       *> RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC
       
       01   ws-campos-trabalho.
            03 ws-nr-linha                         pic 9(02).
            03 ws-nr-coluna                        pic 9(02).
            03 ws-cd-programa                      pic x(08).
            03 ws-id-acesso                        pic x(01).
               88 ws-usuario-acesso                     value "S".
               88 ws-usuario-sem-acesso                 value "N".
            03 ws-linha-modulo                     pic x(58).
       
      *>=================================================================================
       linkage section.
       
       copy CSL00900.cpy.
      
      *>=================================================================================
       screen section.
                                                    
                                                                           
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
                    
       exit.
        
      *>=================================================================================
       2000-processamento section.
       
            perform until wf-frame-retornar
                 
                 evaluate wf-frame
                      when 0
                           perform 8000-tela
                      when 9
                           perform 2999-controle-frame     
                      when other
                           move "Frame inválido!"  to ws-mensagem
                           perform 9000-mensagem
                           perform 9000-abortar
                 end-evaluate
                           
            end-perform
       
       exit.
           
      *>=================================================================================
       2999-controle-frame section.     
            
            perform 8000-accept-opcao
                 
            if   not wf-opcao-retornar   
                 
                 perform 9000-abrir-io-pd00100
                 perform 9000-abrir-io-pd01300
                 
                 initialize                   f00100-modulos
                 move lnk-cd-empresa          to f00100-cd-empresa
                 move lnk-cd-filial           to f00100-cd-filial
                 move c-este-programa         to f00100-programa-principal
                 move wf-opcao                to f00100-operacao-programa
                 perform 9000-ler-pd00100-ran-1
                 if   not ws-operacao-ok
                 and  not ws-registro-inexistente
                      string "Erro de leitura f00100-modulos - Status [" ws-resultado-acesso "]" into ws-mensagem
                      perform 9000-mensagem
                      perform 9000-abortar
                 end-if
                      
                 move f00100-cd-programa           to ws-cd-programa
                      
                 if   not lnk-login-master
                      initialize                   f01300-usuario-modulo
                      move lnk-cd-empresa          to f01300-cd-empresa
                      move lnk-cd-filial           to f01300-cd-filial
                      move lnk-id-usuario          to f01300-id-usuario
                      move ws-cd-programa          to f01300-cd-programa
                      perform 9000-ler-pd01300-ran
                      if   ws-operacao-ok
                           set ws-usuario-acesso             to true
                      end-if
                 end-if
            end-if
       exit.
             
      *>=================================================================================
      *> Leituras
      
       copy CSR00100.cpy. *> Módulos
       copy CSR01300.cpy. *> Usuário x Módulos