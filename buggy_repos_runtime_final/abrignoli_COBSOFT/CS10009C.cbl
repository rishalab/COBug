$set sourceformat"free"
       program-id. CS10009C.
      *>=================================================================================
      *>    
      *>                        Gerenciador de Chamadas Assincronas
      *>
      *>=================================================================================
       environment division.
       configuration section.
            special-names. decimal-point is comma.      

       copy CSS01000.cpy.

      *>=================================================================================
       data division.      
 
       copy CSF01000.cpy.
     
      *>=================================================================================      
       working-storage section.
       
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       78   c-versao                               value "a".
       78   c-este-programa                        value "CS10009C".
       78   c-descricao-programa                   value "PERFIL".
       
       copy CSC00900.cpy.
       copy CSW00900.cpy.
       
       
       01   ws-campos-trabalho.
            03 ws-status-perfil                    pic x(01).
               88 ws-perfil-existente                   value "S".
               88 ws-perfil-inexistente                 value " ".
            03 ws-id-validacao                     pic x(01).
               88 ws-validacao-ok                       value "S".
               88 ws-validacao-nok                      value "N".
            
       01   f-perfil.
            03 f-cd-perfil                         pic 9(03).
            03 f-descricao-perfil                  pic x(55).
      *>=================================================================================
       linkage section.
                 
       copy CSL00900.cpy.
      
      *>=================================================================================
       screen section.
       
       01   frm-perfil.
            03 line 11 col 19   pic x(14) value "Codigo Perfil:".
            03 line 11 col 34   pic 9(03) from f-cd-perfil.
            03 line 13 col 21   pic x(12) value "Nome Perfil:". 
            03 line 13 col 34   pic x(55) from f-descricao-perfil.                                 
       
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

           perform 9000-abrir-io-pd01000

       exit.
        
      *>=================================================================================
       2000-processamento section.
       
             *> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
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
             *> RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION
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
                when other   
                     move "Opção inválida!"   to ws-mensagem
                     perform 9000-mensagem
            end-evaluate
       exit.       
       
      *>=================================================================================
      *> Rotinas Genérias - Frame
 
       copy CSC00903.cpy. *> Frame
       
      *>=================================================================================
      8000-tela section.
                  
            perform 9000-frame-padrao
            display frm-perfil
       
       exit.          

      *>=================================================================================
      8000-limpa-tela section.
       
            initialize                             f-perfil
            
            perform 9000-frame-padrao
            display frm-perfil
       
       exit.                                  
      *>=================================================================================
      *> Rotinas Genérias

       copy CSP00900.cpy. *> Padrão

      *>=================================================================================       
      9000-move-registros-frame section.
       
            initialize                             f-perfil
            move f01000-cd-perfil                  to f-cd-perfil
            move f01000-descricao-perfil           to f-descricao-perfil

            perform 8000-tela
                                  
       exit.

      *>=================================================================================       
      9000-move-registros-arquivo section.

                   
       exit.         

      *>=================================================================================
      9000-validacao section.
       
            set ws-validacao-ok                    to true
            
       
       exit.  
                  
      *>=================================================================================
      *> Leituras

       copy CSR01000.cpy.