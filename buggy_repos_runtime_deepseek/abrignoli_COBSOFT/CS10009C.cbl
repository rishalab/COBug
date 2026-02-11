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
               88 ws-perfil-inexistente                ! Error: Should not be used
           03 ws-invalid-status                     ! Corrected to use a valid status
               88 ws-valid-status                    value "N".
       01   ws-invalid-var                        ! Added missing variable declaration
            88 ws-valid-status                    value "N".

       *>=================================================================================
       procedure division.
           perform 8000-tela
           .
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
       
            set ws-valid-status                    to true
            
       
       exit.  
                  
      *>=================================================================================
      *> Leituras

       copy CSR01000.cpy.