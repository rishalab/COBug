       *>
       *> wui-end-html:
       *>
       *> dispaly html end-tags for browser output
       *> 
       *> Coder: BK 
       *> 
       IDENTIFICATION DIVISION.
       program-id. wui-end-html.
        
       ENVIRONMENT DIVISION.
        
       DATA DIVISION.
       working-storage section.      
        
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       linkage section.
       01  ln-rtn-code    PIC S99.      
        
       PROCEDURE DIVISION USING ln-rtn-code.
       000-end-html.
        
           DISPLAY
               "</body>"
               "</html>"          
           END-DISPLAY        
        
           EXIT PROGRAM
           .
            
       *>******************************************************

