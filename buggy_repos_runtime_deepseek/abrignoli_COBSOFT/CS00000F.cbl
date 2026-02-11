procedure division.
2. **linha-cd-empresa, linha-cd-filial, linha-nome-empresa, linha-cd-programa, linha-vs-programa, linha-ds-programa, linha-dt-sistema, linha-funcoes**: These are variables used to store different parts of the output display.
3. **ws-index-0, ws-byte-ds-programa, ws-nr-coluna, ws-ds-funcao**: Temporary working storage variables used in internal processing.
4. **frm-menu-0, frm-menu-1**: These are the forms or templates used to display different parts of the menu based on conditions.

### Procedures:
1. **0000-controle**: This is the main procedure that calls other sub-procedures for initialization, processing, and finalization.
2. **1000-inicializacao**: This procedure sets up the initial values of various variables based on input parameters (`lnk-par`). It constructs parts of the output display such as company information, program details, and function options.
3. **2000-processamento**: This section checks if `lnk-cd-programa` starts with "M" or "P". If so, it displays a specific menu (`frm-menu-0`). Otherwise, it displays another menu (`frm-menu-1`).
4. **3000-finalizacao**: This section is empty and does not contain any code.

### Conditions and Display Logic:
- The program checks if `lnk-cd-programa` starts with "M" or "P". If it does, it displays `frm-menu-0`. Otherwise, it displays `frm-menu-1`.
- Inside the conditional blocks for displaying menus (`frm-menu-0` and `frm-menu-1`), there are specific messages to be displayed based on further conditions.

### Example of Menu Display:
```cobol
if lnk-cd-programa(08:01) equal "M" or lnk-cd-programa(08:01) equal "P"
    display frm-menu-0
else
    display frm-menu-1
end-if
```
This part of the code checks if `lnk-cd-programa` starts with "M" or "P". If it does, it displays `frm-menu-0`. Otherwise, it displays `frm-menu-1`.

### Conclusion:
The provided COBOL code is a template for setting up and displaying menus based on certain conditions. It uses conditional logic to decide which menu to display (`frm-menu-0` or `frm-menu-1`) and how to construct the output display using various variables and data structures. The comments in the code provide additional context about what each part of the program does, making it easier to understand the overall flow and purpose of this COBOL script.