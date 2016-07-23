INCLUDE "emu8086.inc"
TITLE   8086 Code Template (for EXE file)

;       AUTHOR          emu8086
;       DATE            ?
;       VERSION         1.00
;       FILE            ?.ASM

; 8086 Code Template

; Directive to make EXE output:
       #MAKE_EXE#

DSEG    SEGMENT 'DATA'

; TODO: add your data here!!!!
                                            
entrada 			        db 255,     256     dup (?)


resultado			        dw 0

arrayResultado              db 256      dup (?)
                            
variableActual			    db ?    

esOperador			        db 0
esNumero			        db 0
esVariable			        db 0

variablesEnExpresion		db 256      dup (?)

expresionEnAnalisis	    	db 256      dup (?)
expresionFiltrada       	db 256	    dup (?)

variableEnAnalisis          db 256      dup (?) 
variableEnAnalisisExp       db 256      dup (?)
                                    

arrayVariables              db 256      dup ('$')  
arrayVariablesAux           db 256      dup (?)
                                    
countDeExpresionEnAnalisis  dw 0
countDeExpresionFiltrada    dw ?                                	

errorBool				    db 0 

instruccionPrint            db 0
instruccionUnset            db 0
instruccionList             db 0   
declaraVariable             db 0

printIns                       db  'p','r','i','n','t'
unsetIns                       db  'u','n','s','e','t'
listIns                        db  'l','i','s','t'

indexVariables              dw 16 

VariableNoEncontrada        db 0 

aux                         dw 0 
temp                        dw 0
      
indexadorDeValorDeVariable  dw 0

      ;---VARIABLES RODNEL---

CANTVAR    DW ?

      
UNIONDECONJUNTOS DW 100 DUP(?)
CONJUNTOVALIDADO DW 255 DUP(?)
CONJUNTOSINANALIZAR DW 255 DUP(?)
CONJUNTOPARSEADO DW 255 DUP(?)
CONJUNTOANLAIZADO DW 255 DUP(?)
CONJUNTOAIMPRIMIR DW 255 DUP(?)
EXPERSIONDECONJUNTOANALISADA DW 255 DUP(?)

conjuntoa dw '2','3'
conjuntob dw '2','1'


EXPRESINOSINANALIZAR DW 255 DUP(?)
EXPRESIONANALISADA DW 255 DUP(?)


PRIORIDAD1 DB 0
PRIORIDAD2 DB 0
PRIORIDAD3 DB 0

TMPRAIZ DW ? 

INTERSECCIONDECONJUNTOS DW 255 DUP(?)
DIFERENCIADECONJUNTOS DW 255 DUP(?)
ADICIONDEELEMENTOENCONJUNTO DW 255 DUP(?)

LONGITUDDELCONJUNTO1 DW 2
LONGITUDDELCONJUNTO2 DW 2
LONGITUDDELACADENA DW ?



; TODO: end your data here!!!!

DSEG    ENDS

SSEG    SEGMENT STACK   'STACK'
        DW      100h    DUP(?)
SSEG    ENDS

CSEG    SEGMENT 'CODE'

;*******************************************

START   PROC    FAR

; Store return address to OS:
    PUSH    DS
    MOV     AX, 0
    PUSH    AX

; set segment registers:
    MOV     AX, DSEG
    MOV     DS, AX
    MOV     ES, AX


; TODO: add your code here!!!!


;********************************************
;mov ah,09h                                 *
;mov dx,offset arrayvariables               *
;int 21h                                    *

                                            
;print "Evaluador De Expresiones"           *
;mov ah,02h                                 *
;mov dl,0Ah                                 *
;int 21h                                    *
;mov ah,02h                                 *
;mov dl,0d                                  *
;int 21h                                    *
;********************************************

 ;Interrupcion para leer y guaradar la entrada
;mov	ah,0Ah
;mov	dx, OFFSET entrada
;int 	21h
 ;Fin de la Interrupcion
  

;ZONA DE PRUEBA DE LOS METODOS



call OrdenDeLosProcesos


;HASTA AQUI LA ZONA DE PRUEBA


  
RET
START   ENDP

OrdenDeLosProcesos PROC  

Reiniciar:
mov	ah,0Ah
mov	dx, OFFSET entrada
int 	21h


call AnalizarInicioDeCadena

cmp  instruccionList,1
je   Listar
    

call AgregarAExpresionEnAnalisisRestoDeLaCadena
call CopiarAarrayVariablesExp
call FiltrarExpresion

cmp  declaraVariable,1
je   AsignacionNoCompletada

cmp  instruccionUnset,1
je   SetearVariable 

;cmp instruccionPrint,1
;je  imprimeResultado


;imprimeResultado:
;call ConvertirResultToString
;call PrintR


AsignacionNoCompletada:

call MUEVEPARADW
call CREADOREXPRESION
call EVALUADOREXPRESION

call CompletarAsignacion 
call VariableRegistrada?
cmp  VariableNoEncontrada,1
je   Incluir
call CantidadDeElementosAlaIzquierda
call CantidadDeElementosAlaDerecha
call CopiarParaArrayAuxiliar
call CopiarDesdeAuxParaArrayDeVariables
call IncluirVariable
mov  declaraVariable,0
jmp  Reiniciar

Incluir: 
mov  declaraVariable,0 
call IncluirVariable 
cmp errorbool,1
je Reiniciar
mov ah,02h                                 
mov dl,0Ah                                 
int 21h                                    
mov ah,02h                                 
mov dl,0dh                                  
int 21h
jmp Reiniciar

SetearVariable:
call VariableRegistrada?
cmp  VariableNoEncontrada,1
je   NoHayVariableQueSetear
call CantidadDeElementosAlaIzquierda
call CantidadDeElementosAlaDerecha
call CopiarParaArrayAuxiliar
call CopiarDesdeAuxParaArrayDeVariables
mov  instruccionUnset,0
mov ah,02h                                 
mov dl,0Ah                                 
int 21h                                    
mov ah,02h                                 
mov dl,0dh                                  
int 21h
jmp  Reiniciar


NoHayVariableQueSetear:
mov ah,02h                                 
mov dl,0Ah                                 
int 21h                                    
mov ah,02h                                 
mov dl,0dh                                  
int 21h
mov instruccionUnset,0
jmp Reiniciar 

Listar:
call List 
mov  instruccionList,0
jmp Reiniciar
  
OrdenDeLosProcesos ENDP





;***************************************************************
;********************* INSTRUCCIONES ***************************    
;*************************************************************** 

List PROC
    mov si,offset arrayVariables
    
    CambioDeLinea:
    mov ah,02h                                 
    mov dl,0Ah                                 
    int 21h                                    
    mov ah,02h                                 
    mov dl,0dh
    ;mov dx,[si]
    int 21h 
    
    ImprimeActual:
    cmp [si],';'
    je  ProximaLinea 
    cmp [si],'$'
    je  FinList 
    
    mov dx,[si]
    int 21h
    inc si
    jmp ImprimeActual
    
    ProximaLinea:
    inc si
    jmp CambioDeLinea
    
    FinList:
    ret
    
List ENDP 

PrintR PROC
    mov si,offset arrayResultado 
    
    CambioDeLineaPrint:
    mov ah,02h                                 
    mov dl,0Ah                                 
    int 21h                                    
    mov ah,02h                                 
    mov dl,0dh
    int 21h
    
    Imprime:
    cmp [si],'$'
    je  TerminoPrint
    mov dx,[si]
    int 21h
    inc si
    jmp Imprime
    
    
    TerminoPrint:
    mov ah,02h                                 
    mov dl,0Ah                                 
    int 21h                                    
    mov ah,02h                                 
    mov dl,0dh
    int 21h
    ret
PrintR ENDP    

;**************************************************************
;********************* INSTRUCCIONES ***************************     
;*************************************************************** 



;***************************************************************
;*********************ANALISIS DE LA ENTRADA********************    
;***************************************************************

CopiarAarrayVariablesExp proc
    mov si,offset variableEnAnalisis
    mov di,offset variableEnAnalisisExp
   cicloCopiarArray:
    cmp [si],'$'
    je findecopiar
    mov al,[si]
    mov [di],al
    inc di
    inc si
    jmp cicloCopiarArray
   findecopiar:
    mov [di],'$'
    ret
CopiarAarrayVariablesExp endp

VolverAlInicial proc
     mov di,offset variableEnAnalisis
    mov si,offset variableEnAnalisisExp
   cicloinvertidoCopiar:
    cmp [si],'$'
    je findeinvertidocopiar
    mov al,[si]
    mov [di],al
    inc di
    inc si
    jmp cicloinvertidoCopiar
   findeinvertidocopiar:
    mov [di],'$'
    ret
VolverAlInicial endp


AnalizarInicioDeCadena PROC
            mov si, OFFSET entrada
            add si,2 
            mov instruccionPrint,0
            mov instruccionUnset,0
            mov instruccionList ,0
            mov declaraVariable ,0
         
         AnalizandoEspaciosEnBlanco:
            cmp [si],' '
            je  EspacioEnBlanco  
                                                            
            call CompruebaPrint
            cmp  instruccionPrint,1
            je   InstruccionEncontrada
            
            call CompruebaList
            cmp  instruccionList,1
            je   InstruccionEncontrada
            
            call CompruebaUnset
            cmp  instruccionUnset,1
            je   InstruccionEncontrada
            
            mov  declaraVariable,1
            jmp  InstruccionEncontrada
        
         EspacioEnBlanco:
            inc si
            jmp AnalizandoEspaciosEnBlanco
        
         InstruccionEncontrada:
            ret
                  
AnalizarInicioDeCadena ENDP 


ConvertirResultToString  proc
    call VolverAlInicial
    mov  si,offset ArrayResultado
    ;sub  indexadorDeValorDeVariable, si
;    add  si,  indexadorDeValorDeVariable 
    mov  cx, resultado   
    mov bx, 10000    
    mov ax, cx    
    mov  cx, resultado
    ResultadoToStringr:        
    mov dx,0   
    idiv bx
    cmp ax,0
    je decrementarBX
    jmp continuaResultr
   continuaExtraccionToString:
    mov dx,0
    idiv bx
   continuaResultr: 
    cmp bx,0
    je asignacionCompletadaToString
    add al,'0'
    mov [si],al ; pongo el caracter
    inc si
    sub al,'0'
    
    mul bx    ; obtengo el numero q acabo de poner
    add dx,ax     
    sub cx,dx ; obtngo el resto
    cmp bx,1
    je asignacionCompletadaToString
    mov ax,bx
    mov bx,10
    mov dx,0
    idiv bx
    mov bx,ax
    mov ax,cx
    jmp continuaExtraccionToString
   decrementarBXr: 
    mov ax,bx
    mov bx,10
    mov dx,0
    idiv bx
    mov bx,ax
    mov ax,cx
    cmp bx,1
    je continuaExtraccionToString
    jmp ResultadoToStringr
   asignacionCompletadaToString:    
    mov [si],'$'
    ret

ConvertirResultToString  endp


CompruebaPrint PROC 
    mov bx,OFFSET printIns
    mov cx,5
    CicloPrint: 
    mov al, [si]
    cmp al, [bx]
    jne NoEsPrint
    inc si  
    inc bx
    loop CicloPrint
    
    cmp [si],' '
    jne NoEsPrint 
    mov instruccionPrint,1
    ret
    
    NoEsPrint:
    ret
CompruebaPrint ENDP

CompruebaList  PROC
    mov bx,OFFSET listIns
    mov cx,4
    CicloList:
    mov al  ,[si]
    cmp al  ,[bx]
    jne NoEsList
    inc si   
    inc bx
    loop CicloList
    
    cmp [si],' '
    jne NoEsList
    EsList: 
    mov instruccionList,1
    ret
    
    NoEsList:
    cmp [si],13
    je  EsList
    ret    
CompruebaList  ENDP

CompruebaUnset PROC
    mov bx,OFFSET unsetIns
    mov cx,5
    CicloUnset:
    mov al,[si]
    cmp al,[bx]
    jne NoEsUnset
    inc si  
    inc bx
    loop CicloUnset
    
    cmp [si],' '
    jne NoEsUnset 
    mov instruccionUnset,1
    ret
    
    NoEsUnset:
    ret    
CompruebaUnset ENDP
     
     
AgregarAExpresionEnAnalisisRestoDeLaCadena PROC
    
    mov bx, OFFSET expresionEnAnalisis
    mov countDeExpresionEnAnalisis,0
    
    cmp declaraVariable,0
    je  RecibiUnaInstruccion
    jmp RecibiUnaVariable
    
    RecibiUnaInstruccion:
     cmp [si],13
     je  TerminoLaCadena
     mov al,[si]
     mov [bx],al
     inc bx
     inc si
     inc countDeExpresionEnAnalisis
     jmp RecibiUnaInstruccion
     
    TerminoLaCadena:
     ret
     
    RecibiUnaVariable:
     mov si, OFFSET entrada
     add si, 1
     mov cx, [si]
     add si, 1
     mov bx, OFFSET variableEnAnalisis
     

;******REGION: Guardar NOMBRE DE LA VARIABLE *******     
     LlenarVariableEnAnalisis:
     cmp cx,0
     je  VariableNoValida
     
     cmp [si],' '
     je  EncontreEspacioEnVariable
                                                                                         
     mov  al,[si]
     call EsUnaLetra
     cmp  esVariable,1
     je   AgregarAVariableEnAnalisis
     
     mov  al,[si]
     call EsUnNumero
     cmp  esNumero,1
     je   AgregarAVariableEnAnalisis
     
     cmp  [si],'='
     je   TerminoDeclaracionDeVariable
     
     jmp  VariableNoValida  
     
     AgregarAVariableEnAnalisis:
     mov  al,[si]
     mov  [bx],al
     inc  si
     inc  bx
     dec  cx
     jmp  LlenarVariableEnAnalisis
          
     EncontreEspacioEnVariable:
     dec cx
     inc si
     jmp LlenarVariableEnAnalisis
     
     TerminoDeclaracionDeVariable:
     mov al, '='
     mov [bx],al
     inc bx
     mov al,'$'
     mov [bx],al
     mov indexadorDeValorDeVariable,bx
     inc si
     call ValidarNombreDeLaVariable
     cmp  errorBool,1
     je   VariableNoValida
     jmp  AnalizaRestoDeLaExpresion
     
       
;**************************************************************************************     
     VariableNoValida:
     mov ah,02h                                 
     mov dl,0Ah                                 
     int 21h                                    
     mov ah,02h                                 
     mov dl,0d
     print "La variable introducida no ha sido declarada correctamente"           
     mov ah,02h                                 
     mov dl,0Ah                                 
     int 21h                                    
     mov ah,02h                                 
     mov dl,0d                                  
     int 21h
     
     call OrdenDeLosProcesos                                    
      
     
;************************************************************************************** 
          
;********ENDREGION: Guardar NOMBRE DE LA VARIABLE *******   
     AnalizaRestoDeLaExpresion:
     mov bx,OFFSET expresionEnAnalisis 
     jmp RecibiUnaInstruccion

        
AgregarAExpresionEnAnalisisRestoDeLaCadena ENDP
                                         



ValidarNombreDeLaVariable PROC

    mov bx,OFFSET variableEnAnalisis
    mov al,[bx]
    call EsUnNumero
    cmp  esNumero,1
    je   NombreDeVariableInvalido
    mov  errorBool,0
    ret
    
   NombreDeVariableInvalido:
    mov  errorBool,1
    ret

ValidarNombreDeLaVariable ENDP 

CompletarAsignacion PROC
    call VolverAlInicial
    mov  si,offset variableEnAnalisis
    sub  indexadorDeValorDeVariable, si
    add  si,  indexadorDeValorDeVariable 
    mov  cx, resultado   
    mov bx, 10000    
    mov ax, cx    
    mov  cx, resultado
    ResultadoToString:        
    mov dx,0   
    idiv bx
    cmp ax,0
    je decrementarBX
    jmp continuaResult
   continuaExtraccion:
    mov dx,0
    idiv bx
   continuaResult: 
    cmp bx,0
    je asignacionCompletada
    add al,'0'
    mov [si],al ; pongo el caracter
    inc si
    sub al,'0'
    
    mul bx    ; obtengo el numero q acabo de poner
    add dx,ax     
    sub cx,dx ; obtngo el resto
    cmp bx,1
    je asignacionCompletada
    mov ax,bx
    mov bx,10
    mov dx,0
    idiv bx
    mov bx,ax
    mov ax,cx
    jmp continuaExtraccion
   decrementarBX: 
    mov ax,bx
    mov bx,10
    mov dx,0
    idiv bx
    mov bx,ax
    mov ax,cx
    cmp bx,1
    je continuaExtraccion
    jmp ResultadoToString
   asignacionCompletada:    
    mov [si],'$'
    ret
CompletarAsignacion ENDP


;cuando NoEstaContenidaVariable vale 1 
IncluirVariable PROC
    mov si,offset arrayVariables
    mov bx,offset variableEnAnalisis
    
    BuscarFinalCadena:
    cmp [si],'$'
    je  IncluirAlFinal
    inc si
    jmp BuscarFinalCadena
    
    IncluirAlFinal:
    cmp [bx],'$'
    je  TermineDeIncluirVariable
    
    mov al,[bx]
    mov [si],al
    inc si
    inc bx
    jmp IncluirAlFinal
    
    TermineDeIncluirVariable:
    mov al,';'
    mov [si],al
    inc si
    mov al,'$'
    mov [si],al
    ret
IncluirVariable ENDP    

CantidadDeElementosAlaIzquierda PROC
    dec bx
    mov si,offset variableEnAnalisis
    mov ax,0
    mov aux,bx
    
    ElementosAlaIzq:
    cmp [si],'='
    je  NoHayMasElementosALaIzq
    
    inc ax
    inc si
    jmp ElementosAlaIzq 
    
    NoHayMasElementosALaIzq:
    ret
CantidadDeElementosAlaIzquierda ENDP 

CantidadDeElementosAlaDerecha PROC
    mov cx,0
    
    ElementosAlaDer:
    cmp [bx],';'
    je  NoHayMasElementosALaDer
    
    inc cx
    inc bx
    jmp ElementosAlaDer 
    
    NoHayMasElementosALaDer:
    add cx,1
    mov bx,aux
    ret
CantidadDeElementosAlaDerecha ENDP

CopiarParaArrayAuxiliar PROC 
    mov di,offset arrayVariablesAux
    mov si,offset arrayVariables
    
    sub bx,temp
    mov aux,bx
    sub aux,ax
    mov ax,aux
    mov aux,bx
    add aux,cx
    mov cx,aux
    
    mov aux,si
    mov temp,0
    
    
    
     CopiarArrayAux:
     cmp [si],'$'
     je  FinCopiarArrayAux
     
     mov bx,aux
     mov temp,bx
     mov bx, si
     sub bx,aux
     cmp bx,ax
     jb  RealizaCopiaArrayAux
     
     mov bx,aux
     mov temp,bx
     mov bx, si
     sub bx,aux
     cmp bx,cx
     jae  RealizaCopiaArrayAux
     
     inc si
     jmp CopiarArrayAux
     
     RealizaCopiaArrayAux:
     mov bx,temp
     mov aux,bx
     mov dl,[si]
     mov [di],dl
     inc si
     inc di
     jmp CopiarArrayAux
     
     FinCopiarArrayAux:
     mov [di],'$'
     ret
CopiarParaArrayAuxiliar ENDP 

CopiarDesdeAuxParaArrayDeVariables  PROC
    mov di,offset arrayVariablesAux
    mov si,offset arrayVariables
    
    Copiar:
      cmp [di],'$'
      je  FinCopia
      
    mov dl,[di]
    mov [si],dl
    inc si
    inc di
    jmp Copiar
    
    FinCopia:
    mov [si],'$'
    ret  
CopiarDesdeAuxParaArrayDeVariables  ENDP


;***************************************************************
;*********************ANALISIS DE LA ENTRADA********************    
;***************************************************************

     

;***************************************************************
;*********************Analizando Expresion**********************    
;***************************************************************

FiltrarExpresion	PROC
			mov	    cx, countDeExpresionEnAnalisis	
			mov 	bx, OFFSET expresionEnAnalisis   
			mov	    si, OFFSET expresionFiltrada
		Filtrar:  
		    cmp     cx,-1
		    je      FinIteracion
		    
		    cmp     [bx],'#'
		    je      FinIteracion
		    
		    
			cmp 	[bx],' '
			je	    SiguienteIteracion              
			
			mov     al,[bx]			
			
			call 	EsUnOperador
			cmp	    esOperador, 1
			je	    AgregaOperador
			
			call    EsUnNumero
			cmp	    esNumero,1
			je	    AgregaNumero
			
			call    EsUnaLetra
			cmp     esVariable,1
			je      AgregaVariable
			
			
			;ERROR: NO ES VARIABLE NI OPERADOR NI NUMERO????
			
        SiguienteIteracion: 
			cmp     [bx],'#'
			je      FinIteracion
			inc	    bx
			loop    Filtrar
			
		FinIteracion:
		    mov [si],'$'   
		    ret
		
		AgregaOperador:
		    mov     [si],al
		    inc     si
		    jmp     SiguienteIteracion
		
		AgregaNumero:
			mov     [si],al
			call    SeguirAnalizandoNumero
			inc     si
			cmp     errorBool,1
			je      ERROR
			jmp     SiguienteIteracion
		
		
		AgregaVariable:
		    call TomarNombreVariable
		    cmp  errorBool,1
		    je   ERROR
		    mov  aux,bx
		    call VariableRegistrada?
		    cmp  variableNoEncontrada,1
		    je   ERROR 
		    call CopiaValorDeVariableParaExpresionFiltrada
		    mov  bx, Offset expresionEnAnalisis
		    sub  aux, bx
		    add  bx,  aux		    
		    jmp  Filtrar
		    
		    
			
		ERROR:
	 mov ah,02h                                 
     mov dl,0Ah                                 
     int 21h                                    
     mov ah,02h                                 
     mov dl,0d
     print "La expresion contiene caracteres invalidos o no se pudo encontrar la variable solicitada"
     mov   errorBool,1
     ret  
        		    
			
			
FiltrarExpresion	ENDP  

;*****************************************************************************

TomarNombreVariable PROC
    mov di,offset variableEnAnalisis
    mov [di],al
    inc di
    inc bx
    dec cx
    
    AnalizarNombre:
    cmp cx,0
    je  LlegueAlFinalAnalizandoVariable
    
    cmp [bx],'#'
    je  LlegueAlFinalAnalizandoVariable
    
    mov al,[bx]
    
    call esUnaLetra
    cmp  esVariable,1
    je   CaracterValido
    
    call esUnNumero
    cmp  esNumero,1
    je   CaracterValido
    
    cmp  al,' '
    je   EncontreEspacioAnalizandoNombre 
    
    jmp  ErrorEnElNombre
    
    CaracterValido:
    mov [di],al
    inc di
    inc bx
    dec cx
    jmp AnalizarNombre 
    
    EncontreEspacioAnalizandoNombre:
    inc bx
    dec cx
    jmp AnalizarNombre 
    
    ErrorEnElNombre:
    call esUnOperador
    cmp  esOperador,1
    je   EncontreOperadorAnalizandoElNombre
    mov  errorBool,1
    ret
    
    EncontreOperadorAnalizandoElNombre:
    mov al,'='
    mov [di],al
    ret
        
    LlegueAlFinalAnalizandoVariable:
    mov  al ,'='
    mov [di],al
    mov al,'$'
    mov [si],al
    ret
TomarNombreVariable ENDP


;********************************************************************


VariableRegistrada? PROC
    
    mov bx, offset arrayVariables
    mov temp,bx
    mov di, offset variableEnAnalisis 
    
   Etiqueta:
    cmp [bx],'$'
    je  NoEstaContenidaVariable
    
    mov al,[di]
    cmp [bx],al
    je  EncontreCoincidencia
    
    jmp NoHuboCoincidencia
    
    
   EncontreCoincidencia:
    cmp [di],'='
    je  Iguales
    inc di
    inc bx
    jmp Etiqueta
    
   NoEstaContenidaVariable:
    mov variableNoEncontrada,1
    ret 
    
   NoHuboCoincidencia:
   mov di, offset variableEnAnalisis
   inc bx
   jmp MueveteHastaLaProximaVariable
   
   MueveteHastaLaProximaVariable:  
   mov al,[bx]
   cmp [bx],'$'
   je  NoEstaContenidaVariable
   cmp [bx],';'
   je  LlegueAProximaVariable
   inc bx
   jmp MueveteHastaLaProximaVariable
   
   LlegueAProximaVariable:
   inc bx
   jmp Etiqueta      
   
   Iguales:
   mov variableNoEncontrada,0
   inc bx
   ret
    
VariableRegistrada? ENDP

;********************************************************

CopiaValorDeVariableParaExpresionFiltrada PROC
    CopiarVariables:
    cmp [bx],';'
    je  TerminoLaCopia
    
    mov al,[bx]
    mov [si],al
    
    inc bx
    inc si
    jmp CopiarVariables
    
    TerminoLaCopia:
    ret

CopiaValorDeVariableParaExpresionFiltrada ENDP 


;****************************************************************

SeguirAnalizandoNumero	 PROC    
			        
			        AnalizarNumero:
			        inc si
			        AnalizarNumeroA:
			        inc bx
			        dec cx
			            
			        cmp cx,0                            ; por si llego al final de la cadena
			        je  LlegueAlFinalAnalizandoUnNumero        
			    
			        cmp [bx],'#'
			        je  LlegueAlFinalAnalizandoUnNumero
			            
			        mov al, [bx]
			    
			        call EsUnNumero
			        cmp  esNumero,1
			        je   AunEsNumero
			    
			        cmp  al,' ' 
			        je   AnalizarNumeroA
			    
			        jmp  NoEsNumero
			    
			        ;Aqui hice un cambio, habia puesto si y creo que era cx 
			        ;EncontreEspacioEnBlancoAnalizandoNumero:
			        ;dec  si
			        ;jmp  AnalizarNumero        
			         
			        AunEsNumero:    
			        mov [si],al
			        jmp AnalizarNumero
			                        
			        NoEsNumero:
			        call EsUnOperador
			        cmp  esOperador,1
			        je   EncontreOperadorYTerminoAnalisis
			        jmp  ErrorEnAnalisisDeNumero
			    
			        EncontreOperadorYTerminoAnalisis:
			        mov  [si],al
			        ret
			    
			        ErrorEnAnalisisDeNumero:
			        mov  errorBool,1
			        ret     
			 
			        LlegueAlFinalAnalizandoUnNumero:
			        mov al ,'$'
			        mov [si],al   
			        ret
SeguirAnalizandoNumero   ENDP    

;***************************************************************
;*********************Analizando Expresion**********************    
;***************************************************************





;***************************************************************
;*********************METODOS BOOLEANOS*************************    
;***************************************************************
EsUnOperador	PROC
        mov variableActual, al
		
		cmp	variableActual, '*'
		je	OPERADOR
		cmp	variableActual, '-'
		je	OPERADOR
		cmp	variableActual, '/'
		je	OPERADOR
		cmp	variableActual, '+'
		je	OPERADOR
		cmp	variableActual, '%'
		je	OPERADOR 
		cmp variableActual, '^'
		je  OPERADOR
		cmp variableActual, '!'
		je  OPERADOR
		cmp variableActual, '['
		je  OPERADOR
		cmp variableActual, ']'
		je  OPERADOR
		cmp variableActual, '('
		je  OPERADOR
		cmp variableActual, ')'
		je  OPERADOR
		cmp variableActual, '.'
		je  OPERADOR 
		cmp variableActual, '_'
		je  OPERADOR
		cmp variableActual, '&'
		je  OPERADOR
		cmp variableActual, '|'
		je  OPERADOR
		cmp variableActual, '~'
		je  OPERADOR 
		cmp variableActual, '<'
		je  OPERADOR
		cmp variableActual, '>'
		je  OPERADOR
		mov esOperador,0 
		ret
	OPERADOR:
		mov	esOperador,1
		ret
EsUnOperador    ENDP



EsUnNumero  PROC
		    cmp	al,'0'
		    jb 	NoEsNumeroPosible
		    cmp 	al,'9'
		    ja	NoEsNumeroPosible
		    mov 	EsNumero, 1
		
		    ret
	        NoEsNumeroPosible:
		    mov esNumero,0
		    ret
EsUnNumero  ENDP


EsUnaLetra  PROC
		    cmp  al, "A"
		    jb   ESFALSO
		    cmp  al, "z"
		    ja   ESFALSO
		    cmp  al, "Z"
		    jbe  ESVERDADERO
		    cmp  al, "a"
		    jae  ESVERDADERO
	ESFALSO:
	        mov  esVariable,0
	        ret  
	ESVERDADERO:
		    mov  esVariable,1
		    ret          		
EsUnaLetra  ENDP


;***************************************************************
;*********************METODOS BOOLEANOS*************************    
;***************************************************************

;***************************************************************
;*********************Parte de Rodnel **************************    
;***************************************************************
SUMA		PROC 
        PUSH BP
        MOV BP,SP
        MOV AX,[BP+6]
        MOV BX,[BP+4]	
		add  ax, bx
		mov  resultado,ax
		POP BP
		RET 4
SUMA		ENDP

		
RESTA   	PROC
		PUSH BP
        MOV BP,SP
        MOV AX,[BP+4]
        MOV BX,[BP+6]
		sub  ax, bx
		mov  resultado,ax
		POP BP
		RET 4
RESTA		ENDP


MULTIPLICACION	PROC
		PUSH BP
        MOV BP,SP
        MOV AX,[BP+4]
        MOV BX,[BP+6]
		imul  BX
		mov   resultado, AX
		POP BP
		RET 4
MULTIPLICACION	ENDP

DIVISION	PROC
		PUSH BP
        MOV BP,SP
        MOV AX,[BP+4]
        MOV BX,[BP+6]
		idiv  BX
		mov   resultado, ax 
		POP BP
		RET 4
DIVISION	ENDP

RESTODIVISION	PROC
		PUSH BP
        MOV BP,SP
        MOV AX,[BP+4]
        MOV BX,[BP+6]
		idiv  BX
		mov   resultado, dx 
		POP BP
		RET 4
RESTODIVISION	ENDP
;;FINOPERACIONES



;************************************************************************
                              ;---OPERACIONES ADICIONALES---
POTENCIA PROC
    PUSH BP
    MOV BP,SP
    MOV BX,[BP+4]  ; EL VALOR DE LA BASE
    MOV CX,[BP+6]  ; EL VALOR DE LA EXPONENTE
    XOR AX,AX
    MOV AX,1
    CICLOPOTENCIA:
       MUL BX             
    LOOP CICLOPOTENCIA
    MOV RESULTADO,AX
    POP BP
    RET 4    
POTENCIA ENDP

RAIZ PROC
    PUSH BP
    MOV BP,SP    
    MOV BX,[BP+6]  ;EL VALOR DEL EXPONENTE
    XOR AX,AX
    XOR DX,DX
    INC DX   
 CICLORAIZ:
    
    PUSH BX
    MOV AX,DX
    PUSH AX
    MOV TMPRAIZ,DX
    CALL POTENCIA
    CMP AX,[BP+4] ;EL VALOR AL CUAL LE HALLAMOS LA RAIZ
    JA FIN
    MOV DX,TMPRAIZ
    INC DX
    MOV BX,[BP+6]
    JMP CICLORAIZ
 FIN:    
    MOV AX,TMPRAIZ
    DEC AX
    MOV RESULTADO,AX 
    POP BP
    RET 4
RAIZ ENDP  


LOGARITMO PROC 
    PUSH BP
    MOV BP,SP    
    MOV BX,[BP+6]  ;EL VALOR DE BASE
    XOR AX,AX
    XOR DX,DX
    INC DX   
 CICLOLOG:
    MOV AX,DX
    PUSH DX
    PUSH BX
    MOV TMPRAIZ,DX
    CALL POTENCIA
    CMP AX,[BP+4] ;EL VALOR AL CUAL LE HALLAMOS LA LOGARIMO
    JA FINLOG
    MOV DX,TMPRAIZ
    INC DX
    MOV BX,[BP+6]
    JMP CICLOLOG
 FINLOG:    
    MOV AX,TMPRAIZ
    DEC AX
    MOV RESULTADO,AX 
    POP BP
    RET 4
LOGARITMO ENDP

FACTORIAL PROC
       PUSH BP
       MOV BP,SP
       MOV CX,[BP+4]
       MOV AX,1
       CMP CX,0
       JZ FINFACT       
    CICLOFACT:
       MUL CX
       LOOP CICLOFACT
    FINFACT:
       MOV RESULTADO,AX
       POP BP
       RET 2
FACTORIAL ENDP 

SECUENCIASUMA PROC
       PUSH BP
       MOV BP,SP                  
       MOV CX,[BP+4]  ;VALOR MAXIMO
       MOV BX,[BP+6]  ;VALOR MINIMO
       XOR AX,AX
    SUMASECUENCIA:
       ADD AX,CX
       CMP CX,BX                
       JZ FINSC
       INC CX
       JMP SUMASECUENCIA
    FINSC:
       MOV RESULTADO,AX       
       POP BP
       RET 4
SECUENCIASUMA ENDP

SHIFTRIGHT PROC
    PUSH BP
    MOV BP,SP
    MOV AX,[BP+6]
    MOV CL,[BP+4]
    DEC CL
    SHR AX,CL
    MOV RESULTADO,AX
    POP BP
    RET 4    
SHIFTRIGHT ENDP

SHIFTLEFT PROC
    PUSH BP
    MOV BP,SP
    MOV AX,[BP+6]
    MOV CL,[BP+4]
    DEC CL
    SHL AX,CL
    MOV RESULTADO,AX
    POP BP
    RET 4  
SHIFTLEFT ENDP

BINARYNOT PROC
    PUSH BP
    MOV BP,SP
    MOV AX,[BP+4]
    NOT AX
    MOV RESULTADO,AX
    POP BP
    RET 2
BINARYNOT ENDP

BINARYOR PROC
     PUSH BP
    MOV BP,SP
    MOV AX,[BP+6]
    MOV BX,[BP+4]
    OR AX,BX
    MOV RESULTADO,AX
    POP BP
    RET 4
BINARYOR ENDP  

BINARYAND PROC
    PUSH BP
    MOV BP,SP
    MOV AX,[BP+6]
    MOV BX,[BP+4]
    AND AX,BX
    MOV RESULTADO,AX
    POP BP
    RET 4
BINARYAND ENDP




;****************************************************************************
                                    ;VALIDAR CONJUNTOS
VALIDACIONCONJUNTO PROC
       PUSH BP
      MOV BP,SP
      MOV SI,[BP+4]
      MOV CX,0
      MOV DI,OFFSET CONJUNTOVALIDADO
      MOV [DI],'$'
   INICIALIZANDOdiVC:
      MOV DI,OFFSET CONJUNTOVALIDADO
      SUB DI,2
   CICLOVALIDACION:
      MOV AX,[SI]
      CMP [DI],AX         
      JZ INCsiVC
      ADD CX,2
      ADD DI,2                
      CMP [DI],'$'          
      JNZ CICLOVALIDACION
      MOV AX,[SI]
      MOV [DI],AX
      ADD  DI,2
      MOV [DI],'$'
   INCsiVC:
      ADD SI,2
      CMP [SI],'$'
      JZ FINCVALIDACION
      JMP INICIALIZANDOdiVC
   FINCVALIDACION:
      MOV LONGITUDDELCONJUNTO1,CX
      POP BP
      RET 2
VALIDACIONCONJUNTO ENDP



                                    ;OPERACIONES CON CONJUNTOS
                                    ;---UNION---
UNIONCONJUNTOS PROC
      PUSH BP
      MOV BP,SP
      MOV SI,[BP+4]
      XOR CX,CX
      MOV CX,LONGITUDDELCONJUNTO1
      XOR DX,DX     
      MOV DX,LONGITUDDELCONJUNTO2
      ADD DX,CX
      MOV DI,OFFSET UNIONDECONJUNTOS
   PASANDOCa:
      MOV AX,[SI]
      MOV [DI],AX
      ADD SI,2
      ADD DI,2     
      LOOP PASANDOCa      
      MOV SI,[BP+6]
      MOV CX,LONGITUDDELCONJUNTO2      
    INICIALIZANDOdiC:  
      MOV DI,OFFSET UNIONDECONJUNTOS
      MOV DX, LONGITUDDELCONJUNTO2      
    PASANDOCb:
      MOV AX,[SI]
      CMP [DI],AX         ; COMPARO Y VEO SI YA LO CONTENGO
      JZ INCsi
      ADD DI,2
      DEC DX           ;TODO: QUITAR ESTA SENTENCIA
      CMP DX,0          ;VEO SI ES FIN DE CADENA
      JNZ PASANDOCb
      MOV AX,[SI]
      MOV [DI],AX
      ADD  DI,2
      MOV DX,LONGITUDDELCONJUNTO2
      INC DX
      MOV LONGITUDDELCONJUNTO2,DX            
      INCsi:      
      ADD SI,2
      LOOP INICIALIZANDOdiC
    FINUNION:
      MOV [DI],36      
      POP BP
      RET 4      
UNIONCONJUNTOS ENDP 
                                  ;---INTERSECCION---

INTERSECCIONCONJUNTOS PROC
      PUSH BP
      MOV BP,SP
      MOV SI,[BP+4]      
      MOV DI,OFFSET INTERSECCIONDECONJUNTOS
      XOR CX,CX
      MOV CX,LONGITUDDELCONJUNTO1
      XOR DX,DX     
      MOV DX,LONGITUDDELCONJUNTO2
   INICIANLIZANDOINTERSECCIONC:
      MOV BX,[BP+6]
      MOV DX,LONGITUDDELCONJUNTO2
   CICLOINTERSECCIONC:
      MOV AX,[BX]      
      CMP AX,[SI]
      JNZ INCbIC
      MOV AX,[SI]
      MOV [DI],AX
      ADD DI,2
   INCsiIC:
      ADD SI,2
      DEC CX
      CMP CX,0
      JZ FININTERSECCION
      JMP INICIANLIZANDOINTERSECCIONC
   INCbIC:
      ADD BX,2
      DEC DX
      CMP DX,0
      JZ INCsiIC
      JMP CICLOINTERSECCIONC
   FININTERSECCION:
      MOV [DI],36
      POP BP            
      RET 4
INTERSECCIONCONJUNTOS ENDP
                              ;---DIFERENCIA---
                              
DIFERENCIACONJUNTOS PROC
      PUSH BP
      MOV BP,SP
      MOV SI, [BP+4]
      MOV DI, OFFSET DIFERENCIADECONJUNTOS
      XOR CX,CX
      MOV CX,LONGITUDDELCONJUNTO1
      XOR DX,DX     
      MOV DX,LONGITUDDELCONJUNTO2
      INICIANLIZANDODIFERENCIAC:
      MOV BX,[BP+6]  
      MOV DX,LONGITUDDELCONJUNTO2
   CICLODIFERENCIAC:
      MOV AX,[BX]      
      CMP AX,[SI]
      JZ INCsiDC
      JMP INCbDC
   MOVsiDC:
      MOV AX,[SI]
      MOV [DI],AX
      ADD DI,2
   INCsiDC:
      ADD SI,2
      DEC CX
      CMP CX,0
      JZ FINDIFERENCIA
      JMP INICIANLIZANDODIFERENCIAC
   INCbDC:
      ADD BX,2
      DEC DX
      CMP DX,0
      JZ MOVsiDC
      JMP CICLODIFERENCIAC
   FINDIFERENCIA:
      MOV [DI],36
      POP BP
      RET 4
DIFERENCIACONJUNTOS ENDP

                                 ;---ADICIONAR ELEMENTO---
                                 
ADICIONELEMENTOCONJUNTO PROC
      PUSH BP
      MOV BP,SP
      MOV SI, [BP+4]
      MOV DI, OFFSET ADICIONDEELEMENTOENCONJUNTO
      MOV BX,[BP+6]
      MOV CX,LONGITUDDELCONJUNTO1 
   CICLOADICIONC:
      MOV AX,[SI]
      MOV [DI],AX
      ADD SI,2
      ADD DI,2
   LOOP CICLOADICIONC 
      MOV CX,LONGITUDDELCONJUNTO1
      MOV SI, [BP+4]
      MOV DI, OFFSET ADICIONDEELEMENTOENCONJUNTO
   CICLOCOMPROBACIONC:
      CMP [DI],BX
      JZ FINADICIONC
      ADD DI,2
      LOOP CICLOCOMPROBACIONC
      MOV [DI], BX
   FINADICIONC:
      MOV CX,1
      ADD CX,LONGITUDDELCONJUNTO1
      MOV LONGITUDDELCONJUNTO1,DX
      MOV [DI],36        
      POP BP
      RET 4
ADICIONELEMENTOCONJUNTO ENDP                              

                                ;---LEER CONJUNTO---

LEERCONJUNTOENTRANTE PROC
      MOV SI, CONJUNTOSINANALIZAR
      MOV DI, CONJUNTOANLAIZADO
      XOR BL,BL
      XOR BH,BH
      MOV CX,LONGITUDDELACADENA     
   CICLOLEERC:
      ADD SI,2
      CMP [SI],']'
      JE FINDELALECTURAC
      CMP [SI],' '
      JE CONTINUAC
      CMP [SI],','
      JE INSERTASEPARADORC
      PUSH AX
      CALL ESUNNUMEROPOSIBLE
      CMP AX,0
      JE FINDELALECTURAC
      MOV AX,[SI]
      MOV [DI],AX
   INCDILECTURA:   
      ADD DI,2      
   CONTINUAC:
      LOOP CICLOLEERC
      JMP FINDELALECTURAC
   INSERTASEPARADORC:
      MOV AX,'?'
      MOV [DI],AX
      INC BL
      JMP INCDILECTURA  
   FINDELALECTURAC:
      MOV [DI],'$'
      INC BL
      MOV LONGITUDDELCONJUNTO1,BX  
      RET
LEERCONJUNTOENTRANTE ENDP

IMPRIMIRCONJUNTO PROC
      MOV DI, OFFSET CONJUNTOAIMPRIMIR
      MOV SI, OFFSET CONJUNTOPARSEADO
      MOV BP,LONGITUDDELCONJUNTO1
      MOV [DI],'['
   INICIODEDESPARSEO:
      CMP BP,0
      JE FINDELDESPARSEO
      MOV AX,[SI]
      MOV CX,[SI]
      MOV BX,10000
   SACARCARACTER:
      MOV DX,0
      IDIV BX
      CMP AX,0
      JE DISMINUIRBX
      CMP BX,0
      JNE INSERTARELEMENTO
   PROXIMOELEMENTO:   
      ADD DI,2
      MOV [DI],','      
      ADD SI,2
      DEC BP
      JMP INICIODEDESPARSEO 
   INSERTARELEMENTO:
      ADD DI,2
      ADD AX,'0'
      MOV [DI],AX
      SUB AX,'0'      
      IMUL BX
      MOV CX,[SI]
      SUB CX,AX
      CMP BX,1
      JE PROXIMOELEMENTO  
    DISMINUIRBX: 
      MOV AX,BX 
      MOV BX,10
      MOV DX,0
      IDIV BX
      MOV BX,AX
      MOV AX,CX
      JMP SACARCARACTER
   FINDELDESPARSEO:
      MOV [DI],']'
      ADD DI,2
      MOV [DI],36
      RET
IMPRIMIRCONJUNTO ENDP



;CREADOREXPRESIONPARACONJUNTO PROC
;      MOV SI,OFFSET EXPRESIONSINANALISIS
;      MOV DI,OFFSET EXPERSIONDECONJUNTOANALISADA       
;      XOR CX,CX
;   INICIODELEVALUADORC:      
;      PUSH AX
;      CALL ESUNNUMEROPOSIBLE
;      CMP AX,1
;      JE PARSEANUMEROC
;      
;      CMP [SI],' '
;      JE INCREMENTASIEC
;      MOV DX,[SI]
;      PUSH DX
;      CALL BUSCARPRIORIDADC      
;      CMP PRIORIDAD1,1
;      JE CMPPRIORIDADDELANTERIOR1AC
;      CMP PRIORIDAD2,1
;      JE CMPPRIORIDADDELANTERIOR2C
;      CMP PRIORIDAD3,1
;      JE CMPPRIORIDADDELANTERIOR3C
;   CONTINUAEVALUANDOC:      
;      CMP [SI],')'
;      JE SACATODOSHASTAPARC
;      CMP [SI],36
;      JE SACATODODELAPILAC
;   INCREMENTASIE:
;      ADD SI,2
;      JMP INICIODELEVALUADORC
;   
;   ;PARSEANUMERO:
;;      MOV DX,[SI]
;;      MOV [DI],DX
;;      ADD DI,2
;;      ADD SI,2
;;      CMP [SI],' '
;;      JE INCPARSEO
;;      PUSH AX                       ; PARSEO EL NUMERO HASTA Q TERMINE EN UNA I
;;      CALL ESUNNUMEROPOSIBLE
;;      CMP AX,1
;;      JE PARSEANUMERO
;;      
;;      MOV [DI],'i'
;;      ADD DI,2      
;;      JMP INICIODELEVALUADOR 
;;   INCPARSEO:
;;      ADD SI,2
;;      JMP PARSEANUMERO 
;      
;   CMPPRIORIDADDELANTERIOR1C: 
;      PUSH DX     
;   CMPPRIORIDADDELANTERIOR1AC:   
;      PUSH [SI]
;      ADD SI,2
;      INC CX
;      JMP INICIODELEVALUADORC
;      
;   CMPPRIORIDADDELANTERIOR3C:
;      CMP CX,0
;      JE CMPPRIORIDADDELANTERIOR1AC      
;      POP DX
;      PUSH DX                          ;METO EN LA PILA LOS DE ULTIMA PRIORIDAD
;      CALL BUSCARPRIORIDADC
;      CMP PRIORIDAD3,1
;      JNE CMPPRIORIDADDELANTERIOR1C
;      MOV AX,DX                         ;SACO LOS DE MAYOR PRIORIDAD
;      MOV [DI],AX
;      ADD DI,2
;      DEC CX
;      JMP CMPPRIORIDADDELANTERIOR3C
;      
;   CMPPRIORIDADDELANTERIOR2C:
;      
;   PASAROPERANDOS2C:      
;      CMP CX,0
;      JE CMPPRIORIDADDELANTERIOR1AC
;      POP DX
;      PUSH DX
;      CALL BUSCARPRIORIDADC         ;METO EN LA PILA LOS DE 2DA PRIORIDAD
;      CMP  PRIORIDAD3,1
;      JE CMPPRIORIDADDELANTERIOR1C
;      MOV AX,DX
;      MOV [DI],AX
;      ADD DI,2                         ;SACO LOS DE MAYOR PRIORIDAD
;      DEC CX
;      JMP PASAROPERANDOS2C
;      
;   SACATODOSHASTAPARC:
;      POP DX 
;      DEC CX
;      CMP DX,'('
;      JE INCREMENTASIEC
;      MOV AX,DX
;      MOV [DI],AX
;      ADD DI,2                          ;SACO LOS DE MAYOR PRIORIDAD
;      
;      
;      JMP SACATODOSHASTAPARC
;      
;   SACATODODELAPILAC:      
;      CMP CX,0
;      JE TERMINOEXPRESIONC 
;      POP DX     
;      MOV AX,DX
;      MOV [DI],AX
;      ADD DI,2                          ;SACO LOS DE MAYOR PRIORIDAD
;      DEC CX 
;      JMP SACATODODELAPILAC      
;   TERMINOEXPRESIONC: 
;      MOV [DI],36
;      RET
;CREADOREXPRESIONPARACONJUNTO ENDP

;********************************************************************
                             ;AUXILIARES
                            

MUEVEPARADW PROC
      MOV SI,OFFSET expresionFiltrada
      MOV DI,OFFSET EXPRESINOSINANALIZAR
    
   CONTINUAMOVIENDODW:
      CMP [SI],'$'
      JE FINDELAMUDANZA
      XOR AH,AH
      MOV AL,[SI]
      MOV [DI],AX
      ADD DI,2
      INC SI
      JMP CONTINUAMOVIENDODW
      
   FINDELAMUDANZA:
      MOV [DI],'$'
      RET    
MUEVEPARADW ENDP

ESUNNUMEROPOSIBLE PROC
      PUSH BP
      MOV BP,SP
      MOV AX,0
      CMP [SI],'0'
      JB NOESNUMEROPOS
      CMP [SI],'9'
      JA NOESNUMEROPOS
      MOV AX,1
   NOESNUMEROPOS:
      POP BP
      RET 2
ESUNNUMEROPOSIBLE ENDP

PARSEARCONJUNTO PROC
      MOV DI,OFFSET CONJUNTOPARSEADO
      MOV SI,OFFSET CONJUNTOANLAIZADO
      MOV CX, LONGITUDDELCONJUNTO1
      XOR AX,AX
   CICLOPARSEADOR:
      CMP [SI],36
      JE FINDELPARSE
      JMP PARSEARC
   ELEMENTOPARSEADO:
      ADD SI,2         
      LOOP CICLOPARSEADOR
      JMP FINDELPARSE
   INCSIPARSER:
      ADD SI,2
   PARSEARC:
      XOR AX,AX
      PUSH AX
      CALL ESUNNUMEROPOSIBLE
      CMP AX,0
      JE ESSEPARADORC
      MOV AX,[DI]
      MOV DX,[SI]         ; MOVER EL CARACTER
      SUB DX,'0'          ; OBTENER EL NUMERO 
      MOV [DI],DX         ; GUARDAR EL NUMERO
      MOV DX,10           
      MUL DX              ; SE LE MULTIPLIK 10 POR KDA NUMERO QUE INTRODUZCAS
      ADD [DI],AX         
      ADD SI,2 
      JMP PARSEARC
   ESSEPARADORC:
      CMP [SI],36
      JE FINDELPARSE
      CMP [SI],'?'
      JE PARSEARENC
      CMP [SI],' '
      JE INCSIPARSER
      
      ;PONER EL ERROR --------------------------------------------------------
   PARSEARENC:         
      ADD DI,2 
      JMP ELEMENTOPARSEADO
   FINDELPARSE:
      RET  
        
PARSEARCONJUNTO ENDP


BUSCARPRIORIDAD PROC
      PUSH BP
      MOV BP,SP
      MOV AX,[BP+4]
      CMP AX,'('
      JE ESPRIORIDAD1
      CMP AX,'^'
      JE ESPRIORIDAD1
      CMP AX,'_'
      JE ESPRIORIDAD1
      CMP AX,':'
      JE ESPRIORIDAD1
      CMP AX,'!'
      JE ESPRIORIDAD1
      CMP AX,'*'
      JE ESPRIORIDAD2
      CMP AX,'/'
      JE ESPRIORIDAD2
      CMP AX,'>'
      JE ESPRIORIDAD2
      CMP AX,'<'
      JE ESPRIORIDAD2
      CMP AX,'%'
      JE ESPRIORIDAD2
      CMP AX,'+'
      JE ESPRIORIDAD3
      CMP AX,'.'
      JE ESPRIORIDAD3      
      CMP AX,'-'
      JE ESPRIORIDAD3
      CMP AX,'&'
      JE ESPRIORIDAD3
      CMP AX,'|'
      JE ESPRIORIDAD3
      CMP AX,'~'
      JE ESPRIORIDAD3
      MOV PRIORIDAD1,0
      MOV PRIORIDAD2,0
      MOV PRIORIDAD3,0
      JMP FINBP
   ESPRIORIDAD1:
      MOV PRIORIDAD1,1
      MOV PRIORIDAD2,0
      MOV PRIORIDAD3,0
      JMP FINBP
   ESPRIORIDAD2:
      MOV PRIORIDAD1,0
      MOV PRIORIDAD2,1
      MOV PRIORIDAD3,0
      JMP FINBP
   ESPRIORIDAD3:
      MOV PRIORIDAD1,0
      MOV PRIORIDAD2,0
      MOV PRIORIDAD3,1
      JMP FINBP
   FINBP:
      POP BP
      RET 2
BUSCARPRIORIDAD ENDP  

OPERAENLAPILA PROC
      PUSH BP
      MOV BP,SP
      CMP AX,'^'
      JE ESPOTENCIA
      CMP AX,'<'
      JE ESSHIFTL
      CMP AX,'>'
      JE ESSHIFTR
      CMP AX,'.'
      JE ESSECUENCIA
      CMP AX,'_'
      JE ESRAIZ
      CMP AX,':'
      JE ESLOGARITMO
      CMP AX,'!'
      JE ESFACTORIAL
      CMP AX,'*'
      JE ESMULTIPLICACION
      CMP AX,'/'
      JE ESDIVISION
      CMP AX,'%'
      JE ESRESTO
      CMP AX,'+'
      JE ESSUMA
      CMP AX,'-'
      JE ESRESTA
      CMP AX,'&'
      JE ESYB
      CMP AX,'|'
      JE ESOB
      CMP AX,'~'
      JE ESNOB
      JMP  FINDEOPERAR
   ESPOTENCIA:
      CMP CANTVAR,1
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      MOV BX,[BP+6]
      PUSH AX
      PUSH BX
      CALL POTENCIA 
      POP BP
      RET 4
   ESNOB:
      CMP CANTVAR,0
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      PUSH AX
      CALL BINARYNOT     
      POP BP
      RET 2
   ESYB:
      CMP CANTVAR,1
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      MOV BX,[BP+6]
      PUSH AX
      PUSH BX
      CALL BINARYAND   
      POP BP
      RET 4
   ESOB:
      CMP CANTVAR,1
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      MOV BX,[BP+6]
      PUSH AX
      PUSH BX
      CALL BINARYOR 
      POP BP
      RET 4
   ESRESTA:
      CMP CANTVAR,2
      JAE NOAGREGACERO
      CMP CANTVAR,1
      JE AGREGACERO
      JMP ERRORDEEXPRESION
    AGREGACERO:
      MOV AX,[BP+4]
      MOV BX,0
      MOV CX,2
      JMP CONTINUARESTA
   NOAGREGACERO:
      MOV AX,[BP+4]
      MOV BX,[BP+6] 
      
    CONTINUARESTA:
      PUSH AX
      PUSH BX      
      CALL RESTA
      POP BP
      CMP CX,2
      JE RETORNADOS
      RET 4
   RETORNADOS:
      RET 2    
   ESSUMA:
      CMP CANTVAR,2
      JAE NOAGREGACEROS 
      CMP CANTVAR,1
      JE AGREGACEROS
      JMP ERRORDEEXPRESION
   AGREGACEROS:
      MOV AX,[BP+4]
      MOV BX,0
      MOV CX,2
      JMP CONTINUASUMA
   NOAGREGACEROS:   
      MOV AX,[BP+4]
      MOV BX,[BP+6]
   CONTINUASUMA:   
      PUSH AX
      PUSH BX
      CALL SUMA
      POP BP
      CMP CX,2
      JE RETORNADOS
      RET 4
   ESMULTIPLICACION:
      CMP CANTVAR,1
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      MOV BX,[BP+6]
      PUSH AX
      PUSH BX
      CALL MULTIPLICACION 
      POP BP
      RET 4
   ESDIVISION:
      CMP CANTVAR,1
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      MOV BX,[BP+6]
      PUSH AX
      PUSH BX
      CALL DIVISION
      POP BP
      RET 4   
   ESRESTO:
      CMP CANTVAR,1
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      MOV BX,[BP+6]
      PUSH AX
      PUSH BX
      CALL RESTODIVISION
      POP BP
      RET 4
   ESFACTORIAL:
      CMP CANTVAR,0
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      PUSH AX
      CALL FACTORIAL
      POP BP
      RET 2
   ESLOGARITMO:      
      CMP CANTVAR,1
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      MOV BX,[BP+6]
      PUSH AX
      PUSH BX
      CALL LOGARITMO
      POP BP
      RET 4
   ESRAIZ:
      CMP CANTVAR,1
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      MOV BX,[BP+6]
      PUSH AX
      PUSH BX
      CALL RAIZ
      POP BP
      RET 4
   ESSECUENCIA:
      CMP CANTVAR,1
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      MOV BX,[BP+6]
      PUSH AX
      PUSH BX
      CALL SECUENCIASUMA
      POP BP
      RET 4
   ESSHIFTR:
      CMP CANTVAR,1
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      MOV BX,[BP+6]
      PUSH AX
      PUSH BX
      CALL SHIFTRIGHT
      POP BP
      RET 4
   ESSHIFTL:
      CMP CANTVAR,1
      JE ERRORDEEXPRESION
      MOV AX,[BP+4]
      MOV BX,[BP+6]
      PUSH AX
      PUSH BX
      CALL SHIFTLEFT
      POP BP
      RET 4      
   FINDEOPERAR:
      POP BP
      RET
   ERRORDEEXPRESION:
   ;--------------------ERROR----------------------    
OPERAENLAPILA ENDP



;***************************************************************************************************
                           ;--- EVALUADOR DE EXPRESIONES ---

CREADOREXPRESION PROC
      MOV SI,OFFSET EXPRESINOSINANALIZAR
      MOV DI,OFFSET EXPRESIONANALISADA       
      XOR CX,CX
   INICIODELEVALUADOR:      
      PUSH AX
      CALL ESUNNUMEROPOSIBLE
      CMP AX,1
      JE PARSEANUMERO
      
      CMP [SI],' '
      JE INCREMENTASIE
      MOV DX,[SI]
      PUSH DX
      CALL BUSCARPRIORIDAD      
      CMP PRIORIDAD1,1
      JE CMPPRIORIDADDELANTERIOR1A
      CMP PRIORIDAD2,1
      JE CMPPRIORIDADDELANTERIOR2
      CMP PRIORIDAD3,1
      JE CMPPRIORIDADDELANTERIOR3
   CONTINUAEVALUANDO:      
      CMP [SI],')'
      JE SACATODOSHASTAPAR
      CMP [SI],36
      JE SACATODODELAPILA
   INCREMENTASIE:
      ADD SI,2
      JMP INICIODELEVALUADOR
   
   PARSEANUMERO:
      MOV DX,[SI]
      MOV [DI],DX
      ADD DI,2
      ADD SI,2
      CMP [SI],' '
      JE INCPARSEO
      PUSH AX                       ; PARSEO EL NUMERO HASTA Q TERMINE EN UNA I
      CALL ESUNNUMEROPOSIBLE
      CMP AX,1
      JE PARSEANUMERO
      
      MOV [DI],'i'
      ADD DI,2      
      JMP INICIODELEVALUADOR 
   INCPARSEO:
      ADD SI,2
      JMP PARSEANUMERO 
      
   CMPPRIORIDADDELANTERIOR1: 
      PUSH DX     
   CMPPRIORIDADDELANTERIOR1A:   
      PUSH [SI]
      ADD SI,2
      INC CX
      JMP INICIODELEVALUADOR
      
   CMPPRIORIDADDELANTERIOR3:
      CMP CX,0
      JE CMPPRIORIDADDELANTERIOR1A      
      POP DX
      PUSH DX                          ;METO EN LA PILA LOS DE ULTIMA PRIORIDAD
      CALL BUSCARPRIORIDAD
      CMP PRIORIDAD3,1
      JNE CMPPRIORIDADDELANTERIOR1
      MOV AX,DX                         ;SACO LOS DE MAYOR PRIORIDAD
      MOV [DI],AX
      ADD DI,2
      DEC CX
      JMP CMPPRIORIDADDELANTERIOR3
      
   CMPPRIORIDADDELANTERIOR2:
      
   PASAROPERANDOS2:      
      CMP CX,0
      JE CMPPRIORIDADDELANTERIOR1A
      POP DX
      PUSH DX
      CALL BUSCARPRIORIDAD         ;METO EN LA PILA LOS DE 2DA PRIORIDAD
      CMP  PRIORIDAD3,1
      JE CMPPRIORIDADDELANTERIOR1
      MOV AX,DX
      MOV [DI],AX
      ADD DI,2                         ;SACO LOS DE MAYOR PRIORIDAD
      DEC CX
      JMP PASAROPERANDOS2
      
   SACATODOSHASTAPAR:
      POP DX 
      DEC CX
      CMP DX,'('
      JE INCREMENTASIE
      MOV AX,DX
      MOV [DI],AX
      ADD DI,2                          ;SACO LOS DE MAYOR PRIORIDAD
      
      
      JMP SACATODOSHASTAPAR
      
   SACATODODELAPILA:      
      CMP CX,0
      JE TERMINOEXPRESION 
      POP DX     
      MOV AX,DX
      MOV [DI],AX
      ADD DI,2                          ;SACO LOS DE MAYOR PRIORIDAD
      DEC CX 
      JMP SACATODODELAPILA      
   TERMINOEXPRESION: 
      MOV [DI],36
      RET
CREADOREXPRESION ENDP


EVALUADOREXPRESION PROC
      MOV SI,OFFSET EXPRESIONANALISADA
      XOR BX,BX
   INICIODEEVALUACION:
      XOR BX,BX
      CMP [SI],' '
      JE INCREMENTASIEVAUADOR
      CMP [SI],36
      JE FINDELAEVALUACION      
      PUSH AX
      CALL ESUNNUMEROPOSIBLE
      CMP AX,1
      JE PARSEANUMEROE 
      MOV AX,[SI]              
      CALL OPERAENLAPILA
      PUSH RESULTADO
      MOV AX,CANTVAR
      DEC AX
      MOV CANTVAR,AX
   INCREMENTASIEVAUADOR:
      ADD SI,2
      JMP INICIODEEVALUACION
   PARSEANUMEROE:
      MOV AX,BX
      XOR BX,BX
      MOV BX,[SI]
      SUB BX,'0'
      MOV CX,10
      MUL CX
      ADD BX,AX
   INCREMENTAMAS:   
      ADD SI,2
      CMP [SI],'i'
      JE  ENDNUM
      CMP [SI],' '
      JE INCREMENTAMAS
      PUSH AX      
      CALL  ESUNNUMEROPOSIBLE
      CMP AX,1
      JE  PARSEANUMEROE
      JMP INTNUMERO
   ENDNUM:
      ADD SI,2
   INTNUMERO:
      PUSH BX
      MOV AX,CANTVAR
      INC AX
      MOV CANTVAR,AX      
      JMP INICIODEEVALUACION
   FINDELAEVALUACION:
      POP RESULTADO
      RET
EVALUADOREXPRESION ENDP 

;***************************************************************
;*********************Parte de Rodnel **************************    
;***************************************************************





; TODO: end your code here!!!!

; return to operating system:
    

;*******************************************

CSEG    ENDS 

END    START    ; set entry point.


