CODE	SEGMENT
	ASSUME	CS:CODE,DS:CODE,ES:CODE,SS:CODE	

command	equ	00
key	    equ	01h
stat	equ 02
data	equ	04

	org 1000h        

    call init
    mov si , 0
    start:	
        call scan
        inc si
        cmp al , 12h
	    jz StartSolve
	    jmp start
    StartSolve:
    mov sizeCnt , si
    ;call cls     
    call nextLine
    call editString 
    
    cmp errorFlag , 1
    jz errContinue1
    
    mov si , 0
    mov cx , sizeCnt
    mov bx , 0
         
    StringLoop:        
        call check
        cmp checkOp , 1
        jz addInFix
        call toInt
        add tmpVar , ax
        mov ax , tmpVar
        mul ten
        mov tmpVar , ax
        inc cnt
        jmp count
        addInFix:
            cmp cnt , 0
            jz addOp
            mov ax , tmpVar
            div ten
            mov inFix[bx] , ax
            add bx , 2
            mov cnt , 0
            addOp:
                mov ax , 0
                mov al , editedStr[si]
                neg ax
                mov inFix[bx] , ax
                add bx , 2
                mov tmpVar , 0 
        count:
            inc si
            loop StringLoop
    errContinue1:
        cmp errorFlag , 1
        jz errContinue
    
    mov ax , bx
    div two
    sub ax , 1
    mov sizeCnt , ax
    
    call transForm    
    
    cmp errorFlag , 1
    jz errContinue
    
    call solve
    
    cmp errorFlag , 1
    jz errContinue
    
    call toString
    
    errContinue:
    mov si , 0
    mov cx , 17
    loopInit1:
        mov string[si] , '/'
        inc si
        loop loopInit1
    
    mov si , 0
    mov cx , 30
    loopInit2:
        mov editedStr[si] , '/'
        inc si
        loop loopInit2
        
    mov cnt , 0
    mov flag , 1
    mov errorFlag , 0
    mov si , 0
    mov cx , 30
    loopInit3:
        mov inFix[si] , '*'
        add si , 2
        loop loopInit3
    
    mov si , 0
    mov cx , 30
    loopInit4:
        mov postFix[si] , '*'
        add si , 2
        loop loopInit4
        
    mov tmpVar , 0
    mov checkOp , 0
    mov sizeCnt , 0
    mov op1 , 0
    mov idx , 0
    mov answer , 0
    mov AX , 0
    mov BX , 0
    mov CX , 0
    mov DX , 0
    mov DI , 0
    
    ;------------VARS------
    string db 17 dup('/')
    editedStr db 30 dup('/')
    errorMsg db 'Error Expression',00h
    cnt db 0
    flag db 1
    errorFlag db 0
    inFix dw 30 dup('*')
    postFix dw 30 dup('*')
    tmpVar dw 0  
    ten dw 10
    two dw 2
    checkOp dw 0
    sizeCnt dw 0
    op1 dw 0
    idx dw 0
    answer dw 0
    ;-----------------------
    
    hlt
    
    ;-----------------------------------------------------  
    ;scan PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------
    scan proc
        mov ax , 0h
        IN AL,key					
		TEST AL,10000000b			
		JNZ Scan
		AND al,00011111b			
		OUT key,AL
		cmp al , 0014h
		jz printPlus
		cmp al , 0013h
		jz printMinus
		cmp al , 0010h
		jz printMul
		cmp al , 0Ch
		jz printOpenArc
		cmp al , 0Dh
		jz printCloseArc
		cmp al , 0012h
		jz break
		or al , 00110000b				
		jmp P
		printPlus:
		    mov al , '+'
		    jmp P
		printMinus:
		    mov al , '-' 
		    jmp P
		printMul:
		    mov al , '*'
		    jmp P
		printOpenArc:
		    mov al , '('
		    jmp P
		printCloseArc:
		    mov al , ')'
		    jmp P
		    
		break:
		    ret
		P:
    		mov string[si] , al    
    		call printInput
		
		ret
	scan ENDP
    
    ;-----------------------------------------------------  
    ;init PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------
    init proc	
        call busy
		mov al,38h
		out command,al
		call busy
		mov al,0fh
		out command,al
		call busy
		mov al,06h
		out command,al
		call busy
		mov al,02
		out command,al
		call busy
		mov al,01
		out command,al
		call busy
		
		ret
	init ENDP
	
	
	;-----------------------------------------------------  
    ;cls PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------
	cls proc
	    mov al,01
		out command,al
		call busy
		ret
    cls ENDP
		
    
    ;-----------------------------------------------------  
    ;nextLine PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------
    nextLine proc
        mov al,192
		out command,al
		call busy
		ret
	nextLine ENDP
	
	;-----------------------------------------------------  
    ;busy PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------	
	busy proc	
	    IN AL,Stat
		test AL,10000000b
		jnz busy
		ret
	busy ENDP
	
	;-----------------------------------------------------  
    ;printInput PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------
	printInput proc
		out data,al
		call busy
		ret
    printInput ENDP
    
    ;-----------------------------------------------------  
    ;editString PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------	
    editString proc
        mov ax , 0
        cmp string[0] , '*'
        jz wrong
        cmp string[0] , '('
        jz go 
        mov editedStr[0] , '0'
        mov ax , 1
        jmp go
        wrong:
            mov errorFlag , 1
            call errorExp
            jmp nEnd
        go:
            mov cx , sizeCnt
            mov si , 0
            mov bx , ax
        loopString:
            cmp string[si] , '+'
            jz plusM
            cmp string[si] , '-'
            jz plusM
            cmp string[si] , '('
            jz plusM
            cmp string[si] , '*'
            jz mulIn
            cmp string[si] , ')'
            jz arc
            cmp string[si] , 47
            jg arc
            jmp lastInsert
            
            plusM:
                mov dl , '0'
                cmp string[si + 1] , '+'
                jz insert
                cmp string[si + 1] , '-'
                jz insert
                jmp lastInsert
            
            mulIn:
                mov dl , '1'
                cmp string[si + 1] , '+'
                jz insert
                cmp string[si + 1] , '-'
                jz insert
                jmp lastInsert
            
            arc:
                mov dl , '*'
                
                cmp string[si + 1] , '('
                jz insert
                jmp lastInsert
            
            insert:
                mov al , string[si]
                mov editedStr[bx] , al 
                inc bx
                mov editedStr[bx] , dl
                inc bx
                jmp continue
                
            lastInsert:
                mov al , string[si]
                mov editedStr[bx] , al
                inc bx
                    
            continue:
                inc si
                loop loopString        
        nEnd:
            mov sizeCnt , bx
            ret
    editString ENDP
    
    ;-----------------------------------------------------  
    ;Check PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------	
    check proc
        cmp editedStr[si] , 48
        jl operation
        mov checkOp,0
        ret
        operation:
           mov checkOp,1
           ret
    check ENDP
    
    ;-----------------------------------------------------  
    ;toInt PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------	
    toInt proc
        mov ax , 0
        mov al , editedStr[si]
        and ax , 11001111b
        ret
    toInt ENDP
    
    
    
    ;-----------------------------------------------------  
    ;transForm PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------	
    transForm proc
        mov cx , sizeCnt
        mov sizeCnt , 0
        mov ax , 37
        neg ax
        mov si , 0
        mov idx , 0
        push ax
        loopTrans:
            mov op1 , 0
            cmp inFix[si] , -1
            jg addElement
            cmp inFix[si] , -40
            jz addArc
            cmp inFix[si] , -41
            jz popStack
            mov bx , inFix[si]
            neg bx
            call priority
            mov op1 , ax
            reCompare:
            call compare
            cmp op1 , ax
            jle reCompare
            jmp L
            addElement:
                mov bx , idx
                mov dx , inFix[si]
                mov postFix[bx] , dx
                inc sizeCnt
                add idx , 2
                jmp L
                
            addArc:
                mov dx , inFix[si]
                push dx
                jmp L
            popStack:
                pop dx
                cmp dx , -40
                jz L
                cmp dx , -37
                jz wrongExp
                mov bx , idx
                mov postFix[bx] , dx
                inc sizeCnt
                add idx , 2
                jmp popStack
                wrongExp:
                    call errorExp
                    mov errorFlag , 1                                 
                
            L:
                add si , 2
                loop loopTrans              
            
            
            popAllStack:
                pop dx
                cmp dx , -37
                jz end_
                mov bx , idx
                mov postFix[bx] , dx
                inc sizeCnt
                add idx , 2
                jmp popAllStack
        
        end_:
            
            ret
    transForm ENDP
    
    ;-----------------------------------------------------  
    ;priority PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------	
    priority proc
        cmp bx , '%'
        jz put0
        cmp bx , '('
        jz put1
        cmp bx , '+'
        jz put2
        cmp bx , '-'
        jz put2
        cmp bx , '*'
        jz put3
        put0:
            mov ax,0
            ret
        put1:
            mov ax,1
            ret
        put2:
            mov ax,2
            ret
        put3:
            mov ax,3
            ret
    priority ENDP
    
    ;-----------------------------------------------------  
    ;compare PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------
    compare proc
        pop DI
        pop dx
        mov bx , dx 
        neg bx
        call priority
        cmp op1 , ax
        jle addElementFromStack
        push dx
        mov dx , inFix[si]
        push dx
        push DI
        mov dx , idx
        ret
        addElementFromStack:
            mov bx , idx
            mov postFix[bx] , dx
            inc sizeCnt
            add idx , 2
            push DI
            ret
    compare ENDP
    
    ;-----------------------------------------------------  
    ;solve PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------	
    solve proc
        mov cx , sizeCnt
        mov si , 0
        mov ax , '%'
        push ax
        
        solveLoop:
            cmp postFix[si] , -43
            jz solveAdd
            cmp postFix[si] , -45
            jz solveSub
            cmp postFix[si] , -42
            jz solveMul 
            push postFix[si]
            jmp loopInc
            solveAdd:
                pop ax
                cmp ax, '%'
                jz justError
                pop dx
                cmp dx , '%'
                jz justError
                add ax , dx
                push ax 
                jmp loopInc
                
            solveSub:
                pop ax
                cmp ax, '%'
                jz justError
                pop dx
                cmp dx , '%'
                jz justError
                sub dx , ax
                push dx
                jmp loopInc
              
            justError:
                call errorExp
            
            solveMul:
                pop ax
                cmp ax ,'%'
                jz justError
                pop dx
                cmp dx ,'%'
                jz justError
                Mul dx
                push ax
                jmp loopInc    
                
            loopInc:
                add si , 2
                loop solveLoop
            
        pop ax
        cmp ax , '%'
        jz wrongAgain
        pop dx
        cmp dx , '%'
        jz closeProc    
        wrongAgain:
            call errorExp
            mov errorFlag , 1    
        closeProc:        
            mov answer , ax
        ret
    
    solve ENDP    
    
    ;-----------------------------------------------------  
    ;toString PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------	
    toString proc
        cmp answer , 0
        jl convert
        jmp continueH
        convert:
            mov flag , 0
            neg answer
        continueH:    
        mov dx , '%' 
        mov si , 0
        push dx
        intToStringLoop:
            mov dx , 0
            mov ax , answer
            div ten
            or dx , 00110000b
            push dx
            mov answer , ax
            cmp answer , 0
            jnz intToStringLoop        
        cmp flag , 0
        jz putMinus
        jmp popAllNum
        putMinus:
            mov ax , '-'
            push ax
        popAllNum:
            mov ax , 0
            pop ax
            cmp ax , '%'
            jz close
            call printInput
            jmp popAllNum
        close:
            ret
    toString ENDP
    
    
    ;-----------------------------------------------------  
    ;toString PROC
    ;Scans a kit button from the user into al
    ;Inputs:   
    ;Outputs:  
    ;-----------------------------------------------------
    errorExp proc
        mov si , 0
        mov errorFlag , 1
        ErrorLoop:
            mov al , errorMsg[si]
            call printInput
            inc si
            cmp errorMsg[si] , 00h
            jnz ErrorLoop 
        ret
    errorExp ENDP
        
   				
CODE	ENDS
	END		