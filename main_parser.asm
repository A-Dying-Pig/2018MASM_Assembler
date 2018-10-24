TITLE Main Parser
; this file is used to parse a asm file

; ===================================== includes =============================================
.386
.model flat, stdcall
option casemap:none
include windows.inc
include kernel32.inc
includelib kernel32.lib
include masm32.inc
includelib masm32.lib

include msvcrt.inc
includelib msvcrt.lib
include shell32.inc
includelib shell32.lib

include global.inc

; ======================================= Parser Using Structure ==============================
Instructionproto STRUCT
    operation_type DWORD ?
    operation_len  DWORD ?	
    operation_str  BYTE 8 DUP (?)
    operand1_type  DWORD ?
    operand1_name  BYTE 8 DUP (?)
	operand1_len   DWORD ?
    operand2_type  DWORD ?
    operand2_name   BYTE 8 DUP (?)
	operand2_len	DWORD ?
Instructionproto ENDS

Labelproto STRUCT
	strp BYTE 9 DUP (0)
	off DWORD ?
Labelproto ENDS


.data
; ====================================== temp vars ==============================================

InstructionTable Instructionproto 100 DUP (<>)
LabelTable Labelproto 100 DUP (<>)

my_name1 BYTE "dsa",0
my_name2 BYTE "lo",0
my_name3 BYTE "haha",0
my_name4 BYTE "caoni",0
my_name5 BYTE "nimei",0
my_val1 DWORD 15
my_val2 DWORD 26
my_val3 DWORD 59
my_val4 DWORD 89
my_val5 DWORD 110
util_d BYTE "%d",10,0 ; INVOKE crt_printf, OFFSET util_d, ebx

; 
; ====================================== show table vars =========================================

showGV_title BYTE "The Global Varible:",10,0
showGV_line_deli BYTE "-----------------------",10,0
showGV_line BYTE "%s, %d",10,0

showStr_title BYTE "The String table, length: %d",10,0
showStr_string BYTE "%s,",0

; ====================================== message relative ======================================

msg_argument BYTE "missing filename....",10,0
msg_openfile BYTE "cannot open input file...",10,0
msg_grammar_err BYTE "syntax error with line number: lalal", 10, 0
msg_begin_asm BYTE "Assembling: %s",10,0

; ====================================== cmd relative ==========================================

Argsc DWORD ?
ArgsvList DWORD ?
argument BYTE 512 DUP(0)

; ====================================== file relative =========================================
FileHandle DWORD ?
FileLine BYTE 128 DUP(0)

; ====================================== program relative ======================================
NewLine BYTE 13,10,0

; ====================================== parser relative =======================================
splitList DWORD 10 DUP(?)
dataOffset DWORD 0
whichSection DWORD 0 ; 1 for data, 2 for code

; ====================================== enumerate ============================================

enumerateDataType BYTE "BYTE",0,"DWORD",0,0
enumerateBYTEType BYTE "BYTE",0,0
enumerateDWORDType BYTE "DWORD",0,0
enumerateInstruction BYTE "ADD",0,"SUB",0,"MOV",0,"AND",0,"OR",0,"XOR",0,"CMP",0,"PUSH",0,"POP",0,"NEG",0,"XCHG",0,"LOOP",0,"CALL",0,"JMP",0,0

.code
; ==================================================================================================

; ============================ utilities for the code ==============================================

; ==================================================================================================
;-----------------------------------------
str_len PROC USES edi, strp: PTR BYTE
;
; return the length of a string (until a 0)
;-----------------------------------------
	mov edi, strp
	mov eax, 0
	str_len_LOOP:
	cmp byte ptr [edi], 0
	je str_len_END
	inc eax
	inc edi
	jmp str_len_LOOP
	str_len_END:
	ret
str_len ENDP

;----------------------------------------
str_copy PROC USES eax ecx esi edi,
	source: PTR BYTE, target: PTR BYTE
;
; copy the source string to the target (until a 0)
;----------------------------------------
	INVOKE str_len, source
	mov ecx, eax
	inc ecx
	mov esi, source
	mov edi, target
	cld
	rep movsb
	ret
str_copy ENDP

; ---------------------------------------------------
str_cmp PROC USES eax edx esi edi,
	string1: PTR BYTE,
	string2: PTR BYTE
;
; cmp two strings
;----------------------------------------------------
	mov esi, string1
	mov edi, string2
	str_cmp_L1:
	mov al, [esi]
	mov dl, [edi]
	cmp al, 0
	jne str_cmp_L2
	cmp dl, 0
	jne str_cmp_L2
	jmp str_cmp_L3

	str_cmp_L2:
	inc esi
	inc edi
	cmp al,dl
	je str_cmp_L1

	str_cmp_L3:
	ret
str_cmp ENDP

; -------------------------------------------------------
is_space PROC, sour: BYTE
;
; judge if the input is a space or \t, return 1 if space, 0 if not, 2 if the input is 0
; ------------------------------------------------------
	cmp sour, 32
	je is_space_true
	cmp sour, 9
	je is_space_true
	cmp sour, 0
	je is_space_zero

	mov eax, 0
	ret
	is_space_true:
	mov eax, 1
	ret
	is_space_zero:
	mov eax, 2
	ret
is_space ENDP

; ------------------------------------------------------
is_delimiter PROC,
	sour: BYTE
	splitSpace: DWORD, ; 是否切割空格，水平制表符
	other_deli: BYTE ; 输入其他分隔符，为0代表无其他分隔符
; judge is the input 
; -------------------------------------------------------
	cmp sour, 0
	je is_delimiter_zero ; judge if zero

	cmp splitSpace, 0
	je is_delimiter_no_space ;judge if no need for space
	cmp sour, 32
	je is_delimiter_true
	cmp sour, 9
	je is_delimiter_true

	is_delimiter_other:
	cmp other_deli, 0
	je is_delimiter_con
	mov al, other_deli
	cmp sour, al
	je is_delimiter_true

	is_delimiter_con:
	mov eax, 0
	ret
	is_delimiter_true:
	mov eax, 1
	ret
	is_delimiter_zero:
	mov eax, 2
	ret
is_delimiter ENDP

; -------------------------------------------------------
split_string PROC USES ebx ecx edx esi,
	beginPos: PTR BYTE,
	maxCut: DWORD, ; 获取的最大段数，mul 4， 至少为4
	splitSpace: DWORD, ; 是否切割空格，水平制表符
	other_deli: BYTE ; 输入其他分隔符，为0代表无其他分隔符
;
; split the FileLine string, input the maxCut number (mul 4 must)
; store the result in the splitList, return the count(mul 4)
; -------------------------------------------------------
	LOCAL splitStatus: DWORD
	mov ebx, 0 ;splitCount
	mov splitStatus, 0
	mov esi, beginPos

	split_string_OUTERLOOP:
		mov al, [esi]
		cmp al, 0; L[i] != 0
		je split_string_END

		cmp splitStatus, 1
		je split_string_FindEnd
		
		split_string_FindBegin:
		INVOKE is_delimiter, [esi], splitSpace, other_deli
		cmp eax, 0
		je split_string_ChangeToEnd
		inc esi
		jmp split_string_OUTERLOOP
		
		split_string_ChangeToEnd:
		mov splitList[ebx], esi
		add ebx, 4
		inc esi
		cmp ebx, maxCut
		jge split_string_END
		mov splitStatus, 1
		jmp split_string_OUTERLOOP

		split_string_FindEnd:
		INVOKE is_delimiter, [esi], splitSpace, other_deli
		cmp eax, 1
		je split_string_ChangeToBegin
		inc esi
		jmp split_string_OUTERLOOP

		split_string_ChangeToBegin:
		mov al, 0
		mov [esi], al
		inc esi
		mov splitStatus, 0
		jmp split_string_OUTERLOOP

	split_string_END:
	mov eax, ebx
	ret
split_string ENDP

; -------------------------------------------------------
strip_string PROC USES ebx ecx edx esi,
	beginPos: PTR BYTE
	stripSpace: DWORD, ; 是否切割空格，水平制表符
	other_deli: BYTE ; 输入其他分隔符，为0代表无其他分隔符
; strip each side
; -------------------------------------------------------
	mov esi, beginPos
	mov ebx, beginPos

	strip_string_findBegin:
	INVOKE is_delimiter, [esi], stripSpace, other_deli
	cmp eax, 1
	jne strip_string_getBegin
	inc esi
	jmp strip_string_findBegin

	strip_string_getBegin:
	mov ebx, esi ; store the begining in ebx

	strip_string_findEnd:
	mov al, [esi]
	cmp al ,0
	je strip_string_END
	
	INVOKE is_delimiter, [esi], stripSpace, other_deli
	cmp eax, 1
	je strip_string_getEnd
	inc esi
	jmp strip_string_findEnd

	strip_string_getEnd:
	mov al, 0
	mov [esi], al

	strip_string_END:
	mov eax, ebx
	ret
strip_string ENDP

; -------------------------------------------------------
starts_with PROC USES ebx ecx edx esi,
	beginPos: PTR BYTE,
	head: BYTE
;
; judge if a string start with some byte
; -------------------------------------------------------
	mov edx, beginPos
	mov al, [edx]
	cmp al, head
	je starts_with_true

	mov eax, 0
	ret
	starts_with_true:
	mov eax, 1
	ret
starts_with ENDP


; ---------------------------------------------------------
ends_with PROC USES	ebx ecx edx esi,
	beginPos: PTR BYTE,
	tail: BYTE
;
; judge if a string ends with some byte
; ---------------------------------------------------------
	mov edx, beginPos
	INVOKE str_len, edx
	cmp eax, 0
	je ends_with_false

	dec eax
	add edx, eax
	cmp BYTE PTR [edx], tail
	jne ends_with_false
	mov eax, 1
	ret

	ends_with_false:
	mov eax, 0
	ret
ends_with ENDP

; ----------------------------------------------------------
is_digit PROC USES ebx ecx edx esi,
	beginPos: PTR BYTE
;
; judge if a string is all digit
; ----------------------------------------------------------
	mov edx, beginPos
	cmp BYTE PTR [edx], 0
	je is_digit_False

	is_digit_LOOP:
	cmp BYTE PTR [edx], 0
	je is_digit_True
	
	cmp BYTE PTR [edx], 48
	jl is_digit_False
	cmp BYTE PTR [edx], 57
	jg is_digit_False
	inc edx
	jmp is_digit_LOOP

	is_digit_False:
	mov eax, 0
	ret

	is_digit_True:
	mov eax, 1
	ret
is_digit ENDP



; ; -------------------------------------------------------
; split_string PROC USES ebx ecx edx esi,
; 	maxCut: DWORD, other_deli: BYTE
; ;
; ; split the FileLine string, input the maxCut number (mul 4 must)
; ; store the result in the splitList, return the count(mul 4)
; ; -------------------------------------------------------
; 	LOCAL splitStatus: DWORD
; 	mov ebx, 0 ;splitCount
; 	mov splitStatus, 0
; 	mov esi, OFFSET FileLine

; 	split_string_OUTERLOOP:
; 		mov al, [esi]
; 		cmp al, 0; L[i] != 0
; 		je split_string_END

; 		cmp splitStatus, 1
; 		je split_string_FindEnd
		
; 		split_string_FindBegin:
; 		INVOKE is_space, [esi]
; 		cmp eax, 0
; 		je split_string_ChangeToEnd
; 		inc esi
; 		jmp split_string_OUTERLOOP
		
; 		split_string_ChangeToEnd:
; 		mov splitList[ebx], esi
; 		add ebx, 4
; 		inc esi
; 		mov splitStatus, 1
; 		jmp split_string_OUTERLOOP

; 		split_string_FindEnd:
; 		INVOKE is_space, [esi]
; 		cmp eax, 1
; 		je split_string_ChangeToBegin
; 		inc esi
; 		jmp split_string_OUTERLOOP

; 		split_string_ChangeToBegin:
; 		mov al, 0
; 		mov [esi], al
; 		inc esi
; 		mov splitStatus, 0
; 		jmp split_string_OUTERLOOP

; 	split_string_END:
; 	mov eax, ebx
; 	ret
; split_string ENDP

;---------------------------------------------------------
inEnumerate PROC USES ebx ecx edx esi,
	target: PTR BYTE, enumerate: PTR BYTE
; find in Enumerate, return 1 if in, 0 ifnot
; --------------------------------------------------------
	mov ebx, target
	mov edx, enumerate
	inEnumerate_LOOP:
	INVOKE str_cmp, ebx, edx
	je inEnumerate_res_one
	mov ebx, target
	INVOKE str_len, edx
	add edx, eax
	inc edx
	mov al, [edx]
	cmp al, 0
	je inEnumerate_res_zero
	jmp inEnumerate_LOOP

	inEnumerate_res_one:
	mov eax, 1
	ret
	inEnumerate_res_zero:
	mov eax, 0
	ret
inEnumerate ENDP


; ========================================= show tables ===========================================

;--------------------------------------------
showGlobalV PROC USES eax ebx ecx edx
;
; show the global Varible as a table
;-------------------------------------------
	INVOKE StdOut, OFFSET showGV_title
	; calculate
	mov eax, GlobalVCount
	mov ecx, TYPE GlobalvSymbolTable
	mul ecx
	mov ecx, 0
	showGlobalV_LOOP:
	cmp ecx, eax
	jge showGlobalV_END
	
	pushad
	INVOKE StdOut, OFFSET showGV_line_deli
	popad

	lea edx, GlobalvSymbolTable[ecx].n_name
	mov ebx, GlobalvSymbolTable[ecx].n_value

	pushad
	INVOKE crt_printf, OFFSET showGV_line, edx, ebx
	popad
	
	add ecx, TYPE GlobalvSymbolTable
	jmp showGlobalV_LOOP
	showGlobalV_END:
	INVOKE StdOut, OFFSET showGV_line_deli
	ret
showGlobalV ENDP

;------------------------------------------
showStringTable PROC USES eax ebx ecx edx
;
; show the string table (all)
;------------------------------------------
	mov ecx, DWORD PTR StringTable; the whole len, with the first 4 byte
	pushad ;put the length
	INVOKE crt_printf, OFFSET showStr_title, ecx
	popad

	mov edx, 4
	ShowStr_LOOP:
	cmp edx, ecx
	jge ShowStr_END
	
	push ecx
	push edx
	INVOKE crt_printf, OFFSET showStr_string, ADDR StringTable[edx]
	pop edx
	pop ecx

	add edx, eax
	jmp ShowStr_LOOP

	ShowStr_END:
	ret
showStringTable ENDP

;========================================= push into tables =======================================

;-------------------------------------------
pushGlobalV PROC USES eax ebx edx,
	strp:PTR BYTE, in_val: DWORD
;
; push a global Varible to the table
;-------------------------------------------
	mov eax, GlobalVCount
	mov ebx, TYPE GlobalvSymbolTable
	mul ebx
	lea edx, GlobalvSymbolTable[eax].n_name

	cmp [strp], 0
	je pushGlobalV_off
	INVOKE str_copy, strp, edx
	jmp pushGlobalV_Con1
	pushGlobalV_off:
	mov ebx, DWORD PTR strp
	mov DWORD PTR [edx], ebx

	pushGlobalV_Con1:
	mov ebx, in_val
	mov GlobalvSymbolTable[eax].n_value, ebx
	inc GlobalVCount
	ret
pushGlobalV ENDP

;------------------------------------------
pushStringTable PROC USES ebx edx,
	strp:PTR BYTE
;
; push a string into the table, return the begining of a string
;------------------------------------------
	mov ebx, DWORD PTR StringTable
	mov edx, DWORD PTR StringTable
	add edx, OFFSET StringTable
	INVOKE str_len, strp
	INVOKE str_copy, strp, edx
	inc eax
	mov edx, DWORD PTR StringTable
	add edx, eax
	mov DWORD PTR StringTable, edx
	mov eax, ebx
	ret
pushStringTable ENDP

;=========================================== init varibles ==========================================

;----------------------------------
InitParser PROC USES eax
;
; init the parser module
;----------------------------------
	mov eax, 4
	mov DWORD PTR StringTable, eax
	ret
InitParser ENDP

; ========================================= FileAbout ==============================================

;---------------------------------
OpenCmdFile PROC USES eax ebx ecx edx esi
;
; open the command line input file
;---------------------------------
	INVOKE GetCommandLineW
	INVOKE CommandLineToArgvW, eax, OFFSET Argsc
	mov ArgsvList, eax
	mov esi, eax; esi: **Argsv, [esi]->*Argsv

	;see the argument number
	cmp Argsc,1
	jle OpenCmdFile_no_filename

	;get the filename
    add esi, 4
	INVOKE WideCharToMultiByte, 0,0,[esi],-1,OFFSET argument,SIZEOF argument,0,0
	
	INVOKE crt_printf, OFFSET msg_begin_asm, ADDR argument

	;try open the file and give the handle
	INVOKE CreateFile, OFFSET argument, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
	cmp eax, INVALID_HANDLE_VALUE
	je OpenCmdFile_open_error
	mov FileHandle, eax

    ;Free the memory occupied by CommandLineToArgvW
	INVOKE LocalFree, DWORD PTR [ArgsvList]
	ret

	;errors
	OpenCmdFile_no_filename:
	INVOKE StdOut, OFFSET msg_argument
	INVOKE ExitProcess, 0
	OpenCmdFile_open_error:
	INVOKE StdOut, OFFSET msg_openfile
	INVOKE ExitProcess, 0
OpenCmdFile ENDP

;----------------------------------------------------
ReadLine PROC USES ebx ecx edx esi
;
; read a line from file, put it to FileLine, return the length
;----------------------------------------------------
	LOCAL readCount: DWORD
	mov esi, OFFSET FileLine
	mov ebx, 0
	ReadLine_LOOP:
	INVOKE ReadFile, FileHandle, esi, 1, ADDR readCount, 0

	;reach the end or a newline
	cmp readCount, 1
	jl ReadLine_END
	mov al, [esi]
	cmp al, 13
	je ReadLine_LOOP
	cmp al, 10
	je ReadLine_END
	; cmp eax, 13
	; je ReadLine_END

	inc esi
	inc ebx
	jmp ReadLine_LOOP

	ReadLine_END:
	mov al, 0
	mov [esi], al
	mov eax, ebx
	ret
ReadLine ENDP

; ========================================= Parser PROC ============================================

; -------------------------------------------------
ParseDataDec PROC USES eax ebx ecx edx esi
;
; the FileLine is a data declaration, parse this declaration
; ---------------------------------------------------
	LOCAL NameStr:DWORD
	; data declaration as some_name BYTE ?
	INVOKE split_string

	;see if the size is enough
	cmp eax, 12
	jl ParseDataError


	mov ebx, splitList[0] ;value name
	mov edx, splitList[4] ;value type
	mov ecx, splitList[8] ;init value (as a string)

	; ---------------------------- Name
	INVOKE str_len, ebx
	cmp eax, 8
	jl ParseDataDec_pushGLO

	; push to string table first
	INVOKE pushStringTable, ebx
	mov WORD PTR [NameStr+2], ax
	mov WORD PTR [NameStr], 0
	INVOKE pushGlobalV, ADDR NameStr, dataOffset
	jmp ParseDataDec_Con1

	ParseDataDec_pushGLO: ; directly push
	INVOKE pushGlobalV, ebx, dataOffset

	ParseDataDec_Con1:
	; ---------------------------- type
	INVOKE inEnumerate, edx, OFFSET enumerateBYTEType
	cmp eax, 1
	je ParseDataDec_BYTE

	INVOKE inEnumerate, edx, OFFSET enumerateDWORDType
	cmp eax, 1
	je ParseDataDec_DWORD

	jmp ParseDataError

	ParseDataDec_BYTE:
	add dataOffset, 1
	;--------------------------------- Init
	jmp ParseDataDec_Con2
	ParseDataDec_DWORD:
	add dataOffset, 4
	jmp ParseDataDec_Con2

	ParseDataDec_Con2:

	; current work: add to .data section
	
	ret
	ParseDataError:
	INVOKE StdOut, OFFSET msg_grammar_err
	INVOKE ExitProcess, 0
ParseDataDec ENDP

;-------------------------------------------------------
ParseTextStr PROC USES eax ebx ecx edx esi
;
; the FileLine is a code line, parser this line
;-------------------------------------------------------
	ret
ParseTextStr ENDP

; ======================================== MAIN PROC ===============================================

;-------------------------------------------------
MainParser PROC USES eax ebx ecx edx esi edi
;
; main parser proc
;-------------------------------------------------

	INVOKE InitParser
	INVOKE OpenCmdFile

	MainParser_Parsering:
	INVOKE ReadLine

	; judge if end
	cmp BYTE PTR [FileLine], 0
	je MainParser_FileEnd

	; judge if data or code
	cmp whichSection, 1
	je MainParser_Data
	cmp whichSection, 2
	je MainParser_Code

	; both not, start of the file, declaration
	--------------------- Not implemented! -----------------------------------
	jmp MainParser_Parsering

	; data section
	MainParser_Data:
	INVOKE ParseDataDec
	jmp MainParser_Parsering

	; code section
	MainParser_Code:
	INVOKE ParseTextStr
	jmp MainParser_Parsering

	MainParser_FileEnd:
	ret
MainParser ENDP

main PROC

	;INVOKE ReadLine
	;INVOKE ParseDataDec
	
	;INVOKE ReadLine
	;INVOKE StdOut, OFFSET FileLine
	;mov ebx, eax
	;INVOKE StdOut, OFFSET NewLine
	;INVOKE crt_printf, OFFSET util_d, ebx
	
	;INVOKE ReadLine
	;INVOKE StdOut, OFFSET FileLine
	;mov ebx, eax
	;INVOKE StdOut, OFFSET NewLine
	;INVOKE crt_printf, OFFSET util_d, ebx

	;INVOKE ReadLine
	;INVOKE StdOut, OFFSET FileLine
	;mov ebx, eax
	;INVOKE StdOut, OFFSET NewLine
	;INVOKE crt_printf, OFFSET util_d, ebx

	;INVOKE ReadLine
	;INVOKE StdOut, OFFSET FileLine
	;mov ebx, eax
	;INVOKE StdOut, OFFSET NewLine
	;INVOKE crt_printf, OFFSET util_d, ebx
    ;invoke LocalFree, dword ptr [szArglist] ; Free the memory occupied by CommandLineToArgvW
	; mov eax, 5
	; mov GlobalvSymbolTable[0].n_value, eax
	; mov eax, 21
	; mov GlobalvSymbolTable[1].n_value, eax
	
	; mov ebx, GlobalvSymbolTable[0].n_value
	; pushad
	; INVOKE crt_printf, OFFSET util_d, ebx
	; popad
	; mov ebx, GlobalvSymbolTable[1].n_value
	; pushad
	; INVOKE crt_printf, OFFSET util_d, ebx
	; popad
	;mov eax, 0
	;INVOKE crt_printf, OFFSET util_d, eax
	;INVOKE crt_printf, OFFSET my_name
	;INVOKE StdOut, OFFSET my_name
	;INVOKE crt_printf, OFFSET util_d , eax
	INVOKE ExitProcess, 0
main ENDP

END main