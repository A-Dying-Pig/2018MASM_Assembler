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
msg_parser_byte BYTE "Not implemented! 8 bit current not supported.",10,0
msg_label_long BYTE "label too long....",10,0

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
splitListSize = 10
splitList DWORD splitListSize DUP(?)
dataOffset DWORD 0
whichSection DWORD 0 ; 1 for data, 2 for code

; ====================================== enumerate ============================================

;enumerateDataType BYTE "BYTE",0,"DWORD",0,0
enumerateBYTEType BYTE "BYTE",0,0
enumerateDWORDType BYTE "DWORD",0,0
enumerateInclude BYTE "INCLUDE",0,0
enumerateIncludeLib BYTE "INCLUDELIB",0,0

enumerateOneInstruction BYTE "PUSH",0,"POP",0,"NEG",0,"LOOP",0,"CALL",0,"JMP",0,0
enumerateTwoInstruction BYTE "ADD",0,"SUB",0,"MOV",0,"AND",0,"OR",0,"XOR",0,"CMP",0,0
enumerateJump BYTE "JMP",0,"LOOP",0,0
enumerateCall BYTE "CALL",0,0

enumerateReg BYTE "EAX",0,"EBX",0,"ECX",0,"EDX",0,"ESI",0,"EDI",0,"ESP",0,"EBP",0,"EAX",0,0

enumerateCode BYTE ".code",0
enumerateData BYTe ".data",0

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

; ---------------------------------------------------
str_count PROC USES edx ebx, strp: PTR BYTE, target: BYTE
;
; count the number of a byte in a string
; ---------------------------------------------------
	mov bl, target
	mov edx, strp
	mov eax, 0

	str_count_LOOP:
	cmp BYTE PTR [edx], 0
	je str_count_END

	cmp BYTE PTR [edx], bl
	jne str_count_noeq
	inc eax

	str_count_noeq:
	inc edx
	jmp str_count_LOOP

	str_count_END:
	ret
str_count ENDP


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
to_upper PROC USES eax ebx ecx edx esi,
	source: PTR BYTE
;
; change the string to upper case
; ------------------------------------------------------
	mov esi, source
	to_upper_LOOP:
	cmp BYTE PTR [esi], 0
	je to_upper_END

	cmp BYTE PTR [esi], 97
	jl to_upper_Con
	cmp BYTE PTR [esi], 122
	jg to_upper_Con
	sub BYTE PTR [esi], 32

	to_upper_Con:
	inc esi
	jmp to_upper_LOOP
	to_upper_END:
	ret
to_upper ENDP

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

	mov ecx, splitListSize
	split_string_CLEAR:
	mov DWORD PTR splitList[ebx], 0
	add ebx, 4
	loop splitListSize

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

	; judge length, if too long push string table
	INVOKE str_len, strp
	cmp eax, 8
	jl pushGlobalV_driect

	; push to string table first
	INVOKE pushStringTable, strp
	mov DWORD PTR [edx], 0
	add edx, 4
	mov DWORD PTR [edx], eax
	jmp pushGlobalV_Con2

	pushGlobalV_driect:
	INVOKE str_copy, strp, edx

	pushGlobalV_Con2:
	mov ebx, in_val
	mov GlobalvSymbolTable[eax].n_value, ebx
	inc GlobalVCount
	ret
pushGlobalV ENDP

;------------------------------------------
pushStringTable PROC USES ebx edx,
	strp:PTR BYTE
;
; push a string into the table, return the begining of the new string
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

; -------------------------------------------------------------
pushLabel PROC USES eax ebx ecx edx,
	l_name: PTR BYTE
;
; push a string to label table, including own func and label
; -------------------------------------------------------------
	mov eax, LabelCount
	mov ebx, TYPE LabelTable
	mul ebx
	lea edx, LabelTable[eax].label_name

	; judge length, if too long push string table
	INVOKE str_len, strp
	cmp eax, 8
	jg pushLabel_error

	INVOKE str_copy, strp, edx
	inc LabelCount
	ret
	pushLabel_error:
	INVOKE StdOut, OFFSET msg_label_long
	INVOKE ExitProcess, 0
pushLabel ENDP

; ========================================== find in tables =========================================

; -----------------------------------------------------------
findStringTable PROC USES ebx, edx,
	strp: PTR BYTE
;
; find the string in the table, return the pos of begin, if 0 -> no found
; -----------------------------------------------------------
	mov ebx, DWORD PTR StringTable
	mov edx, 4

	findStr_Loop:
	INVOKE str_cmp, StringTable[edx], strp
	je findStr_END
	INVOKE str_len, StringTable[edx]
	add edx, eax
	cmp edx, ebx
	je findStr_NO
	jmp findStr_Loop

	findStr_END:
	mov eax, edx
	ret
	findStr_NO:
	mov eax, 0
	ret
findStringTable ENDP


;=========================================== init varibles ==========================================

;----------------------------------
InitParser PROC USES eax
;
; init the parser module
;----------------------------------
	mov eax, 4
	mov DWORD PTR StringTable, eax
	mov LabelCount, 0
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

; -----------------------------------------------------
ParseByteDec PROC USES eax ebx ecx edx esi edi
	initValStr: PTR BYTE
;	NOT supported
; parse a string of vars of 8 bit
; -----------------------------------------------------
	INVOKE StdOut, OFFSET msg_parser_byte
	INVOKE ExitProcess, 0
ParseByteDec ENDP

; -----------------------------------------------------
ParseDwordDec PROC USES eax ebx ecx edx esi edi
	initValStr: PTR BYTE
;
; parse a string of vars of 32 bit
; ------------------------------------------------------
	LOCAL arrayCount:DWORD
	mov esi, initValStr
	INVOKE starts_with, esi, 44
	cmp eax, 0
	je ParseDwordError

	INVOKE split_string, ecx, 0, 0, 44
	mov arrayCount, eax
	mov ebx, 0

	ParseDwordDec_L1:
	cmp ebx, arrayCount
	jge ParseDwordDec_END

	;read the str
	INVOKE strip_string, splitList[ebx], 1, 0
	mov edx, eax
	INVOKE is_digit, edx
	cmp eax, 0
	je ParseDwordError

	; success, in edx
	INVOKE atodw, edx

	;添加eax到.data的rawdata中
	mov edx, OFFSET data_rawdata
	add edx, dataOffset
	mov DWORD PTR [edx], eax
	
	add dataOffset, 4
	add ebx, 4

	jmp ParseDwordDec_L1
	ParseDwordDec_END:
	ret
	ParseDwordError: ; error
	INVOKE StdOut, OFFSET msg_grammar_err
	INVOKE ExitProcess, 0
ParseDwordDec ENDP

; -------------------------------------------------
ParseDataDec PROC USES eax ebx ecx edx esi
;
; the FileLine is a data declaration, parse this declaration
; ---------------------------------------------------
	LOCAL NameStr:DWORD, arrayCount: DWORD
	mov arrayCount, 0
	; data declaration as some_name BYTE ?
	INVOKE split_string, OFFSET FileLine, 12, 1, 0

	cmp eax, 12 ;see if the size is enough
	jl ParseDataError

	mov ebx, splitList[0] ;value name
	mov edx, splitList[4] ;value type
	mov ecx, splitList[8] ;init value (as a string)

	; ---------------------------- Name
	;INVOKE str_len, ebx
	;cmp eax, 8
	;jl ParseDataDec_pushGLO

	; push to string table first
	;INVOKE pushStringTable, ebx
	;mov DWORD PTR [NameStr+2], ax
	;mov DWORD PTR [NameStr], 0
	INVOKE pushGlobalV, ebx, dataOffset
	;jmp ParseDataDec_Con1

	;ParseDataDec_pushGLO: ; directly push
	;INVOKE pushGlobalV, ebx, dataOffset

	ParseDataDec_Con1:
	; ---------------------------- type
	INVOKE inEnumerate, edx, OFFSET enumerateBYTEType
	cmp eax, 1
	je ParseDataDec_BYTE

	INVOKE inEnumerate, edx, OFFSET enumerateDWORDType
	cmp eax, 1
	je ParseDataDec_DWORD

	jmp ParseDataError

	;--------------------------------- Init
	;|--------byte--------
	ParseDataDec_BYTE:
		INVOKE ParseByteDec, ecx
		jmp ParseDataDec_Con2

	ParseDataDec_DWORD:
		INVOKE ParseDwordDec, ecx
		jmp ParseDataDec_Con2

	ParseDataDec_Con2:
	ret
	ParseDataError:
	INVOKE StdOut, OFFSET msg_grammar_err
	INVOKE ExitProcess, 0
ParseDataDec ENDP

; -------------------------------------------------
ParseOneOpIns PROC USES eax ebx ecx edx esi,
	Operand: PTR BYTE
;
; parser a one op instruction
; --------------------------------------------------
	INVOKE split_string, Operand, 0, 1, 0
	cmp eax, 4
	jne ParseOneOpIns_error

	mov edx, splitList[0]
	INVOKE strip_string, edx, 1, 0
	mov edx, eax

	INVOKE AddOper1, edx
	ret

	ParseOneOpIns_error:
	INVOKE StdOut, OFFET msg_grammar_err
	INVOKE ExitProcess, 0
ParseOneOpIns ENDP

; -------------------------------------------------
; add opers to Instruction
AddOper1 PROC USES eax ebx ecx edx esi,
	oper: PTR BYTE
	mov edx, oper

	INVOKE is_digit, edx
	cmp eax, 1
	je AddOper1_Imm
	INVOKE inEnumerate, edx, enumerateReg
	cmp eax, 1
	je AddOper1_Reg
	jmp AddOper1_Mem

	AddOper1_Imm:
		mov Instruction.operand1_type, 0
		INVOKE atodw, edx
		mov Instruction.operand1_name, eax
		ret

	AddOper1_Reg:
		mov Instruction.operand1_type, 1
		INVOKE str_len, edx
		mov Instruction.operand1_len, eax
		INVOKE str_copy, edx, OFFSET Instruction.operand1_name
		mov eax, 1
		ret
	
	AddOper1_Mem:
		mov Instruction.operand1_type, 2
		INVOKE str_len, edx
		cmp eax, 8
		jl AddOper1_Con2

		mov Instruction.operand1_len, 8
		INVOKE findStringTable, edx
		cmp eax, 0
		je AddOper1_error

		mov esi, OFFSET Instruction.operand1_name
		mov DWORD PTR [esi], 0
		mov DWORD PTR [esi + 4], eax
		ret

		AddOper1_Con2:
		mov Instruction.operand1_len, eax
		INVOKE str_copy, edx, OFFSET Instruction.operand1_name
		ret

	AddOper1_error:
	INVOKE StdOut, OFFET msg_grammar_err
	INVOKE ExitProcess, 0
AddOper1 ENDP
; 
AddOper2 PROC USES eax ebx ecx edx esi,
	oper: PTR BYTE
	mov edx, oper 

	INVOKE is_digit, edx
	cmp eax, 1
	je AddOper2_Imm
	INVOKE inEnumerate, edx, enumerateReg
	cmp eax, 1
	je AddOper2_Reg
	jmp AddOper2_Mem

	AddOper2_Imm:
		mov Instruction.operand2_type, 0
		INVOKE atodw, edx
		mov Instruction.operand2_name, eax
		ret

	AddOper2_Reg:
		mov Instruction.operand2_type, 1
		INVOKE str_len, edx
		mov Instruction.operand2_len, eax
		INVOKE str_copy, edx, OFFSET Instruction.operand2_name
		mov eax, 1
		ret
	
	AddOper2_Mem:
		mov Instruction.operand2_type, 2
		INVOKE str_len, edx
		cmp eax, 8
		jl AddOper2_Con2

		mov Instruction.operand2_len, 8
		INVOKE findStringTable, edx
		cmp eax, 0
		je AddOper2_error

		mov esi, OFFSET Instruction.operand2_name
		mov DWORD PTR [esi], 0
		mov DWORD PTR [esi + 4], eax
		ret

		AddOper2_Con2:
		mov Instruction.operand2_len, eax
		INVOKE str_copy, edx, OFFSET Instruction.operand2_name
		ret

	AddOper2_error:
	INVOKE StdOut, OFFET msg_grammar_err
	INVOKE ExitProcess, 0
AddOper2 ENDP
; --------------------------------------------------

; -------------------------------------------------
ParseTwoOpIns PROC USES eax ebx ecx edx esi,
	Operand: PTR BYTE
;
; parser a two op instruction
; --------------------------------------------------
	INVOKE split_string, Operand, 0, 0, 44
	cmp eax, 8
	jne ParseTwoOpIns_error

	mov ebx, splitList[0]
	mov edx, splitList[4]
	INVOKE strip_string, ebx, 1, 0
	mov ebx, eax
	INVOKE strip_string, edx, 1, 0
	mov edx, eax

	INVOKE AddOper1, edx
	INVOKE AddOper2, edx
	ret

	ParseTwoOpIns_error:
	INVOKE StdOut, OFFSET msg_grammar_err
	INVOKE ExitProcess, 0
ParseTwoOpIns ENDP

;-------------------------------------------------------
ParseTextStr PROC USES eax ebx ecx edx esi
;
; the FileLine is a code line, parser this line
;-------------------------------------------------------
	mov edx, OFFSET FileLine
	INVOKE str_count, edx, 58
	cmp eax, 1
	jg ParseTextError
	cmp eax, 1
	jl ParseTextStr_Con1 ; no label occur

	INVOKE split_string, edx, 8, 0, 58
	push eax

	mov ebx, splitList[0]
	mov edx, splitList[4] ; pass the label

	; check if label has space in it
	INVOKE str_count, ebx, 32
	cmp eax, 0
	jne ParseTextError
	INVOKE str_count, ebx, 9
	cmp eax, 0
	jne ParseTextError
	
	;done: add the label to the label table
	INVOKE pushLabel, ebx

	pop eax
	cmp eax, 4
	je ParseTextStr_END

	ParseTextStr_Con1: ; finish label, edx has the string
	INVOKE split_string, edx, 8, 1, 0

	;see the operation
	mov ebx, splitList[0]
	mov edx, splitList[4]
	INVOKE inEnumerate, ebx, ADDR enumerateJump
	cmp eax, 1
	je ParseTextStr_jump
	INVOKE inEnumerate, ebx, ADDR enumerateCall
	cmp eax, 1
	je ParseTextStr_call
	INVOKE inEnumerate, ebx, ADDR enumerateOneInstruction
	cmp eax, 1
	je ParseTextStr_one
	INVOKE inEnumerate, ebx, ADDR enumerateTwoInstruction
	cmp eax, 1
	je ParseTextStr_two
	jmp ParseTextError


	ParseTextStr_one:
	INVOKE str_len, ebx
	mov Instruction.operation_len, eax
	INVOKE str_copy, ebx, ADDR Instruction.operation_str
	INVOKE ParseOneOpIns, edx
	jmp ParseTextStr_END

	ParseTextStr_two:
	INVOKE str_len, ebx
	mov Instruction.operation_len, eax
	INVOKE str_copy, ebx, ADDR Instruction.operation_str
	INVOKE ParseTwoOpIns, edx
	jmp ParseTextStr_END

	ParseTextStr_jump:
	INVOKE str_len, ebx
	mov Instruction.operation_len, eax
	INVOKE str_copy, ebx, ADDR Instruction.operation_str
	INVOKE ParseJump, edx

	jmp ParseTextStr_END

	ParseTextStr_call:
	INVOKE str_len, ebx
	mov Instruction.operation_len, eax
	INVOKE str_copy, ebx, ADDR Instruction.operation_str
	INVOKE ParseCall, edx
	jmp ParseTextStr_END

	ParseTextStr_END:
	ret
	ParseTextError:
	INVOKE StdOut, OFFSET msg_grammar_err
	INVOKE ExitProcess, 0
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
	INVOKE split_string, ADDR FileLine, 8, 1, 0

	mov edx, splitList[0]
	INVOKE inEnumerate, edx, OFFSET enumerateInclude
	cmp eax, 1
	je MainParser_include
	
	INVOKE inEnumerate, edx, OFFSET enumerateIncludeLib
	cmp eax, 1
	je MainParser_includelib

	INVOKE str_cmp, edx, enumerateCode
	je MainParser_code
	INVOKe str_cmp, edx, enumerateData
	je MainParser_data

	jmp MainParser_Parsering

	MainParser_include:
	-------------------------------Not implemented----------------------

	MainParser_includelib:
	-------------------------------Not implemented----------------------

	MainParser_code:
	mov whichSection, 2
	jmp MainParser_Parsering

	MainParser_data:
	mov whichSection, 1
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