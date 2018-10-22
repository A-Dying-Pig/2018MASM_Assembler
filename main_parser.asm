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

Instructionproto STRUCT
    operation_type DWORD ?
    operation_len  DWORD ?	
    operation_str  BYTE 4 DUP (?)
    operand1_type  DWORD ?
    operand1_name  BYTE 8 DUP (?)
	operand1_len   DWORD ?
    operand2_type  DWORD ?
    operand2_name   BYTE 8 DUP (?)
	operand2_len	DWORD ?
Instructionproto ENDS

.data
; ====================================== temp vars ==============================================

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

; ====================================== cmd relative ==========================================

Argsc DWORD ?
ArgsvList DWORD ?
argument BYTE 512 DUP(0)

; ====================================== file relative =========================================
FileHandle DWORD ?
FileLine BYTE 128 DUP(0)

; ====================================== program relative ======================================
NewLine BYTE 13,10,0


.code
; ====================================== utilities for the code ================================
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
	INVOKE str_copy, strp, edx
	mov ebx, in_val
	mov GlobalvSymbolTable[eax].n_value, ebx
	inc GlobalVCount
	ret
pushGlobalV ENDP

;------------------------------------------
pushStringTable PROC USES eax edx,
	strp:PTR BYTE
;
; push a string into the table
;------------------------------------------
	mov edx, DWORD PTR StringTable
	add edx, OFFSET StringTable
	INVOKE str_len, strp
	INVOKE str_copy, strp, edx
	inc eax
	mov edx, DWORD PTR StringTable
	add edx, eax
	mov DWORD PTR StringTable, edx
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
ReadLine PROC USES eax ebx ecx edx esi
;
; read a line from file, put it to FileLine
;----------------------------------------------------
	LOCAL readCount: DWORD
	mov esi, OFFSET FileLine
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
	jmp ReadLine_LOOP

	ReadLine_END:
	mov al, 0
	mov [esi], al
	ret
ReadLine ENDP

; ======================================== MAIN PROC ==============================================

main PROC
	INVOKE InitParser
	INVOKE OpenCmdFile
	
	INVOKE ReadLine
	INVOKE StdOut, OFFSET FileLine
	mov ebx, eax
	INVOKE StdOut, OFFSET NewLine
	INVOKE crt_printf, OFFSET util_d, ebx
	
	INVOKE ReadLine
	INVOKE StdOut, OFFSET FileLine
	mov ebx, eax
	INVOKE StdOut, OFFSET NewLine
	INVOKE crt_printf, OFFSET util_d, ebx

	INVOKE ReadLine
	INVOKE StdOut, OFFSET FileLine
	mov ebx, eax
	INVOKE StdOut, OFFSET NewLine
	INVOKE crt_printf, OFFSET util_d, ebx

	INVOKE ReadLine
	INVOKE StdOut, OFFSET FileLine
	mov ebx, eax
	INVOKE StdOut, OFFSET NewLine
	INVOKE crt_printf, OFFSET util_d, ebx
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