TITLE Main Parser

; this file is used to parse a asm file

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

include global.inc

Instructionproto STRUCT
	operation_len  DWORD ?
	operation_type DWORD ?
	operation_str  BYTE 4 DUP (?)
	operand1_type  DWORD ?
	operand1_name  BYTE 8 DUP (?)
	operand2_type  DWORD ?
	operand2_name  BYTE 8 DUP (?)
Instructionproto ENDS

.data
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

showGV_title BYTE "The Global Varible:",10,0
showGV_line_deli BYTE "-----------------------",10,0
showGV_line BYTE "%s, %d",10,0
util_d BYTE "%d",10,0
; INVOKE crt_printf, OFFSET util_d, ebx

.code
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

pushString

main PROC
	INVOKE pushGlobalV, ADDR my_name1, my_val1
	INVOKE pushGlobalV, ADDR my_name2, my_val2
	INVOKE pushGlobalV, ADDR my_name3, my_val3
	INVOKE pushGlobalV, ADDR my_name4, my_val4
	INVOKE pushGlobalV, ADDR my_name5, my_val5
	INVOKE showGlobalV
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