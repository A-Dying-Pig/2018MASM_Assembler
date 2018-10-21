.386
.model flat, stdcall
option casemap : none

include windows.inc
include kernel32.inc
includelib kernel32.lib
include masm32.inc
includelib masm32.lib


include msvcrt.inc
includelib msvcrt.lib

;include masm32rt.inc
;includelib masm32rt.lib


;instruction table structure
;|--------------------------------------------------------------------|
;|LENGTH OF OPERATIONS  |STRING OF OPERATIONS|ADDRESS OF OPCODE TABLE |
;|----------------------|--------------------|------------------------|
;|        DWORD         |       BYTES        |         DWORD          |
;|--------------------------------------------------------------------|

;opcode table structure
;|-----------------------------------------------------------------------------|
;| SITUATION  | VALID  |  LOCK   |O PREFIX |    O    | OPCODE PREFIX |  OPCODE |
;|------------|--------|---------|---------| --------|---------------|---------|
;|    BYTE    |   BYTE |   BYTE  |   BYTE  |   BYTE  |     BYTE      |   WORD  |
;|-----------------------------------------------------------------------------|

;situation
;|---------------------------------------------------|
;|   DESTINATION     |     SOURCE       |    CODE    |
;|-------------------|------------------|------------|
;|       r32         |       r32        |     000    |
;|       r32         |       m32        |     001    |
;|       m32         |       r32        |     010    |
;|       r32         |      imm32       |     011    |
;|       m32         |      imm32       |     100    |
;|		            r32					|	  101	 |
;|					m32					|	  110	 |
;|				   imm32				|	  111	 |
;|---------------------------------------------------|

;opcode extension prefix
;|----------------------------|
;|   2  |   NO OP EXTENSION   |
;|   1  |     REGISTER        |
;|   0  |      NUMBER         |
;|----------------------------|

;opcode prefix
;|----------------------------|
;|  1  |  OPCODE + REGISTER   |
;|  0  |       OPCODE         |
;|----------------------------|

.data
;BUFFER
buffer_size DWORD 2000
buffer BYTE 2000 DUP(?)
bytes_read DWORD ?
;TABLES
instruction_table_name BYTE "instruction_table.txt",0
opcode_table_name BYTE "opcode_table.txt",0
instruction_table BYTE 256 DUP(?)
opcode_table BYTE 2015 DUP(?)

;HANDLERS
open_table_file_handler DWORD ?

;INFO
error BYTE "ERROR!",0
success BYTE "SUCCESS",0
program_end BYTE "Press any key to continue...",0dh,0ah,0
input_char BYTE ?
fun_load_tables BYTE "..FUNCTION LOAD_TABLES",0dh,0ah,0
integer BYTE "%d",0dh,0ah,0
char BYTE "%c",0dh,0ah,0

.code

;function: load tables into memory
load_tables PROC
	pushad
	INVOKE StdOut,ADDR fun_load_tables

	;LOADING INSTRUCTION TABLE
	INVOKE CreateFile,
		ADDR instruction_table_name,
		GENERIC_READ,
		0,
		0,
		OPEN_EXISTING,
		FILE_ATTRIBUTE_NORMAL,
		0
	cmp eax,INVALID_HANDLE_VALUE
	je L1	
	mov open_table_file_handler,eax
	INVOKE ReadFile,
		open_table_file_handler,
		ADDR buffer,
		buffer_size,
		ADDR bytes_read,
		0
	cmp eax,0
	je L1	

	;READING INSTRUCTION TABLE SUCCESSFULLY
	mov esi,OFFSET buffer
	mov edi,OFFSET instruction_table
	mov eax,bytes_read
	mov BYTE PTR [esi + eax - 1],0
	;OUTPUT INSTRUCTION TABLE
L_instruction:	
	mov BYTE PTR [esi+1],0
	;INVOKE StdOut,ADDR buffer
	INVOKE atodw, esi
	mov ecx,eax
	;INVOKE crt_printf,ADDR integer,eax
	add esi,2	
	mov DWORD PTR [edi],eax
	add edi,4
	
L_instruction_str:
	mov al,byte ptr [esi]
	mov byte ptr [edi],al
	;push ecx
	;invoke crt_printf,addr char,byte ptr [edi]
	;pop ecx
	add esi,1
	add edi,1
	loop L_instruction_str
	add esi,1
	add edi,4
	cmp BYTE PTR [esi],0
	jne L_instruction
	
	;CLOSE INSTRUCTION TABLE
	invoke CloseHandle,open_table_file_handler

	;LOADING OPCODE TABLE
	INVOKE CreateFile,
		ADDR opcode_table_name,
		GENERIC_READ,
		0,
		0,
		OPEN_EXISTING,
		FILE_ATTRIBUTE_NORMAL,
		0
	cmp eax,INVALID_HANDLE_VALUE
	je L1	
	mov open_table_file_handler,eax
	INVOKE ReadFile,
		open_table_file_handler,
		ADDR buffer,
		buffer_size,
		ADDR bytes_read,
		0
	cmp eax,0
	je L1
	
	;READING OPCODE TABLE SUCCESSFULLY
	push offset instruction_table
	mov edi,offset opcode_table
	mov esi,offset buffer
	mov ecx,bytes_read
	mov byte ptr [esi + ecx],0
	INVOKE StdOut,esi

L_opcode_table_entry:
	;SITUATION
	mov byte ptr [esi + 1],0
	invoke atodw, esi
	cmp eax,0
	je L_set_pointer
L_set_table_pointer_finished:
	mov byte ptr [edi],al
	inc edi
	add esi,2
	;VALID
	mov byte ptr [esi + 1],0
	invoke atodw, esi
	mov byte ptr [edi],al
	inc edi
	add esi,2
	;LOCK
	mov byte ptr [esi + 1],0
	invoke atodw, esi
	mov byte ptr [edi],al
	inc edi
	add esi,2
	;O PREFIX
	mov byte ptr [esi + 1],0
	invoke atodw, esi
	mov byte ptr [edi],al
	inc edi
	add esi,2
	;OPCODE EXTENSION
	mov byte ptr [esi + 1],0
	invoke atodw, esi
	mov byte ptr [edi],al
	inc edi
	add esi,2
	;OPCODE PREFIX
	mov byte ptr [esi + 1],0
	invoke atodw, esi
	mov byte ptr [edi],al
	inc edi
	add esi,2
	;OPCODE
	mov ecx,esi
L_opcode:
	inc ecx
	cmp byte ptr [ecx],10
	jne L_opcode
	mov byte ptr [ecx],0
	push ecx
	invoke atodw,esi
	mov word ptr[edi],ax
	add edi,2
	pop esi
	inc esi

	cmp byte ptr [esi],0
	jne L_opcode_table_entry	
	jmp L2
L1:
	INVOKE StdOut, ADDR error
L2:
	popad
	ret
L_set_pointer:
	;SET INSTRUCTION TABLE POINTER 
	pop edx
	movzx eax,byte ptr [edx]
	inc eax
	add edx,eax
	mov dword ptr [edx],edi
	add edx,4
	push edx
	jmp L_set_table_pointer_finished

load_tables ENDP

main PROC
	call load_tables 

	INVOKE StdOut,ADDR program_end
	INVOKE StdIn,ADDR input_char,1
	INVOKE ExitProcess,0
main ENDP

END main