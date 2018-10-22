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

include global.inc


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
;|                  r32                 |     101    |
;|                  m32                 |     110    |
;|                 imm32                |     111    |
;|                Fun PROC              |     1000   |
;|                Fun ENDP              |     1001   |
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
;----INSTRUCTION TABLE
instruction_table_name BYTE "instruction_table.txt",0
instruction_table BYTE 256 DUP(?)

;----OPCODE TABLE
opcode_table_name BYTE "opcode_table.txt",0
opcode_table BYTE 2015 DUP(?)

;----REGISTER TABLE
;register_name BYTE 4 DUP (0)
;register_id	  DWORD ?

register_table BYTE 80 DUP (?)
eax_str byte "EAX",0
ecx_str byte "ECX",0
edx_str byte "EDX",0
ebx_str byte "EBX",0
esp_str byte "ESP",0
ebp_str byte "EBP",0
esi_str byte "ESI",0
edi_str byte "EDI",0

;KEY INFORMATION
text_ptr DWORD offset text_rawdata
section_name BYTE ".text",0,0,0

;HANDLERS
open_table_file_handler DWORD ?

;INFO
error BYTE "ERROR!",0dh,0ah,0
success BYTE "SUCCESS",0
program_end BYTE "Press any key to continue...",0dh,0ah,0
input_char BYTE ?
fun_load_tables BYTE "...FUNCTION LOAD_TABLES...",0dh,0ah,0
fun_code_translation BYTE "...FUNCTION CODE_TRANSLATION...",0dh,0ah,0
fun_r32_r32 BYTE "...FUNCTION R32_R32...",0dh,0ah,0
fun_get_opcode BYTE "...FUNCTION GET_OPCODE...",0dh,0ah,0
fun_r32_id BYTE "...FUNCTION R32_ID...",0dh,0ah,0
integer BYTE "%x",0dh,0ah,0
char BYTE "%c",0dh,0ah,0


.code

;----------------------------------------------
load_tables PROC
;functions:load tables into memory
;receive:None
;return:None
;----------------------------------------------
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
	;RECOGNITION OF THE END OF INSTRUCTION TABLE
	mov eax,InstructionEnd
	mov dword ptr [edi],eax
	
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
	;INVOKE StdOut,esi

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

;CLOSE OPCODE TABLE FILE
	invoke CloseHandle,open_table_file_handler

;LOADING REGISTER TABLE	
	mov esi,offset eax_str
	mov ebx,0
	mov edi,offset register_table
L_register:
	cld
	mov ecx,4
	rep movsb
	mov dword ptr [edi],ebx
	add edi,4
	inc ebx
	cmp ebx,8
	jb L_register

	;ALL OPERATION FINISHED
	jmp L2
L1:
	INVOKE StdOut, ADDR error
L2:
	popad
	ret
L_set_pointer:
	;SET INSTRUCTION TABLE POINTER 
	pop edx
	mov eax,dword ptr [edx]
	add edx,eax
	add edx,4
	mov dword ptr [edx],edi
	add edx,4
	push edx
	jmp L_set_table_pointer_finished
load_tables ENDP


;-----------------------------------------
fill_text_section_header PROC,
				ssize:DWORD,
				relocation_number:DWORD,
				line_number:DWORD
;function: fill the section header of text
;receiver: size,relocation number,line number
;return: None
;-----------------------------------------
	 pushad
	;SECTION NAME
	mov edi, offset SectionHeader[0].s_name
	mov esi, offset section_name
	cld
	mov ecx,8
	rep movsb

	;SECTION SIZE
	mov eax,ssize
	mov SectionHeader[0].s_size,eax
	;NUMBER OF RELOCATION TABLE ENTRIES
	mov eax,relocation_number
	mov SectionHeader[0].s_nreloc,ax
	;NUMBER OF LINE NUMBER TABLE ENTRIES
	mov eax,line_number
	mov SectionHeader[0].s_nlnno,ax
	
	;DEBUG
	;invoke StdOut, addr SectionHeader[0].s_name
	;invoke crt_printf,addr integer,SectionHeader[0].s_size
	;invoke crt_printf,addr integer,SectionHeader[0].s_nreloc
	;invoke crt_printf,addr integer,SectionHeader[0].s_nlnno
	popad
	ret
fill_text_section_header ENDP


;------------------------------------------------
get_opcode PROC,
			op_len:DWORD,
			op_str_ptr:DWORD,
			op_type:DWORD
;function: get opcode from table
;receive:operation type,len,str
;return:eax -- ptr to entry in opcode table
;------------------------------------------------
	invoke StdOut,addr fun_get_opcode
	mov edi,offset instruction_table
	;invoke crt_printf,addr char,byte ptr [edi+4]

	;invoke StdOut,op_str_ptr
L_cmp_entry:
	mov edx,dword ptr [edi]
	cmp edx,op_len
	jne L_next_directly
	mov ecx,edx
	mov esi,op_str_ptr
	add edi,4
	;invoke crt_printf,addr char,byte ptr[edi]
L_cmp_str:
	mov al,byte ptr[edi]
	mov bl,byte ptr[esi]
	inc edi
	inc esi
	cmp al,bl
	loope L_cmp_str

	jnz L_not_pair
	;PAIR
	mov edi,dword ptr [edi]		;operation_table_ptr
	mov eax,op_type
	lea eax,[edi + eax * 8]
	jmp L2
L_not_pair:
	add edi,ecx
	add edi,4
	jmp L_next

L_next_directly:
	add edi,8
	add edi,edx
	jmp L_next

L_next:
	cmp edx,InstructionEnd
	je L1
	jmp L_cmp_entry
L1:
	invoke StdOut,addr error
L2:
	ret
get_opcode ENDP


;------------------------------------------------
r32_id PROC,
		register_str_ptr:DWORD
;function:return id of the register
;receive:ptr of register string(CAPITAL)
;return: eax:id
;------------------------------------------------

	mov edi,offset register_table
	mov ecx,8
L_register_table_entry:
	mov esi,register_str_ptr
	push ecx
	mov ecx,3
L_cmp_str:
	mov al,byte ptr [edi]
	mov bl,byte ptr [esi]
	inc edi
	inc esi
	cmp al,bl
	loope L_cmp_str
	je L_equal
	add edi,ecx
	add edi,5
	pop ecx
	loop L_register_table_entry
	jmp L1
L_equal:
	inc edi
	mov eax,dword ptr [edi]
	jmp L2
L1:
	invoke StdOut,addr error
L2:
	ret
r32_id ENDP



;-------------------------------------------------
r32_r32 PROC
;function: des:r32,source:r32
;receive:instruction struct
;return: None
;-------------------------------------------------
	pushad
	invoke StdOut,addr fun_r32_r32
	invoke get_opcode,Instruction.operation_len,addr Instruction.operation_str,Instruction.operation_type

	;VALIDATE
	movzx ebx,byte ptr [eax + 1] 
	cmp ebx,0
	je L1
	
	;IS VALID
	push eax

	;DESTINATION REGISTER ID
	invoke r32_id,addr Instruction.operand1_name
	push eax
	;SOURCE REGISTER ID
	invoke r32_id,addr Instruction.operand2_name

	;MOD-REG-R/M ---- bl
	pop ebx
	shl ebx,3
	or ebx,eax
	or ebx,0C0h

	;OPCODE ------ dl
	pop eax
	mov dx,word ptr [eax + 6]
	cmp dh,0
	jne L_opcode_two_bytes
	mov line_bytes,2

	mov edi,text_ptr
	mov byte ptr [edi],dl
	inc edi
	mov byte ptr [edi],bl
	jmp L2
L_opcode_two_bytes:
	mov edi,text_ptr
	mov word ptr [edi],dx
	add edi,2
	mov byte ptr [edi],bl
	mov line_bytes,3
	jmp L2
L1:
	invoke StdOut,addr error
L2:
	mov eax,line_bytes
	add current_code_bytes,eax
	add text_ptr,eax
	popad
	ret
r32_r32 ENDP


;-------------------------------------------------
code_translation PROC
;function: code translation
;receive:
;return:
;-------------------------------------------------
	pushad
	invoke StdOut,addr fun_code_translation
	cmp Instruction.operation_type,0
	je L0
	cmp Instruction.operation_type,1
	je L1
	cmp Instruction.operation_type,2
	je L2
	cmp Instruction.operation_type,3
	je L3
	cmp Instruction.operation_type,4
	je L4
	cmp Instruction.operation_type,5
	je L5
	cmp Instruction.operation_type,6
	je L6
	cmp Instruction.operation_type,7
	je L7
	cmp Instruction.operation_type,8
	je L8
	cmp Instruction.operation_type,9
	je L9
	jmp Lerror
L0:
	call r32_r32
	mov edi,offset text_rawdata
	movzx eax,word ptr [edi]
	invoke crt_printf,addr integer,eax
	jmp Lret
L1:

	jmp Lret
L2:

	jmp Lret
L3:

	jmp Lret
L4:

	jmp Lret
L5:

	jmp Lret
L6:

	jmp Lret
L7:

	jmp Lret
L8:

	jmp Lret
L9:

	jmp Lret
Lerror:
	invoke StdOut,addr error
Lret:
	popad
	ret
code_translation ENDP



main PROC

	call load_tables

	;instruction 
	mov Instruction.operation_type,0
	mov Instruction.operation_len,3
	mov edi,offset Instruction.operation_str
	mov byte ptr [edi],"C"
	inc edi
	mov byte ptr [edi],"M"
	inc edi
	mov byte ptr [edi],"P"
	inc edi
	mov byte ptr [edi],0
	mov Instruction.operand1_type,0
	mov Instruction.operand2_type,0
	mov edi,offset Instruction.operand1_name
	mov byte ptr [edi],"E"
	inc edi
	mov byte ptr [edi],"A"
	inc edi
	mov byte ptr [edi],"X"
	inc edi
	mov byte ptr [edi],0
	mov edi,offset Instruction.operand2_name
	mov byte ptr [edi],"E"
	inc edi
	mov byte ptr [edi],"D"
	inc edi
	mov byte ptr [edi],"X"
	inc edi
	mov byte ptr [edi],0




	
	;invoke fill_text_section_header,3,2,1 
	call code_translation


	INVOKE StdOut,ADDR program_end
	INVOKE StdIn,ADDR input_char,1
	INVOKE ExitProcess,0
main ENDP

END main