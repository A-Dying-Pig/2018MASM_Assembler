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
include codeTranslation.inc

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
;|                rel16/32              |     111    |
;|                Fun PROC              |     1000   |
;|                Fun ENDP              |     1001   |
;|				    ret                 |	  1010   |
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

;-----JUMP TABLE
jump_table_entry STRUCT
	label_name BYTE 8 DUP(?)
	label_len DWORD ?
	label_address_len DWORD ?
	label_address_offset DWORD ?
	label_EIP DWORD ?
jump_table_entry ENDS
jump_table jump_table_entry 100 DUP(<>)
jump_table_count DWORD 0


;KEY INFORMATION
text_ptr DWORD offset text_rawdata
line_code BYTE 30 DUP (?)
section_name BYTE ".text",0,0,0
function_start_offset DWORD 0
function_start_on DWORD 0

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
fun_r32_imm32 BYTE "...FUNCTION R32_IMM32...",0dh,0ah,0
fun_m32_imm32 BYTE "..FUNCTION M32_IMM32...",0dh,0ah,0
fun_get_opcode BYTE "...FUNCTION GET_OPCODE...",0dh,0ah,0
fun_r32_id BYTE "...FUNCTION R32_ID...",0dh,0ah,0
fun_fill_section_header BYTE "...FUNCTION FILL TEXT SECTION HEADER...",0dh,0ah,0
fun_r32_m32 BYTE "...FUNCTION R32_M32...",0dh,0ah,0
fun_m32_r32 BYTE "...FUNCTION M32_R32...",0dh,0ah,0
fun_r32 BYTE "...FUNCTION R32...",0dh,0ah,0
fun_m32 BYTE "...FUNCTION M32...",0dh,0ah,0
fun_definition_start BYTE "...FUNCTION PROC...",0dh,0ah,0
fun_definition_end BYTE "...FUNCTION ENDP...",0dh,0ah,0
fun_rel32 BYTE "...FUNCTION REL32...",0dh,0ah,0
integer BYTE "%x ",0
char BYTE "%c",0dh,0ah,0
relocation_table_is byte "relocation entry: ",0dh,0ah,0
a_relocation_entry byte "%x %x %hd",0dh,0ah,0

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

	mov al,byte ptr [esi]
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
				;line_number:DWORD
;function: fill the section header of text
;receiver: size,relocation number,line number
;return: None
;-----------------------------------------
	pushad
	invoke StdOut,addr fun_fill_section_header
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
	;mov eax,line_number
	;mov SectionHeader[0].s_nlnno,ax
	
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
fill_jump_table_entry PROC,
				label_ptr:DWORD,
				label_len:DWORD,
				address_len:DWORD,
				address_offset:DWORD,
				EIP_offset:DWORD
;function:add a jump table entry to the jump table
;receive:params
;return:None
;-------------------------------------------------
	pushad
	mov edi,offset jump_table	
	mov ecx,jump_table_count
	lea edi,[edi + ecx * 8]
	lea edi,[edi + ecx * 8]
	lea edi,[edi + ecx * 8]
	
	;LABEL LEN
	mov ecx,label_len
	mov dword ptr [edi + 8],ecx
	lea edx,[edi + 12]
	push edx
	;LABEL NAME
	mov esi,label_ptr
	mov ecx,label_len
L_copy_label_name:
	mov al,byte ptr [esi]
	mov byte ptr [edi],al
	inc esi
	inc edi
	loop L_copy_label_name
	;LABEL_ADDRESS_LEN
	pop edi
	mov eax,address_len
	mov dword ptr [edi],eax
	add edi,4
	;LABEL_ADDRESS_OFFSET
	mov eax,address_offset
	mov dword ptr [edi],eax
	add edi,4
	;LABEL EIP
	mov eax,EIP_offset
	mov dword ptr [edi],eax

	inc jump_table_count
	popad
	ret
fill_jump_table_entry ENDP

;------------------------------------------------
fill_relocation_table PROC,
				typeid:DWORD,
				name_ptr:DWORD,
				bytes:DWORD
;function:add an entry to relocation table
;receive:type 0-variable,1-function;name_ptr;bytes
;return:None
;-----------------------------------------------
	cmp typeid,0
	jne L_function
	
	;VARIABLE SYMBOL TABLE
	mov edi,offset GlobalvSymbolTable.n_name
	mov ecx,GlobalVCount
	cmp ecx,0
	je L1

L_v:
	push ecx
	mov esi,name_ptr
	mov ecx,8
L_v_cmp_str:
	mov al, byte ptr [edi]
	mov bl, byte ptr [esi]
	inc edi
	inc esi
	cmp al,bl
	loope L_v_cmp_str
	je L_v_pair
	add edi,ecx
	add edi,20
	pop ecx
	loop L_v
	;NOT PAIRED AT ALL
	jmp L1 
L_v_pair:
	pop ecx
	mov eax,GlobalVCount
	sub eax,ecx
	push eax
	mov ecx,RelocationCount
	mov edi,offset RelocationTable
	cmp ecx,0
	je L_v_in_position
L_v_add:
	add edi,10
	loop L_v_add
L_v_in_position:
	mov eax,bytes
	mov dword ptr [edi],eax
	add edi,4
	pop eax
	mov dword ptr [edi],eax
	add edi,4
	mov word ptr [edi],6h
	inc RelocationCount
	jmp L2

L_function:
	;FUNCTION SYMBOL TABLE
	mov edi,offset CalledFunctionSymbolTable
	mov ecx,CalledFunctionSymbolEntryCount
	cmp ecx,0
	je L1
L_f:
	push ecx
	mov esi,name_ptr
	mov ecx,8
L_f_cmp_str:
	mov al, byte ptr [edi]
	mov bl, byte ptr [esi]
	inc edi
	inc esi
	cmp al,bl
	loope L_f_cmp_str
	je L_f_pair
	add edi,ecx
	add edi,20
	pop ecx
	loop L_f
	;NOT PAIRED AT ALL
	jmp L1 
L_f_pair:
	pop ecx
	mov eax,CalledFunctionSymbolEntryCount
	sub eax,ecx
	push eax
	mov ecx,RelocationCount
	mov edi,offset RelocationTable
	cmp ecx,0
	je L_f_in_position
L_f_add:
	add edi,10
	loop L_f_add
L_f_in_position:
	mov eax,bytes
	mov dword ptr [edi],eax
	add edi,4
	pop eax
	mov dword ptr [edi],eax
	add edi,4
	mov word ptr [edi],14h
	inc RelocationCount
	jmp L2
L1:
	invoke StdOut,addr error
L2:
	ret
fill_relocation_table ENDP


;-----------------------------------------------------------------
line_code_to_raw_data PROC
;function:put line code to raw data area,update current_code_bytes
;receive:None
;return:None
;-----------------------------------------------------------------
	;ADD TO TEXT RAW DATA
	mov edi,text_ptr
	mov esi,offset line_code
	mov ecx,line_bytes
L_code_copy:
	mov al,byte ptr [esi]
	mov byte ptr [edi],al
	inc esi
	inc edi
	loop L_code_copy

	;UPDATE current_code_bytes
	mov eax,line_bytes
	add current_code_bytes,eax
	add text_ptr,eax

	ret
line_code_to_raw_data ENDP



;-------------------------------------------------
r32_r32 PROC
;function: des:r32,source:r32
;receive:instruction struct
;return: None
;-------------------------------------------------
	pushad
	invoke StdOut,addr fun_r32_r32
	invoke get_opcode,Instruction.operation_len,addr Instruction.operation_str,Instruction.operation_type
	push eax

	;VALIDATE
	movzx ebx,byte ptr [eax + 1] 
	cmp ebx,0
	je L0
	
	;IS VALID
	mov line_bytes,0
	
	;LOCK

	;OPCODE ------ dl
	pop eax
	mov dx,word ptr [eax + 6]
	cmp dh,0
	jne L_opcode_two_bytes
	mov esi,offset line_code
	mov ebx,line_bytes
	mov byte ptr [esi + ebx],dl
	add line_bytes,1
	jmp L_MOD

L_opcode_two_bytes:
	mov ebx,line_bytes
	mov word ptr [esi + ebx],dx
	add line_bytes,2
	jmp L_MOD
L_MOD:
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

	mov esi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [esi + ecx],bl
	inc line_bytes
	jmp L2
L0:
	pop eax	
L1:
	invoke StdOut,addr error
L2:
	call line_code_to_raw_data
	popad
	ret
r32_r32 ENDP


;------------------------------------------------
r32_imm32 PROC
;function:destination:r32 source:imm32
;receive:instruction struct
;return:None
;------------------------------------------------
	pushad
	invoke StdOut,addr fun_r32_imm32
	invoke get_opcode,Instruction.operation_len,addr Instruction.operation_str,Instruction.operation_type
	push eax

	;VALIDATE 
	movzx ebx,byte ptr [eax + 1]
	cmp ebx,0
	je L0

	;IS VALID
	mov line_bytes,0

	;LOCK PREFIX

	;OPCODE
	pop esi
	mov dx,word ptr [esi+6]
	push esi
	invoke r32_id,addr Instruction.operand1_name
	pop esi
	push esi
	push eax
	movzx edx,byte ptr [esi + 5]
	cmp edx,1
	je L_prefix_1_opcode
	movzx ebx,word ptr [esi + 6]
	push ebx
	jmp L_opcode_byte
L_prefix_1_opcode:
	movzx ebx,word ptr [esi + 6]
	pop eax
	add ebx,eax
	push eax
	push ebx
	jmp L_opcode_byte
L_opcode_byte:
	pop ebx
	cmp bh,0
	jne L_opcode_two_bytes
	mov esi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [esi + ecx],bl
	add line_bytes,1
	jmp L_MOD
L_opcode_two_bytes:
	mov esi,offset line_code
	mov ecx,line_bytes
	mov word ptr [esi + ecx],bx
	add line_bytes,2
	jmp L_MOD

L_MOD:
	pop edx
	pop esi
	push edx
	movzx eax, byte ptr [esi + 3]
	cmp eax,2
	je L_MOD_NONE
	cmp eax,0
	je L_MOD_NUMBER
	jmp L0
L_MOD_NUMBER:
	movzx eax, byte ptr [esi + 4]
	pop ebx
	shl eax,3
	or ebx,eax
	or ebx,0C0h
	mov esi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [esi + ecx],bl
	add line_bytes,1
	jmp L_DIS_OR_IMM
L_MOD_NONE:
	pop ebx
	jmp L_DIS_OR_IMM
L_DIS_OR_IMM:
	;IS A NUMBER DIRECTLY
	cmp Instruction.operand2_type,2
	je L_NUMBER 
	;IS OFFSET ---- RELOCATION TABLE
	cmp Instruction.operand2_type,1
	je L_OFFSET
L_NUMBER:
	mov esi,offset line_code
	mov ecx,line_bytes
	mov edx,offset Instruction.operand2_name
	mov eax,dword ptr [edx]
	mov dword ptr [esi + ecx],eax
	add line_bytes,4
	jmp L2
L_OFFSET:
	;TO DO
	jmp L2
L0:
	pop eax
L1:
	invoke StdOut,addr error
L2:
	call line_code_to_raw_data
	popad
	ret
r32_imm32 ENDP


;-------------------------------------------------
m32_imm32 PROC
;function:destination:memory32 ,source:imm32
;receive: Instruction struct
;return:None
;-------------------------------------------------
	pushad
	invoke StdOut,addr fun_m32_imm32
	invoke get_opcode,Instruction.operation_len,addr Instruction.operation_str,Instruction.operation_type
	push eax
	
	;VALIDATE
	movzx ebx,byte ptr [eax + 1] 
	cmp ebx,0
	je L0
	
	;IS VALID
	mov line_bytes,0
	
	;LOCK PREFIX

	;OPCODE
	pop esi
	push esi
	movzx edx,byte ptr [esi + 5]
	cmp edx,1
	je L0
	movzx ebx,word ptr [esi + 6]
L_opcode_byte:
	cmp bh,0
	jne L_opcode_two_bytes
	mov esi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [esi + ecx],bl
	add line_bytes,1
	jmp L_MOD
L_opcode_two_bytes:
	mov esi,offset line_code
	mov ecx,line_bytes
	mov word ptr [esi + ecx],bx
	add line_bytes,2

L_MOD:
	pop esi
	movzx eax, byte ptr [esi + 4]
	shl eax,3
	push eax

	mov eax,Instruction.operand1_type
	cmp eax,1
	je L_VAR
	;TODO
	jmp L0
L_VAR:
	;MOD -- 32 DISPLACEMENT ONLY  - al
	pop eax
	or eax,5h
	mov edi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [edi + ecx],al
	add line_bytes,1
	;DISPLACEMENT -- RELOCATION TABLE
	mov ecx,current_code_bytes
	add ecx,line_bytes
	invoke fill_relocation_table,0,addr Instruction.operand1_name,ecx
	mov edi,offset line_code
	mov ecx,line_bytes
	mov dword ptr [edi + ecx],0
	add line_bytes,4
	;IMM32
	mov edx,offset Instruction.operand2_name
	mov eax,dword ptr [edx]
	mov edi,offset line_code
	mov ecx,line_bytes
	mov dword ptr [edi + ecx],eax
	add line_bytes,4
	
	jmp L2
L_SIB:
	;TO DO
	jmp L2

L0:
	pop eax
L1:
	invoke StdOut,addr error
L2:
	call line_code_to_raw_data
	popad
	ret
m32_imm32 ENDP




;-------------------------------------------------
r32_m32 PROC
;function:destination:r32,source:m32
;receive:Instruction struct
;return:None
;-------------------------------------------------
	pushad
	invoke StdOut,addr fun_r32_m32
	invoke get_opcode,Instruction.operation_len,addr Instruction.operation_str,Instruction.operation_type
	push eax
	
	;VALIDATE
	movzx ebx,byte ptr [eax + 1] 
	cmp ebx,0
	je L0
	
	;IS VALID
	mov line_bytes,0
	
	;LOCK PREFIX

	;OPCODE
	pop esi
	push esi
	movzx edx,byte ptr [esi + 5]
	cmp edx,1
	je L0
	movzx ebx,word ptr [esi + 6]
L_opcode_byte:
	cmp bh,0
	jne L_opcode_two_bytes
	mov esi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [esi + ecx],bl
	add line_bytes,1
	jmp L_MOD
L_opcode_two_bytes:
	mov esi,offset line_code
	mov ecx,line_bytes
	mov word ptr [esi + ecx],bx
	add line_bytes,2

L_MOD:
	pop esi
	invoke r32_id,addr Instruction.operand1_name
	shl eax,3
	push eax

	mov eax,Instruction.operand2_type
	cmp eax,1
	je L_VAR
	;TODO
	jmp L0
L_VAR:
	;MOD -- 32 DISPLACEMENT ONLY  - al
	pop eax
	or eax,5h
	mov edi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [edi + ecx],al
	add line_bytes,1
	;DISPLACEMENT -- RELOCATION TABLE
	mov ecx,current_code_bytes
	add ecx,line_bytes
	invoke fill_relocation_table,0,addr Instruction.operand2_name,ecx
	mov edi,offset line_code
	mov ecx,line_bytes
	mov dword ptr [edi + ecx],0
	add line_bytes,4
	
	jmp L2
L_SIB:
	;TO DO
	jmp L2
L0:
	pop eax
L1:
	invoke StdOut,addr error
L2:
	call line_code_to_raw_data
	popad
	ret
r32_m32 ENDP


;------------------------------------------------
m32_r32 PROC
;function:destination:m32,source r32
;receive:Instruction struct
;return:None
;-----------------------------------------------
	pushad
	invoke StdOut,addr fun_m32_r32
	invoke get_opcode,Instruction.operation_len,addr Instruction.operation_str,Instruction.operation_type
	push eax
	
	;VALIDATE
	movzx ebx,byte ptr [eax + 1] 
	cmp ebx,0
	je L0
	
	;IS VALID
	mov line_bytes,0
	
	;LOCK PREFIX

	;OPCODE
	pop esi
	push esi
	movzx edx,byte ptr [esi + 5]
	cmp edx,1
	je L0
	movzx ebx,word ptr [esi + 6]
L_opcode_byte:
	cmp bh,0
	jne L_opcode_two_bytes
	mov esi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [esi + ecx],bl
	add line_bytes,1
	jmp L_MOD
L_opcode_two_bytes:
	mov esi,offset line_code
	mov ecx,line_bytes
	mov word ptr [esi + ecx],bx
	add line_bytes,2

L_MOD:
	pop esi
	invoke r32_id,addr Instruction.operand2_name
	shl eax,3
	push eax

	mov eax,Instruction.operand1_type
	cmp eax,1
	je L_VAR
	;TODO
	jmp L0
L_VAR:
	;MOD -- 32 DISPLACEMENT ONLY  - al
	pop eax
	or eax,5h
	mov edi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [edi + ecx],al
	add line_bytes,1
	;DISPLACEMENT -- RELOCATION TABLE
	mov ecx,current_code_bytes
	add ecx,line_bytes
	invoke fill_relocation_table,0,addr Instruction.operand1_name,ecx
	mov edi,offset line_code
	mov ecx,line_bytes
	mov dword ptr [edi + ecx],0
	add line_bytes,4
	
	jmp L2
L_SIB:
	;TO DO
	jmp L2

L0:
	pop eax
L1:
	invoke StdOut,addr error
L2:
	call line_code_to_raw_data
	popad
	ret

m32_r32 ENDP


;------------------------------------------------
r32 PROC
;function:one operand-r32
;receive:Instruction struct
;return:None
;------------------------------------------------
	pushad
	invoke StdOut,addr fun_r32
	invoke get_opcode,Instruction.operation_len,addr Instruction.operation_str,Instruction.operation_type
	push eax

	;VALIDATE 
	movzx ebx,byte ptr [eax + 1]
	cmp ebx,0
	je L0

	;IS VALID
	mov line_bytes,0

	;LOCK PREFIX

	;OPCODE
	pop esi
	mov dx,word ptr [esi+6]
	push esi
	invoke r32_id,addr Instruction.operand1_name
	pop esi
	push esi
	push eax
	movzx edx,byte ptr [esi + 5]
	cmp edx,1
	je L_prefix_1_opcode
	movzx ebx,word ptr [esi + 6]
	push ebx
	jmp L_opcode_byte
L_prefix_1_opcode:
	movzx ebx,word ptr [esi + 6]
	pop eax
	add ebx,eax
	push eax
	push ebx
	jmp L_opcode_byte
L_opcode_byte:
	pop ebx
	cmp bh,0
	jne L_opcode_two_bytes
	mov esi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [esi + ecx],bl
	add line_bytes,1
	jmp L_MOD
L_opcode_two_bytes:
	mov esi,offset line_code
	mov ecx,line_bytes
	mov word ptr [esi + ecx],bx
	add line_bytes,2
	jmp L_MOD

L_MOD:
	pop edx
	pop esi
	push edx
	movzx eax, byte ptr [esi + 3]
	cmp eax,2
	je L_MOD_NONE
	cmp eax,0
	je L_MOD_NUMBER
	jmp L0
L_MOD_NUMBER:
	movzx eax, byte ptr [esi + 4]
	pop ebx
	shl eax,3
	or ebx,eax
	or ebx,0C0h
	mov esi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [esi + ecx],bl
	add line_bytes,1
	jmp L2
L_MOD_NONE:
	pop ebx
	jmp L2

L0:
	pop eax
L1:
	invoke StdOut,addr error
L2:
	call line_code_to_raw_data
	popad
	ret
r32 ENDP


;------------------------------------------------
m32 PROC
;function:one operand;m32
;receive:Instruction struct
;return:None
;------------------------------------------------
	pushad
	invoke StdOut,addr fun_m32
	invoke get_opcode,Instruction.operation_len,addr Instruction.operation_str,Instruction.operation_type
	push eax
	
	;VALIDATE
	movzx ebx,byte ptr [eax + 1] 
	cmp ebx,0
	je L0
	
	;IS VALID
	mov line_bytes,0
	
	;LOCK PREFIX

	;OPCODE
	pop esi
	push esi
	movzx edx,byte ptr [esi + 5]
	cmp edx,1
	je L0
	movzx ebx,word ptr [esi + 6]
L_opcode_byte:
	cmp bh,0
	jne L_opcode_two_bytes
	mov esi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [esi + ecx],bl
	add line_bytes,1
	jmp L_MOD
L_opcode_two_bytes:
	mov esi,offset line_code
	mov ecx,line_bytes
	mov word ptr [esi + ecx],bx
	add line_bytes,2

L_MOD:
	pop esi
	movzx eax, byte ptr [esi + 4]
	shl eax,3
	push eax

	mov eax,Instruction.operand1_type
	cmp eax,1
	je L_VAR
	;TODO
	jmp L0
L_VAR:
	;MOD -- 32 DISPLACEMENT ONLY  - al
	pop eax
	or eax,5h
	mov edi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [edi + ecx],al
	add line_bytes,1
	;DISPLACEMENT -- RELOCATION TABLE
	mov ecx,current_code_bytes
	add ecx,line_bytes
	invoke fill_relocation_table,0,addr Instruction.operand1_name,ecx
	mov edi,offset line_code
	mov ecx,line_bytes
	mov dword ptr [edi + ecx],0
	add line_bytes,4
	
	jmp L2
L_SIB:
	;TO DO
	jmp L2

L0:
	pop eax
L1:
	invoke StdOut,addr error
L2:
	call line_code_to_raw_data
	popad
	ret
m32 ENDP


;------------------------------------------------
rel32  PROC
;function: one operand:rel32
;receive:Instruction struct
;return:None
;-------------------------------------------------
	pushad
	invoke StdOut,addr fun_rel32
	invoke get_opcode,Instruction.operation_len,addr Instruction.operation_str,Instruction.operation_type
	push eax

	;VALIDATE 
	movzx ebx,byte ptr [eax + 1]
	cmp ebx,0
	je L0

	;IS VALID
	mov line_bytes,0

	;OPCODE
	pop esi
	movzx ebx,word ptr [esi + 6]
L_opcode_byte:
	cmp bh,0
	jne L_opcode_two_bytes
	mov esi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [esi + ecx],bl
	add line_bytes,1
	jmp L_jump
L_opcode_two_bytes:
	mov esi,offset line_code
	mov ecx,line_bytes
	mov word ptr [esi + ecx],bx
	add line_bytes,2
	jmp L_jump

L_jump:
	;JUDGE JUMP TYPE
	mov esi,offset Instruction.operation_str
	mov al,byte ptr [esi]
	cmp al,"L"

	je L_is_loop
	;fill jump table -- jmp/call
	mov esi,offset Instruction.operand1_name
	mov ecx,Instruction.operand1_len
	mov eax, current_code_bytes
	add eax,line_bytes
	mov ebx,eax
	add ebx,4
	invoke fill_jump_table_entry,esi,ecx,4,eax,ebx
	mov esi,offset line_code
	mov ecx,line_bytes
	mov dword ptr [esi + ecx],0
	add line_bytes,4
	jmp L2
L_is_loop:
	;fill jump table -- loops
	mov esi,offset Instruction.operand1_name
	mov ecx,Instruction.operand1_len
	mov eax, current_code_bytes
	add eax,line_bytes
	mov ebx,eax
	add ebx,1
	invoke fill_jump_table_entry,esi,ecx,1,eax,ebx
	mov esi,offset line_code
	mov ecx,line_bytes
	mov byte ptr [esi + ecx],0
	add line_bytes,1
	jmp L2

L0:
	pop eax
L1:
	invoke StdOut,addr error
L2:
	call line_code_to_raw_data
	popad
	ret
rel32 ENDP

;-----------------------------------------------
update_jump_bytes PROC
;function:update jump bytes
;receive:jump_table,label_table
;return:None
;-----------------------------------------------
	pushad

	mov ecx,jump_table_count
	cmp ecx,0
	je L2

	mov esi,offset jump_table
L_jump_table_entry:
	push ecx
	push esi
	mov edi,offset LabelTable

	mov ecx,LabelCount
	cmp ecx,0
	je L1
L_label_table_entry:
	push ecx
	mov ecx,dword ptr [esi + 8]
	mov edx,9
	sub edx,ecx
L_cmp_str:
	mov al,byte ptr [esi]
	mov bl,byte ptr [edi]
	inc esi
	inc edi
	cmp al,bl
	loope L_cmp_str
	;PAIRED
	je L_paired
	;CURRENT LABEL ENTRY NOT PAIR
	add edi,edx
	add edi,ecx
	add edi,4
	pop ecx
	pop esi
	push esi
	loop L_label_table_entry
	;NOT PAIRED
	jmp L_not_in_label_table
L_jump_table_next:
	pop esi
	add esi,24
	pop ecx
	loop L_jump_table_entry
	jmp L2

L_paired:
	add edi,edx
	mov edx,dword ptr [edi]
	pop ecx
	pop esi
	push esi
	mov eax, dword ptr [esi + 12]
	cmp eax,4
	jne L_one_byte
	mov eax,dword ptr [esi + 16]
	mov ebx,dword ptr [esi + 20]
	sub edx,ebx
	mov edi,offset text_rawdata
	mov dword ptr [edi + eax],edx
	jmp L_jump_table_next
L_one_byte:
	mov eax,dword ptr [esi + 16]
	mov ebx,dword ptr [esi + 20]
	sub edx,ebx
	cmp edx,-128
	jl L1
	cmp edx,127
	jg L1
	mov edi,offset text_rawdata
	mov byte ptr [edi + eax],dl
	jmp L_jump_table_next
L_not_in_label_table:
	pop esi
	push esi
	invoke fill_relocation_table,1,esi,dword ptr [esi + 16]
	jmp L_jump_table_next
L1:
	invoke StdOut,addr error
L2:
	popad
	ret
update_jump_bytes ENDP

;-----------------------------------------------
search_function_table PROC,
				name_ptr:DWORD
;function:search in function table
;receive: name ptr
;return: eax - ptr
;-----------------------------------------------
	mov esi,offset FunctionInfoTable
	mov ecx,FunctionInfoCount
	cmp ecx,0
	je L1

L_function_info_entry:
	push ecx
	mov edi,name_ptr
	mov ecx,8
L_cmp_str:
	mov al,byte ptr [edi]
	mov bl,byte ptr [esi]
	inc edi
	inc esi
	cmp al,bl
	loope L_cmp_str
	je L_paired
	add esi,ecx
	add esi,20
	pop ecx
	loop L_function_info_entry

	jmp L1
L_paired:
	pop ecx
	sub esi,8
	mov eax,esi
	jmp L2
L1:
	invoke StdOut,addr error
L2:
	ret
search_function_table ENDP


;------------------------------------------------
function_definition_start PROC
;function: function PROC
;receive: Instruction struct
;return;None
;-----------------------------------------------
	pushad
	mov line_bytes,0
	cmp function_start_on,1
	je L1
	mov function_start_on,1
	invoke StdOut,addr fun_definition_start
	invoke search_function_table,addr Instruction.operation_str
	mov edx,current_code_bytes
	mov dword ptr [eax + 8],edx
	mov function_start_offset,edx
	jmp L2
L1:
	invoke StdOut,addr error
L2:
	popad
	ret
function_definition_start ENDP

;---------------------------------------------------
function_definition_end PROC
;function: function ENDP
;receive: Instruction struct
;return:None
;---------------------------------------------------
	pushad
	cmp function_start_on,1
	jne L1
	mov function_start_on,0
	mov line_bytes,0
	invoke StdOut,addr fun_definition_end
	invoke search_function_table,addr Instruction.operation_str
	mov edx,current_code_bytes
	mov ebx,function_start_offset
	sub edx,ebx
	mov dword ptr [eax + 12],edx
	jmp L2
L1:
	invoke StdOut,addr error
L2:
	popad
	ret
function_definition_end ENDP

;-----------------------------------------------
function_return PROC
;function:Translate 'ret'
;receive:None
;return:None
;-----------------------------------------------
	pushad

	mov line_bytes,1
	mov edi,offset line_code
	mov ebx,0C3h
	mov byte ptr [edi],bl
	call line_code_to_raw_data

	popad
	ret
function_return ENDP

;------------------------------------------------
show_relocation_table PROC
;function:show relocation entry
;receive:None
;return:None
;------------------------------------------------
L_relocation:
	;SHOW RELOCATION TABLE
	invoke StdOut,addr relocation_table_is
	mov ecx,RelocationCount
	cmp ecx,0
	je L2
	mov edi,offset RelocationTable
L_relocation_entry:
	push ecx
	push edi
	invoke crt_printf,addr a_relocation_entry,dword ptr [edi],dword ptr [edi + 4],word ptr [edi + 8]
	pop edi
	add edi,10
	pop ecx
	loop L_relocation_entry
L2:
	ret
show_relocation_table ENDP


;-------------------------------------------------
show_line_code PROC
;function: display machine code of the line 
;receive:None
;return:None
;------------------------------------------------
	;SHOW MACHINE CODES
	mov edi,offset text_rawdata
	mov ebx,current_code_bytes
	mov ecx,line_bytes
	cmp ecx,0
	je L_re
	sub ebx,ecx
L_show:
	movzx edx,byte ptr [edi + ebx]
	inc ebx
	pushad
	invoke crt_printf,addr integer,edx
	popad
	loop L_show
	invoke crt_printf,addr char,13

L_re:
	;SHOW RELOCATION TABLE
	call show_relocation_table
L2:
	ret
show_line_code ENDP


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
	cmp Instruction.operation_type,10
	je L10
	jmp Lerror
L0:
	call r32_r32
	jmp Lret
L1:
	call r32_m32
	jmp Lret
L2:
	call m32_r32
	jmp Lret
L3:
	call r32_imm32
	jmp Lret
L4:
	call m32_imm32
	jmp Lret
L5:
	call r32
	jmp Lret
L6:
	call m32
	jmp Lret
L7:
	call rel32
	jmp Lret
L8:
	call function_definition_start
	jmp Lret
L9:
	call function_definition_end
	jmp Lret
L10:
	call function_return
	jmp Lret
Lerror:
	invoke StdOut,addr error
Lret:
	call show_line_code
	popad
	ret
code_translation ENDP



main PROC

	call load_tables

	;function info entry added
	add FunctionInfoCount,1
	mov edi,offset FunctionInfoTable
	mov byte ptr [edi],"f"
	inc edi
	mov byte ptr [edi],"u"
	inc edi
	mov byte ptr [edi],"n"
	inc edi
	mov byte ptr [edi],"1"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	add FunctionInfoCount,1
	add edi,20
	mov byte ptr [edi],"f"
	inc edi
	mov byte ptr [edi],"u"
	inc edi
	mov byte ptr [edi],"n"
	inc edi
	mov byte ptr [edi],"2"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0

	;function symbol table added
	add CalledFunctionSymbolEntryCount,1
	mov edi,offset CalledFunctionSymbolTable
	mov byte ptr [edi],"f"
	inc edi
	mov byte ptr [edi],"u"
	inc edi
	mov byte ptr [edi],"n"
	inc edi
	mov byte ptr [edi],"3"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	add CalledFunctionSymbolEntryCount,1
	add edi,20
	mov byte ptr [edi],"f"
	inc edi
	mov byte ptr [edi],"u"
	inc edi
	mov byte ptr [edi],"n"
	inc edi
	mov byte ptr [edi],"4"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0

	;add Label Table entry
	mov edi,offset LabelTable
	add LabelCount,1
	mov byte ptr [edi],"L"
	inc edi
	mov byte ptr [edi],"1"
	inc edi
	mov byte ptr [edi],"0"
	inc edi
	mov byte ptr [edi],"0"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	add edi,1
	mov dword ptr [edi],0ah
	add edi,4
	add LabelCount,1
	mov byte ptr [edi],"f"
	inc edi
	mov byte ptr [edi],"u"
	inc edi
	mov byte ptr [edi],"n"
	inc edi
	mov byte ptr [edi],"1"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	add edi,1
	mov dword ptr [edi],1
	add edi,4
	add LabelCount,1
	mov byte ptr [edi],"f"
	inc edi
	mov byte ptr [edi],"u"
	inc edi
	mov byte ptr [edi],"n"
	inc edi
	mov byte ptr [edi],"2"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	add edi,1
	mov dword ptr [edi],10h
	add edi,4

	;test rel32
	mov Instruction.operation_type,7
	mov Instruction.operation_len,3
	mov edi,offset Instruction.operation_str
	mov byte ptr [edi],"J"
	inc edi
	mov byte ptr [edi],"M"
	inc edi
	mov byte ptr [edi],"P"
	inc edi
	mov byte ptr [edi],0
	mov Instruction.operand1_len,2
	mov edi,offset Instruction.operand1_name
	mov byte ptr [edi],"L"
	inc edi
	mov byte ptr [edi],"1"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	call code_translation

	mov Instruction.operation_type,7
	mov Instruction.operation_len,4
	mov edi,offset Instruction.operation_str
	mov byte ptr [edi],"L"
	inc edi
	mov byte ptr [edi],"O"
	inc edi
	mov byte ptr [edi],"O"
	inc edi
	mov byte ptr [edi],"P"
	mov Instruction.operand1_len,4
	mov edi,offset Instruction.operand1_name
	mov byte ptr [edi],"f"
	inc edi
	mov byte ptr [edi],"u"
	inc edi
	mov byte ptr [edi],"n"
	inc edi
	mov byte ptr [edi],"2"
	inc edi
	call code_translation

	mov Instruction.operation_type,7
	mov Instruction.operation_len,4
	mov edi,offset Instruction.operation_str
	mov byte ptr [edi],"C"
	inc edi
	mov byte ptr [edi],"A"
	inc edi
	mov byte ptr [edi],"L"
	inc edi
	mov byte ptr [edi],"L"
	mov Instruction.operand1_len,4
	mov edi,offset Instruction.operand1_name
	mov byte ptr [edi],"f"
	inc edi
	mov byte ptr [edi],"u"
	inc edi
	mov byte ptr [edi],"n"
	inc edi
	mov byte ptr [edi],"1"
	inc edi
	call code_translation

		mov Instruction.operation_type,7
	mov Instruction.operation_len,4
	mov edi,offset Instruction.operation_str
	mov byte ptr [edi],"C"
	inc edi
	mov byte ptr [edi],"A"
	inc edi
	mov byte ptr [edi],"L"
	inc edi
	mov byte ptr [edi],"L"
	mov Instruction.operand1_len,4
	mov edi,offset Instruction.operand1_name
	mov byte ptr [edi],"f"
	inc edi
	mov byte ptr [edi],"u"
	inc edi
	mov byte ptr [edi],"n"
	inc edi
	mov byte ptr [edi],"4"
	inc edi
	call code_translation


	;test r32 - r32
	mov Instruction.operation_type,0
	mov Instruction.operation_len,3
	mov edi,offset Instruction.operation_str
	mov byte ptr [edi],"X"
	inc edi
	mov byte ptr [edi],"O"
	inc edi
	mov byte ptr [edi],"R"
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
	call code_translation

	;test PROC
	mov Instruction.operation_type,8
	mov edi,offset Instruction.operation_str
	mov byte ptr [edi],"f"
	inc edi
	mov byte ptr [edi],"u"
	inc edi
	mov byte ptr [edi],"n"
	inc edi
	mov byte ptr [edi],"2"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	call code_translation

	;test r32 imm32 
	mov Instruction.operation_type,3
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
	mov edi,offset Instruction.operand1_name
	mov byte ptr [edi],"E"
	inc edi
	mov byte ptr [edi],"D"
	inc edi
	mov byte ptr [edi],"X"
	inc edi
	mov byte ptr [edi],0
	mov Instruction.operand2_type,2
	mov edi,offset Instruction.operand2_name
	mov dword ptr [edi],0
	call code_translation

	;test m32 imm32
	mov Instruction.operation_type,4
	mov Instruction.operation_len,3
	mov edi,offset Instruction.operation_str
	mov byte ptr [edi],"C"
	inc edi
	mov byte ptr [edi],"M"
	inc edi
	mov byte ptr [edi],"P"
	inc edi
	mov byte ptr [edi],0
	mov Instruction.operand1_type,1
	mov edi,offset Instruction.operand1_name
	mov byte ptr [edi],"w"
	inc edi
	mov byte ptr [edi],"o"
	inc edi
	mov byte ptr [edi],"r"
	inc edi
	mov byte ptr [edi],"k"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	mov Instruction.operand2_type,2
	mov edi,offset Instruction.operand2_name
	mov dword ptr [edi],1h

	add GlobalVCount,1
	mov edi,offset GlobalvSymbolTable
	mov byte ptr [edi],"w"
	inc edi
	mov byte ptr [edi],"o"
	inc edi
	mov byte ptr [edi],"r"
	inc edi
	mov byte ptr [edi],"0"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0

	add GlobalVCount,1
	mov edi,offset GlobalvSymbolTable
	add edi,28
	mov byte ptr [edi],"w"
	inc edi
	mov byte ptr [edi],"o"
	inc edi
	mov byte ptr [edi],"r"
	inc edi
	mov byte ptr [edi],"k"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0

	call code_translation
	
	;test r32_m32
	mov Instruction.operation_type,1
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
	mov edi,offset Instruction.operand1_name
	mov byte ptr [edi],"E"
	inc edi
	mov byte ptr [edi],"D"
	inc edi
	mov byte ptr [edi],"X"
	inc edi
	mov byte ptr [edi],"0"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	mov Instruction.operand2_type,1
	mov edi,offset Instruction.operand2_name
	mov byte ptr [edi],"w"
	inc edi
	mov byte ptr [edi],"o"
	inc edi
	mov byte ptr [edi],"r"
	inc edi
	mov byte ptr [edi],"0"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	
	call code_translation

	;test m32 r32
	mov Instruction.operation_type,2
	mov Instruction.operation_len,3
	mov edi,offset Instruction.operation_str
	mov byte ptr [edi],"C"
	inc edi
	mov byte ptr [edi],"M"
	inc edi
	mov byte ptr [edi],"P"
	inc edi
	mov byte ptr [edi],0
	mov Instruction.operand1_type,1
	mov edi,offset Instruction.operand1_name
	mov byte ptr [edi],"w"
	inc edi
	mov byte ptr [edi],"o"
	inc edi
	mov byte ptr [edi],"r"
	inc edi
	mov byte ptr [edi],"0"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	mov Instruction.operand2_type,0
	mov edi,offset Instruction.operand2_name
	mov byte ptr [edi],"E"
	inc edi
	mov byte ptr [edi],"D"
	inc edi
	mov byte ptr [edi],"X"
	inc edi
	mov byte ptr [edi],"0"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	call code_translation

	;test r32
	mov Instruction.operation_type,5
	mov Instruction.operation_len,3
	mov edi,offset Instruction.operation_str
	mov byte ptr [edi],"N"
	inc edi
	mov byte ptr [edi],"E"
	inc edi
	mov byte ptr [edi],"G"
	inc edi
	mov byte ptr [edi],"0"
	mov Instruction.operand1_type,0
	mov edi,offset Instruction.operand1_name
	mov byte ptr [edi],"E"
	inc edi
	mov byte ptr [edi],"C"
	inc edi
	mov byte ptr [edi],"X"
	inc edi
	mov byte ptr [edi],"0"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0

	call code_translation

	;test m32
	mov Instruction.operation_type,6
	mov Instruction.operation_len,3
	mov edi,offset Instruction.operation_str
	mov byte ptr [edi],"N"
	inc edi
	mov byte ptr [edi],"E"
	inc edi
	mov byte ptr [edi],"G"
	inc edi
	mov byte ptr [edi],0
	mov Instruction.operand1_type,1
	mov edi,offset Instruction.operand1_name
	mov byte ptr [edi],"w"
	inc edi
	mov byte ptr [edi],"o"
	inc edi
	mov byte ptr [edi],"r"
	inc edi
	mov byte ptr [edi],"0"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0

	call code_translation

	;test ret
	mov Instruction.operation_type,10
	call code_translation

	;test ENDP
	mov Instruction.operation_type,9
	mov edi,offset Instruction.operation_str
	mov byte ptr [edi],"f"
	inc edi
	mov byte ptr [edi],"u"
	inc edi
	mov byte ptr [edi],"n"
	inc edi
	mov byte ptr [edi],"2"
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	inc edi
	mov byte ptr [edi],0
	call code_translation
	mov edi,offset FunctionInfoTable
	add edi,28
	invoke crt_printf,addr char,13
	invoke crt_printf,addr integer,dword ptr [edi + 8]
	invoke crt_printf,addr integer,dword ptr [edi + 12]
	invoke crt_printf,addr char,13


	;EXECUTE WHEN CODE TRANSLATION IS FINISHED
	;FILL TEXT SECTION HEADER
	invoke fill_text_section_header,current_code_bytes,RelocationCount
	invoke update_jump_bytes

	;SHOW ALL RAW DATA
	mov edi,offset text_rawdata
	mov ecx,current_code_bytes
	cmp ecx,0
	je L2
L_show:
	movzx edx,byte ptr [edi]
	inc edi
	pushad
	invoke crt_printf,addr integer,edx
	popad
	loop L_show
	invoke crt_printf,addr char,13

	call show_relocation_table

L2:
	INVOKE StdOut,ADDR program_end
	INVOKE StdIn,ADDR input_char,1
	INVOKE ExitProcess,0
main ENDP

END main