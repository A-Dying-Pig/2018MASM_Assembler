SectionCount EQU 4
GlobalVMaxCount EQU 20
FunctionMaxCount EQU 20
OtherMaxCount EQU 2
EXTERNDEF GlobalVCount:DWORD
EXTERNDEF FunctionCount:DWORD
EXTERNDEF OtherCount:DWORD
;fileheader
FileHeaderproto STRUCT
    f_magic WORD ?
    f_nscns WORD SectionCount;text,data,STACK,drectve
    f_timdat DWORD ?
    f_symptr DWORD ?
    f_nsyms DWORD ?
    f_opthdr DWORD 0
    f_flags WORD ?
FileHeaderproto ENDS
EXTERNDEF FileHeader:FileHeaderproto

;section header
SectionHeaderproto STRUCT
    s_name BYTE 8 DUP(?)
    s_paddr DWORD ?
    s_vaddr DWORD ?
    s_size DWORD ?
    s_scnptr DWORD ?
    s_relptr DWORD ?
    s_lnnoptr DWORD ?
    s_nreloc WORD ?
    s_nlnno WORD ?
    s_flags DWORD ?
SectionHeaderproto ENDS
EXTERNDEF SectionHeader:SectionHeaderproto

EXTERNDEF text_rawdata:BYTE
EXTERNDEF data_rawdata:BYTE
EXTERNDEF drectve_rawdata:BYTE
EXTERNDEF STACK_rawdata:BYTE

;relocation table
RelocationEntryproto STRUCT
    r_vaddr DWORD ?
    r_symndx DWORD ?
    r_type WORD ?
RelocationEntryproto ENDS
EXTERNDEF RelocationCount:DWORD
EXTERNDEF RelocationTable:RelocationEntryproto

;linenum table
LineNumEntryproto STRUCT
    l_addr DWORD ?
    l_lnno WORD ?
LineNumEntryproto ENDS
EXTERNDEF LineNumCount:DWORD
EXTERNDEF LineNumTable:LineNumEntryproto

;symbol table
SymbolEntryproto STRUCT
    n_name BYTE 8 DUP(?)
    n_value DWORD ?
    n_scnum WORD ?
    n_type WORD ?
    n_sclass BYTE ?
    n_numaux BYTE ?
    RawDataptr DWORD ?
    RawDataSize DWORD ?
    Auxidx WORD ?
SymbolEntryproto ENDS
EXTERNDEF SymbolEntryCount:DWORD
EXTERNDEF SectionSymbolTable:SymbolEntryproto
EXTERNDEF GlobalvSymbolTable:SymbolEntryproto
EXTERNDEF FunctionSymbolTable:SymbolEntryproto
EXTERNDEF OtherSymbolTable:SymbolEntryproto

;symbol aux
SymbolauxEntryproto STRUCT
    content BYTE 18 DUP(?)
SymbolauxEntryproto ENDS
EXTERNDEF SymbolauxEntryCount:DWORD
EXTERNDEF SymbolauxTable:SymbolauxEntryproto



;string table
EXTERNDEF StringEntryCount:DWORD
EXTERNDEF StringTable:BYTE

;per instruction
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
EXTERNDEF Instruction:Instructionproto
EXTERNDEF InstructionEnd:DWORD

;Function
FunctionInfoproto STRUCT
	f_name BYTE 8 DUP(?)
	f_offset DWORD ?
	f_size DWORD ?
	f_lnof DWORD ?
	f_bf WORD ?
	f_lf DWORD ?
	f_ef WORD ?
FunctionInfoproto ENDS
EXTERNDEF FunctionInfoCount:DWORD
EXTERNDEF FunctionInfoTable:FunctionInfoproto

;current_byte
EXTERNDEF current_code_bytes:DWORD
EXTERNDEF line_bytes:DWORD

;label table
LabelEntryproto STRUCT
	label_name BYTE 9 DUP(0)
	label_offset DWORD ?
LabelEntryproto ENDS
EXTERNDEF LabelCount:DWORD
EXTERNDEF LabelTable:LabelEntryproto