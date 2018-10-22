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
    n_name BYTE 8 DUP(0)
    n_value DWORD ?
    n_scnum WORD 2
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

FunctionAuxSymbolTableproto STRUCT
	tagIndex DWORD 0
	totalSize DWORD 0
	pointerToLinenumber DWORD 0
	pointerToNextFunction DWORD 0
	unused WORD 0
FunctionAuxSymbolTableproto ENDS
MaxFunctionAuxSymbolTable EQU 20
EXTERNDEF FunctionAuxSymbolTableCount:DWORD
EXTERNDEF FunctionAuxSymbolTable:FunctionAuxSymbolTableproto

FunctionbfefAuxSymbolTableproto STRUCT
	unused1 DWORD 0
	linenumber WORD 0
	unused2 DWORD 0
	unused3 WORD 0
	pointerToNextFunction DWORD 0
	unused4 WORD 0
FunctionbfefAuxSymbolTableproto ENDS
MaxFunctionbfefAuxSymbolTable EQU 40
EXTERNDEF FunctionbfefAuxSymbolTableCount:DWORD
EXTERNDEF FunctionbfefAuxSymbolTable:FunctionbfefAuxSymbolTableproto

SectionAuxSymbolTableproto STRUCT
	seclength DWORD 0
	numberOfRelocations WORD 0
	numberOfLinenumbers WORD 0
	checkNum DWORD 0
	number WORD 0
	selection BYTE 0
	unused WORD 0
	unused1 BYTE 0
SectionAuxSymbolTableproto ENDS
SectionAuxSymbolTableMaxCount EQU 4
EXTERNDEF SectionAuxSymbolTableCount:DWORD
EXTERNDEF SectionAuxSymbolTable:SectionAuxSymbolTableproto