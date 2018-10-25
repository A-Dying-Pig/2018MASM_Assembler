SectionCount EQU 4
GlobalVMaxCount EQU 20
FunctionMaxCount EQU 20
OtherMaxCount EQU 2
CalledFunctionMazCount EQU 50
EXTERNDEF GlobalVCount:DWORD
EXTERNDEF FunctionCount:DWORD
EXTERNDEF OtherCount:DWORD
EXTERNDEF CalledFunctionSymbolEntryCount:DWORD

;fileheader
FileHeaderproto STRUCT
    f_magic WORD 0
    f_nscns WORD SectionCount;text,data,STACK,drectve
    f_timdat DWORD 0
    f_symptr DWORD 0
    f_nsyms DWORD 0
    f_opthdr WORD 0
    f_flags WORD 0
FileHeaderproto ENDS
EXTERNDEF FileHeader:FileHeaderproto

;section header
SectionHeaderproto STRUCT
    s_name BYTE 8 DUP(0)
    s_paddr DWORD 0
    s_vaddr DWORD 0
    s_size DWORD 0
    s_scnptr DWORD 0
    s_relptr DWORD 0
    s_lnnoptr DWORD 0
    s_nreloc WORD 0
    s_nlnno WORD 0
    s_flags DWORD 0
SectionHeaderproto ENDS
EXTERNDEF SectionHeader:SectionHeaderproto

EXTERNDEF text_rawdata:BYTE
EXTERNDEF data_rawdata:BYTE
EXTERNDEF drectve_rawdata:BYTE
EXTERNDEF STACK_rawdata:BYTE

;relocation table
RelocationEntryproto STRUCT
    r_vaddr DWORD 0
    r_symndx DWORD 0
    r_type WORD 0
RelocationEntryproto ENDS
EXTERNDEF RelocationCount:DWORD
EXTERNDEF RelocationTable:RelocationEntryproto

;linenum table
LineNumEntryproto STRUCT
    l_addr DWORD 0
    l_lnno WORD 0
LineNumEntryproto ENDS
EXTERNDEF LineNumCount:DWORD
EXTERNDEF LineNumTable:LineNumEntryproto

;symbol table
SymbolEntryproto STRUCT
    n_name BYTE 8 DUP(0)
    n_value DWORD 0
    n_scnum WORD 0
    n_type WORD 0
    n_sclass BYTE 0
    n_numaux BYTE 0
    RawDataptr DWORD 0
    RawDataSize DWORD 0
SymbolEntryproto ENDS
EXTERNDEF SymbolEntryCount:DWORD
EXTERNDEF FileSymbolTable:SymbolEntryproto
EXTERNDEF SectionSymbolTable:SymbolEntryproto
EXTERNDEF GlobalvSymbolTable:SymbolEntryproto
EXTERNDEF FunctionSymbolTable:SymbolEntryproto
EXTERNDEF OtherSymbolTable:SymbolEntryproto
EXTERNDEF CalledFunctionSymbolTable:SymbolEntryproto


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
EXTERNDEF FilePath:BYTE
EXTERNDEF COMPIDSymbolTable:BYTE

;Teacher Na, Please don't delete the double quotation marks!!!!!
DrectveRawDataEntryproto STRUCT
	sizep DWORD 0
	libpath BYTE 100 DUP(0)
DrectveRawDataEntryproto ENDS
DrectveRawDataMaxEntryCount EQU 20
EXTERNDEF DrectveRawDataEntryCount:DWORD
EXTERNDEF DrectveRawDataTable:DrectveRawDataEntryproto