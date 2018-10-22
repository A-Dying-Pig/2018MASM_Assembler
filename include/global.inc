SectionCount EQU 4
GlobalVMaxCount EQU 20
FunctionMaxCount EQU 20
OtherMaxCount EQU 2
EXTERNDEF GlobalVCount:DWORD
EXTERNDEF FunctionCount:WORD
EXTERNDEF OtherCount:WORD
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
EXTERNDEF RelocationCount:WORD
EXTERNDEF RelocationTable:RelocationEntryproto

;linenum table
LineNumEntryproto STRUCT
    l_addr DWORD ?
    l_lnno WORD ?
LineNumEntryproto ENDS
EXTERNDEF LineNumCount:WORD
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
EXTERNDEF SymbolEntryCount:WORD
EXTERNDEF SectionSymbolTable:SymbolEntryproto
EXTERNDEF GlobalvSymbolTable:SymbolEntryproto
EXTERNDEF FunctionSymbolTable:SymbolEntryproto
EXTERNDEF OtherSymbolTable:SymbolEntryproto

;symbol aux
SymbolauxEntryproto STRUCT
    content BYTE 18 DUP(?)
SymbolauxEntryproto ENDS
EXTERNDEF SymbolauxEntryCount:WORD
EXTERNDEF SymbolauxTable:SymbolauxEntryproto


;string table
StringEntryproto STRUCT
    content BYTE 8 DUP(?)
StringEntryproto ENDS
EXTERNDEF StringEntryCount:WORD
EXTERNDEF StringTable:StringEntryproto