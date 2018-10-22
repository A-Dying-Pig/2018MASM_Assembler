.386
.model flat, stdcall
option casemap : none
include global.inc
.data

FileHeader FileHeaderproto <>
FileOffset DWORD 0

SectionHeader SectionHeaderproto SectionCount DUP(<>)

;section raw-data,fixed size
text_rawdata BYTE 1000 DUP(?)
data_rawdata BYTE 1000 DUP(?)
drectve_rawdata BYTE 100 DUP(?)
STACK_rawdata BYTE 100 DUP(?)


RelocationCount WORD 0
RelocationTable RelocationEntryproto 100 DUP(<>)

LineNumCount WORD 0
LineNumTable LineNumEntryproto 200 DUP(<>)


SymbolEntryCount WORD 0
GlobalVCount DWORD 0
FunctionCount WORD 0
OtherCount WORD 0
SectionSymbolTable SymbolEntryproto SectionCount DUP(<>)
GlobalvSymbolTable SymbolEntryproto GlobalVMaxCount DUP(<>)
FunctionSymbolTable SymbolEntryproto FunctionMaxCount DUP(<>)
OtherSymbolTable SymbolEntryproto OtherMaxCount DUP(<>)

SymbolauxEntryCount WORD 0
SymbolauxTable SymbolauxEntryproto 100 DUP(<>)


StringEntryCount WORD 0
StringTable StringEntryproto 100 DUP(<>)

END