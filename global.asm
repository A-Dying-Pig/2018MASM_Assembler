.386
.model flat, stdcall
option casemap : none
include global.inc
.data
FileHeader FileHeaderproto <>

SectionHeader SectionHeaderproto 4 DUP(<>)

;section raw-data,fixed size
text_rawdata BYTE 1000 DUP(?)
data_rawdata BYTE 1000 DUP(?)
drectve_rawdata BYTE 100 DUP(?)
STACK_rawdata BYTE 100 DUP(?)


RelocationCount WORD 0
RelocationTable RelocationEntry 100 DUP(<>)

LineNumCount WORD 0
LineNumTable LineNumEntry 200 DUP(<>)


SymbolEntryCount WORD 0
SymbolTable SymbolEntry 100 DUP(<>)

SymbolauxEntryCount WORD 0
SymbolauxTable SymbolauxEntry 100 DUP(<>)


StringEntryCount WORD 0
StringTable StringEntry 100 DUP(<>)

END