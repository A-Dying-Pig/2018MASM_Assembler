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


RelocationCount DWORD 0
RelocationTable RelocationEntryproto 100 DUP(<>)

LineNumCount DWORD 0
LineNumTable LineNumEntryproto 200 DUP(<>)


SymbolEntryCount DWORD 0
GlobalVCount DWORD 0
FunctionCount DWORD 0
OtherCount DWORD 0
SectionSymbolTable SymbolEntryproto SectionCount DUP(<>)
GlobalvSymbolTable SymbolEntryproto GlobalVMaxCount DUP(<>)
FunctionSymbolTable SymbolEntryproto FunctionMaxCount DUP(<>)
OtherSymbolTable SymbolEntryproto OtherMaxCount DUP(<>)

SymbolauxEntryCount DWORD 0
SymbolauxTable SymbolauxEntryproto 100 DUP(<>)


Instruction Instructionproto <>
InstructionEnd DWORD 1123

StringEntryCount DWORD 0
StringTable BYTE 200 DUP(?)

FunctionInfoCount DWORD 0
FunctionInfoTable FunctionInfoproto FunctionMaxCount DUP(<>)

current_code_bytes DWORD 0
line_bytes DWORD 0


LabelCount DWORD 0
LabelTable LabelEntryproto 100 DUP(<>)

END