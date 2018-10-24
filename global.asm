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
FileSymbolTable SymbolEntryproto <>
SectionSymbolTable SymbolEntryproto SectionCount DUP(<>)
GlobalvSymbolTable SymbolEntryproto GlobalVMaxCount DUP(<>)
FunctionSymbolTable SymbolEntryproto FunctionMaxCount DUP(<>)
OtherSymbolTable SymbolEntryproto OtherMaxCount DUP(<>)
CalledFunctionSymbolTable SymbolEntryproto CalledFunctionMazCount DUP(<>)
CalledFunctionSymbolEntryCount DWORD 0
SymbolauxEntryCount DWORD 0
SymbolauxTable SymbolauxEntryproto 100 DUP(<>)


StringEntryCount DWORD 0
StringTable BYTE 200 DUP(?)

FunctionInfoCount DWORD 0
FunctionInfoTable FunctionInfoproto FunctionMaxCount DUP(<>)

FunctionAuxSymbolTableCount DWORD 0
FunctionAuxSymbolTable FunctionAuxSymbolTableproto MaxFunctionAuxSymbolTable DUP(<>)

FunctionbfefAuxSymbolTableCount DWORD 0
FunctionbfefAuxSymbolTable FunctionbfefAuxSymbolTableproto MaxFunctionbfefAuxSymbolTable DUP(<>)

SectionAuxSymbolTableCount DWORD 0
SectionAuxSymbolTable SectionAuxSymbolTableproto SectionAuxSymbolTableMaxCount DUP(<>)

FilePath BYTE 255 DUP(0)
COMPIDSymbolTable BYTE 40h,63h,6fh,6dh,70h,2eh,69h,64h,0fch,20h,12h,00h,0ffh,0ffh,00h,00h,03h,00h



; new added
Instruction Instructionproto <>

current_code_bytes DWORD 0
line_bytes DWORD 0

LabelCount DWORD 0
LabelTable LabelEntryproto 100 DUP(<>)

END