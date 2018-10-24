;;-------------------------------
;; transform the structure to coff
;;-------------------------------
;;----------TOOLS----------------
;--------------------------------------
; result stored in ebx
;--------------------------------------
GetStringLength PROTO FAR C mstr:DWORD
RegPush PROTO FAR C
RegPop PROTO FAR C
;--------------------------------------
; result stored in eax
;--------------------------------------
idxTransform PROTO FAR C idx:DWORD,protosize:DWORD
;;---------ENDTOOLS---------------


;--------------------------------
; finish the FileHeader structure
; Needed:
;	FileSymbolTableFini,(SectionCount,SectionAuxSymbolTableCount,CalledFunctionSymbolEntryCount,GlobalVCount,FunctionInfoCount)
; Finish:
;	FileHeader:f_magic,f_nscns,f_nsyms,f_opthdr,f_flags(remain:f_timdat,f_symptr)
; Note:
;	Called after all symboltableFini functions
;--------------------------------
FileHeaderFini PROTO

;--------------------------------
; finish section header structure and symbol table of sections
; Needed:
;	SectionHeader:s_name,s_size,s_nreloc,s_nlnno
;	SectionCount
; Finish:
;	SectionSymbolTable:n_name,n_value,n_scnum,n_type,n_sclass,n_numaux,RawDataSize,RawDataptr(all)
;	SectionAuxSymbolTable:seclength,numberOfRelocations,numberOfLinenumbers(all)
;	SectionAuxSymbolTableCount
; Note:
;	called before all functions using SectionAuxSymbolTableCount
;--------------------------------
SectionSymbolFini PROTO

;----------------------------------------
; finish global variable symbol table
; Needed:
;	GlobalvSymbolTable:n_name,n_value
; Finish:
;	GlobalvSymbolTable:n_scnum,n_type,n_sclass,n_numaux
;----------------------------------------
GlobalvSymbolTableFini PROTO

;----------------------------------------
; finish Function Symbol
; Needed:
;	FunctionInfo completed
; Remain:
;	Function Definition Aux:PointerToLineNumber(abandoned)
;	test
;----------------------------------------
FunctionSymbolTableFini PROTO

;---------------------------------------------
; Finish .file symbol table
;---------------------------------------------
FileSymbolTableFini PROTO

;------------------------------------
; Finish called function symbol table
; Finish:
;	CalledFunctionSymbolTable:n_type,n_sclass(all)
; Needed:
;	CalledFunctionSymbolTable:n_name,
;	CalledFunctionSymbolEntryCount
;------------------------------------
CalledFunctionSymbolTableFini PROTO

;-------------------------------------
; change relocation table index
; Finish:
;	RelocationTable:r_symndx
; Need:
;	RelocationTable:r_type,symbol table except functions
; Remain:
;	test
; Note:
;	called after all symbol tables except functions finished
;-------------------------------------
RelocationTableFini PROTO

;----------------------------------------------
; Finish all file offset
; Need:
;	SectionHeader:s_size
; Finish:
;	SectionHeader:s_paddr,s_scnptr,s_flags,s_relptr
;	FileHeader:f_symptr
; Remain:
;	no
;----------------------------------------------
AllOffsetFini PROTO

;------------------------------------------------
; Finish drectve raw_data and section header
; Finish:
;	drectve_rawdata
; Need:
;	DrectveRawDataTable:all
;	DrectveRawDataEntryCount
; Remain:
;	
; Note:
;	called before all procs using SectionHeader
;------------------------------------------------
DrectveFini PROTO
