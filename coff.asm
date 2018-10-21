.386
.model flat, stdcall
option casemap : none
include global.inc
include coff.inc
include Irvine32.inc
includelib "g:\masm32\lib\Irvine32.lib"
includelib "g:\masm32\lib\kernel32.lib"
includelib "g:\masm32\lib\user32.lib"
.data
	bfname BYTE ".bf",0
	lfname BYTE ".lf",0
	efname BYTE ".ef",0
.code
;;-------------------------------
;; transform the structure to coff
;;-------------------------------


;--------------------------------
; finish the FileHeader structure
;--------------------------------
FileHeaderFini PROC
    mov FileHeader.f_magic,014Ch
    mov FileHeader.f_nscns,SectionCount
    ;TODO
    ;FileHeader.f_timdat = 
    ;FileHeader.f_symptr = 
    ;FileHeader.f_nsyms = 
    mov FileHeader.f_opthdr,0
    mov FileHeader.f_flags,0
FileHeaderFini ENDP





;--------------------------------
; finish section header structure and symbol table of sections
; Needed:
;	SectionHeader:s_name,s_size,s_nreloc,s_nlnno
; Remain:
;	RawDataptr,
;--------------------------------
SectionSymbolFini PROC USES eax ebx ecx edx esi edi
    mov ecx,SectionCount
sectionsymbol:
	push ecx
	neg ecx
	add ecx,4
	;n_name
	cld
	lea esi,SectionHeader[ecx].s_name
	lea edi,SectionSymbolTable[ecx].n_name
	mov ecx,8
	rep movsb
	;value of symbol
	mov SectionSymbolTable[ecx].n_value,0
	;section num
	mov eax,ecx
	inc eax
	mov SectionSymbolTable[ecx].n_scnum,ax
	;symbol type
	mov SectionSymbolTable[ecx].n_type,0
	;storage class
	mov SectionSymbolTable[ecx].n_sclass,3
	;aux
	mov eax,SectionAuxSymbolTableCount
	mov ebx,SectionHeader[ecx].s_size
	mov SectionAuxSymbolTable[eax].seclength,ebx
	mov bx,SectionHeader[ecx].s_nreloc
	mov SectionAuxSymbolTable[eax].numberOfRelocations,bx
	mov bx,SectionHeader[ecx].s_nlnno
	mov SectionAuxSymbolTable[eax].numberOfLinenumbers,bx

	;add aux count
	inc SectionAuxSymbolTableCount

	;RawData size
	mov eax,SectionHeader[ecx].s_size
	mov SectionSymbolTable[ecx].RawDataSize,eax

	pop ecx
	dec ecx
	jne sectionsymbol
	ret
SectionSymbolFini ENDP



;----------------------------------------
; finish global variable symbol table
; Needed:
;	GlobalvSymbolTable:n_name,n_value
; Remain:
;	no
;----------------------------------------
GlobalvSymbolTableFini PROC USES eax ebx ecx edx esi edi
	mov ecx,GlobalVCount
GlobalvSymbolTablel:
	push ecx
	neg ecx
	add ecx,DWORD PTR GlobalVCount
	mov GlobalvSymbolTable[ecx].n_scnum,2
	mov GlobalvSymbolTable[ecx].n_type,0
	mov GlobalvSymbolTable[ecx].n_sclass,3
	mov GlobalvSymbolTable[ecx].n_numaux,0
	pop ecx
	dec ecx
	jne GlobalvSymbolTablel
GlobalvSymbolTableFini ENDP


;----------------------------------------
; finish Function Symbol
; Needed:
;	FunctionInfo completed
; Remain:
;	Function Definition Aux:PointerToLineNumber,PointerToNextFunction
;	bf Aux:PointerToNextFunction
;----------------------------------------
FunctionSymbolTableFini PROC USES eax ebx ecx edx esi edi
	mov ecx, FunctionInfoCount
functionsymboltable:
	push ecx
	neg ecx
	add ecx,DWORD PTR FunctionInfoCount
	mov eax,ecx
	mov ebx,4
	mul ebx
	;function def
	lea esi,FunctionInfoTable[ecx].f_name
	lea edi,FunctionSymbolTable[eax].n_name
	push ecx
	mov ecx,8
	cld
	rep movsb
	pop ecx

	mov ebx,FunctionInfoTable[ecx].f_offset
	mov FunctionSymbolTable[eax].n_value,ebx
	mov FunctionSymbolTable[eax].n_scnum,1
	mov FunctionSymbolTable[eax].n_type,20h
	mov FunctionSymbolTable[eax].n_sclass,2
	mov FunctionSymbolTable[eax].n_numaux,1
	;function aux
	push eax
	mov eax,FunctionAuxSymbolTableCount
	mov ebx,FunctionInfoTable[ecx].f_size
	mov FunctionAuxSymbolTable[eax].totalSize,ebx
	inc FunctionAuxSymbolTableCount
	pop eax
	;bf
	inc eax
	lea esi,bfname
	lea edi,FunctionSymbolTable[eax].n_name
	push ecx
	mov ecx,3
	rep movsb
	pop ecx
	mov ebx,FunctionInfoTable[ecx].f_offset
	mov FunctionSymbolTable[eax].n_value,ebx
	mov FunctionSymbolTable[eax].n_scnum,1
	mov FunctionSymbolTable[eax].n_type,0
	mov FunctionSymbolTable[eax].n_sclass,65h
	mov FunctionSymbolTable[eax].n_numaux,1
	;bf aux
	push eax
	mov eax,FunctionbfefAuxSymbolTableCount
	mov bx,FunctionInfoTable[ecx].f_bf
	mov FunctionbfefAuxSymbolTable[eax].linenumber,bx
	inc FunctionbfefAuxSymbolTableCount
	pop eax
	;lf
	inc eax
	lea esi,lfname
	lea edi,FunctionSymbolTable[eax].n_name
	push ecx
	mov ecx,3
	rep movsb
	pop ecx
	mov ebx,FunctionInfoTable[ecx].f_lf
	mov FunctionSymbolTable[eax].n_value,ebx
	mov FunctionSymbolTable[eax].n_scnum,1
	mov FunctionSymbolTable[eax].n_type,0
	mov FunctionSymbolTable[eax].n_sclass,65h
	mov FunctionSymbolTable[eax].n_numaux,0

	;ef
	inc eax
	lea esi,efname
	lea edi,FunctionSymbolTable[eax].n_name
	push ecx
	mov ecx,3
	rep movsb
	pop ecx
	mov ebx,FunctionInfoTable[ecx].f_size
	mov FunctionSymbolTable[eax].n_value,ebx
	mov FunctionSymbolTable[eax].n_scnum,1
	mov FunctionSymbolTable[eax].n_type,0
	mov FunctionSymbolTable[eax].n_sclass,65h
	mov FunctionSymbolTable[eax].n_numaux,1
	;ef aux
	push eax
	mov eax,FunctionbfefAuxSymbolTableCount
	mov bx,FunctionInfoTable[ecx].f_ef
	mov FunctionbfefAuxSymbolTable[eax].linenumber,bx
	inc FunctionbfefAuxSymbolTableCount
	pop eax
	pop ecx
	dec ecx
	jne functionsymboltable
FunctionSymbolTableFini ENDP


END