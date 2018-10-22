.386
.model flat, stdcall
option casemap : none
include global.inc
include coff.inc
include masm32.inc
includelib masm32.lib
.data
	bfname BYTE ".bf",0
	lfname BYTE ".lf",0
	efname BYTE ".ef",0
	filename BYTE ".file",0
	atemp DWORD ?
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
	add ecx,SectionCount
	invoke idxTransform,ecx,SIZEOF SectionSymbolTable
	mov edx,eax
	invoke idxTransform,ecx,SIZEOF SectionHeaderproto
	mov ecx,edx
	mov edx,eax
	;n_name
	cld
	lea esi,SectionHeader[edx].s_name
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
	mov ebx,SectionHeader[edx].s_size
	mov SectionAuxSymbolTable[eax].seclength,ebx
	mov bx,SectionHeader[edx].s_nreloc
	mov SectionAuxSymbolTable[eax].numberOfRelocations,bx
	mov bx,SectionHeader[edx].s_nlnno
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
	add ecx,GlobalVCount
	invoke idxTransform,ecx,SIZEOF SymbolEntryproto
	mov ecx,eax
	mov GlobalvSymbolTable[ecx].n_scnum,2
	mov GlobalvSymbolTable[ecx].n_type,0
	mov GlobalvSymbolTable[ecx].n_sclass,3
	mov GlobalvSymbolTable[ecx].n_numaux,0
	pop ecx
	dec ecx
	jne GlobalvSymbolTablel
	ret
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
	add ecx,FunctionInfoCount
	invoke idxTransform,ecx,SIZEOF FunctionInfoproto
	mov ecx,eax
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
	add eax,SIZEOF SymbolEntryproto
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
	add eax,SIZEOF SymbolEntryproto
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
	add eax,SIZEOF SymbolEntryproto
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
	ret
FunctionSymbolTableFini ENDP

FileSymbolTableFini PROC USES eax ebx ecx edx esi edi
	;gen file define
	lea esi,filename
	lea edi,FileSymbolTable.n_name
	mov ecx,5
	cld
	rep movsb
	mov FileSymbolTable.n_scnum,0fffeh
	mov FileSymbolTable.n_sclass,67h
	mov FileSymbolTable.n_numaux,3h
	;get filepathlen
	invoke GetStringLength,addr FilePath
	;get ecx loop
	mov eax,ebx
	mov edx,0
	mov ebx,18
	div ebx
	mov ecx,0
	mov cx,ax
	mov ebx,SymbolauxEntryCount
	lea esi,FilePath
	cmp cx,0
	je copyfileremainpath
	push edx

	mov eax,ebx
	mov edx,SIZEOF SymbolauxEntryproto
	mul edx
	mov ebx,eax
copyfilepath:
	push ecx
	lea edi,SymbolauxTable[ebx].content
	mov ecx,18
	rep movsb
	push ecx
	pop ecx
	inc SymbolauxEntryCount
	mov eax,SymbolauxEntryCount
	mov edx,SIZEOF SymbolauxEntryproto
	mul edx
	mov ebx,eax
	pop ecx
	dec ecx
	jne copyfilepath

	pop edx
copyfileremainpath:
	;mov ebx,SymbolauxEntryCount
	mov ecx,0
	mov cx,dx
	lea edi,(SymbolauxTable[ebx]).content
	rep movsb
	inc SymbolauxEntryCount
	;
	mov ecx,SymbolauxEntryCount
	mov ebx,0

	invoke StdOut,ADDR SymbolauxTable[0].content
	ret
FileSymbolTableFini ENDP

;--------------------------------------
; result stored in ebx
;--------------------------------------
GetStringLength PROC FAR C USES eax ecx edx esi edi,mstr:DWORD
	;invoke StdOut,mstr
	cld
	mov edi,mstr
	mov ecx,255
	mov al,0
	repne scasb
	neg ecx
	add ecx,254
	mov ebx,ecx
	ret
GetStringLength ENDP

RegPush PROC
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
RegPush ENDP

RegPop PROC
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
RegPop ENDP


idxTransform PROC FAR C USES ebx ecx edx esi edi,idx:DWORD,protosize:DWORD
	mov eax,idx
	mov ebx,protosize
	mul ebx
	ret
idxTransform ENDP
END