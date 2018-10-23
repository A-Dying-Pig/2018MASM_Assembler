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
	allFileOffset DWORD 0
.code
;;-------------------------------
;; transform the structure to coff
;;-------------------------------


;--------------------------------
; finish the FileHeader structure
; Note:Called after symbol table
;--------------------------------
FileHeaderFini PROC USES eax ebx ecx edx esi edi
    mov FileHeader.f_magic,014Ch
    mov FileHeader.f_nscns,SectionCount
    ;TODO
    ;FileHeader.f_timdat = 
	mov ebx,0
	add ebx,1;file
	add ebx,SymbolauxEntryCount
	add ebx,1;comp
	add ebx,SectionCount;section
	add ebx,SectionAuxSymbolTableCount
	add ebx,CalledFunctionSymbolEntryCount
	add ebx,GlobalVCount
	mov eax,FunctionInfoCount
	mov ecx,7
	mul ecx
	add ebx,eax

    mov FileHeader.f_nsyms,ebx
    mov FileHeader.f_opthdr,0
    mov FileHeader.f_flags,0
	ret
FileHeaderFini ENDP
;--------------------------------
; finish section header structure and symbol table of sections
; Needed:
;	SectionHeader:s_name,s_size,s_nreloc,s_nlnno
; Remain:
;	no
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

	;change ptr
	mov eax,TYPE SectionSymbolTable
	mov ecx,0
	lea edx,text_rawdata
	mov SectionSymbolTable[ecx].RawDataptr,edx
	add ecx,eax
	lea edx,data_rawdata
	mov SectionSymbolTable[ecx].RawDataptr,edx
	add ecx,eax
	lea edx,STACK_rawdata
	mov SectionSymbolTable[ecx].RawDataptr,edx
	add ecx,eax
	lea edx,drectve_rawdata
	mov SectionSymbolTable[ecx].RawDataptr,edx
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
;	Function Definition Aux:PointerToLineNumber(abandoned)
;----------------------------------------
FunctionSymbolTableFini PROC USES eax ebx ecx edx esi edi
	mov ecx, FunctionInfoCount
	mov edx,0
	add edx,1;file
	add edx,SymbolauxEntryCount
	add edx,1;comp
	add edx,SectionCount;section
	add edx,SectionAuxSymbolTableCount
	add edx,CalledFunctionSymbolEntryCount
	add edx,GlobalVCount
	inc edx
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
	mov FunctionAuxSymbolTable[eax].pointerToNextFunction,edx
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
	mov FunctionbfefAuxSymbolTable[eax].pointerToNextFunction,edx
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

;---------------------------------------------
; Finish file symbol table
; Remain:
;	no
;---------------------------------------------
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

;------------------------------------
; Finish called function symbol table
; Needed:
;	n_name,
;------------------------------------
CalledFunctionSymbolTableFini PROC
	mov ecx,CalledFunctionSymbolEntryCount
calledfunctionsymbol:
	push ecx
	invoke idxTransform,ecx,TYPE SymbolEntryproto
	mov ecx,eax
	mov CalledFunctionSymbolTable[ecx].n_type,20h
	mov CalledFunctionSymbolTable[ecx].n_sclass,2h

	inc CalledFunctionSymbolEntryCount
	pop ecx
	dec ecx
	jne calledfunctionsymbol
	ret
CalledFunctionSymbolTableFini ENDP

;-------------------------------------
; change relocation table index
; Remain:
;	test
;-------------------------------------
RelocationTableFini PROC USES eax ebx ecx edx esi edi
	;change called function relocation
	mov edx,0
	add edx,1;file
	add edx,SymbolauxEntryCount
	add edx,1;comp
	add edx,SectionCount;section
	add edx,SectionAuxSymbolTableCount
	add ebx,CalledFunctionSymbolEntryCount
	mov ecx,RelocationCount
reloloop:
	push ecx
	neg ecx
	add ecx,RelocationCount
	invoke idxTransform,ecx,TYPE RelocationEntryproto
	cmp RelocationTable[eax].r_type,14h
	je calledfunction
globalv:
	add RelocationTable[eax].r_symndx,ebx
	pop ecx
	dec ecx
	jne reloloop
	ret
calledfunction:
	add RelocationTable[eax].r_symndx,edx
	pop ecx
	dec ecx
	jne reloloop
	ret

RelocationTableFini ENDP

;----------------------------------------------
; Finish all file offset
; Remain:
;	test
;----------------------------------------------
AllOffsetFini PROC USES eax ebx ecx edx esi edi
	mov eax,40
	mov ebx,4
	mul eax
	add eax,20
	mov allFileOffset,eax
	;section ptr
	;.text special
	mov SectionHeader[0].s_paddr,0
	mov eax,allFileOffset
	mov SectionHeader[0].s_scnptr,eax
	mov SectionHeader[0].s_flags,60300020h
	mov edx,SectionHeader[0].s_size
	and edx,1
	jnz oddsize 
	evensize:
	mov ebx,SectionHeader[0].s_size
	add allFileOffset,ebx
	mov eax,allFileOffset
	mov SectionHeader[0].s_relptr,eax
	jmp othersectionheader
	oddsize:
	mov ebx,SectionHeader[0].s_size
	add allFileOffset,ebx
	inc allFileOffset
	mov eax,allFileOffset
	mov SectionHeader[0].s_relptr,eax

	;other section header
	othersectionheader:
	mov ecx,SectionCount
	dec ecx
	mov ebx,0
sectionoffsetloop:
	push ecx
	neg ecx
	add ecx,SectionCount
	invoke idxTransform,ecx,TYPE SectionHeaderproto
	mov ecx,eax
	sub ecx,TYPE SectionHeaderproto
	;s_paddr
	mov eax,SectionHeader[ecx].s_paddr
	add eax,SectionHeader[ecx].s_size
	add ecx,TYPE SectionHeaderproto
	mov SectionHeader[ecx].s_paddr,eax
	pop ecx
	dec ecx
	jne sectionoffsetloop

	;.data header and .drectve header
	movzx eax,SectionHeader[0].s_nreloc
	mov ebx,10
	mul ebx
	add eax,SectionHeader[0].s_relptr
	mov SectionHeader[TYPE SectionHeaderproto].s_scnptr,eax
	add eax,SectionHeader[TYPE SectionHeaderproto].s_size
	mov SectionHeader[3*(TYPE SectionHeaderproto)].s_scnptr,eax
	;file symboltable offset
	mov eax,SectionHeader[3*(TYPE SectionHeaderproto)].s_scnptr
	add eax,SectionHeader[3*(TYPE SectionHeaderproto)].s_size
	mov ebx,eax
	and ebx,1
	jz filesymboleven
	inc eax
	filesymboleven:
	mov FileHeader.f_symptr,eax
	ret
AllOffsetFini ENDP

END