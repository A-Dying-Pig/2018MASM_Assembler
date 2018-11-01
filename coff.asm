.386
.model flat, stdcall
option casemap : none

include global.inc
include coff.inc


.data
	zeroBlank BYTE 0
	bfname BYTE ".bf",0
	lfname BYTE ".lf",0
	efname BYTE ".ef",0
	filename BYTE ".file",0
	atemp DWORD ?
	allFileOffset DWORD 0
	drectvelib BYTE "-defaultlib:",0
	drectveentry BYTE "-entry:main@0 ",0
	drectvename BYTE ".drectve",0
	blankspace BYTE " ",0
	coffname BYTE "a.obj",0
	filehandler HANDLE ?
.code
;;-------------------------------
;; transform the structure to coff
;;-------------------------------


;--------------------------------
; finish the FileHeader structure
; Needed:
;	FileSymbolTableFini,(SectionCount,SectionAuxSymbolTableCount,CalledFunctionSymbolEntryCount,GlobalVCount,FunctionInfoCount)
; Finish:
;	FileHeader:f_magic,f_nscns,f_nsyms,f_opthdr,f_flags(remain:f_timdat,f_symptr)
; Note:
;	Called after all symboltableFini functions
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
	mov eax,TYPE FileHeaderproto
	ret
FileHeaderFini ENDP


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
SectionSymbolFini PROC USES eax ebx ecx edx esi edi
    mov ecx,SectionCount
sectionsymbol:
	push ecx
	neg ecx
	add ecx,SectionCount
	invoke idxTransform,ecx,TYPE SectionSymbolTable
	mov edx,eax
	invoke idxTransform,ecx,TYPE SectionHeaderproto
	mov ecx,edx
	mov edx,eax
	;n_name
	push ecx
	cld
	lea esi,SectionHeader[edx].s_name
	lea edi,SectionSymbolTable[ecx].n_name
	mov ecx,8
	rep movsb
	pop ecx
	;value of symbol
	mov SectionSymbolTable[ecx].n_value,0
	;section num
	push edx
	push ebx
	mov eax,ecx
	mov edx,0
	mov ebx,TYPE SymbolEntryproto
	div ebx
	inc eax
	mov SectionSymbolTable[ecx].n_scnum,ax
	pop ebx
	pop edx
	;symbol type
	mov SectionSymbolTable[ecx].n_type,0
	;storage class
	mov SectionSymbolTable[ecx].n_sclass,3
	;aux
	mov SectionSymbolTable[ecx].n_numaux,1
	mov eax,SectionAuxSymbolTableCount
	invoke idxTransform,eax,TYPE SectionAuxSymbolTableproto
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
; Finish:
;	GlobalvSymbolTable:n_scnum,n_type,n_sclass,n_numaux
;----------------------------------------
GlobalvSymbolTableFini PROC USES eax ebx ecx edx esi edi
	mov ecx,GlobalVCount
	cmp ecx,0
	je GlobalvSymbolTableFiniend
GlobalvSymbolTablel:
	push ecx
	neg ecx
	add ecx,GlobalVCount
	invoke idxTransform,ecx,type SymbolEntryproto
	mov ecx,eax
	mov GlobalvSymbolTable[ecx].n_scnum,2
	mov GlobalvSymbolTable[ecx].n_type,0
	mov GlobalvSymbolTable[ecx].n_sclass,3
	mov GlobalvSymbolTable[ecx].n_numaux,0
	pop ecx
	dec ecx
	jne GlobalvSymbolTablel
	GlobalvSymbolTableFiniend:
	ret
GlobalvSymbolTableFini ENDP


;----------------------------------------
; finish Function Symbol
; Needed:
;	FunctionInfo completed, all stmbol table except function symbol
; Remain:
;	Function Definition Aux:PointerToLineNumber(abandoned)
;	test
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
	
functionsymboltable:
	add edx,7
	push ecx
	neg ecx
	add ecx,FunctionInfoCount
	invoke idxTransform,ecx,TYPE FunctionInfoproto
	mov ecx,eax
	mov ebx,4
	push edx
	mul ebx
	pop edx
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
	sub edx,5
	mov FunctionAuxSymbolTable[eax].tagIndex,edx
	add edx,5
	mov FunctionAuxSymbolTable[eax].pointerToNextFunction,edx
	inc FunctionAuxSymbolTableCount
	pop eax
	;bf
	add eax,TYPE SymbolEntryproto
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
	mov ebx,TYPE FunctionbfefAuxSymbolTableproto
	push edx
	mul ebx
	pop edx
	mov ebx,0
	mov bx,FunctionInfoTable[ecx].f_bf
	mov FunctionbfefAuxSymbolTable[eax].linenumber,bx
	mov FunctionbfefAuxSymbolTable[eax].pointerToNextFunction,edx
	inc FunctionbfefAuxSymbolTableCount
	pop eax
	;lf
	add eax,TYPE SymbolEntryproto
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
	add eax,TYPE SymbolEntryproto
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
	mov ebx,TYPE FunctionbfefAuxSymbolTableproto
	push edx
	mul ebx
	pop edx
	mov bx,FunctionInfoTable[ecx].f_ef
	mov FunctionbfefAuxSymbolTable[eax].linenumber,bx
	inc FunctionbfefAuxSymbolTableCount
	pop eax
	pop ecx
	dec ecx
	jne functionsymboltable
	;change next function pointer to 0
	mov ecx,FunctionInfoCount
	dec ecx
	mov eax,ecx
	mov edx,TYPE SymbolEntryproto
	mul edx
	mov FunctionAuxSymbolTable[eax].pointerToNextFunction,0
	mov edx,2
	mul edx
	mov FunctionbfefAuxSymbolTable[eax].pointerToNextFunction,0
	
	ret
FunctionSymbolTableFini ENDP

;---------------------------------------------
; Finish .file symbol table
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
	mov edx,TYPE SymbolauxEntryproto
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
	mov edx,TYPE SymbolauxEntryproto
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

RegPush PROC FAR C
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
RegPush ENDP

RegPop PROC FAR C
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
; Finish:
;	CalledFunctionSymbolTable:n_type,n_sclass(all)
; Needed:
;	CalledFunctionSymbolTable:n_name,
;	CalledFunctionSymbolEntryCount
;------------------------------------
CalledFunctionSymbolTableFini PROC
	mov ecx,CalledFunctionSymbolEntryCount
	cmp ecx,0
	je CalledFunctionSymbolTableFiniend
calledfunctionsymbol:
	push ecx
	neg ecx
	add ecx,CalledFunctionSymbolEntryCount
	invoke idxTransform,ecx,TYPE SymbolEntryproto
	mov ecx,eax
	mov CalledFunctionSymbolTable[ecx].n_type,20h
	mov CalledFunctionSymbolTable[ecx].n_sclass,2h

	pop ecx
	dec ecx
	jne calledfunctionsymbol
CalledFunctionSymbolTableFiniend:
	ret
CalledFunctionSymbolTableFini ENDP

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
RelocationTableFini PROC USES eax ebx ecx edx esi edi
	;change called function relocation
	mov edx,0
	add edx,1;file
	add edx,SymbolauxEntryCount
	add edx,1;comp
	add edx,SectionCount;section
	add edx,SectionAuxSymbolTableCount
	mov ebx,edx
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
; Need:
;	SectionHeader:s_size
; Finish:
;	SectionHeader:s_paddr,s_scnptr,s_flags,s_relptr
;	FileHeader:f_symptr
; Remain:
;	no
;----------------------------------------------
AllOffsetFini PROC USES eax ebx ecx edx esi edi
	mov eax,40
	mov ebx,SectionCount
	mul ebx
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
	mov SectionHeader[TYPE SectionHeaderproto].s_flags,0c0300040h
	add eax,SectionHeader[TYPE SectionHeaderproto].s_size
	mov SectionHeader[3*(TYPE SectionHeaderproto)].s_scnptr,eax
	mov SectionHeader[2*(TYPE SectionHeaderproto)].s_flags,0c0300040h
	mov SectionHeader[3*(TYPE SectionHeaderproto)].s_flags,00000a00h
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
DrectveFini PROC USES eax ebx ecx edx esi edi
	cld
	lea edi,drectve_rawdata
	mov ecx,DrectveRawDataEntryCount
	mov ebx,0
drectvestrcpy:
	push ecx
	neg ecx
	add ecx,DrectveRawDataEntryCount
	invoke idxTransform,ecx,TYPE DrectveRawDataEntryproto
	;copy
	lea esi,drectvelib
	mov ecx,LENGTHOF drectvelib
	dec ecx
	add ebx,ecx
	rep movsb
	lea esi,DrectveRawDataTable[eax].libpath
	mov ecx,DrectveRawDataTable[eax].sizep
	add ebx,ecx
	rep movsb
	lea esi,blankspace
	add ebx,1
	mov ecx,1
	rep movsb
	
	pop ecx
	dec ecx
	jne drectvestrcpy
	lea esi,drectveentry
	mov ecx,LENGTHOF drectveentry
	dec ecx
	add ebx,ecx
	rep movsb
	;header
	lea esi,drectvename
	lea edi,SectionHeader[3*SectionHeaderproto].s_name
	cld
	movsb
	mov SectionHeader[3*SectionHeaderproto].s_size,ebx
	ret
DrectveFini ENDP

;--------------------------------------------
; all procs in coff.asm will be called
;--------------------------------------------
COFFStructFix PROC USES eax ebx ecx edx esi edi

	call DrectveFini

	call AllOffsetFini

	call FileSymbolTableFini

	call SectionSymbolFini

	call CalledFunctionSymbolTableFini

	call GlobalvSymbolTableFini

	call RelocationTableFini

	call FunctionSymbolTableFini

	call FileHeaderFini

	ret
COFFStructFix ENDP


;----------------------------------------------
; output coff
;----------------------------------------------
COFFsave PROC USES eax ebx ecx edx esi edi
	;create file
	invoke CreateFile,addr coffname,GENERIC_WRITE , FILE_SHARE_WRITE , NULL,CREATE_ALWAYS, FILE_ATTRIBUTE_ARCHIVE, NULL
	mov filehandler,eax

	;file header
	mov eax,TYPE FileHeaderproto
	invoke WriteStruct,filehandler,ADDR FileHeader,TYPE FileHeaderproto,TYPE FileHeaderproto,1
	;section header
	invoke WriteStruct,filehandler,ADDR SectionHeader,TYPE SectionHeaderproto,TYPE SectionHeaderproto,SectionCount

	;.text
	invoke WriteStruct,filehandler,ADDR text_rawdata,1,1,SectionHeader[0].s_size
	;if .text is odd
	mov eax,SectionHeader[0].s_size
	and eax,1
	jz eventextjmp
	invoke WriteBlank,filehandler
	eventextjmp:
	;relo and linenum
	invoke WriteStruct,filehandler,ADDR RelocationTable,TYPE RelocationEntryproto,TYPE RelocationEntryproto,RelocationCount
	;.data
	invoke WriteStruct,filehandler,ADDR data_rawdata,1,1,SectionHeader[SectionHeaderproto].s_size
	;.drectve
	invoke WriteStruct,filehandler,ADDR drectve_rawdata,1,1,SectionHeader[3*SectionHeaderproto].s_size
	;if symbol is even
	mov eax,SectionHeader[3*SectionHeaderproto].s_scnptr
	add eax,SectionHeader[3*SectionHeaderproto].s_size
	and eax,1
	jz evendrecjmp
	invoke WriteBlank,filehandler
	evendrecjmp:

	;write file symbol
	invoke WriteStruct,filehandler,ADDR FileSymbolTable,TYPE SymbolEntryproto,18,1
	invoke WriteStruct,filehandler,ADDR SymbolauxTable,TYPE SymbolEntryproto,18,SymbolauxEntryCount

	;write compid
	invoke WriteStruct,filehandler,ADDR COMPIDSymbolTable,1,1,18

	;write section and aux
	mov ecx,SectionCount
	sectionandauxloop:
	push ecx
	neg ecx
	add ecx,SectionCount
	mov eax,TYPE SymbolEntryproto
	mul ecx
	add eax,OFFSET SectionSymbolTable
	invoke WriteStruct,filehandler,eax,TYPE SymbolEntryproto,18,1
	mov eax,TYPE SectionAuxSymbolTableproto
	mul ecx
	add eax,OFFSET SectionAuxSymbolTable
	invoke WriteStruct,filehandler,eax,TYPE SectionAuxSymbolTableproto,18,1
	pop ecx
	dec ecx
	jne sectionandauxloop

	;write lib function
	mov eax,CalledFunctionSymbolEntryCount
	invoke WriteStruct,filehandler,ADDR CalledFunctionSymbolTable,TYPE SymbolEntryproto,18,CalledFunctionSymbolEntryCount

	;write global v
	invoke WriteStruct,filehandler,ADDR GlobalvSymbolTable,TYPE SymbolEntryproto,18,GlobalVCount

	;write function
	mov ecx,FunctionInfoCount
	cmp ecx,0
	je writefunctionnext
	functionoutloop:
	push ecx
	neg ecx
	add ecx,FunctionInfoCount
	;func def
	mov eax,TYPE SymbolEntryproto
	mul ecx
	mov edx,4
	mul edx
	add eax,OFFSET FunctionSymbolTable
	invoke WriteStruct,filehandler,eax,TYPE SymbolEntryproto,18,1
	push eax
	;func def aux
	mov eax,TYPE FunctionAuxSymbolTableproto
	mul ecx
	add eax,OFFSET FunctionAuxSymbolTable
	invoke WriteStruct,filehandler,eax,TYPE FunctionAuxSymbolTableproto,18,1
	;.bf def
	pop eax
	add eax,TYPE SymbolEntryproto
	invoke WriteStruct,filehandler,eax,TYPE SymbolEntryproto,18,1
	push eax
	;.bf aux
	mov eax,TYPE FunctionbfefAuxSymbolTableproto 
	mul ecx
	mov edx,2
	mul edx
	add eax,OFFSET FunctionbfefAuxSymbolTable
	invoke WriteStruct,filehandler,eax,TYPE FunctionbfefAuxSymbolTableproto,18,1
	mov edx,eax
	pop eax
	push edx
	;.lf def
	add eax,TYPE SymbolEntryproto
	invoke WriteStruct,filehandler,eax,TYPE SymbolEntryproto,18,1
	;.ef def
	add eax,TYPE SymbolEntryproto
	invoke WriteStruct,filehandler,eax,TYPE SymbolEntryproto,18,1
	;.ef aux
	pop eax
	add eax,TYPE FunctionbfefAuxSymbolTableproto
	invoke WriteStruct,filehandler,eax,TYPE FunctionbfefAuxSymbolTableproto,18,1
	pop ecx
	dec ecx
	jne functionoutloop

	writefunctionnext:
	;write string table
	mov ebx,DWORD PTR StringTable
	invoke WriteStruct,filehandler,ADDR StringTable,1,1,ebx

	;close file
	invoke CloseHandle,filehandler
	ret
COFFsave ENDP

WriteStruct PROC FAR C USES eax ebx ecx edx esi edi,fhandler:HANDLE,structbegin:DWORD,structsize:DWORD,savesize:DWORD,savenum:DWORD
	mov ecx,savenum
writestructloop:
	push ecx
	neg ecx
	add ecx,savenum
	mov eax,structsize
	mul ecx
	add eax,structbegin

	invoke WriteFile,fhandler,eax,savesize,NULL,NULL
	pop ecx
	dec ecx
	jne writestructloop
	ret
WriteStruct ENDP

WriteBlank PROC FAR C USES eax ebx ecx edx esi edi,fhandler:HANDLE
	invoke WriteFile,fhandler,addr zeroBlank,1,NULL,NULL
	ret
WriteBlank ENDP
END