.386
.model flat, stdcall
option casemap : none
include global.inc
include coff.inc
include Irvine32.inc
includelib "g:\masm32\lib\Irvine32.lib"
includelib "g:\masm32\lib\kernel32.lib"
includelib "g:\masm32\lib\user32.lib"
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
; Note:
;	RawDataptr isn't finished.
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
	inc ax
	mov SectionSymbolTable[ecx].n_scnum,ax
	;symbol type
	mov SectionSymbolTable[ecx].n_type,0
	;storage class
	mov SectionSymbolTable[ecx].n_sclass,3
	;aux
	;memset
	mov SectionSymbolTable[ecx].Auxidx,SymbolauxEntryCount
	mov SectionSymbolTable[ecx].n_numaux,1
	lea ebx,SymbolauxEntryCount
	lea eax,SymbolauxTable[ebx].content
	invoke ClearByte,eax,18
	;

	;add aux count
	inc SymbolauxEntryCount
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
;	GlobalvSymbolTable:n_name,n_value,n_scnum
;----------------------------------------
GlobalvSymbolTableFini PROC USES eax ebx ecx edx esi edi
	mov ecx,GlobalVCount
GlobalvSymbolTablel:
	push ecx
	neg ecx
	add ecx,GlobalVCount
	mov GlobalvSymbolTable[ecx].n_type,0
	mov GlobalvSymbolTable[ecx].n_sclass,3
	mov GlobalvSymbolTable[ecx].n_numaux,0
	pop ecx
	dec ecx
	jne GlobalvSymbolTablel
GlobalvSymbolTableFini ENDP

ClearByte PROC FAR C USES ecx,DataOffset:DWORD,bytenum:DWORD
	mov ecx,bytenum
	lea eax,DataOffset
clearbytel:
	mov [eax],BYTE PTR 0
	inc eax
	loop clearbytel
	ret
ClearByte ENDP
END