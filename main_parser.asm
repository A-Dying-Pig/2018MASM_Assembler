TITLE Main Parser

; this file is used to parse a asm file

.386
.model flat, stdcall
include global.inc
include Irvine32.inc

.data
cmdTail byte 120 DUP(0)

.code

cmdArgu1 PROC USES esi, edx
	
	mov edx, OFFSET cmdTail
	call GetcommandTail
	