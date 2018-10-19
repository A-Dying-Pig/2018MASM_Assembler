.386
.model flat, stdcall
option casemap : none
include Irvine32.inc
include test.inc
includelib "g:\masm32\lib\Irvine32.lib"
includelib "g:\masm32\lib\kernel32.lib"
includelib "g:\masm32\lib\user32.lib"
.data
.code
pro1 PROC
    mov eax,1
pro1 ENDP
main PROC
    mov eax,mword
    exit
main ENDP
END main