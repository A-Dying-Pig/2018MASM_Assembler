.386
.model flat, stdcall
option casemap : none
include global.inc
include Irvine32.inc
includelib "g:\masm32\lib\Irvine32.lib"
includelib "g:\masm32\lib\kernel32.lib"
includelib "g:\masm32\lib\user32.lib"
.code
main PROC
mov FileHeader.f_magic,4
movzx eax,FileHeader.f_magic
exit
main ENDP
END main