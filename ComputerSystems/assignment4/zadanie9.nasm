[bits 64]

mov edi, 0x7fffffff
mov esi, 0x1123

; Program
; r8  - sign(edi) == sign(esi)
; r9  - sign(esi)
; r10 - sign(edi+esi)
; r11 - tmp. under/overflow

mov eax, edi
add eax, esi

mov r8d, edi
shr r8d, 31

mov r9d, esi
shr r9d, 31

xor r8, r9
not r8d

mov r10d, eax
shr r10d, 31

; Check if underflow
mov r11, r8
and r11, r9
not r10
and r11, r10
not r10

; If underflow, zero eax and add 80000000h
sub r11, 1
mul r11
add r11, 1
shl r11, 31
add rax, r11

; Check if overflow
mov r11, r8
and r11, r10
not r9
and r11, r9

; If overflow, zero eax and add 7fffffffh
sub r11, 1
mul r11
add r11, 1
shl r11, 31
sub r11, r10
add rax, r11

