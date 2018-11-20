[bits 64]

; rdi input
; rax output

mov rdi, 0x1122334455667788

; ------
mov rax, rdi

; mov ebx, eax
; ror bx, 8
; ror ebx, 16
; ror bx, 8
mov r8d, eax
ror r8w, 8
ror r8d, 16
ror r8w, 8

ror rax, 32

; mov ecx, eax
; ror cx, 8
; ror ecx, 16
; ror cx, 8
mov r9d, eax
ror r9w, 8
ror r9d, 16
ror r9w, 8

; mov eax, ebx
; shl rax, 32
; add rax, rcx
mov eax, r8d
shl rax, 32
add rax, r9


