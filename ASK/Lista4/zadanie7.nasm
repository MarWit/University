[bits 64]

mov rdi, 0xffffffffffffffff ; high a
mov rsi, 0xffffffffffffffff ; low a

mov rdx, 0xffffffffffffffff ; high b
mov rcx, 0x1                ; low b

; output = rax:rdx
; tmp = r8 .. r11

; r8 = low
; r9 = carry

mov r8, rsi     ; r8 = rsi
shr rsi, 63     ; rasi >>> 63
add r8, rcx     ; r8 = r8 + rcx
mov r9, rdx     ; r9 = rdx
shr r9, 63      ; r9 >>> 63
xor rsi, r9     ; rsi = rsi ^ r9

mov rax, rdi    ; rax <- rdi
add rax, rdx    ; rax = rax + rdx
add rax, rsi    ; rax = rax + rsi
mov rdx, r8     ; rdx = r8
