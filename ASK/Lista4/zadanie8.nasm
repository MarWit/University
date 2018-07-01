[bits 64]

mov rdi, 0xffffffffffce1232 ; high a
mov rsi, 0xffffffffffffffff ; low a

mov rdx, 0xfffffff565854648 ; high b
mov rcx, 0x00000000007a7bd1 ; low b

; Algorytm Karacuby
; RAX = (High A * Low B + High B * Low A) + High bits of Lows multiplication
; RDX = Low bits of Lows multiplication

; output = rax:rdx
; temp r8 .. r11
; mul r/m64, r/m64 -> RDX:RAX

; r8  = rdx copy
; r9  = low multiplication
; r10 = rest of low multiplication
; r11 = rest of first high multiplication

mov r8, rdx  ; r8 = rdx
mov rax, rsi ; rax = rsi
mul rcx      ; rcx * rax -> rdx:rax
mov r9, rax  ; r9 = rax
mov r10, rdx ; r10 = rdx
; Tu jest k
mov rax, rdi ; rax = rdi
; High a * Low b
mul rcx      ; rdi * rcx -> rdx:rax
mov r11, rax ; r11 = rax
mov rax, r8  ; rax = r8
; High b * Low a
mul rsi      ; r8 * rsi -> rdx:rax
add rax, r10 ; rax = rax + r10
add rax, r11 ; rax = rax + r11
mov rdx, r9  ; rdx = r9


