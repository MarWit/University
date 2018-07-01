[bits 64]

; rdi = input
; rax = output
; rol
; rop

glob _start
_start:
    mov rdi, 0x12345678     ; rdi  = 0x12345678

    ; Program
    mov rdi, rax
    ror rdi, 8             ; rax  = 0x56781234
    ror al,  4              ; rax  = 0x56783412
    ror ah,  4              ; rax  = 0x78563412



