section .bss
    int_buf resb 20

section .rodata
    newline db `\n`

section .text

;; args:
;;   rax: int
;;   rdi: buffer address
;;
;; returns:
;;   rcx: length
;;
;; uses:
;;   rsi, rbx, r8, rdx
    global int_to_str
int_to_str:
    mov rsi, rax
    call int_len
    mov rax, rsi
    cmp r8, 0
    je int_to_str_positive
    neg rax
    mov [rdi], byte '-'
int_to_str_positive:
    add rdi, rcx
    mov rbx, 10
int_to_str_loop:
    xor rdx, rdx
    div rbx
    add rdx, 48
    dec rdi
    mov [rdi], dl
    cmp rax, 0
    jne int_to_str_loop
    ret

;; args:
;;   rdi: buffer address
;;
;; returns:
;;   rcx: length
    global str_len
str_len:
    xor rcx, rcx
str_len_loop:
    cmp [rdi], byte 0
    je str_len_end
    inc rdi
    inc rcx
    jmp str_len_loop
str_len_end:
    ret

;; args:
;;   rdi: address
;;   rcx: length
;;
;; returns:
;;   rax: int
;;
;; uses:
;;   r8, rbx, rdx
    global str_to_int
str_to_int:
    xor rax, rax
    xor r8, r8
    mov rbx, 10
    cmp [rdi], byte '-'
    jne str_to_int_loop
    dec rcx
    inc rdi
    inc r8
str_to_int_loop:
    cmp rcx, 0
    je str_to_int_after
    mul rbx
    xor rdx, rdx
    mov dl, [rdi]
    sub rdx, 48
    add rax, rdx
    dec rcx
    inc rdi
    jmp str_to_int_loop
str_to_int_after:
    cmp r8, 0
    je str_to_int_end
    neg rax
str_to_int_end:
    ret

;; args
;;   rax: int
;;
;; uses:
;;   rsi, rdi, rbx, r8, rdx
    global print_int
print_int:
    mov rdi, int_buf
    call int_to_str
    mov rax, 1
    mov rdi, 1
    mov rsi, int_buf
    mov rdx, rcx
    syscall
    ret

;; args
;;   rsi: buffer address
;;   rcx: length
    global print_str
print_str:
    mov rax, 1
    mov rdi, 1
    mov rdx, rcx
    syscall
    ret

    global print_newline
print_newline:
    mov rax, 1
    mov rdi, 1
    mov rsi, newline
    mov rdx, 1
    syscall
    ret

;; args:
;;   rdi: exit code (0~255)
    global exit
exit:
    mov rax, 60
    syscall

;; args
;;   rax: int
;;
;; returns:
;;   rcx: length
;;   r8: 1=negative, 0=non-negative
;;
;; uses:
;;   rbx, rdx
int_len:
    mov rbx, 10
    xor rcx, rcx
    xor r8, r8
    cmp rax, 0
    jge int_len_loop
    inc rcx
    neg rax
    inc r8
int_len_loop:
    inc rcx
    xor rdx, rdx
    div rbx
    cmp rax, 0
    jne int_len_loop
    ret