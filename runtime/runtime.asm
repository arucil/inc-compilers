section .bss
    int_buf resb 20
    char_buf resb 1
    input_buf resb 8192
    input_buf_len equ $-input_buf

section .data
    has_char dq 0

section .rodata
    newline db `\n`
    true_lit db `true`
    true_lit_len equ $-true_lit
    false_lit db `false`
    false_lit_len equ $-false_lit
    invalid_integer db `Invalid integer\n`
    invalid_integer_len equ $-invalid_integer

;; args
;;   rax: int
;;
;; returns:
;;   rcx: length
;;   r8: 1=negative, 0=non-negative
;;
;; uses:
;;   r9, rdx
%macro int_len 0
    mov r9, 10
    xor rcx, rcx
    xor r8, r8
    cmp rax, 0
    jge %%loop
    inc rcx
    neg rax
    inc r8
%%loop:
    inc rcx
    xor rdx, rdx
    div r9
    cmp rax, 0
    jne %%loop
%endmacro

%macro is_space 1
    cmp %1, ' '
    je %%end
    cmp %1, 10
    je %%end
    cmp %1, 8
    je %%end
    cmp %1, 13
    je %%end
%%end:
%endmacro

%macro read_char 1
    xor rax, rax
    mov rdi, 0
    mov rsi, %1
    mov rdx, 1
    syscall
%endmacro

section .text

;; args:
;;   rax: int
;;   rdi: buffer address
;;
;; returns:
;;   rcx: length
;;
;; uses:
;;   rsi, r9, r8, rdx
    global int_to_str
int_to_str:
    mov rsi, rax
    int_len
    mov rax, rsi
    cmp r8, 0
    je .positive
    neg rax
    mov [rdi], byte '-'
.positive:
    add rdi, rcx
    mov r9, 10
.loop:
    xor rdx, rdx
    div r9
    add rdx, 48
    dec rdi
    mov [rdi], dl
    cmp rax, 0
    jne .loop
    ret

;; args:
;;   rdi: buffer address
;;
;; returns:
;;   rcx: length
    global str_len
str_len:
    xor rcx, rcx
.loop:
    cmp [rdi], byte 0
    je .end
    inc rdi
    inc rcx
    jmp .loop
.end:
    ret

;; rdi: address
;; rcx: length
;;
;; returns:
;;   rax: int
;;
;; uses:
;;   r8, r9, rdx
    global str_to_int
str_to_int:
    xor rax, rax
    xor r8, r8
    mov r9, 10
    cmp [rdi], byte '-'
    jne .loop
    dec rcx
    inc rdi
    inc r8
.loop:
    cmp rcx, 0
    je .after
    mul r9
    xor rdx, rdx
    mov dl, [rdi]
    cmp dl, 57
    jg .error
    cmp dl, 48
    jl .error
    sub rdx, 48
    add rax, rdx
    dec rcx
    inc rdi
    jmp .loop
.after:
    cmp r8, 0
    je .end
    neg rax
.end:
    ret
.error:
    mov rax, 1
    mov rdi, 1
    mov rsi, invalid_integer
    mov rdx, invalid_integer_len
    syscall
    mov rax, 60
    mov rdi, 1
    syscall

;; args
;;   rax: int
;;
;; uses:
;;   rsi, rdi, r9, r8, rdx
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
;;   rdi: string address (string length followed by string data)
    global print_str
print_str:
    mov rdx, [rdi]
    lea rsi, [rdi + 8]
    mov rax, 1
    mov rdi, 1
    syscall
    ret

;; args
;;   rax: bool
;; uses:
;;   rsi, rdi, rdx
    global print_bool
print_bool:
    cmp rax, 0
    je .false
    mov rsi, true_lit
    mov rdx, true_lit_len
    mov rax, 1
    mov rdi, 1
    syscall
    ret
.false:
    mov rsi, false_lit
    mov rdx, false_lit_len
    mov rax, 1
    mov rdi, 1
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

;; returns:
;;   rax: int
;;
;; uses:
;;   rax, rsi, rdx, r8, r9, r10
    global read_int
read_int:
    mov rdi, input_buf
    mov rcx, input_buf_len
    call read_line
    mov rsi, input_buf
    ; skip spaces
.skip_spaces:
    mov al, [rsi]
    is_space al
    jne .skip_starting_spaces
    inc rsi
    dec rcx
    jmp .skip_spaces
.skip_starting_spaces:
    mov rdi, rsi
    add rsi, rcx
    dec rsi
.skip_starting_spaces_loop:
    mov al, [rsi]
    is_space al
    jne str_to_int
    dec rsi
    dec rcx
    jmp .skip_starting_spaces_loop

;; args:
;;   rdi: buffer address
;;   rcx: max length
;;
;; returns:
;;   rcx: length
;;
;; uses:
;;   rax, rsi, rdx, r8, r9, r10
    global read_line
read_line:
    mov r8, rdi
    mov r10, rcx
    xor r9, r9
    cmp qword [has_char], 0
    jne .skip_read
    jmp .start
.loop:
    mov byte [r8], al
    inc r8
    inc r9
    cmp r9, r10
    jge .end
.start:
    read_char char_buf
    cmp rax, 0
    je .end
.skip_read:
    mov al, [char_buf]
    cmp al, `\n`
    jne .loop
.end:
    mov rcx, r9
    mov qword [has_char], 0
    ret