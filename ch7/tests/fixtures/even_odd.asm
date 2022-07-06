extern rt_initialize, rt_print_bool, rt_print_newline, rt_read_int
section .text

    global _start
    align 8
_start:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    push rbx
    mov rdi, 65536
    mov rsi, 64
    call rt_initialize
    mov r15, rax
    call rt_read_int
    mov rbx, rax
    mov rdi, rbx
    call even
    mov rcx, rax
    mov rdi, rcx
    call rt_print_bool
    call rt_print_newline
    mov rdi, rbx
    call odd
    mov rcx, rax
    mov rdi, rcx
    call rt_print_bool
    call rt_print_newline
    pop rbx
    mov rsp, rbp
    pop rbp
    mov rax, 60
    mov rdi, 0
    syscall

    align 8
even:
    push rbp
    mov rbp, rsp
    mov rcx, rdi
    cmp rcx, 0
    je .block0
    sub rcx, 1
    mov rdi, rcx
    pop rbp
    jmp odd

.block0:
    mov rax, 1
    pop rbp
    ret

    align 8
odd:
    push rbp
    mov rbp, rsp
    mov rcx, rdi
    cmp rcx, 0
    je .block2
    sub rcx, 1
    mov rdi, rcx
    pop rbp
    jmp even

.block2:
    xor rax, rax
    pop rbp
    ret
