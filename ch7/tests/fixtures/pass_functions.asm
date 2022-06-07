extern rt_initialize, rt_print_int, rt_print_newline, rt_read_int
section .text

    global _start
    align 8
_start:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    mov rdi, 65536
    mov rsi, 64
    call rt_initialize
    mov r15, rax
    call rt_read_int
    mov rcx, rax
    cmp rcx, 0
    setg al
    movzx rcx, al
    lea r12, [rel foo]
    lea rbx, [rel bar]
    mov rdi, rcx
    mov rsi, r12
    mov rdx, rbx
    call which
    mov rcx, rax
    mov rdi, rcx
    call rt_print_int
    call rt_print_newline
    pop r12
    pop rbx
    pop rbp
    mov rax, 60
    mov rdi, 0
    syscall

    align 8
which:
    push rbp
    mov rbp, rsp
    sub rsp, 8
    push rbx
    mov rcx, rsi
    mov rbx, rdx
    cmp rdi, 0
    je .block2
    mov rbx, rcx

.block0:
    mov rdi, 7
    call rcx
    mov rcx, rax
    mov rdi, rcx
    mov rax, rbx
    pop rbx
    mov rsp, rbp
    pop rbp
    jmp rax

.block2:
    jmp .block0

    align 8
foo:
    push rbp
    mov rbp, rsp
    mov rcx, rdi
    mov rax, rcx
    add rax, 10
    pop rbp
    ret

    align 8
bar:
    push rbp
    mov rbp, rsp
    mov rcx, rdi
    mov rax, rcx
    xor rdx, rdx
    imul 10
    pop rbp
    ret
