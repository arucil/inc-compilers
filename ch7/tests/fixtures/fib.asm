extern rt_initialize, rt_print_int, rt_print_newline, rt_read_int
section .text

    global _start
    align 8
_start:
    push rbp
    mov rbp, rsp
    mov rdi, 65536
    mov rsi, 64
    call rt_initialize
    mov r15, rax
    call rt_read_int
    mov rcx, rax
    mov rdi, rcx
    call fib
    mov rcx, rax
    mov rdi, rcx
    call rt_print_int
    call rt_print_newline
    pop rbp
    mov rax, 60
    mov rdi, 0
    syscall

    align 8
fib:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    mov rbx, rdi
    cmp rbx, 2
    jl .block0
    mov rcx, rbx
    sub rcx, 1
    mov rdi, rcx
    call fib
    mov r12, rax
    sub rbx, 2
    mov rdi, rbx
    call fib
    mov rcx, rax
    mov rax, r12
    add rax, rcx

.epilogue:
    pop r12
    pop rbx
    pop rbp
    ret

.block0:
    mov rax, rbx
    jmp .epilogue
