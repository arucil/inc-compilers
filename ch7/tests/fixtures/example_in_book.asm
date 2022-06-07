extern rt_initialize, rt_print_int, rt_print_newline
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
    call main
    mov rcx, rax
    mov rdi, rcx
    call rt_print_int
    call rt_print_newline
    pop rbp
    mov rax, 60
    mov rdi, 0
    syscall

    align 8
add:
    push rbp
    mov rbp, rsp
    mov rcx, rdi
    mov rdx, rsi
    mov rax, rcx
    add rax, rdx
    pop rbp
    ret

    align 8
main:
    push rbp
    mov rbp, rsp
    mov rdi, 4
    mov rsi, 2
    pop rbp
    jmp add
