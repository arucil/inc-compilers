extern rt_allocate, rt_fill_array, rt_initialize, rt_length_error, rt_out_of_bounds_error, rt_print_int, rt_print_newline
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
    mov rax, 5
    cmp rax, 0
    jge .block28
    mov rdi, 5
    call rt_length_error

.block28:
    mov rsi, 5
    shl rsi, 3
    mov rdi, 5
    shl rdi, 5
    or rdi, 5
    mov rdx, r15
    call rt_allocate
    mov rdi, rax
    xor rsi, rsi
    call rt_fill_array
    mov rcx, rax
    mov rax, 0
    cmp rax, 0
    jge .block26

.block25:
    mov r11, rcx
    mov rcx, qword [r11]
    shr rcx, 5
    xor rdi, rdi
    mov rsi, rcx
    call rt_out_of_bounds_error

.block26:
    mov r11, rcx
    mov rdx, qword [r11]
    shr rdx, 5
    mov rax, 0
    cmp rax, rdx
    jl .block24
    jmp .block25

.block24:
    xor r11, r11
    add r11, 1
    shl r11, 3
    add r11, rcx
    mov qword [r11], 3
    mov rax, 1
    cmp rax, 0
    jge .block22

.block21:
    mov r11, rcx
    mov rcx, qword [r11]
    shr rcx, 5
    mov rdi, 1
    mov rsi, rcx
    call rt_out_of_bounds_error

.block22:
    mov r11, rcx
    mov rdx, qword [r11]
    shr rdx, 5
    mov rax, 1
    cmp rax, rdx
    jl .block20
    jmp .block21

.block20:
    mov r11, 1
    add r11, 1
    shl r11, 3
    add r11, rcx
    mov qword [r11], 2
    mov rax, 2
    cmp rax, 0
    jge .block18

.block17:
    mov r11, rcx
    mov rcx, qword [r11]
    shr rcx, 5
    mov rdi, 2
    mov rsi, rcx
    call rt_out_of_bounds_error

.block18:
    mov r11, rcx
    mov rdx, qword [r11]
    shr rdx, 5
    mov rax, 2
    cmp rax, rdx
    jl .block16
    jmp .block17

.block16:
    mov r11, 2
    add r11, 1
    shl r11, 3
    add r11, rcx
    mov qword [r11], 6
    mov rax, 3
    cmp rax, 0
    jge .block14

.block13:
    mov r11, rcx
    mov rcx, qword [r11]
    shr rcx, 5
    mov rdi, 3
    mov rsi, rcx
    call rt_out_of_bounds_error

.block14:
    mov r11, rcx
    mov rdx, qword [r11]
    shr rdx, 5
    mov rax, 3
    cmp rax, rdx
    jl .block12
    jmp .block13

.block12:
    mov r11, 3
    add r11, 1
    shl r11, 3
    add r11, rcx
    mov qword [r11], 5
    mov rax, 4
    cmp rax, 0
    jge .block10

.block9:
    mov r11, rcx
    mov rcx, qword [r11]
    shr rcx, 5
    mov rdi, 4
    mov rsi, rcx
    call rt_out_of_bounds_error

.block10:
    mov r11, rcx
    mov rdx, qword [r11]
    shr rdx, 5
    mov rax, 4
    cmp rax, rdx
    jl .block8
    jmp .block9

.block8:
    mov r11, 4
    add r11, 1
    shl r11, 3
    add r11, rcx
    mov qword [r11], 4
    mov rdi, rcx
    call prod
    mov rcx, rax
    mov rdi, rcx
    call rt_print_int
    call rt_print_newline
    pop rbp
    mov rax, 60
    mov rdi, 0
    syscall

    align 8
prod:
    push rbp
    mov rbp, rsp
    mov rsi, rdi
    mov rdx, 1
    xor rcx, rcx

.block0:
    mov r11, rsi
    mov rdi, qword [r11]
    shr rdi, 5
    cmp rcx, rdi
    jl .block5
    mov rax, rdx
    pop rbp
    ret

.block5:
    cmp rcx, 0
    jge .block4

.block3:
    mov r11, rsi
    mov rdx, qword [r11]
    shr rdx, 5
    mov rdi, rcx
    mov rsi, rdx
    call rt_out_of_bounds_error

.block4:
    mov r11, rsi
    mov rdi, qword [r11]
    shr rdi, 5
    cmp rcx, rdi
    jl .block2
    jmp .block3

.block2:
    mov r11, rcx
    add r11, 1
    shl r11, 3
    add r11, rsi
    mov rdi, qword [r11]
    imul rdx, rdi
    add rcx, 1
    jmp .block0
