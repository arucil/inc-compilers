	.file	"runtime.c"
	.text
	.type	syscall5, @function
syscall5:
.LFB0:
	.cfi_startproc
#APP
# 11 "runtime.c" 1
	movq %rdi,%rax
movq %rsi,%rdi
movq %rdx,%rsi
movq %rcx,%rdx
movq %r8,%r10
movq %r9,%r8
syscall

# 0 "" 2
#NO_APP
	ud2
	.cfi_endproc
.LFE0:
	.size	syscall5, .-syscall5
	.type	write, @function
write:
.LFB2:
	.cfi_startproc
	movq	%rsi, %rax
	movq	%rdx, %rcx
	movslq	%edi, %rsi
	movl	$0, %r9d
	movl	$0, %r8d
	movq	%rax, %rdx
	movl	$1, %edi
	call	syscall5
	ret
	.cfi_endproc
.LFE2:
	.size	write, .-write
	.globl	print_int
	.type	print_int, @function
print_int:
.LFB4:
	.cfi_startproc
	testq	%rdi, %rdi
	jne	.L6
	movb	$48, 21+buf.1(%rip)
	leaq	21+buf.1(%rip), %rsi
	jmp	.L5
.L6:
	leaq	22+buf.1(%rip), %rsi
	movabsq	$-3689348814741910323, %r8
.L4:
	subq	$1, %rsi
	movq	%rdi, %rax
	mulq	%r8
	shrq	$3, %rdx
	leaq	(%rdx,%rdx,4), %rcx
	addq	%rcx, %rcx
	movq	%rdi, %rax
	subq	%rcx, %rax
	addl	$48, %eax
	movb	%al, (%rsi)
	movq	%rdi, %rax
	movq	%rdx, %rdi
	cmpq	$9, %rax
	ja	.L4
.L5:
	leaq	22+buf.1(%rip), %rdx
	subq	%rsi, %rdx
	movl	$1, %edi
	call	write
	ret
	.cfi_endproc
.LFE4:
	.size	print_int, .-print_int
	.globl	print_str
	.type	print_str, @function
print_str:
.LFB5:
	.cfi_startproc
	leaq	8(%rdi), %rsi
	movq	(%rdi), %rdx
	movl	$1, %edi
	call	write
	ret
	.cfi_endproc
.LFE5:
	.size	print_str, .-print_str
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"true"
	.text
	.globl	print_bool
	.type	print_bool, @function
print_bool:
.LFB6:
	.cfi_startproc
	testq	%rdi, %rdi
	je	.L10
	movl	$4, %edx
	leaq	.LC0(%rip), %rsi
	movl	$1, %edi
	call	write
	ret
.L10:
	movl	$4, %edx
	leaq	.LC0(%rip), %rsi
	movl	$1, %edi
	call	write
	ret
	.cfi_endproc
.LFE6:
	.size	print_bool, .-print_bool
	.section	.rodata.str1.1
.LC1:
	.string	"\n"
	.text
	.globl	print_newline
	.type	print_newline, @function
print_newline:
.LFB7:
	.cfi_startproc
	movl	$1, %edx
	leaq	.LC1(%rip), %rsi
	movl	$1, %edi
	call	write
	ret
	.cfi_endproc
.LFE7:
	.size	print_newline, .-print_newline
	.globl	read_line
	.type	read_line, @function
read_line:
.LFB8:
	.cfi_startproc
	pushq	%r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	pushq	%rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	pushq	%rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	movq	%rdi, %rbp
	movq	%rsi, %r12
	movq	%rdi, %rbx
.L17:
	movl	$0, %r9d
	movl	$0, %r8d
	movl	$1, %ecx
	movq	%rbx, %rdx
	movl	$0, %esi
	movl	$0, %edi
	call	syscall5
	testq	%rax, %rax
	je	.L14
	cmpb	$10, (%rbx)
	je	.L14
	addq	$1, %rbx
	movq	%rbx, %rax
	subq	%rbp, %rax
	cmpq	%r12, %rax
	jne	.L17
	movq	%r12, %rax
	jmp	.L13
.L14:
	movq	%rbx, %rax
	subq	%rbp, %rax
.L13:
	popq	%rbx
	.cfi_def_cfa_offset 24
	popq	%rbp
	.cfi_def_cfa_offset 16
	popq	%r12
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE8:
	.size	read_line, .-read_line
	.section	.rodata.str1.1
.LC2:
	.string	"invalid integer\n"
	.text
	.globl	read_int
	.type	read_int, @function
read_int:
.LFB9:
	.cfi_startproc
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movl	$80, %esi
	leaq	buf.0(%rip), %r12
	movq	%r12, %rdi
	call	read_line
	leaq	(%r12,%rax), %rbp
	movq	%r12, %rbx
	cmpq	%r12, %rbp
	jbe	.L22
.L21:
	cmpb	$32, (%rbx)
	jne	.L22
	addq	$1, %rbx
	cmpq	%rbx, %rbp
	jne	.L21
	movq	%rbp, %rbx
.L23:
	movl	$16, %edx
	leaq	.LC2(%rip), %rsi
	movl	$2, %edi
	call	write
	movl	$0, %r9d
	movl	$0, %r8d
	movl	$0, %ecx
	movl	$0, %edx
	movl	$1, %esi
	movl	$60, %edi
	call	syscall5
	cmpb	$45, (%rbx)
	je	.L26
	movl	$0, %r12d
	movl	$0, %r13d
	jmp	.L31
.L22:
	cmpq	%rbx, %rbp
	je	.L23
	movl	$0, %r12d
	cmpb	$45, (%rbx)
	je	.L26
.L28:
	movl	$0, %r13d
	cmpq	%rbp, %rbx
	jnb	.L31
	movzbl	(%rbx), %eax
	leal	-48(%rax), %edx
	cmpb	$9, %dl
	jbe	.L30
.L29:
	cmpb	$0, (%rbx)
	jne	.L41
.L31:
	movq	%r13, %rax
	negq	%rax
	testl	%r12d, %r12d
	cmovne	%rax, %r13
	movq	%r13, %rax
	popq	%rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
.L26:
	.cfi_restore_state
	addq	$1, %rbx
	movl	$1, %r12d
	jmp	.L28
.L30:
	leaq	0(%r13,%r13,4), %rdx
	movsbl	%al, %eax
	subl	$48, %eax
	cltq
	leaq	(%rax,%rdx,2), %r13
	addq	$1, %rbx
	cmpq	%rbx, %rbp
	je	.L31
	movzbl	(%rbx), %eax
	leal	-48(%rax), %edx
	cmpb	$9, %dl
	jbe	.L30
	cmpq	%rbx, %rbp
	ja	.L29
	jmp	.L31
.L41:
	movl	$16, %edx
	leaq	.LC2(%rip), %rsi
	movl	$2, %edi
	call	write
	movl	$0, %r9d
	movl	$0, %r8d
	movl	$0, %ecx
	movl	$0, %edx
	movl	$1, %esi
	movl	$60, %edi
	call	syscall5
	jmp	.L31
	.cfi_endproc
.LFE9:
	.size	read_int, .-read_int
	.local	buf.0
	.comm	buf.0,80,32
	.local	buf.1
	.comm	buf.1,22,16
	.ident	"GCC: (GNU) 11.2.0"
	.section	.note.GNU-stack,"",@progbits
