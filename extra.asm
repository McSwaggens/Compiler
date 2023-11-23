section .text

global system_call
system_call:
	;      rdi, rsi, rdx, rcx, r8, r9, [rsp+8]
	;     /    /    /    /    /   /   /
	;    /    /    /    /    /   /   /
	; rax, rdi, rsi, rdx, r10, r8, r9
	mov rax, rdi
	mov rdi, rsi
	mov rsi, rdx
	mov rdx, rcx
	mov r10, r8
	mov r8, r9
	mov r9, QWORD [rsp+8]
	syscall
	ret

global lzcnt
lzcnt:
	lzcnt rax, rdi
	ret

global tzcnt
tzcnt:
	tzcnt rax, rdi
	ret

global read_timestamp_counter
read_timestamp_counter:
	rdtsc
	shl rdx, 32
	or rax, rdx
	ret

