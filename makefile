compiler: *.c *.h makefile
	nasm -felf64 extra.asm -o assembler_stuff.o
	# clang compiler.c assembler_stuff.o -fno-builtin-free -fno-builtin-realloc -std=c99 -march=znver3         -O3       -o compiler
	clang compiler.c assembler_stuff.o -fno-builtin-free -fno-builtin-realloc -std=c99 -march=znver3 -DDEBUG -ggdb -Og -o compiler
	rm assembler_stuff.o
	# objdump -drwCS --no-addresses --no-show-raw-insn --visualize-jumps -Mintel -Mx86-64 compiler > compiler.asm
	objdump -drwCS --no-addresses --no-show-raw-insn -Mintel -Mx86-64 compiler > compiler.asm

clean:
	rm ./compiler

run: compiler
	fish -C "time ./compiler"
