compiler: *.c *.h
	nasm -felf64 extra.asm -o assembler_stuff.o
	clang compiler.c assembler_stuff.o -fno-builtin-free -fno-builtin-realloc -DDEBUG -std=c99 -O0 -ggdb -o compiler
	# clang compiler.c assembler_stuff.o -std=c99 -O3 -o compiler
	rm assembler_stuff.o

clean:
	rm ./compiler

run: compiler
	fish -C "time ./compiler"
