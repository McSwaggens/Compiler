debug    = -DDEBUG -g3 -O0
release  = -O3

args = \
	-ffreestanding\
	-fno-builtin-free\
	-fno-builtin-realloc\
	-std=c99\
	$(debug)

	# -nostartfiles\
	# -static\
	# -nolibc\

compiler: *.c *.h *.asm makefile
	clang compiler.c $(args) -o compiler

linux: *.c *.h *.asm makefile
	nasm -felf64 extra.asm -o assembler_stuff.o
	# objdump -drwCS --no-addresses --no-show-raw-insn --visualize-jumps -Mintel -Mx86-64 compiler > compiler.asm

clean:
	rm *.o ./compiler

run: compiler
	fish -C "time ./compiler"
