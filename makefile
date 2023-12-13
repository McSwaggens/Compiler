compiler = clang
debug    = -DDEBUG -ggdb -Og
release  = -O3

args = \
	-mcmodel=medium\
	-ffreestanding\
	-fno-builtin-free\
	-fno-builtin-realloc\
	-std=c99\
	-march=znver3 \
	$(debug)

	# -nostartfiles\
	# -static\
	# -nolibc\

compiler: *.c *.h *.asm makefile
	nasm -felf64 extra.asm -o assembler_stuff.o
	$(compiler) compiler.c assembler_stuff.o $(args) -o compiler
	rm assembler_stuff.o
	# objdump -drwCS --no-addresses --no-show-raw-insn --visualize-jumps -Mintel -Mx86-64 compiler > compiler.asm
	objdump -drwCS --no-addresses --no-show-raw-insn -Mintel -Mx86-64 compiler > compiler.asm

clean:
	rm ./compiler

run: compiler
	fish -C "time ./compiler"
