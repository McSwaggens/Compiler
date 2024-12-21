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

compiler: *.c *.h makefile
	clang compiler.c $(args) -o compiler

clean:
	rm *.o ./compiler

run: compiler
	fish -C "time ./compiler"
