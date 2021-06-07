
tester : tester.o optimizing.o expected.o
	clang -m64 -o tester optimizing.o tester.o expected.o

runner : runner.o optimizing.o
	clang -m64 -o runner optimizing.o runner.o

tester.o : tester.c expected.h
	clang -m64 -c tester.c

runner.o : runner.c
	clang -m64 -c runner.c

optimizing.o : optimizing.s
	nasm -f elf64 -o optimizing.o optimizing.s

expected.o : expected.c expected.h
	clang -m64 -c expected.c
