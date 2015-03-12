CFLAGS =	-g
#CFLAGS =	-O3
#CFLAGS =	-O3 -pg

all:
	gcc $(CFLAGS) -c -fPIC ovm.c
	gcc -shared ovm.o -o libovm.so
	gcc $(CFLAGS) -L. -lovm -o test test.c
