CFLAGS =	-g
#CFLAGS =	-O3
#CFLAGS =	-O3 -pg

all:
	gcc $(CFLAGS) ovm.c test.c
