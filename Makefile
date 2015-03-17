#CFLAGS =	-g
CFLAGS =	-O3 -fomit-frame-pointer -DNDEBUG
#CFLAGS =	-O3 -DNDEBUG
#CFLAGS =	-pg

all:
	gcc $(CFLAGS) -c -fPIC ovm.c
	gcc -shared ovm.o -o libovm.so
	gcc $(CFLAGS) -L. -lovm -o test test.c

static:
	gcc $(CFLAGS) -o test ovm.c test.c

clean:
	rm -f *~ *.o *.so test *.core
