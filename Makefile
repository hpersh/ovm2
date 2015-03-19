CFLAGS =	-g
#CFLAGS =	
#CFLAGS =	-O3 -DNDEBUG
#CFLAGS =	-pg

all:
	gcc -O3 -fomit-frame-pointer -DNDEBUG -c -fPIC ovm.c
	gcc -shared ovm.o -o libovm.so
	gcc -O3 -fomit-frame-pointer -DNDEBUG -L. -lovm -o test test.c

static:
	gcc -O3 -fomit-frame-pointer -DNDEBUG -o test ovm.c test.c

debug:
	gcc -g -pg -o test ovm.c test.c

hll:
	bison -d -t hll.y
	flex -d hll.l
	gcc -DYYDEBUG=1 lex.yy.c hll.tab.c hll.c

clean:
	rm -f *~ *.o *.so test *.core
