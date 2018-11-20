CC=gcc
CFLAGS=-std=gnu99 -Wall -Wextra

router: router.o table.o vec.o packets.o
table.o: vec.o

clean:
	rm -f *.o
distclean: clean
	rm -f router
