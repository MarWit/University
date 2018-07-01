CC=gcc
CFLAGS=-std=gnu11 -Wall -Wextra

all: lib.so
lib.so:
	${CC} ${CFLAGS} -fPIC -shared lib_shared.c -o lib.so
clean:
	rm -f lib *.o *.so *~
