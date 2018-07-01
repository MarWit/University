CC=gcc
CFLAGS=-std=gnu11 -Wall -Wextra
LDFLAGS=-fPIC -shared

all: zad5_lib.so zad5_1 zad5_2
zad5_1: zad5.c
	${CC} ${CFLAGS} -Wl,-R. -D VARIANT -o zad5_1 zad5.c zad5_lib.so
zad5_2: zad5.c
	${CC} ${CFLAGS} -ldl -o zad5_2 zad5.c
zad5_lib.so: zad5_lib.c
	${CC} ${CFLAGS} ${LDFLAGS} -o zad5_lib.so zad5_lib.c
clean:
	rm -f zad5_1 zad5_2 zad5_lib *.so *.o *~
