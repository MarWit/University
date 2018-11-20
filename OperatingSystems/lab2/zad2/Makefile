CC=gcc
CFLAGS=-std=gnu11 -Wall -Wextra -pthread

all: zad2_my zad2_named
zad2_my: sem.o
sem.o:
	${CC} ${CFLAGS} -c ../zad1/sem.c
clean:
	rm -f zad2_my zad2_named *.o *~
