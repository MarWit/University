CC = gcc -g
CFLAGS = -Og -Wall
LDFLAGS =
ASFLAGS = -g

all: clz zad1

zad1: zad1.o clz.o

clz: LDFLAGS += -nostdlib -static
clz: clz.o

clean:
	rm -f clz zad1 *.o *~
