CC = gcc -g
CFLAGS = -Og -Wall
LDFLAGS =
ASFLAGS = -g

all: mulf zad5

zad5: zad5.o mulf.o

mulf: LDFLAGS += -nostdlib -static
mulf: mulf.o

clean:
	rm -f mulf zad5 *.o *~
