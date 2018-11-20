CC = gcc -g
CFLAGS = -Og -Wall
LDFLAGS =
ASFLAGS = -g

all: approx_sqrt zad8

zad8: zad8.o approx_sqrt.o

approx_sqrt: LDFLAGS += -nostdlib -static
approx_sqrt: approx_sqrt.o

clean:
	rm -f approx_sqrt zad8 *.o *~
