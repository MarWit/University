CC = gcc -g
CFLAGS = -Og -Wall
LDFLAGS =
ASFLAGS = -g

all: fibonacci zad4

zad4: zad4.o fibonacci.o

fibonacci: LDFLAGS += -nostdlib -static
fibonacci: fibonacci.o

clean:
	rm -f fibonacci zad4 *.o *~
