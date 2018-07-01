CC = gcc -g
CFLAGS = -Og -Wall
LDFLAGS =
ASFLAGS = -g

all: insert_sort zad3

zad3: zad3.o insert_sort.o

insert_sort: LDFLAGS += -nostdlib -static
insert_sort: insert_sort.o

clean:
	rm -f insert_sort zad3 *.o *~
