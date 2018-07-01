CC = gcc -g
CFLAGS = -Og -Wall
LDFLAGS =
ASFLAGS = -g

all: lcm_gcd zad2

zad2: zad2.o lcm_gcd.o

lcm_gcd: LDFLAGS += -nostdlib -static
lcm_gcd: lcm_gcd.o

clean:
	rm -f lcm_gcd zad2 *.o *~
