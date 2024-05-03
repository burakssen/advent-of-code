CC=gcc
CFLAGS=-Wall -Wextra -Werror -std=c99 -pedantic -g

all: main-part1 main-part2
	$(CC) $(CFLAGS) -o main-part1 main-part1.o
	$(CC) $(CFLAGS) -o main-part2 main-part2.o

main-part1: main-part1.c
	$(CC) $(CFLAGS) -c main-part1.c

main-part2: main-part2.c
	$(CC) $(CFLAGS) -c main-part2.c
