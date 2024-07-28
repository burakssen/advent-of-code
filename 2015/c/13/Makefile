CC=gcc
CFLAGS=-Wall -Wextra -Werror -std=c99 -pedantic -g

all: main-part-1 main-part-2
	$(CC) $(CFLAGS) -o main-part-1 main-part-1.o
	$(CC) $(CFLAGS) -o main-part-2 main-part-2.c

main-part-1: main-part-1.c
	$(CC) $(CFLAGS) -c main-part-1.c

main-part-2: main-part-2.c
	$(CC) $(CFLAGS) -c main-part-2.c

clean:
	rm -f main-part-1 main-part-1.o
	rm -f main-part-2 main-part-2.o

run: all
	./main-part-1 ../../../inputs/2015/13.txt && ./main-part-2 ../../../inputs/2015/13.txt