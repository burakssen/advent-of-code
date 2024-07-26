CC=gcc
CFLAGS=-Wall -Wextra -Werror -std=c99 -pedantic -g

all: main
main: main.o
	$(CC) $(CFLAGS) -o main main.o
	
main.o: main.c
	$(CC) $(CFLAGS) -c main.c

clean:
	rm -f main main.o

run: all
	./main ../../../inputs/2015/2.txt