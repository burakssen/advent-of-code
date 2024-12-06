CC=gcc
CFLAGS=-Wall -Wextra -Werror -std=c99 -pedantic -g

all: main
	$(CC) $(CFLAGS) -o main main.o

main: main.c
	$(CC) $(CFLAGS) -c main.c

clean:
	rm -f main main.o

run: all
	./main ../../../inputs/2015/23.txt