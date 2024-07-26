CC=gcc
CFLAGS=-Wall -Wextra -Werror -pedantic -std=c99 -g

all: main
	$(CC) $(CFLAGS) -o main main.o

main: main.c
	$(CC) $(CFLAGS) -c main.c

clean:
	rm -f main main.o

run: all
	./main ../../../inputs/2015/5.txt