CC=gcc
CFLAGS=-Wall -std=c99 -pedantic -g
# Adjust as needed
OPENSSL_CFLAGS := $(shell pkg-config --cflags openssl)
OPENSSL_LIBS := $(shell pkg-config --libs openssl)

all: main

main: main.o
	$(CC) $(CFLAGS) -o main main.o $(OPENSSL_LIBS)

main.o: main.c
	$(CC) $(CFLAGS) $(OPENSSL_CFLAGS) -c main.c

clean:
	rm -f main main.o

run: all
	./main ../../../inputs/2015/4.txt