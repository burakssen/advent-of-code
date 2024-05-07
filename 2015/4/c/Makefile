CC=gcc
CFLAGS=-Wall -std=c99 -pedantic -g
INCLUDE=-I/opt/homebrew/Cellar/openssl@3/3.3.0/include
LIBS=-L/opt/homebrew/Cellar/openssl@3/3.3.0/lib -lssl -lcrypto

all: main
	$(CC) $(CFLAGS) -o main main.o $(LIBS)

main: main.c
	$(CC) $(CFLAGS) -c main.c $(INCLUDE)

clean:
	rm -f main main.o