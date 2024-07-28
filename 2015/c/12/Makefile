CC=gcc
CFLAGS=-Wall -Wextra -Werror -std=c99 -pedantic -g

all: main-part-1 main-part-2 cJSON
	$(CC) $(CFLAGS) -o main-part-1 main-part-1.o
	$(CC) $(CFLAGS) -o main-part-2 main-part-2.c cJSON.o 

main-part-1: main-part-1.c
	$(CC) $(CFLAGS) -c main-part-1.c

main-part-2: main-part-2.c
	$(CC) $(CFLAGS) -c main-part-2.c

cJSON: cJSON/cJSON.c
	$(CC) $(CFLAGS) -c cJSON/cJSON.c

clean:
	rm -f main-part-1 main-part-1.o
	rm -f main-part-2

run:
	./main-part-1 ../../../inputs/2015/12.txt && ./main-part-2 ../../../inputs/2015/12.txt