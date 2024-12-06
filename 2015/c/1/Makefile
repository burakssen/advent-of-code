CC=gcc
CFLAGS=-Wall -Wextra -Werror -std=c99 -pedantic -O3
TARGET=main
SRC=main.c

all: $(TARGET)
	$(CC) $(CFLAGS) $(SRC) -o $(TARGET)

clean:
	rm -f $(TARGET)

run: all
	./$(TARGET) ../../../inputs/2015/1.txt

.PHONY: all clean
