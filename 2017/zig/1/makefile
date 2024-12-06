all: main

main: main.zig
	zig build-exe main.zig

run: main
	./main ../../../inputs/2017/1.txt

clean:
	rm -f main

.PHONY: all run clean
