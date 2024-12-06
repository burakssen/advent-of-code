all: main

main: src/main.rs
	cargo build --release

run:
	cargo run --release -- ../../../inputs/2017/9.txt

clean:
	rm -f main

.PHONY: all run clean
