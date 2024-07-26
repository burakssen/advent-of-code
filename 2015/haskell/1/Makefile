all: main.hs
	ghc -o main main.hs

clean:
	rm -f main
	rm -f *.hi
	rm -f *.o

run: all
	./main ../../../inputs/2015/1.txt