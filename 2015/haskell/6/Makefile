all: main-part-1.hs main-part-2.hs
	ghc -o main-part-1 main-part-1.hs
	ghc -o main-part-2 main-part-2.hs

clean:
	rm -f main-part-1
	rm -f main-part-2
	rm -f *.hi
	rm -f *.o

run: all
	./main-part-1 ../../../inputs/2015/6.txt && ./main-part-2 ../../../inputs/2015/6.txt