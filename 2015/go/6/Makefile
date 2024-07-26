all: main-part-1.go main-part-2.go
	go build -o main-part-1 main-part-1.go
	go build -o main-part-2 main-part-2.go

clean:
	rm -f main-part-1
	rm -f main-part-2

run: all
	./main-part-1 ../../../inputs/2015/6.txt && ./main-part-2 ../../../inputs/2015/6.txt