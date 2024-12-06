all: part1/Main.java part2/Main.java position/Position.java
	javac part1/Main.java position/Position.java
	javac part2/Main.java position/Position.java

clean:
	rm -f part1/Main.class
	rm -f part2/Main.class
	rm -f position/Position.class

run: all
	java Part1.Main ../../../inputs/2015/3.txt && java Part2.Main ../../../inputs/2015/3.txt