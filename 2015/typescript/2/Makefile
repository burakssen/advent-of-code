all: build run

install_deps: package.json
	npm install

build: node_modules
	tsc main.ts

clean:
	rm -rf node_modules
	rm -f main.js
	rm -f main.js.map

run: main.js
	node main.js ../../../inputs/2015/2.txt