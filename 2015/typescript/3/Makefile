all: build run

install_deps: package.json
	npm install

build: node_modules
	tsc main-part-1.ts
	tsc main-part-2.ts

clean:
	rm -rf node_modules
	rm -f main-part-1.js
	rm -f main-part-2.js

run: main-part-1.js main-part-2.js
	node main-part-1.js ../../../inputs/2015/3.txt && node main-part-2.js ../../../inputs/2015/3.txt