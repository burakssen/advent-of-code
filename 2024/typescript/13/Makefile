all: build run

install_deps: package.json
	npm install
	npm i --save-dev @types/node

build: node_modules
	tsc --downlevelIteration main.ts

clean:
	rm -rf node_modules
	rm -f main.js
	rm -f main.js.map

run: main.js
	node main.js ../../../inputs/2024/13.txt