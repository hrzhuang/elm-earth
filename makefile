.PHONY: all clean

all: build/index.html build/main.js build/textures

clean:
	rm -r build

build:
	mkdir build

build/index.html: build index.html
	cp index.html build

build/main.js: build src/Main.elm
	elm make --optimize --output=build/main.js src/Main.elm

build/textures: build textures
	cp -r textures build
