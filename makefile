.PHONY: all

all: build/index.html build/textures

build:
	mkdir build

build/index.html: build src/Main.elm
	elm make --optimize --output=build/index.html src/Main.elm

build/textures: build textures
	cp -r textures build
