HC=ghc
SOURCES=src/Main.hs src/Grammar.hs
GEN_SOURCES=src/Lexer.x src/Parser.y
GENERATED=src/Lexer.hs src/Parser.hs
PACKAGE=tt2020B.zip

.PHONY: pack all run clean test

all: solution

run: solution
	./solution

test: solution
	./test.sh

clean:
	rm -rf src/*.o src/*.hi
	rm -rf $(GENERATED)
	rm -f solution

solution: $(GENERATED) $(SOURCES)
	$(HC) -Wall -O2 -XBangPatterns -i./src -tmpdir . ./src/Main.hs -o solution

$(GENERATED): $(GEN_SOURCES) $(SOURCES)
	alex src/Lexer.x -o src/Lexer.hs
	happy src/Parser.y -o src/Parser.hs

pack: $(GENERATED)
	zip $(PACKAGE) -r Makefile src
