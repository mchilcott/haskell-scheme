all: parser

parser: parser.hs
	ghc $^

clean:
	rm -f *.o
	rm -f *.hi
	rm parser
