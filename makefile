all: parser

parser: parser.hs
	ghc $^

clean:
	rm -f *.o
	rm -f *.hi
	rm parser

# Some very poor testing
test:
# Atom
	./parser "atom"
# Boolean
	./parser "#t"
	./parser "#f"
# Character
	./parser "#\a"
	./parser "#\A"
	./parser "#\("
	./parser "#\ "
	./parser "#\space"
	./parser "#\newline"
# Number
	./parser 8375
	./parser 83.75
# String
	./parser "\"string\""
	./parser "\"\\\"\""
	./parser "\"\n\""
	./parser "\"\f\""
