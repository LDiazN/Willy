




all: Lexer
	ghc --make willy.hs 

Lexer:
	alex Lexer.x -o Lexer.hs -g

clear:
	rm *.hi *.o Lexer.hs