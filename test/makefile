




all: Lexer
	ghc --make Willy.hs 

Lexer:
	alex Lexer.x -o Lexer.hs -g

clear:
	rm *.hi *o Lexer.hs