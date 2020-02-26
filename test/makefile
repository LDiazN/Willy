all: Lexer Parser 
	ghc --make -dynamic Willy.hs 

Lexer: Lexer.x
	alex Lexer.x -o Lexer.hs -g

Parser: Parser.y
	happy Parser.y

clear:
	rm  Willy *.hi *o Lexer.hs Parser.hs