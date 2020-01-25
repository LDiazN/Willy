

all: lexer test


#Compila el Lexer con Alex
lexer: lexer.x
	alex lexer.x -o Lexer.hs -g

#Compila test
test: test.hs
	ghc --make -dynamic test.hs