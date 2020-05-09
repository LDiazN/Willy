all: Lexer Parser 
	ghc --make -dynamic Willy.hs 


Willy: Expresions.hs ContextAnalyzer.hs SymbolTable.hs Tokens.hs Willy.hs ProgramState.hs Simulator.hs Interpreter.hs
	ghc --make -dynamic Willy.hs 
Lexer: Lexer.x
	alex Lexer.x -o Lexer.hs -g

Parser: Parser.y
	happy Parser.y

clear:
	rm  Willy *.hi *o Lexer.hs Parser.hs