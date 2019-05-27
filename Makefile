Goat: Goat.hs GoatAST.hs PrettyPrinter.hs CodeGen.hs SymbolTable.hs
	ghc Goat.hs

clean:
	rm -f *.o *.hi
	rm -f Goat

