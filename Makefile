Goat: Goat.hs GoatAST.hs PrettyPrinter.hs CodeGen.hs SymTable.hs
	ghc Goat.hs

clean:
	rm -f *.o *.hi
	rm -f Goat

