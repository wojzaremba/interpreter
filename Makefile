all: clean
	bnfc -haskell Ast.cf
	happy -gca ParAst.y
	alex -g LexAst.x

clean:
	cp -f Ast.cf temp.cf
	rm -rf *Ast.*
	rm -rf ErrM.*
	mv temp.cf Ast.cf
