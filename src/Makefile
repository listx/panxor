all:
	ghc -O2 -Wall -fno-spec-constr-count -threaded --make panxor

prod:
	ghc -O2 -Wall -Werror -fno-spec-constr-count -threaded --make panxor

prof:
	ghc -rtsopts -prof -auto-all -O2 -v -Wall -Werror --make panxor
	./panxor +RTS -p

srclist:
	$(shell find -type f -regex ".*\.lhs" > srclist)

clean:
	$(RM) panxor
	find -type f -iregex ".+\.\(aux\|o\|hi\|hp\|mix\|ps\|tix\|html\)" -exec rm -v {} \;
