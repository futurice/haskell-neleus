.PHONY : build neleus doctest

build : 
	cabal new-build --enable-tests all

neleus : 
	cabal new-build --enable-tests neleus

doctest :
	doctest neleus/src
	doctest eldapo/src
