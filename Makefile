.PHONY : build neleus doctest dot

build : 
	cabal new-build --enable-tests all

neleus : 
	cabal new-build --enable-tests neleus

doctest :
	doctest neleus/src
	doctest neleus-ldap/src

run :
	cabal new-run eldapo

test-request :
	ldapsearch -x -h 'localhost:9999'

dot :
	cabal-plan --hide-builtin dot | tred | dot -Tpng -o deps.png
