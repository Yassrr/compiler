flex PartieLex.l 
bison -d PartieSyntax.y 
gcc lex.yy.c PartieSyntax.tab.c  -lfl -ly -o test
test <MonExemple.txt