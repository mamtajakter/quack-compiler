BUILDER=stack
COMP=gcc 

quack:
	make clean
	$(BUILDER) exec alex src/Parser/Lexer/QuackLexer.x
	$(BUILDER) exec happy src/Parser/QuackParser.y
	$(BUILDER) build
	# --fast --file-watch --ghc-options='-Wall'



clean:
	rm -fv src/Parser/Lexer/*.hs
	rm -fv src/Parser/QuackParser.hs
	rm -fv test/parseText.qk
	rm -fv output/named_types.c
	# stack clean

# ccode: 
# #    make clean 
#    $(COMP) output/Builtins.c 
#    $(COMP) named_types.c 


# SRC=samples/named_types.c output/Builtins.c 
# OBJ=$(SRC:.C=.o)

# quackc: $(OBJ)
# 	gcc $(OBJ) -o cquack 

# .C.o: $<
# 	gcc -I. -c $<

# cclean:
# 	rm *.o cquack 
