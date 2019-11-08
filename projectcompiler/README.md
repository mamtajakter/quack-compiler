What things you need to install the software and how to install them

install stack tool from https://get.haskellstack.org/stable/osx-x86_64.tar.gz

open terminal and run the following command:

git clone https://makter2@bitbucket.org/makter2/compiler.git

cd compiler/projectcompiler/src

alex Lexer.x

ghci Lexer.hs
 
:quit

happy Parser.y

ghci Parser.hs

main_Parser "../samples/bad_break.qk"

main_Parser "../samples/bad_escape.qk"

main_Parser "../samples/bad_init.qk"

main_Parser "../samples/GoodWalk.qk"

main_Parser "../samples/hands.qk"

main_Parser "../samples/LexChallenge.qk"

main_Parser "../samples/named_types.qk"

main_Parser "../samples/not_a_duck.qk"

main_Parser "../samples/Pt.qk"

main_Parser "../samples/robot.qk"

main_Parser "../samples/schroedinger.qk"

main_Parser "../samples/schroedinger2.qk"

main_Parser "../samples/Sqr.qk"

main_Parser "../samples/SqrBadLex.qk"

main_Parser "../samples/SqrBadSyntax.qk"

main_Parser "../samples/SqrDecl.qk"

main_Parser "../samples/SqrDeclEQ.qk"

main_Parser "../samples/TypeWalk.qk"


This parser gives the following three kinds of output :

1. when there is no parse error: 

		Beginning Parsing of the Quack program in file ../samples/Sqr.qk
		()

2. when there is a lexer error:

		Beginning Parsing of the Quack program in file ../samples/SqrBadLex.qk
		*** Exception: Unexpected character
		 at 5:30 on char '%' before : 'not_a_token'

		 at 5:27 on char '{' before : '%not_a_token'

		CallStack (from HasCallStack):
		  error, called at PARSER.hs:1411:25 in main:Parser


3. when there is a parser error:

		Beginning Parsing of the Quack program in file ../samples/SqrBadSyntax.qk
		*** Exception: Parse error
		 Invalid Syntax at the First token of [COLON,ID "this",DOT,ID "x",GETS,ID "x",SEMICOLON,ID "this",DOT,ID "y"]...
		CallStack (from HasCallStack):
		  error, called at PARSER.hs:1396:17 in main:Parser
