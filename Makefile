auto-test.h: automata.lisp
	rm auto-test.h || echo lol
	sbcl --eval '(progn (load "automata.lisp") (test-automata-eval) (sb-ext:quit))'

auto-test: auto-test.c auto-test.h
	gcc -Wall -pedantic -std=c99 -O3 -lSDL -lm auto-test.c -o auto-test

clean:
	rm auto-test.h auto-test || echo lol

from-file: automata.lisp input.automata
	rm auto-test.h || echo lol
	sbcl --load automata-eval-file.lisp
	gcc -Wall -pedantic -std=c99 -O3  -lSDL -lm auto-test.c -o auto-test

auto: auto.c
	gcc -Wall -pedantic -std=c99 -O3 -lHarbinger -lSDL -lm auto.c -o auto
