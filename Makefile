CFLAGS += -std=c11 -Wall -Werror -DDEBUG -g
TESTFLAGS = $(CFLAGS) -g -DDEBUG
BINDIR := bin
DATE=$(shell date '+%Y-%m-%d')

all: monkey repl tests

repl: $(BINDIR)
	$(CC) $(CFLAGS) src/repl.c src/eval/*.c src/lexer/*.c src/parser/*.c -ledit -o $(BINDIR)/repl

monkey: 
	$(CC) $(CFLAGS) src/monkey.c src/eval/*.c src/lexer/*.c src/parser/*.c -Ofast -march=native -DNDEBUG -o $(BINDIR)/monkey 

tests: $(BINDIR) lexer_test parser_test eval_test code_test

lexer_test:
	$(CC) $(TESTFLAGS) tests/lexer_test.c src/lexer/*.c -o $(BINDIR)/lexer_test
	$(BINDIR)/lexer_test	

parser_test:
	$(CC) $(TESTFLAGS) tests/parser_test.c src/parser/*.c src/lexer/*.c -o $(BINDIR)/parser_test
	$(BINDIR)/parser_test

eval_test:
	$(CC) $(TESTFLAGS) tests/eval_test.c src/eval/*.c src/parser/*.c src/lexer/*.c -o $(BINDIR)/eval_test
	$(BINDIR)/eval_test

code_test:
	$(CC) $(TESTFLAGS) tests/code_test.c -o $(BINDIR)/code_test
	$(BINDIR)/code_test	

$(BINDIR):
	mkdir -p $(BINDIR)

bench: monkey
	echo "**$(shell date '+%Y-%m-%d %H:%M')** (fib 35)" >> benchmarks.md
	/usr/bin/time --append -o benchmarks.md ./bin/monkey fibonacci.monkey
	echo "" >> benchmarks.md

.PHONY: clean
clean:
	rm -r $(BINDIR)

.PHONY: watch
watch:
	find src/ | entr -s 'make tests'

.PHONY: valgrind
valgrind: 
	docker run -v $(PWD):/root/build -d -p 22021:22 messeb/valgrind	
	ssh -p 22021 root@localhost