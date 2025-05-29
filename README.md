# C TO CLISP TRANSLATOR

Frontend translator that parses a subset of C and generates equivalent Common Lisp code. Handled variable declarations, control structures, expressions, and function definitions using Bison/Yacc with custom grammar and semantic rules. 

Steps to execute:
```bash
bison -d trad.y
gcc -o back trad.tab.c -lm
./trad
```
