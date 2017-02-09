# Lisp Intepreter

---

[![Build Status](https://travis-ci.com/b-liu14/lisp-interpreter.svg?token=T9dFvqXR4JmmQPEt9A86&branch=master)](https://travis-ci.com/b-liu14/lisp-interpreter)

A lisp-like language interpreter implemented in Haskell


### How to execute

1. Install `stack`. refer to [stack install_and_upgrade](https://docs.haskellstack.org/en/stable/install_and_upgrade/)。

2. REPL mode.

   * Execute statement: `:i <stmt>`
   * Execute expression: `:e <expr>`
   * Define function: `:f <func>`
   * Quit REPL: `:q`
   * Help: `:h`
   * Print semantic tree of last instruction: `:t`
   * Use left/right arrow to control the cursor.
   * Use up/down arrow to choose history command.

3. Run interpreter. There are some options to do so.

    2.1 Compile and execute: `stack build && stack exec ki -- ARGS`. `ARGS` is the argument of `ki`, and the usage is as follows.

    ```
    Usage: ki [-i|--instructions INPUT_PATH] [-o|--output OUTPUT_PATH]
              [-t|--tree INPUT_PATH] [-r|--repl]

    Available options:
      -h,--help                Show this help text
      -i,--instructions INPUT_PATH
                               path of instructions to execute.
      -o,--output OUTPUT_PATH  path to output
      -t,--tree INPUT_PATH     path of program to parse
      -r,--repl                REPL mode
    ```

    2.2 Just execute. `stack runhaskell -- app/ki.hs ARGS`. The meaning of `ARGS` is same with above description.

4. Run Translator

    3.1 Compile and execute：`stack build && stack exec kc -- ARGS`. `ARGS` is the argument of `kc`, and the usage is as follows.
    ```
    Usage: kc [INPUT] [-o OUTPUT] [-h]
    example:
      kc input.txt -o output.py
    Available options:
      -o   path to output
      -h   show this help message
    ```

    3.2 Just execute：`stack runhaskell -- app/kc.hs ARGS`. The meaning of `ARGS` is same with above description.


4. Run the test： `stack test`。If you want to test parser or eval seperately, execute `stack test project:parser-test` or `stack test project:eval-test`.

5. Run the test with coverage: `stack build; stack clean; stack test --coverage`

### Grammar
Please refer to final_project.pdf(Chinese).

### Implemented features
1. Arithmetic expression
   *    `(<operator> <expression> <expression>)`
   *    Operator
        - Logic operator: `and, or, not`
        - Calculation operator: `+, -, *, /`
        - Compare operator: `=, <, <=, >=, >`
   *    Literal constant: `True, False, 1, 1.0, 1.1e10`

    ```
    ki >>> :e (+ 1.0 1.1e2)
    111.0
    ki >>> :e (and True False)
    False
    ki >>> :e (= 2 3)
    False
    ```

2. Assignment statement and variable

   * `(set! <var> <expr>)
   * Variable should begin with `_` or letter and consist of `_`, letter, digit: `a, a3b4, _a3b4`

   ```
   ki >>> :i (set! t True)
   variable name |   value
   --------------+-----------
   t             | True
   ```


3. List and String

   * Empty list: `nil`
   * Cons: `(cons <expr> <expr>)`
   * Car: `(car <expr>)`
   * Cdr: `(cdr <expr>)`
   * Char literal: `'c'`
   * String literal: `"string\"s"`

   ```
   ki >>> :i (set! a "String\"")
   variable name |   value
   --------------+-----------
   a             | "String\""
   ki >>> :i (set! b nil)
   variable name |   value
   --------------+-----------
   a             | "String\""
   b             | ""
   ki >>> :e (cons 'c' a)
   "cString\""
   ki >>> :e (car a)
   'S'
   ki >>> :e (cdr a)
   "tring\""
   ```

4. If statement

   * `(if <expr> <stmt1> <stmt2>)`

   ```
   ki >>> :i (if True (set! a True) (set! a False))
   variable name | value
   --------------+------
   a             | True
   ```

5. While statement.

   * `(while <condition> <stmt>)`

   ```
   ki >>> :i (set! a 0)
   variable name | value
   --------------+------
   a             | 0.0

   ki >>> :i (while (< a 3) (set! a (+ a 1)))
   variable name | value
   --------------+------
   a             | 3.0
   ```

6. Statement list

   * `(begin statement ..)`

   ```
   ki >>> :i (begin (set! a 0) (set! b a))
   variable name | value
   --------------+------
   a             | 0.0
   b             | 0.0
   ```

7. Array

   * `(make-vector <vec> <len>)`
   * `(vector-set! <vec> <index> <value>)`
   * `(vector-ref <vec> <index>)`

   ```
   ki >>> :i (make-vector vec 3)
   variable name |              value
   --------------+--------------------------------
   vec           | [Undefined,Undefined,Undefined]
   ki >>> :i (vector-set! vec 0 True)
   variable name |           value
   --------------+---------------------------
   vec           | [True,Undefined,Undefined]
   ki >>> :e (vector-ref vec 0)
   True
   ```

8. Function 

   * Define: `(define (<function-name> <param> ..) <statement>)`
   * Return Statement: `(return <expr>)`
   * Function call expression: `(function-name <expr> ..)`

   ```
   ki >>> :f (define (f x) (return x))
   variable name |   value
   --------------+-----------
   f             | <Function>

   ki >>> :e (f 3)
   3.0
   ```

9. Variable scope

   * ` (let <variable> <expression> <expression>)`

   ```
   ki >>> :e (let a 3 (+ a 1))
   4.0
   ```

10. Lambda (Only support one parameter)

*    Define `(lambda variable expression)`
*    Call `(expression expression)`

      ​

### examples

see the examples directory.