## Read Me



The following contains the Scanner and Parser written by Ocaml.

**Scanner: converts token streams to S-expressions**

```
(* EXAMPLE: The token stream for a simple "(+ 3 2)" sexpr *)
let example_stream =  LPAREN ^ SYMBOL("+") ^ NUMBER(3) ^ NUMBER(2) ^ RPAREN ^ eof ()
```



**Parser: Parser : evaluate the S-expression**

```
(*
  Now we want to evaluate our sexprs to give us a small arithmetic language.

  The language will include: arithmetic, booleans, if-then-else.
  Specifically, here is how our expressions will compare in specific instances to OCaml:
           sexprs                      Ocaml
     ------------------------- ---------------------------
             Can take any number of arguments:
     (and true false true)    ~    true && false && true
     (or  false false)        ~    false || false
     (+ 1 2 3)                ~    1 + 2 + 3
     (- 3 4 1 3)              ~    ((3 - 4) - 1) - 3
     ( * 2 2 2)               ~    2 * 2 * 2
     (/ 5 2 1)                ~    (5 / 2) / 1

                  Can take one argument:
     (not false)              ~    not false

                  Can take two arguments:
     (< 3 2)                  ~    3 < 2
     (= 2 2)                  ~    2 = 2

                  Control structures:
     (if (< 1 3) 0 1)         ~  if 1 < 3 then 0 else 1
*)
```

also, I write some basic local tests here in `parser_test.ml`.