(* not *)
(* (not 1) -> NotABool*)
let ex0 = (LPAREN ^ SYMBOL("not") ^ NUMBER(1) ^ RPAREN ^ eof())

(* (not false) -> true*)
let ex1 = (LPAREN ^ SYMBOL("not") ^ SYMBOL("false") ^ RPAREN ^ eof())

(* < *)
(* (< 3 2) -> false *)
let ex2 = (LPAREN ^ SYMBOL("<") ^ NUMBER(3) ^ NUMBER(2) ^ RPAREN ^ eof())

(* (< 3 true) -> NotANumber*)
let ex2_1 = (LPAREN ^ SYMBOL("<") ^ NUMBER(3) ^ SYMBOL("true") ^ RPAREN ^ eof())

(*(< ture 3) -> NotANumber*)
let ex2_2 = (LPAREN ^ SYMBOL("<") ^ SYMBOL("true") ^ NUMBER(3) ^ RPAREN ^ eof())


(* = *)
(* (= 3 2) -> false *)
let ex3 = (LPAREN ^ SYMBOL("=") ^ NUMBER(3) ^ NUMBER(2) ^ RPAREN ^ eof())

(* (= 3 3) -> true *)
let ex4 = (LPAREN ^ SYMBOL("=") ^ NUMBER(3) ^ NUMBER(3) ^ RPAREN ^ eof())

(* and *)
(* ( and true true true) -> true *)
let ex5 = (LPAREN ^ SYMBOL("and") ^ SYMBOL("true") ^ SYMBOL("true") ^ SYMBOL("true") ^ RPAREN ^ eof())

(* ( and true true false) -> false *)
let ex6 = (LPAREN ^ SYMBOL("and") ^ SYMBOL("true") ^ SYMBOL("true") ^ SYMBOL("false") ^ RPAREN ^ eof())

(* (and false true) -> false*)
let ex7 = (LPAREN ^ SYMBOL("and") ^ SYMBOL("false") ^ SYMBOL("true") ^ RPAREN ^ eof())

(* (and 1 true true) -> NotABool*)
let ex8 = (LPAREN ^ SYMBOL("and") ^ NUMBER(1) ^ SYMBOL("true") ^ SYMBOL("true") ^ RPAREN ^ eof())

(* (and false 1) -> NotABool*)
let ex9 = (LPAREN ^ SYMBOL("and") ^ SYMBOL("false") ^ NUMBER(1) ^ RPAREN ^ eof())

(* (and false true 1 false true) -> NotABool*)
let ex10 = (LPAREN ^ SYMBOL("and") ^ SYMBOL("false") ^ SYMBOL("true") ^ NUMBER(1) ^ SYMBOL("false") ^ SYMBOL("true") ^ RPAREN ^ eof())

(* or *)

(* (or true true true) -> true *)
let ex11 = (LPAREN ^ SYMBOL("or") ^ SYMBOL("true") ^ SYMBOL("true") ^ SYMBOL("true") ^ RPAREN ^ eof())

(* (or true true false) -> true *)
let ex12 = (LPAREN ^ SYMBOL("or") ^ SYMBOL("true") ^ SYMBOL("true") ^ SYMBOL("false") ^ RPAREN ^ eof())

(* (or false 1 true) -> NotABool*)
let ex13 = (LPAREN ^ SYMBOL("or") ^ SYMBOL("false") ^ NUMBER(1) ^ SYMBOL("true") ^ RPAREN ^ eof())

(* (or false 1) -> NotABool*)
let ex14 = (LPAREN ^ SYMBOL("or") ^ SYMBOL("false") ^ NUMBER(1) ^ RPAREN ^ eof())

(* (or 1 true true) -> NotABool*)
let ex15 = (LPAREN ^ SYMBOL("or") ^ NUMBER(1) ^ SYMBOL("true") ^ SYMBOL("true") ^ RPAREN ^ eof())

(* (or false false false) -> false*)
let ex16 = (LPAREN ^ SYMBOL("or") ^ SYMBOL("false") ^ SYMBOL("false") ^ SYMBOL("false") ^ RPAREN ^ eof())

(* + *)
(* (+ 1 2 3) -> 6 *)
let ex17 = (LPAREN ^ SYMBOL("+") ^ NUMBER(1) ^ NUMBER(2) ^ NUMBER(3) ^ RPAREN ^ eof())

(* (+ 1 2 3 4 5 6 7 8 9 10) -> 55 *)
let ex18 = (LPAREN ^ SYMBOL("+") ^ NUMBER(1) ^ NUMBER(2) ^ NUMBER(3) ^ NUMBER(4) ^ NUMBER(5) ^ NUMBER(6) ^ NUMBER(7) ^ NUMBER(8) ^ NUMBER(9) ^ NUMBER(10) ^ RPAREN ^ eof())

(* (+ 1 2 3 4 5 6 7 false 9 10 11) -> NotANumber*)
let ex19 = (LPAREN ^ SYMBOL("+") ^ NUMBER(1) ^ NUMBER(2) ^ NUMBER(3) ^ NUMBER(4) ^ NUMBER(5) ^ NUMBER(6) ^ NUMBER(7) ^ SYMBOL("false") ^ NUMBER(9) ^ NUMBER(10) ^ NUMBER(11) ^ RPAREN ^ eof())

(* (-) *)
(* (- 1 2 3) -> -4 *)
let ex20 = (LPAREN ^ SYMBOL("-") ^ NUMBER(1) ^ NUMBER(2) ^ NUMBER(3) ^ RPAREN ^ eof())

(* (- 1 2 3 4 5 6 7 8 9 10) -> -53 *)
let ex21 = (LPAREN ^ SYMBOL("-") ^ NUMBER(1) ^ NUMBER(2) ^ NUMBER(3) ^ NUMBER(4) ^ NUMBER(5) ^ NUMBER(6) ^ NUMBER(7) ^ NUMBER(8) ^ NUMBER(9) ^ NUMBER(10) ^ RPAREN ^ eof())

(* (- 1 2 3 4 5 6 7 false 9 10 11) -> NotANumber*)
let ex22 = (LPAREN ^ SYMBOL("-") ^ NUMBER(1) ^ NUMBER(2) ^ NUMBER(3) ^ NUMBER(4) ^ NUMBER(5) ^ NUMBER(6) ^ NUMBER(7) ^ SYMBOL("false") ^ NUMBER(9) ^ NUMBER(10) ^ NUMBER(11) ^ RPAREN ^ eof())

(* ( * ) *)
(* ( * 1 2 3) -> 6 *)
let ex23 = (LPAREN ^ SYMBOL("*") ^ NUMBER(1) ^ NUMBER(2) ^ NUMBER(3) ^ RPAREN ^ eof())

(* ( * 1 2 3 4 5 6 7 8 9 10) -> 3628800 *)
let ex24 = (LPAREN ^ SYMBOL("*") ^ NUMBER(1) ^ NUMBER(2) ^ NUMBER(3) ^ NUMBER(4) ^ NUMBER(5) ^ NUMBER(6) ^ NUMBER(7) ^ NUMBER(8) ^ NUMBER(9) ^ NUMBER(10) ^ RPAREN ^ eof())

(* ( * 1 2 3 4 5 6 7 false 9 10 11) -> NotANumber*)
let ex25 = (LPAREN ^ SYMBOL("*") ^ NUMBER(1) ^ NUMBER(2) ^ NUMBER(3) ^ NUMBER(4) ^ NUMBER(5) ^ NUMBER(6) ^ NUMBER(7) ^ SYMBOL("false") ^ NUMBER(9) ^ NUMBER(10) ^ NUMBER(11) ^ RPAREN ^ eof())

(* ( / ) *)
(* ( / 1 2 3) -> 0 *)
let ex26 = (LPAREN ^ SYMBOL("/") ^ NUMBER(1) ^ NUMBER(2) ^ NUMBER(3) ^ RPAREN ^ eof())

(* ( / 80 5 4 4) -> 1*)
let ex27 = (LPAREN ^ SYMBOL("/") ^ NUMBER(80) ^ NUMBER(5) ^ NUMBER(4) ^ NUMBER(4) ^ RPAREN ^ eof())

(* ( / 1 2 3 4 5 6 7 false 9 10 11) -> NotANumber*)
let ex28 = (LPAREN ^ SYMBOL("/") ^ NUMBER(1) ^ NUMBER(2) ^ NUMBER(3) ^ NUMBER(4) ^ NUMBER(5) ^ NUMBER(6) ^ NUMBER(7) ^ SYMBOL("false") ^ NUMBER(9) ^ NUMBER(10) ^ NUMBER(11) ^ RPAREN ^ eof())

(* ( / 1 0) -> Invalid SEXP*)
let ex29 = (LPAREN ^ SYMBOL("/") ^ NUMBER(1) ^ NUMBER(0) ^ RPAREN ^ eof())

(* if *)
(*(if (< 1 3) 0 1) -> 0*)
let ex30 = (LPAREN ^ SYMBOL("if") ^ LPAREN ^ SYMBOL("<") ^ NUMBER(1) ^ NUMBER(3) ^ RPAREN ^ NUMBER(0) ^ NUMBER(1) ^ RPAREN ^ eof())

(*(if (< 1 3) 0) -> Invalid SEXP*)
let ex31 = (LPAREN ^ SYMBOL("if") ^ LPAREN ^ SYMBOL("<") ^ NUMBER(1) ^ NUMBER(3) ^ RPAREN ^ NUMBER(0) ^ RPAREN ^ eof())

(*(if (< 1 3) 0 1 2) -> Invalid SEXP*)
let ex32 = (LPAREN ^ SYMBOL("if") ^ LPAREN ^ SYMBOL("<") ^ NUMBER(1) ^ NUMBER(3) ^ RPAREN ^ NUMBER(0) ^ NUMBER(1) ^ NUMBER(2) ^ RPAREN ^ eof())

(* ( * ( + 3 2 5) 7) -> 70  *)
let ex33 = (LPAREN ^ SYMBOL("*") ^ LPAREN ^ SYMBOL("+") ^ NUMBER(3) ^ NUMBER(2) ^ NUMBER(5) ^ RPAREN ^ NUMBER(7) ^ RPAREN ^ eof())

(*mixture cases*)

(* ( * ( + 3 2 5) 7 8) -> 560  *)
let ex34 = (LPAREN ^ SYMBOL("*") ^ LPAREN ^ SYMBOL("+") ^ NUMBER(3) ^ NUMBER(2) ^ NUMBER(5) ^ RPAREN ^ NUMBER(7) ^ NUMBER(8) ^ RPAREN ^ eof())

(*(not (and false false true)) -> true*)
let ex35 = (LPAREN ^ SYMBOL("not") ^ LPAREN ^ SYMBOL("and") ^ SYMBOL("false") ^ SYMBOL("false") ^ SYMBOL("true") ^ RPAREN ^ RPAREN ^ eof())

(*(not (and false false true) 1) -> InvalidExpr*)
let ex36 = (LPAREN ^ SYMBOL("not") ^ LPAREN ^ SYMBOL("and") ^ SYMBOL("false") ^ SYMBOL("false") ^ SYMBOL("true") ^ RPAREN ^ NUMBER(1) ^ RPAREN ^ eof())

(* ( * ( + 3 2 5) 7 8 ( / 80 5 4 4)) -> 560  *)
let ex37 = (LPAREN ^ SYMBOL("*") ^ LPAREN ^ SYMBOL("+") ^ NUMBER(3) ^ NUMBER(2) ^ NUMBER(5) ^ RPAREN ^ NUMBER(7) ^ NUMBER(8) ^ LPAREN ^ SYMBOL("/") ^ NUMBER(80) ^ NUMBER(5) ^ NUMBER(4) ^ NUMBER(4) ^ RPAREN ^ RPAREN ^ eof())

(* ( * ( + 3 2 5) 7 8 ( / 80 5 4 4) (if (< 1 3) 0 1)) -> 560  *)
let ex38 = (LPAREN ^ SYMBOL("*") ^ LPAREN ^ SYMBOL("+") ^ NUMBER(3) ^ NUMBER(2) ^ NUMBER(5) ^ RPAREN ^ NUMBER(7) ^ NUMBER(8) ^ LPAREN ^ SYMBOL("/") ^ NUMBER(80) ^ NUMBER(5) ^ NUMBER(4) ^ NUMBER(4) ^ RPAREN ^ LPAREN ^ SYMBOL("if") ^ LPAREN ^ SYMBOL("<") ^ NUMBER(1) ^ NUMBER(3) ^ RPAREN ^ NUMBER(0) ^ NUMBER(1) ^ RPAREN ^ RPAREN ^ eof())

(* ( * ( + 3 2 5) 7 8 ( / 80 5 4 4) (if (< 3 1) 0 1)) -> 560  *)
let ex38_1 = (LPAREN ^ SYMBOL("*") ^ LPAREN ^ SYMBOL("+") ^ NUMBER(3) ^ NUMBER(2) ^ NUMBER(5) ^ RPAREN ^ NUMBER(7) ^ NUMBER(8) ^ LPAREN ^ SYMBOL("/") ^ NUMBER(80) ^ NUMBER(5) ^ NUMBER(4) ^ NUMBER(4) ^ RPAREN ^ LPAREN ^ SYMBOL("if") ^ LPAREN ^ SYMBOL("<") ^ NUMBER(3) ^ NUMBER(1) ^ RPAREN ^ NUMBER(0) ^ NUMBER(1) ^ RPAREN ^ RPAREN ^ eof())

(* ( < (+ 7  3) (+ 5 9)) -> true *)
let ex39 = (LPAREN ^ SYMBOL("<") ^ LPAREN ^ SYMBOL("+") ^ NUMBER(7) ^ NUMBER(3) ^ RPAREN ^ LPAREN ^ SYMBOL("+") ^ NUMBER(5) ^ NUMBER(9) ^ RPAREN ^ RPAREN ^ eof())

(*( = (- 7 5) (+ 1 1) -> true)*)
let ex40 = (LPAREN ^ SYMBOL("=") ^ LPAREN ^ SYMBOL("-") ^ NUMBER(7) ^ NUMBER(5) ^ RPAREN ^ LPAREN ^ SYMBOL("+") ^ NUMBER(1) ^ NUMBER(1) ^ RPAREN ^ RPAREN ^ eof())

(*(= (+ 2 6)(+ 4 4) -> true)*)
let ex41 = (LPAREN ^ SYMBOL("=") ^ LPAREN ^ SYMBOL("+") ^ NUMBER(2) ^ NUMBER(6) ^ RPAREN ^ LPAREN ^ SYMBOL("+") ^ NUMBER(4) ^ NUMBER(4) ^ RPAREN ^ RPAREN ^ eof())

(* and (and false true)(and false false) -> false*)
let ex42 = (LPAREN ^ SYMBOL("and") ^ LPAREN ^ SYMBOL("and") ^ SYMBOL("false") ^ SYMBOL("true") ^ RPAREN ^ LPAREN ^ SYMBOL("and") ^ SYMBOL("false") ^ SYMBOL("false") ^ RPAREN ^ RPAREN ^ eof())

(* or (and false true)(and false false)(or true false) -> true*)
let ex43 = (LPAREN ^ SYMBOL("or") ^ LPAREN ^ SYMBOL("and") ^ SYMBOL("false") ^ SYMBOL("true") ^ RPAREN ^ LPAREN ^ SYMBOL("and") ^ SYMBOL("false") ^ SYMBOL("false") ^ RPAREN ^ LPAREN ^ SYMBOL("or") ^ SYMBOL("true") ^ SYMBOL("false") ^ RPAREN ^ RPAREN ^ eof())

(* + (+ 3 4 5)(+ 45 2 5) -> 64*)
let ex44 = (LPAREN ^ SYMBOL("+") ^ LPAREN ^ SYMBOL("+") ^ NUMBER(3) ^ NUMBER(4) ^ NUMBER(5) ^ RPAREN ^ LPAREN ^ SYMBOL("+") ^ NUMBER(45) ^ NUMBER(2) ^ NUMBER(5) ^ RPAREN ^ RPAREN ^ eof())

(* - (- 3 4 5)(- 8 5 1) -> -8*)
let ex45 = (LPAREN ^ SYMBOL("-") ^ LPAREN ^ SYMBOL("-") ^ NUMBER(3) ^ NUMBER(4) ^ NUMBER(5) ^ RPAREN ^ LPAREN ^ SYMBOL("-") ^ NUMBER(8) ^ NUMBER(5) ^ NUMBER(1) ^ RPAREN ^ RPAREN ^ eof())

(* ( * ( + 3 4 5)( - 8 5 1)( * 5 10) -> 1200) *)
let ex46 = (LPAREN ^ SYMBOL("*") ^ LPAREN ^ SYMBOL("+") ^ NUMBER(3) ^ NUMBER(4) ^ NUMBER(5) ^ RPAREN ^ LPAREN ^ SYMBOL("-") ^ NUMBER(8) ^ NUMBER(5) ^ NUMBER(1) ^ RPAREN ^ LPAREN ^ SYMBOL("*") ^ NUMBER(5) ^ NUMBER(10) ^ RPAREN ^ RPAREN ^ eof())

(* / ( * 3 5 4 )(+ 4 5 3)(/ 100 2 10) -> 1*)
let ex47 = (LPAREN ^ SYMBOL("/") ^ LPAREN ^ SYMBOL("*") ^ NUMBER(3) ^ NUMBER(5) ^ NUMBER(4) ^ RPAREN ^ LPAREN ^ SYMBOL("+") ^ NUMBER(4) ^ NUMBER(5) ^ NUMBER(3) ^ RPAREN ^ LPAREN ^ SYMBOL("/") ^ NUMBER(100) ^ NUMBER(2) ^ NUMBER(10) ^ RPAREN ^ RPAREN ^ eof())

(*( + (if 14) 7 8)*)
let ex48 = (LPAREN ^ SYMBOL("+") ^ LPAREN ^ SYMBOL("if") ^ NUMBER(14) ^ RPAREN ^ NUMBER(7) ^ NUMBER(8) ^ RPAREN ^ eof())