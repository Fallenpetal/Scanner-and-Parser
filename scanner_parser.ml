(**************************************** Problem 1 ****************************************)
(* Simple functions

   Just like our list-manipulating higher-order functions, we often want similar
   sorts of functions on /functions/. These make it easier to pass and compose
   our utilities together, since all our abstractions are functions.
*)

(* Implement the identity function.

   Why? Sometimes we want to pass this to another function to have it do
   nothing. (See in 'const' below)
*)
let id (x : 'a): 'a = x

(* Implement the constant function.

   Why? Sometimes we don't care about the input.

   We can combine this with fold_left:
     let last_or: 'a -> 'a list -> 'a = fold_left (const id)
     (* What does this function do? *)
*)
let const (x : 'a): 'ignored -> 'a = fun _ -> x

(* Implement the function composition operator.

   Why? Putting two functions together to make 1 function is often useful.
     ((+) % length) [1; 2] 1  => (+) (length [1;2]) 1
     (* A bit silly, but this produces 3. The type is:
        (+) % length : 'a list -> int -> int *)
*)
(* NOTE Binary operators are defined with '(operator)' in OCaml. *)
let (%) (f : 'a -> 'output) (g : 'input -> 'a): 'input -> 'output = fun x ->
  f (g x)

(* Implement the 'flip' function, which swaps arguments.

  Why? Sometimes we have a function which takes arguments one way, but we need
  them to be taken the other way.
    let divide : int -> int -> int = fun n m -> n / m
    let divide_by_2 = flip divide 2
*)
let flip (f : 'a -> 'b -> 'c): 'b -> 'a -> 'c = fun x y -> f y x

(* Implement the application operator.

   Why? Parsing! Instead of wrapping our arguments in parenthesis:

   You cannot pass constructors directly to functions:
     my_fun MyConstructor(3)      (* WRONG! *)
     my_fun (MyConstructor(3))    (* RIGHT! *)
     my_fun @@ MyConstructor(3)   (* RIGHT! *)

     or, use with fun and and_then where parenthesis usually pile up (shown below)
*)
let (@@) (f : 'a -> 'b) (a : 'a): 'b = f a

(* Implement the /reverse/ application operator.

   Why? To give a method of writing 'sequenced' operators in a cleaner way.
     More examples will be given later.
     This operator is used /extensively/ throughout the assignment.
*)
let (|>) (a : 'a) (f : 'a -> 'b): 'b = f a

(* We can write map functions for pairs.

   map_fst not (true, 3) = (false, 3)
*)
let map_fst (f : 'a -> 'c) ((a, b) : 'a * 'b): 'c * 'b = (f a, b)
let map_snd (f : 'b -> 'c) ((a, b) : 'a * 'b): 'a * 'c = (a, f b)

(**************************************** Problem 2 ****************************************)
(* As with the previous assignment, we will begin with defining our basic
   datatypes and their functions. *)

(* Unlike the last programming assignment, we will 'properly' define list with
   the typical operator version of cons.
   NOTE: utop, etc. will still PRINT in prefix notation: (::) (1, []).
*)

type 'a list =
  | []
  | (::) of 'a * 'a list

(* For all our data types define functions to wrap our constructors, so we can
   this function as a value. You /cannot/ pass constructors as values.
   e.g.
     fold_right Cons my_list []    (* Does *not* work. *)
     fold_right cons my_list []    (* Does work. *)
*)
let nil : 'a list = []
let cons (hd : 'a) (tl : 'a list): 'a list = hd :: tl

(* Standard fold_left *)
let rec fold_left (f : 'a -> 'b -> 'a) (acc : 'a) (ls : 'b list): 'a =
  match ls with
  | [] -> acc
  | hd::rest -> fold_left f (f acc hd) rest

(* Reverse can be implemented with fold_left, note the use of flip! *)
let reverse (ls : 'a list): 'a list =
  fold_left (flip cons) [] ls
(* vs *)
(* fold_left (fun acc x -> x :: acc) [] ls *)

(* A more thorough example of using our function utilities. *)
(* Notice how this reads,
   fold_right is:
   - fold_left with its second and third arguments flipped.
   - fold_left with its function argument's arguments flipped.
   - fold_left with its output order reversed.
*)
let fold_right (f : 'a -> 'b -> 'b) =
  flip (fold_left (flip f)) % reverse

(* NOTE There is no use to fold_right in this assignment, this is an example *)

(* Implement map making use of "@@", "flip" and "%". *)

let map (f : 'a -> 'b) (ls : 'a list): 'b list = fold_right (cons % f) ls []

(* Implement length making use of "@@", "flip", and "const"

   Hint: You can turn "+" into a function by wrapping it in parenthesis "(+)".
*)

let length (ls : 'a list): int = fold_left (flip ((+) % const 1)) 0 ls
(* let length (ls : 'a list): int = fold_left (flip @@ (+) % const 1) 0 ls;; *)

(* Standard append. *)
let append (ls1 : 'a list) (ls2 : 'a list): 'a list =
  fold_left (flip cons) ls2 @@ reverse ls1

(**************************************** Problem 3 ****************************************)
(* Standard definition of options *)
type 'a option =
  | None
  | Some of 'a

(* Wrappers again! *)
let none: 'a option = None
let some (x : 'a) : 'a option = Some(x)

(*******************************************
   A result is a type with two type parameters (list and option have one type
   parameter). Unlike option, which holds nothing or one thing, a result holds
   one thing or another thing (sometimes called an 'either' type).

   Result types let us represent errors better
   
   than option types, because we
   either have the value we expect or we have an error value.

   Let us see an example:
*)

type ('a, 'e) result =
  | Ok of 'a
  | Err of 'e

let ok  (res : 'a): ('a, 'e) result = Ok(res)
let err (err : 'e): ('a, 'e) result = Err(err)

(* EXAMPLE *)
type numeric_err =
  | DivisionByZero
  | IDoNotLikeNegatives

(* EXAMPLE Safe divide, which can not go wrong, or be negative! *)
let divide (n : int) (m : int): (int, numeric_err) result =
  if n < 0 || m < 0 then
    Err(IDoNotLikeNegatives)
  else if m = 0 then
    Err(DivisionByZero)
  else
    Ok(n / m)

    (* Let's write some helpful library functions for result: *)

(* Implement 'fold' on results. *)
let fold_result (f : 'a -> 'b) (g : 'e -> 'b) (res : ('a, 'e) result): 'b = match res with
| Ok (x) -> f x
| Err (x) -> g x 

(* Implement map(s) on results.

   Hint: Should be a call to fold_result and NO anonymous functions. (Use the
   operators on functions above!)
*)
let map_ok (f : 'a -> 'b) (res : ('a, 'e) result): ('b, 'e) result = fold_result (ok % f) err res

let map_err (f : 'e -> 'g) (res : ('a, 'e) result): ('a, 'g) result = fold_result ok (err % f) res

(* Implement and_then on results.

   This is an extremely powerful function, just like we had on options before.
   Now, we can also make use of reverse application (|>) to make very readable
   error handling.

   Suppose we want to implement the equation: (n / m) + (a / b) + (r / s)
   We can do it like so:

   If we did NOT use our utilities:

   let my_equation n m a b r s: (int, numeric_err) result =
     match divide n m with
     | Err(e) -> Err(e)
     | Ok(x) -> match divide a b with
                | Err(e) -> Err(e)
                | Ok(y) -> match divide r s with
                           | Err(e) -> Err(e)
                           | Ok(z) -> Ok(x + y + z)

   let my_equation n m a b r s : (int, numeric_err) result =
     divide n m |> and_then @@ fun x ->
     divide a b |> and_then @@ fun y ->
     divide r s |> and_then @@ fun z ->
     Ok(x + y + z)

   Nested pattern matching is definitely much nastier to deal with, while the
   above reads almost like English (with a lot of silly punctuation)! You should
   make heavy use of and_then in this assignment, or it will get very messy.
*)

let and_then (f : 'a -> ('b, 'e) result) (res : ('a, 'e) result): ('b, 'e) result = fold_result f err res

(* Implement , which combines two results.
   If either is an Err, the result is an Err.

   Hint: Use and_then
*)

let combine_result
    (f : 'a -> 'b -> 'c)
    (r1 : ('a, 'e) result)
    (r2 : ('b, 'e) result): ('c, 'e) result = r1 |> and_then @@ fun x -> 
                                              r2 |> and_then @@ fun y ->
                                                  ok (f x y)

(* let combine_result
(f : 'a -> 'b -> 'c)
(r1 : ('a, 'e) result)
(r2 : ('b, 'e) result): ('c, 'e) result = match r1 with
| Ok (x) -> match r2 with
          | Ok (y) -> Ok (f x y)
          | Err (e) -> err (e)
| Err (e) -> err (e) *)

(* Implement all_ok, which takes a list of results and combines them all.
   If any result in the list is an Err, the result is an Err.
*)
let all_ok (ls : ('a, 'e) result list) : ('a list, 'e) result = fold_right (combine_result cons) ls (ok [])

(* Very useful. Sometimes we call a helper which returns an option, but that is
   an error to us, the caller. *)
  let from_option (e : 'e) (o : 'a option): ('a, 'e) result =
  match o with
  | None -> Err(e)
  | Some(a) -> Ok(a)

  (* We define our atoms *)
type atom =
| Symbol of string
| Number of int

(* transfer string and int to Symbol and Number *)
let symbol (s : string): atom = Symbol(s)
let number (n : int): atom    = Number(n)

(* Now our sexprs, notice the use of 'and' like we have with streams. *)
type sexpr =
| Atom of atom
| SList of sexpr_list
and sexpr_list = sexpr list

(* transfer atom and sexpr list to a sexpr *)
let atom  (a : atom): sexpr = Atom(a)
let slist (l : sexpr_list): sexpr = SList(l)

let snil: sexpr_list = []
let scons (s : sexpr) (rest : sexpr_list): sexpr_list = s :: rest

(* transfer int and string to sexpr, it's more convenient than symbol() and number()*)
let snumber (n : int)   : sexpr = atom @@ number n
let ssymbol (s : string): sexpr = atom @@ symbol s

(**************************************** Scanner ****************************************)
(*
  You will implement a parser on a stream of 'tokens'. Tokens are just a data
  type representing characters/words that we would get from a file or string.
  You will need to transform a stream of tokens into sexprs.
*)

type token =
  | SYMBOL of string
  | NUMBER of int
  | LPAREN
  | RPAREN
  | EOF

(* Our definition of streams as we have seen in lecture *)
type 'a str =
  | Next of 'a * 'a stream
and 'a stream = unit -> 'a str

let head (st : 'a stream): 'a =
  let Next(h, _) = st () in h

let tail (st : 'a stream): 'a stream =
  let Next(_, t) = st () in t

(* We will define a simple operator for building streams. *)
let (^) (a : token) (st : token stream): token stream =
  fun _ -> Next(a, st)

(* A token stream is terminated by an End Of File (EOF) token forever. *)
let rec eof (_ : unit): token stream =
  fun _ -> Next(EOF, eof ())

(* EXAMPLE: The token stream for a simple "(+ 3 2)" sexpr *)
let example_stream =  LPAREN ^ SYMBOL("+") ^ NUMBER(3) ^ NUMBER(2) ^ RPAREN ^ eof ()

(* We define an error type to return for when parsing fails:
   UnexpectedEOF occurs when we hit EOF but did not expect it,
     e.g. "(+ 2", we expected an ")"
   UnmatchedParen occurs in the other case where the /right/ paren is left open,
     e.g. "(+ 2 3))", we expected one less ")"
*)
type parse_err =
  | UnexpectedEOF
  | UnmatchedParen

(* Implement parse_sexpr and parse_sexpr_list, which take a token stream and
   produce an sexpr (or sexpr_list) and the remainder of the token stream.

   (* parse is a wrapper defined below *)

   parse example_stream
    = Ok(SList(Atom(Symbol("+"))::Atom(Number(3))::Atom(Number(2))::[])::[])

   parse (LPAREN ^ eof ())
    = Err(UnexpectedEOF)

   parse (LPAREN ^ RPAREN ^ RPAREN ^ eof ())
    = Err(UnexpectedParen)

    ( 
    ())
    (+ 3 2)
    (3)
    ((3)) parse (LPAREN ^ LPAREN ^ NUMBER(3) ^ RPAREN ^ RPAREN ^ eof());;
    ( ( + 3 2 ) )
    ( * (+ 2 6 4) 3)
     (LPAREN ^ SYMBOL("*") ^ LPAREN ^ SYMBOL("+") ^ NUMBER(2) ^ NUMBER(6) ^ NUMBER(4) ^ RPAREN ^ NUMBER(3) ^ RPAREN ^ eof())
*)

(* type ('a, 'e) result =
  | Ok of 'a
  | Err of 'e *)

(* 这里我以为是((x, y), z)类型，因为sexpr * token stream是一个pair，然后我又看到parse_err和前者用逗号隔开，就误判了，实际上是一个result类型，然后这个类型要么是'a 要么是'b, Ok的话就是相当于'a，然后相当于一个sexpr * token stream的pair，然后只有error的时候才是'b, 对应parse_err类型，所以这里的fun(x, y)就已经相当于sexpr * token stream的pair了   *)

let rec parse_sexpr (st : token stream): (sexpr * token stream, parse_err) result = match head st with
| EOF -> err UnexpectedEOF
| LPAREN -> parse_sexpr_list (tail st) |> and_then @@ fun (x, y) -> ok (slist(x), y)
| RPAREN -> err UnmatchedParen
| NUMBER (n) -> ok (snumber n, tail st)
| SYMBOL (s) -> ok (ssymbol s, tail st)
and parse_sexpr_list (st : token stream): (sexpr_list * token stream, parse_err) result = match head st with
| EOF -> err UnexpectedEOF
| LPAREN -> parse_sexpr_list (tail st) |> and_then @@ fun (x, y) -> parse_sexpr_list y |> and_then @@ fun (z, w) -> ok (scons (slist(x)) z, w)
| RPAREN -> ok (snil, tail st)
| NUMBER (n) -> parse_sexpr_list (tail st) |> and_then @@ fun (x, y) -> ok (scons (snumber n) x, y)
| SYMBOL (s) -> parse_sexpr_list (tail st) |> and_then @@ fun (x, y) -> ok (scons (ssymbol s) x, y)


(* Tries to parse the token stream into sexprs until it hits EOF *)
let parse (st : token stream): (sexpr list, parse_err) result =
  let rec parse_all st acc =
    let Next(tok, _) = st () in
    match tok with
    | EOF -> Ok(reverse acc)
    | _   -> parse_sexpr st
             |> and_then @@ fun (s, ts) ->
      parse_all ts (s :: acc)
  in
  parse_all st []

(**************************************** Parser ****************************************)
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

(* The type for values. When we e/valu/ate we turn expressions into values. *)
(* In our language, either we have a number or a boolean. *)
type value =
  | Int of int
  | Bool of bool

let val_int (n : int): value = Int(n)
let val_bool (b : bool): value = Bool(b)



(* Our errors for evaluating.

   UnknownSymbol    , if we encounter a symbol we do not know
   NotABool         , if we have (and 1 true), "1" should be a boolean
   NotANumber       , if we have (+ 1 true), "true" should be a number
   InvalidExpr      , if the sexpr is none of the valid sexprs
*)
type eval_err =
  | UnknownSymbol of string
  | NotABool of value
  | NotANumber of value
  | InvalidExpr of sexpr

(* Implement eval_sexpr and eval_sexpr_list
   (* run is a wrapper defined below *)
*)


let rec eval_sexpr (e : sexpr): (value, eval_err) result = match e with
  | SList(Atom(Symbol(s)) :: rst) -> eval_sexpr_list (Atom(Symbol(s)) :: rst)   
  | Atom(Number(n)) -> Ok(Int(n))                               
  | Atom(Symbol(s)) ->
    begin
      match s with
      | "true" -> Ok(Bool(true))
      | "false" -> Ok(Bool(false))
      | _ -> Err(UnknownSymbol s)
    end
  | _ -> Err(InvalidExpr e)
      
and eval_sexpr_list (e : sexpr_list): (value, eval_err) result = match e with
    | Atom(Symbol(s)) :: rst ->                                                 (*symbol(s) is the 1th symbol*)
      begin
      match s, rst with
      | "not", Atom(Symbol(v)) :: [] ->                                          (*one argument*)
        begin
        match v with
        | "true" -> ok(Bool(false))
        | "false" -> ok(Bool(true))
        | _ -> Err(InvalidExpr (SList(e)))                                      (*not not case*)
        end
      | "not", SList(x) :: [] -> eval_sexpr (SList(x)) |> and_then @@ fun (v) ->  (*one argument*)
        begin                                                                 (*mixture case*)
        match v with
        | Bool(true) -> ok(Bool(false))
        | Bool(false) -> ok(Bool(true))
        | _ -> Err(NotABool v)
        end
      | "not", _ -> Err(InvalidExpr(SList(e)))                                      (*not 8 case*)

      | "<", s1 :: s2 :: [] -> 
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* < *) 
          eval_sexpr s2 |> and_then @@ fun (v2) ->
            begin
            match v1, v2 with
            | Int(n1), Int(n2) -> ok(Bool(n1 < n2))
            | Int(n1), _ -> Err(NotANumber v2)
            | _ -> Err(NotANumber v1)
            end

      | "=", s1 :: s2 :: [] ->
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* = *)
          eval_sexpr s2 |> and_then @@ fun (v2) ->
            begin
            match v1, v2 with
            | Int(n1), Int(n2) -> ok(Bool(n1 = n2))
            | Int(n1), _ -> Err(NotANumber( v2))
            | _ -> Err(NotANumber v1)
            end

      | "and", s1 :: s2 :: tl -> 
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* and *)
          eval_sexpr s2 |> and_then @@ fun (v2) ->
            begin
            match v1, v2 with
            | Bool(true), Bool(true) -> eval_sexpr_list(Atom(Symbol("and")) :: Atom(Symbol("true")) :: tl)
            | Bool(x1), Bool(y1) -> eval_sexpr_list(Atom(Symbol("and")) :: Atom(Symbol("false")) :: tl)
            | Bool(x1), _ -> Err(NotABool v2)
            | _, Bool(x2) -> Err(NotABool v1)
            | _ -> Err(NotABool v1)
            end
      | "and", s1 :: [] -> 
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* and *)
          begin
          match v1 with
          | Bool(true) -> ok(Bool(true))
          | Bool(false) -> ok(Bool(false))
          | _ -> Err(NotABool v1)
          end

      | "or", s1 :: s2 :: tl ->
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* or *)
          eval_sexpr s2 |> and_then @@ fun (v2) ->
            begin
            match v1, v2 with
            | Bool(true), Bool(true) -> eval_sexpr_list(Atom(Symbol("or")) :: Atom(Symbol("true")) :: tl)
            | Bool(x1), Bool(y1) -> eval_sexpr_list(Atom(Symbol("or")) :: Atom(Symbol("true")) :: tl)
            | Bool(x1), _ -> Err(NotABool v2)
            | _, Bool(x2) -> Err(NotABool v1)
            | _ -> Err(NotABool v1)
            end
      | "or", s1 :: [] ->
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* or *)
          begin
          match v1 with
          | Bool(true) -> ok(Bool(true))
          | Bool(false) -> ok(Bool(false))
          | _ -> Err(NotABool v1)
          end
      
      | "+", s1 :: s2 :: tl -> 
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* + *)
          eval_sexpr s2 |> and_then @@ fun (v2) ->
            begin
            match v1, v2 with
            | Int(n1), Int(n2) -> eval_sexpr_list(Atom(Symbol("+")) :: Atom(Number(n1 + n2)) :: tl)
            | Int(n1), _ -> Err(NotANumber v2)
            | _ -> Err(NotANumber v1)
            end
      | "+", s1 :: [] -> 
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* base case *)
          begin
          match v1 with
          | Int(n1) -> ok(Int(n1))
          | _ -> Err(NotANumber v1)
          end
      
      | "-", s1 :: s2 ::tl ->
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* - *)
          eval_sexpr s2 |> and_then @@ fun (v2) ->
            begin
            match v1, v2 with
            | Int(n1), Int(n2) -> eval_sexpr_list(Atom(Symbol("-")) :: Atom(Number(n1 - n2)) :: tl)
            | Int(n1), _ -> Err(NotANumber v2)
            | _ -> Err(NotANumber v1)
            end
      | "-", s1 :: [] ->
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* base case *)
          begin
          match v1 with
          | Int(n1) -> ok(Int(n1))
          | _ -> Err(NotANumber v1)
          end
      
      | "*", s1 :: s2 :: tl ->
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* * *)
          eval_sexpr s2 |> and_then @@ fun (v2) ->
            begin
            match v1, v2 with
            | Int(n1), Int(n2) -> eval_sexpr_list(Atom(Symbol("*")) :: Atom(Number(n1 * n2)) :: tl)
            | Int(n1), _ -> Err(NotANumber v2)
            | _ -> Err(NotANumber v1)
            end
      | "*", s1 :: [] ->
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* base case *)
          begin
          match v1 with
          | Int(n1) -> ok(Int(n1))
          | _ -> Err(NotANumber v1)
          end

      | "/", s1 :: s2 :: tl ->
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* / *)
          eval_sexpr s2 |> and_then @@ fun (v2) ->
            begin
            match v1, v2 with
            | Int(n1), Int(0) -> Err(InvalidExpr(SList(e)))
            | Int(n1), Int(n2) -> eval_sexpr_list(Atom(Symbol("/")) :: Atom(Number(n1 / n2)) :: tl)
            | Int(n1), _ -> Err(NotANumber v2)
            | _ -> Err(NotANumber v1)
            end
      | "/", s1 :: [] ->
        eval_sexpr s1 |> and_then @@ fun (v1) ->                              (* base case *)
          begin
          match v1 with
          | Int(n1) -> ok(Int(n1))
          | _ -> Err(NotANumber v1)
          end

      (* Control structures:
      (if (< 1 3) 0 1)         ~  if 1 < 3 then 0 else 1 *)
      | "if", x :: y :: z :: [] -> 
        begin
        match eval_sexpr x with
        | Ok(Bool(true)) -> eval_sexpr y
        | Ok(Bool(false)) -> eval_sexpr z
        | Ok(Int(v)) -> Err(NotABool(Int(v)))
        | _ -> Err(InvalidExpr(SList(e)))
        end
      | "if", _ -> Err(InvalidExpr(SList(e)))

      | _, _ -> Err(UnknownSymbol s)
      end
    | _ -> Err(InvalidExpr (SList(e)))
              


type run_err =
  | ParseError of parse_err
  | EvalError of eval_err

let parse_error x = ParseError(x)
let eval_error  x = EvalError(x)

(* Takes a token stream,
   parses it into a list of sexpr,
   evaluates all the sexpr to get a list of values 
   
parse example_stream
    = Ok(SList(Atom(Symbol("+"))::Atom(Number(3))::Atom(Number(2))::[])::[])
   *)

let run (st : token stream): (value list, run_err) result =
  parse st
  |> map_err parse_error
  |> and_then @@
  map_err eval_error % all_ok % map (eval_sexpr)


