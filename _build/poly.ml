(* Sum type to encode efficiently polynomial expressions *)
type pExp =
  | Term of string*int (*
      First int is the constant
      Second int is the power of x 
      10  -> Term(10,0)
      2x -> Term(2,1)
      3x^20 -> Term(3, 20)
    *)
  | Plus of pExp list
  (*
    List of terms added
    Plus([Term(2,1); Term(1,0)])
  *)
  | Times of pExp list (* List of terms multiplied *)
  | Minus of pExp list
  | Negation of pExp list
  | Divide of pExp list
  | Number of int


(*
  Function to traslate betwen AST expressions
  to pExp expressions
*)
let rec from_expr (_e: Expr.expr) : pExp =
    Printf.printf("from_expr called");
  match _e with
    | Num(i) -> Term(string_of_int i, 1)
    | Var(c) -> Term(Char.escaped c, 1)
    | Add(e1,e2) -> Plus([from_expr e1; from_expr e2])
    | Sub(e1,e2) -> Minus([from_expr e1; from_expr e2])
    | Mul(e1,e2) -> Times([from_expr e1; from_expr e2])
    | Pow(e,i) -> match e with
                  | Var(c) -> Term(Char.escaped c, i)
                  | _ -> Printf.printf("Bad exp for Power"); Term(string_of_int 0,0)
    | _ -> Printf.printf("expr Not Handled."); Term(string_of_int 0,0)

let find_max (le:int) (re:int) : int =
if le > re then le
else re
(* 
  Compute degree of a polynomial expression.

  Hint 1: Degree of Term(n,m) is m
  Hint 2: Degree of Plus[...] is the max of the degree of args
  Hint 3: Degree of Times[...] is the sum of the degree of args 
*)

let rec degree (_e:pExp): int = 
  match _e with
      | Term(c, i) -> i
      | Plus(l) -> find_max (degree (List.hd l)) (degree (List.nth l 1))
      | Minus(l) -> find_max (degree (List.hd l)) (degree (List.nth l 1))
      | Times(l) -> find_max (degree (List.hd l)) (degree (List.nth l 1))
      | _ -> Printf.printf("expr Not Handled."); 0

(* 
  Comparison function useful for sorting of Plus[..] args 
  to "normalize them". This way, terms that need to be reduced
  show up one after another.
  *)
let compare (e1: pExp) (e2: pExp) : bool =
  degree e1 > degree e2

(* Print a pExpr nicely 
  Term(3,0) -> 3
  Term(5,1) -> 5x 
  Term(4,2) -> 4x^2
  Plus... -> () + () 
  Times ... -> ()() .. ()

  Hint 1: Print () around elements that are not Term() 
  Hint 2: Recurse on the elements of Plus[..] or Times[..]
*)
let print_pExp (_e: pExp): unit =
  (* TODO *)
  Printf.printf("Not implemented");
  print_newline()

(* 
  Function to simplify (one pass) pExpr

  n1 x^m1 * n2 x^m2 -> n1*n2 x^(m1+m2)
  Term(n1,m1)*Term(n2,m2) -> Term(n1*n2,m1+m2)

  Hint 1: Keep terms in Plus[...] sorted
  Hint 2: flatten plus, i.e. Plus[ Plus[..], ..] => Plus[..]
  Hint 3: flatten times, i.e. times of times is times
  Hint 4: Accumulate terms. Term(n1,m)+Term(n2,m) => Term(n1+n2,m)
          Term(n1, m1)*Term(n2,m2) => Term(n1*n2, m1+m2)
  Hint 5: Use distributivity, i.e. Times[Plus[..],] => Plus[Times[..],]
    i.e. Times[Plus[Term(1,1); Term(2,2)]; Term(3,3)] 
      => Plus[Times[Term(1,1); Term(3,3)]; Times[Term(2,2); Term(3,3)]]
      => Plus[Term(2,3); Term(6,5)]
  Hint 6: Find other situations that can arise
*)
let simplify1 (e:pExp): pExp =
    e

(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let equal_pExp (_e1: pExp) (_e2: pExp) :bool =
  true

(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
let rec simplify (e:pExp): pExp =
    let rE = simplify1(e) in
      print_pExp rE;
      let i = degree e in 
      Printf.printf("Degree of expression: %i \n") i;
      if (equal_pExp e rE) then
        e
      else  
        simplify(rE)




