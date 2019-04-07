(* Sum type to encode efficiently polynomial expressions *)
type pExp =
  | Term of int*int (*
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


(*
  Function to traslate betwen AST expressions
  to pExp expressions
*)
let rec from_expr (_e: Expr.expr) : pExp =
    (* Printf.printf("from_expr called \n"); *)
  match _e with
    | Var(c) -> Printf.printf("Parsed Term \n"); Term(1, 1)
    | Num(i) -> Printf.printf("Parsed Term \n"); Term(i, 0)
    | Add(e1,e2) -> Printf.printf("Parsed Plus \n"); Plus([from_expr e1; from_expr e2])
    | Mul(e1,e2) -> Printf.printf("Parsed Times \n"); Times([from_expr e1; from_expr e2])
    | Pow(e,i) -> Printf.printf("Parsed Term \n"); Term(1, i)
    | _ -> Printf.printf("expr Not Handled. \n"); Term(0,0)

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
try
  match _e with
      | Term(c, i) -> i
      | Plus(l) -> find_max (degree (List.hd l)) (degree (List.nth l 1))
      | Times(l) -> find_max (degree (List.hd l)) (degree (List.nth l 1))
      | _ -> Printf.printf("expr Not Handled."); 0
with _ -> Printf.printf("degree failure\n"); 0

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
let rec print_pExp (_e: pExp): unit =
  (* TODO *)
  try 
  match _e with
  | Term(n, e) -> Printf.printf("(%dx^%d) ") n e;
  | Plus(l) -> print_pExp (List.hd l); Printf.printf("+"); print_pExp (List.hd (List.tl l));
  | Times(l) -> List.iter print_pExp l; Printf.printf("")
  (* print_newline() *)
  with _ -> Printf.printf("Print Failure\n")

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

let rec distributePlus (ll:pExp list) (rl:pExp list) : pExp = 
  Printf.printf("\n Distributing... Starting with Plus operators\n");
  print_pExp (Plus(ll)); 
  Printf.printf(" \n Distributing multiplication \n");
  print_pExp (Times(rl));
  Printf.printf("\n Result \n");    
  print_pExp (Plus( List.map (fun s -> Times(rl @ [s])) ll ));
  Plus( List.map (fun s -> simplify1 (Times(rl @ [s]))) ll )

and simplifyTimes (ol:pExp list) : pExp =
  match List.hd ol with
        | Plus(il) -> Printf.printf("Plus inside Times \n"); (distributePlus (List.tl ol) il)
        | Times(il) -> Printf.printf("Times inside Times \n");  (distributePlus (List.tl ol) il)
        | Term(n, ex) -> Printf.printf("Multiplying Terms \n"); 
              let re = List.hd (List.tl ol) in
              match re with 
              | Term(n1, ex1) -> Printf.printf("Result: %dx^%d\n") (n1*n) (ex1+ex); Term(n1 * n, ex1 + ex)
              | _ -> Times(ol)
        | _ -> Times(ol)

and simplify1 (e:pExp): pExp =
    match e with
    | Term(n, ex) -> Printf.printf("Just a Term, returning\n"); e
    | Times(ol) -> Printf.printf("Entering Times \n"); simplifyTimes ol  
    | Plus(ol) ->Printf.printf("Entering Plus \n"); 
    try 
        match List.hd ol with
              | Plus(il) -> Printf.printf("Plus inside Plus \n"); Plus ((List.tl ol) @ il)
              | Times(il) -> Printf.printf("Times inside Plus \n"); Plus ((List.tl ol) @ [(simplifyTimes il)])
              | Term(n, ex) -> Printf.printf("Adding Terms \n"); 
                    let re = List.hd (List.tl ol) in
                    match re with 
                    | Term(n1, ex1) -> if ex1 = ex then
                                          Term (n1 + n, ex)
                                        else if ex1 = 0 then
                                          Term (n1 + n, ex)
                                        else if ex = 0 then
                                          Term (n1 + n, ex1)
                                        else 
                                          e
                    | _ -> e
              | _ -> e
      with _ -> e
      | _ -> e
    
(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let equal_pExp (e1:pExp) (e2:pExp) : bool =
  let ree = match e1 with
    | Term (n, e) -> Term(n, e)
    | Times (list) ->  Times(list)
    | Plus (list) -> Plus(list)
    | _ -> Term(0,0)
    in
  let ree2 = match e2 with
    | Term (n, e) -> Term(n, e)
    | Times (list) ->  Times(list)
    | Plus (list) -> Plus(list)
    | _ -> Term(0,0)
    in
    if ree = ree2 then true
    else false

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
        rE
      else  
        simplify(rE)




