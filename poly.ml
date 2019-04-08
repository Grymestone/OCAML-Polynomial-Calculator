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

 let rec print_pExp (_e: pExp): unit =
    (* TODO *)
    try 
    match _e with
    | Term(n, e) -> Printf.printf("%dx^%d,") n e;
    | Plus(l) ->  (*Printf.printf("|Leneght %i  |") (List.length (List.tl l));*)
            Printf.printf("["); List.iter print_pExp l;  Printf.printf("]+");      
    | Times(l) -> Printf.printf("["); List.iter print_pExp l; Printf.printf("]*");               
    (* print_newline() *)
    with _ -> Printf.printf("Print Failure\n")
    (*
  let rec print_pExp (_e: pExp): unit =
    (* TODO *)
    try 
    match _e with
    | Term(n, e) -> Printf.printf("%dx^%d") n e;
    | Plus(l) -> print_pExp (List.hd l); Printf.printf(" + "); List.iter print_pExp (List.tl l);
    | Times(l) -> Printf.printf("("); print_pExp (List.hd l); Printf.printf(" * "); List.iter print_pExp (List.tl l); Printf.printf(")");
    (* print_newline() *)
    with _ -> Printf.printf("Print Failure\n")
    *)


  let rec distribute_pow e n =
        match n with
        | 1 -> e
        | _ -> Times([e] @ [(distribute_pow e (n-1))])


(*
  Function to traslate betwen AST expressions
  to pExp expressions
*)
and from_expr (_e: Expr.expr) : pExp =
    (* Printf.printf("from_expr called \n"); *)
  match _e with
    | Var(c) -> Printf.printf("Parsed Term \n"); Term(1, 1)
    | Num(i) -> Printf.printf("Parsed Term \n"); Term(i, 0)
    | Pos(e) -> Printf.printf("Parsed Pos \n"); Times([Term(1, 0); from_expr e])
    | Neg(e) -> Printf.printf("Parsed Neg \n"); Times([Term(-1, 0); from_expr e])
    | Sub(e1, e2) -> Printf.printf("Parsed Sub \n"); Plus([from_expr e1; Times([Term(-1, 0); from_expr e2])])
    | Add(e1,e2) -> Printf.printf("Parsed Plus \n"); Plus([from_expr e1; from_expr e2])
    | Mul(e1,e2) -> Printf.printf("Parsed Times \n"); Times([from_expr e1; from_expr e2])
    | Pow(e,i) -> Printf.printf("Parsed Pow Term \n"); let out = distribute_pow (from_expr e) i in print_pExp out; out
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
      | Plus(l) -> find_max (degree (List.hd l)) (degree (List.hd (List.tl l))) 
      | Times(l) -> find_max (degree (List.hd l)) (degree (List.hd (List.tl l)))  
      | _ -> Printf.printf("expr Not Handled."); 0
with _ -> Printf.printf("degree failure: "); print_pExp _e; 0

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

let comparison (ti: pExp) (tl: pExp) : bool = 
      match ti with 
      | Term(n, ex) -> (match tl with
                    | Term (n1, ex1) -> if ex >= ex1 then false
                                        else true
                    | _ -> false)

      | Plus _ -> true
      | Times _ -> true

let rec distributePlus (ll:pExp list) (rl:pExp list) : pExp list = 
  Printf.printf("\n Distributing... Starting with Plus operators\n");
  print_pExp (Plus(ll)); 
  Printf.printf(" \n Distributing multiplication \n");
  print_pExp (Times(rl));
  Printf.printf("\n Result \n");    
  print_pExp (Plus( List.map (fun s -> Times(rl @ [s])) ll ));
 ( List.map (fun s -> simplify1 (Times(rl @ [s]))) ll )

and simplifyTimes (ol:pExp list) : pExp =
  try 
  match List.hd ol with
        | Plus(il) -> Printf.printf("Plus inside Times. Head: "); print_pExp (List.hd ol); Printf.printf("Tail: "); print_pExp (Plus(il)); print_newline(); Plus (distributePlus il (List.tl ol))
        | Times(il) -> Printf.printf("Times inside Times \n");  Times(distributePlus il (List.tl ol))
        | Term(n, ex) -> Printf.printf("Multiplying Terms \n"); 
              let newList = List.tl ol in
              let re = List.hd newList in
              (* print_pExp re; *)
              match re with 
              | Term(n1, ex1) -> Printf.printf("Result: %dx^%d\n") (n1*n) (ex1+ex); if List.length newList > 1 then
                                                                                      let re = (Times( List.tl newList @ [Term(n1 * n, ex1 + ex)])) in
                                                                                      print_pExp re;
                                                                                      (* print_newline(); *)
                                                                                      re
                                                                                    else
                                                                                      let ree = Term(n1 * n, ex1 + ex) in
                                                                                      (* print_pExp ree; *)
                                                                                      (* print_newline(); *)
                                                                                      ree
              | Times(ol) -> let out = simplifyTimes ol in 
                    ( match out with
                        | Term (n2, ex2) -> Term(n * n2, ex2)
                        | Plus _ -> failwith "oof"
                        | Times _ -> failwith "oof2"
                    )

              | _ -> Printf.printf("simplifyTimes Failure"); Times(ol)
        | _ -> Times(ol)
    with _ -> Printf.printf("simplifyTimes Failure -> Empty List \n"); Times(ol)

and simplify1 (e:pExp): pExp =
    match e with
    | Term(n, ex) -> Printf.printf("Just a Term, returning"); print_pExp e; Printf.printf("\n"); e
    | Times(ol) -> Printf.printf("Entering Times \n"); simplifyTimes ol  
    | Plus(ol) -> Printf.printf("Entering Plus \n"); 
    try
        match List.hd ol with
              | Plus(il) -> Printf.printf("Plus inside Plus \n"); print_pExp (Plus(il)); Printf.printf(" tail: "); print_pExp (Plus(List.tl ol)); print_newline(); Plus ((List.tl ol) @ il)
              | Times(il) -> Printf.printf("Times inside Plus "); print_pExp e; Printf.printf(" Head: "); print_pExp (Times([List.hd ol])); print_newline(); Printf.printf("Tail: "); print_pExp (Times(Sort.list comparison (List.tl ol))); print_newline() ; Printf.printf("Inner List: "); print_pExp (Times(il)); print_newline() ; let out = Plus ((Sort.list comparison (List.tl ol)) @ [(simplifyTimes il)]) in Printf.printf("RESULT: "); print_pExp out; Printf.printf("\n"); (match out with
                                                                | Plus(reeil) -> let out = Plus(Sort.list comparison reeil) in print_pExp out; print_newline; out
                                                                | _ -> out)
              | Term(n, ex) -> Printf.printf("Adding Terms \n");
                    let newList = List.tl ol in
                    let re = List.hd newList in
                    print_pExp (List.hd ol);
                    print_newline();
                    print_pExp (Plus(List.tl ol));
                    print_newline();
                    print_pExp re;
                    print_newline();
                    match re with 
                    | Term(n1, ex1) -> if ex1 = ex then
                                          if List.length newList > 1 then
                                          let ret = List.tl newList @ [Term (n1 + n, ex)] in
                                          Printf.printf("REE1\n");
                                          let ret2 = Sort.list comparison ret in
                                          Printf.printf("THE ADDED LIST: ");
                                          print_pExp (Plus(ret));
                                          print_newline();
                                          simplify1 (Plus(ret2))
                                          else
                                            let ree = (Term(n1+ n, ex)) in
                                          Printf.printf("REEE2\n");
                                            (* print_pExp ree; *)
                                            (* print_newline(); *)
                                            ree
                                        else 
                                          if List.length newList > 1 then
                                          let ret2 = Sort.list comparison (List.tl ol) in
                                          Printf.printf("REEE3\n");
                                              let out = Plus([List.hd ol] @ [simplify1 (Plus(List.tl ol))]) in 
                                              print_pExp out;
                                            out
                                          else
                                              let out = e in
                                              Printf.printf("REE4\n");
                                              print_pExp out;
                                              out
                    | Plus(il) -> Printf.printf("DOING PLUS\n"); Plus([List.hd ol] @ [simplify1 (Plus(List.tl ol))])

                    | Times(il) -> Printf.printf("DOING TIMES\n il: "); print_pExp (Plus(List.tl ol)); print_newline; let out = Plus ([List.hd ol] @ [simplify1 (Plus(List.tl ol))]) in 
                    Printf.printf("OUt: "); print_pExp out; Printf.printf " Where ol = "; print_pExp (List.hd ol); out
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
    if ree = ree2 then
      let reet = match e1 with
        | Term (n, e) -> [Term(n, e)]
        | Times (list) ->  list
        | Plus (list) -> list
        | _ -> []
      in
      let reet2 = match e2 with
        | Term (n, e) ->  [Term(n, e)]
        | Times (list) ->  (list)
        | Plus (list) -> (list)
        | _ -> []
      in
      if List.length reet = List.length reet2 then
        true
      else
        false
    else 
      false

(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
let rec simplify (e:pExp): pExp =
    let rE = simplify1(e) in
      (*print_pExp rE;*)
      let i = degree e in 
      (* Printf.printf("Degree of expression: %i \n") i; *)
      if (equal_pExp e rE) then
        rE
      else  
        simplify(rE)




