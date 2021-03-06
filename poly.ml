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
    *)

  let rec print_pExp (_e: pExp): unit =
    (* TODO *)
    try 
    match _e with
    | Term(n, e) -> Printf.printf("%dx^%d") n e;
    | Plus(l) -> print_pExp (List.hd l); Printf.printf(" + "); List.iter print_pExp (List.tl l);
    | Times(l) -> Printf.printf("("); print_pExp (List.hd l); Printf.printf(" * "); List.iter print_pExp (List.tl l); Printf.printf(")");
    (* print_newline() *)
    with _ -> Printf.printf("Print Failure\n")


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
    | Var(c) -> Term(1, 1)
    | Num(i) -> Term(i, 0)
    | Pos(e) -> Times([Term(1, 0); from_expr e])
    | Neg(e) -> Times([Term(-1, 0); from_expr e])
    | Sub(e1, e2) -> Plus([from_expr e1; Times([Term(-1, 0); from_expr e2])])
    | Add(e1,e2) ->  Plus([from_expr e1; from_expr e2])
    | Mul(e1,e2) -> Times([from_expr e1; from_expr e2])
    | Pow(e,i) -> let out = distribute_pow (from_expr e) i in out
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
with _ -> print_pExp _e; 0

(* 
  Comparison function useful for sorting of Plus[..] args 
  to "normalize them". This way, terms that need to be reduced
  show up one after another.
  *)
let compare (e1: pExp) (e2: pExp) : bool =
  degree e1 > degree e2



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
  (*print_pExp (Plus( List.map (fun s -> Times(rl @ [s])) ll ));*)
 ( List.map (fun s -> simplify1 (Times(rl @ [s]))) ll )

and simplifyTimes (ol:pExp list) : pExp =
  try 
  match List.hd ol with
        | Plus(il) ->   Plus (distributePlus il (List.tl ol))
        | Times(il) ->   let out = simplify1 (Times(Sort.list comparison ([simplifyTimes il] @ (List.tl ol)))) in out
        | Term(n, ex) -> 
              let newList = List.tl ol in
              let re = List.hd newList in
              (* print_pExp re; *)
              match re with 
              | Term(n1, ex1) -> if List.length newList > 1 then
                                                                                      let re = (Times( List.tl newList @ [Term(n1 * n, ex1 + ex)])) in
                                                                                      re
                                                                                    else
                                                                                      let ree = Term(n1 * n, ex1 + ex) in
                                                                                      ree
              | Times(ol2) -> let out = simplifyTimes ol2 in 
                    ( match out with
                        | Term (n2, ex2) -> let out = Term(n * n2, ex2 + ex) in out
                        | Plus _ -> failwith "oof"
                        | Times(nn) -> let v = ([List.hd ol] @ [out]) in Times(Sort.list comparison v)
                                
                    )

              | Plus(il) -> Plus(distributePlus il [List.hd ol])
        | _ -> Times(ol)
  with _ -> Times(ol)

and simplify1 (e:pExp): pExp =
    match e with
    | Term(n, ex) -> e
    | Times(ol) -> simplifyTimes ol  
    | Plus(ol) ->
    try
        match List.hd ol with
              | Plus(il) -> if (List.length ol) > 1 then simplify1 (Plus (Sort.list comparison ((List.tl ol) @ il))) else List.hd ol
              | Times(il) -> let out = simplify1 (Plus ((Sort.list comparison (List.tl ol)) @ [(simplifyTimes (Sort.list comparison il))])) in (match out with
                                                                | Plus(reeil) -> let out = simplify1 (Plus(Sort.list comparison reeil)) in out
                                                                | _ -> out)
              | Term(n, ex) ->
                    let newList = List.tl ol in
                    let re = List.hd newList in
                    match re with 
                    | Term(n1, ex1) -> if ex1 = ex then
                                          if List.length newList > 1 then
                                          let ret = List.tl newList @ [Term (n1 + n, ex)] in
                                          let ret2 = Sort.list comparison ret in
                                          simplify1 (Plus(ret2))
                                          else
                                            let ree = (Term(n1+ n, ex)) in
                                            (* print_pExp ree; *)
                                            (* print_newline(); *)
                                            ree
                                        else 
                                          if List.length newList > 1 then
                                          let ret2 = Sort.list comparison (List.tl ol) in
                                              let out = Plus(Sort.list comparison ([List.hd ol] @ [simplify1 (Plus(List.tl ol))])) in 
                                            out
                                          else
                                              let out = e in
                                              out
                    | Plus(il) -> simplify1 (Plus(Sort.list comparison ([List.hd ol] @ [simplify1 (Plus(List.tl ol))])))

                    | Times(il) -> let out = Plus ([List.hd ol] @ [simplify1 (Plus(List.tl ol))]) in simplify1 out
              | _ -> e
      with _ -> e
      | _ -> e
    
(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let rec equal_pExp (e1:pExp) (e2:pExp) : bool =
    match e1, e2 with
    | Term(x1,y1), Term(x2, y2) -> if x1=x2 then if y1 = y2 then true else false else false
    | Plus(t1), Plus(t2) -> equal_pExp (List.hd t1) (List.hd t2)
    | Times(t1), Times(t2) -> equal_pExp (List.hd t1) (List.hd t2)
    | (Term _ | Plus _ | Times _), _ -> false

  
(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
let rec simplify (e:pExp): pExp =
    let rE = simplify1(e) in
      let i = degree e in 
      (* Printf.printf("Degree of expression: %i \n") i; *)
      if (equal_pExp e rE) then
        rE
      else  
        simplify(rE)
