open Types 

let rec to_debruijn ctx = function
  | Var x -> 
      let rec index n = function
        | [] -> failwith "Unbound variable"
        | y::ys -> if x = y then n else index (n+1) ys
      in
      Var (string_of_int (index 0 ctx))
  | Abs (x, t) -> Abs (x, to_debruijn (x::ctx) t)
  | App (t1, t2) -> App (to_debruijn ctx t1, to_debruijn ctx t2)

let rec shift d c = function
  | Var k -> let k' = (int_of_string k) in if k' < c then Var k else Var (string_of_int (k' + d))
  | Abs (x, t) -> Abs (x, shift d (c + 1) t)
  | App (t1, t2) -> App (shift d c t1, shift d c t2)

let rec substitute j s body = 
  match body with 
  | Var k -> let k' = (int_of_string k) in if k' = j then s else Var k
  | Abs (x, t) -> Abs (x, substitute (j + 1) (shift 1 0 s) t)
  | App (t1, t2) -> App (substitute j s t1, substitute j s t2)

let rec eval ast =
  let ast = to_debruijn [] ast in 
  match ast with
  | Var x -> Var x
  | Abs (x, t) -> Abs (x, eval t)
  | App (t1, t2) -> 
      let t1' = eval t1 in
      let t2' = eval t2 in
      match t1' with
      | Abs (x, body) -> 
          (* Beta reduction: substitute t2' for bound variable in body *)
          eval (substitute 0 t2' body)
      | _ -> App (t1', t2')  (* If t1 isn't an abstraction, keep as application *)
