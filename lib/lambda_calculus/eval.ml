open Types

let rec eval ast =
  let substitute x t body =
    match body with
    | _ -> body
  in
  match ast with
  | Var x -> Var x
  | Abs (x, t) -> Abs (x, eval t)
  | App (t1, t2) -> 
      let t1' = eval t1 in
      let t2' = eval t2 in
      match t1' with
      | Abs (x, body) -> eval (substitute x t2' body)
      | _ -> App (t1', t2') (* Can't reduce further *)