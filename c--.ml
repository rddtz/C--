(* Operações numéricas *)
type bop =
  | Sum | Sub | Mul | Div   (* operações aritméticas *)
  | Eq  | Neq | Lt | Gt   (* operações relacionais  *)
  | And | Or   (* operações lógicas *)

(*  Tipos *)
type tipo =
  | TyInt
  | TyBool
  | TyRef of tipo
  | TyUnit

(*  Expressões *)
type expr =
  | Num of int
  | Bool of bool
  | Id of string
  | If of expr * expr * expr
  | Binop of bop * expr * expr
  | Wh of expr * expr
  | Asg of expr * expr
  | Let of string * tipo * expr * expr
  | New of expr
  | Deref of expr
  | Unit
  | Seq of expr * expr
  | Read
  | Print of expr

type entrada =
  | Entry of expr * int list * int list * int list

(* value : expr -> bool *)
let rec value (e:expr) : bool =
  match e with
  | Bool b   -> true
  | Num n    -> true
  | Unit     -> true
  | Id z     -> true
  | _ -> false



let rec num_expr (e:expr) : int =
  match e with
  | Num n -> n

let rec bool_expr (e:expr) : bool =
  match e with
  | Bool b -> b



let rec step ((e : expr), (m : int list), (ip : int list), (out : int list)) : expr option * int list * int list * int list =
  match e with

    (* Values *)
  | Num n             -> (None, m, ip, out) (* Values don't move *)
  | Bool b            -> (None, m, ip, out)
  | Unit              -> (None, m, ip, out)
  | Id z              -> (None, m, ip, out)
                         
  (* Operações Binárias *)
  | Binop (op,e1,e2) -> (match step (e1, m, ip, out) with
      | (None, m', ip', out') -> (match step (e2, m', ip', out') with
          | (None, m'', ip'', out'') -> (match op with
              | Sum -> (Some (Num (num_expr(e1) + num_expr(e2))), m'', ip'', out'')
              | Sub -> (Some (Num (num_expr(e1) - num_expr(e2))), m'', ip'', out'')
              | Mul -> (Some (Num (num_expr(e1) * num_expr(e2))), m'', ip'', out'')
              | Div -> (Some (Num (num_expr(e1) / num_expr(e2))), m'', ip'', out'')
              | Eq  -> (Some (Bool (num_expr(e1) == num_expr(e2))), m'', ip'', out'')
              | Neq -> (Some (Bool (num_expr(e1) != num_expr(e2))), m'', ip'', out'')
              | Lt  -> (Some (Bool (num_expr(e1) < num_expr(e2))), m'', ip'', out'')
              | Gt  -> (Some (Bool (num_expr(e1) > num_expr(e2))) , m'', ip'', out'')
              | And -> (Some (Bool (bool_expr(e1) && bool_expr(e2))), m'', ip'', out'')
              | Or  -> (Some (Bool (bool_expr(e1) || bool_expr(e2))), m'', ip'', out'')
            )
          | (Some e2', m'', ip'', out'') -> (Some (Binop (op, e1, e2')), m'', ip'', out'') 
        )
      | (Some e1', m', ip', out')  -> (Some (Binop (op,e1',e2)), m', ip', out')
    )
    
  (* If *)
  | If (e1, e2, e3) -> (match step (e1, m, ip, out) with
      | (None, m', ip', out') -> (match (e1, m', ip', out') with
          | (Bool true, m'', ip'', out'')  -> (Some e2, m'', ip'', out'')  
          | (Bool false, m'', ip'', out'') -> (Some e3, m'', ip'', out'')  
        )
      | (Some e1', m', ip', out')  -> (Some(If (e1', e2, e3)), m', ip', out') 
    )
  | _ -> (Some (Num (-1)), m, ip, out) 

(* steps : expr -> expr *)
let rec steps ((e : expr), (m : int list), (ip : int list), (out : int list)): expr * int list * int list * int list =
  match step (e, m, ip, out) with
  | (None, m', ip', out')   -> (e, m', ip', out')
  | (Some e', m', ip', out') -> steps (e', m', ip', out')


          (*  TEST CASE

            let  x: int     =  read() in
            let  z: ref int = new x in
            let  y: ref int = new 1 in

            (while (!z > 0) (
                   y :=  !y * !z;
                   z :=  !z - 1);
            print (! y))

*)


let cndwhi = Binop(Gt, Deref (Id "z"),Num 0)
let asgny = Asg(Id "y", Binop(Mul, Deref (Id "y"),Deref(Id "z")))
let asgnz = Asg(Id "z", Binop(Sub, Deref (Id "z"),Num 1))
let bdwhi = Seq(asgny, asgnz)
let whi = Wh(cndwhi, bdwhi)
let prt = Print(Deref (Id "y"))
let seq = Seq(whi, prt)

let fat = Let("x", TyInt, Read,
              Let("z", TyRef TyInt, New (Id "x"),
                  Let("y", TyRef TyInt, New (Num 1),
                      seq)))
