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
  
  
(* value : expr -> bool *)
let rec value (e:expr) : bool =
  match e with 
  | Bool b   -> true 
  | Num n    -> true
  | Unit     -> true 
  | Id z     -> true
  | _ -> false



let rec step (e:expr) : expr option = 
  match e with
  | Num n          -> None (* Values don't move *)
  | Bool b         -> None
  | Unit           -> None
  | Id z           -> None

(* steps : expr -> expr *) 
let rec steps (e:expr) : expr =
  match step e with
  | None -> e
  | Some e' -> steps e' 

                     
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

    
