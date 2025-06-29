(*  
Rayan Raddatz de Matos - 00584919
Gabriel Henrique Fiszczuk Brandeburski - 00580296
*)

open List

module StringMap = Map.Make(String)

(* Operações numéricas *)
type bop =
  | Sum | Sub | Mul (* operações aritméticas *)
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
  (* Adição: For *)
  | For of expr * expr * expr

(* typeInfer : (string, tipo) list, expr -> tipo option *)
let rec typeInfer (gm: (string * tipo) list) (e:expr) : tipo option = 
  match e with
  | Num n         -> Some TyInt
  | Bool b        -> Some TyBool
  | Unit          -> Some TyUnit
  | Read          -> Some TyInt
  | Print e'      -> (match typeInfer gm e' with
      | Some TyInt -> Some TyUnit
      | _          -> None       )
  | Id x          -> assoc_opt x gm 
  | If (e1, e2, e3) -> (match typeInfer gm e1 with
      | Some TyBool ->  (match (typeInfer gm e2, typeInfer gm e3) with
          | (Some t2, Some t3)  -> if t2 = t3
              then Some t2
              else None
          | _                   -> None)
      | _         -> None)
  | Binop (bop, e1, e2) -> (match bop with
      | Sum 
      | Sub 
      | Mul -> (match (typeInfer gm e1, typeInfer gm e2) with
                | (Some TyInt, Some TyInt) -> Some TyInt
                | _ -> None)
      | Eq  
      | Neq 
      | Lt 
      | Gt  -> (match (typeInfer gm e1, typeInfer gm e2) with
                | (Some TyInt, Some TyInt) -> Some TyBool
                | _ -> None)
      | And 
      | Or  ->  (match (typeInfer gm e1, typeInfer gm e2) with
                | (Some TyBool, Some TyBool) -> Some TyBool
                | _ -> None))
  | Wh (e1, e2) -> (match (typeInfer gm e1, typeInfer gm e2) with
      | (Some TyBool, Some TyUnit)  -> Some TyUnit
      | _                           -> None)
  | Asg (e1, e2) -> (match (typeInfer gm e1, typeInfer gm e2) with
      | (Some TyRef(t1), Some t2) -> if t1 = t2
          then Some TyUnit
          else None 
      | _ -> None)     
  | Let (x, tp, e1, e2) -> (match (typeInfer gm e1, typeInfer ((x, tp) :: gm) e2) with
      | (Some t1, Some t2) -> if t1 = tp
          then Some t2
          else None
      | _ -> None)
  | New e' -> (match typeInfer gm e' with
      | Some t -> Some (TyRef t)
      | _ -> None)                          
  | Deref e' -> (match typeInfer gm e' with
      | Some TyRef(t) -> Some t
      | _ -> None)
  | Seq (e1, e2) -> (match (typeInfer gm e1, typeInfer gm e2) with
      | (Some TyUnit, Some t) -> Some t
      | _ -> None)  
  (*
  
  Γ ⊢ e1 : bool {teste}    Γ ⊢ e2 : unit  {incremento}    Γ ⊢ e3 : unit {código interno}
  ---------------------------------------------------------------------------------------
                          for(e1, e2, e3) : unit
              
  *)
  | For(e1, e2, e3) -> (match (typeInfer gm e1, typeInfer gm e2, typeInfer gm e3) with
      | (Some TyBool, Some TyUnit, Some TyUnit) -> Some TyUnit
      | _ -> None)

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

exception Not_a_number of string

let rec num_expr (e:expr) : int =
  match e with
  | Num n -> n
  | _ -> raise (Not_a_number "The input expression is not a number")

exception Not_a_bool of string

let rec bool_expr (e:expr) : bool =
  match e with
  | Bool b -> b
  | _ -> raise (Not_a_bool "The input expression is not a boolean")
 
exception Not_an_id of string

let rec id_expr (e:expr) : string =
  match e with
  | Id z -> z
  | _ -> raise (Not_an_id "The input expression is not an identifier")

let rec subst ((s: expr), (x: string), (e: expr)) : expr =
  match e with
  | Num n                  -> e
  | Bool b                 -> e
  | Id y                   -> (if y == x then s else e)
  | Let(y, t, e1, e2)      -> (if y == x then Let(y, t, subst(s, x, e1), e2)
                               else Let(y, t, subst(s, x, e1), subst(s, x, e2))
                              )
  | Binop(op, e1, e2)      -> Binop(op, subst(s, x, e1), subst(s, x, e2))
  | If(e1, e2, e3)         -> If(subst(s,x,e1), subst(s,x,e2), subst(s,x,e3))
  | Print(e1)              -> Print(subst(s, x, e1))
  | Seq(e1, e2)            -> Seq(subst(s, x, e1), subst(s, x, e2))
  | Wh(e1, e2)             -> Wh(subst(s, x, e1), subst(s, x, e2))
  | New(e1)                -> New(subst(s, x, e1))
  | Deref(e1)              -> Deref(subst(s, x, e1))
  | Asg(e1, e2)            -> Asg(subst(s, x, e1), subst(s, x, e2))
  | For(e1,e2,e3)          -> For(subst(s, x, e1), subst(s, x, e2), subst(s, x, e3)) 
  | _                      -> e

let rec alloc ((e : expr), (m : (string * expr) list)) : (string * expr) =
  match m with
  | [] -> ("x_", e)
  | _ -> (
      let x = String.concat "" [fst (List.hd m); Printf.sprintf "%d" (List.length m)] in
      match assoc_opt x m with
      | None -> (x, e)
      | _    -> alloc(e, List.tl m)
    )

let rec step ((e : expr), (m : (string * expr) list), (ip : int list), (out : int list)) : expr option * (string * expr) list * int list * int list =
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
          | _ -> raise (Not_a_bool "The condition for the 'if' is not a valid boolean")
        )
      | (Some e1', m', ip', out')  -> (Some(If (e1', e2, e3)), m', ip', out')
    )

  (* Input/Output *)
  | Read -> (Some (Num (List.hd ip)), m, (List.tl ip), out)
  | Print (e1) -> (match step (e1, m, ip, out) with
      | (None, m', ip', out') -> (Some Unit, m, ip, out @ [(num_expr e1)])
      | (Some e1', m', ip', out')  -> (Some (Print e1'), m', ip', out')
    )

  (* Sequência *)
  | Seq(e1,e2) -> (match step (e1, m, ip, out) with
      | (None, m', ip', out') -> (Some e2, m', ip', out')
      | (Some e1', m', ip', out')  -> (Some (Seq(e1',e2)), m', ip', out')
    )

  (* While *)
  | Wh(e1, e2) -> (Some(If (e1, Seq(e2,Wh(e1, e2)), Unit)), m, ip, out) 

  (* Atribuição de variáveis *)
  | Asg(e1, e2) -> (match step (e1, m, ip, out) with
      | (None, m', ip', out') -> (match step (e2, m', ip', out') with
          | (None, m'', ip'', out'')  -> (Some Unit, (id_expr e1 , e2) :: List.remove_assoc (id_expr e1) m'', ip'', out'')
          | (Some e2', m'', ip'', out'') -> (Some(Asg(e1, e2')), m'', ip'', out'')
        )
      | (Some e1', m', ip', out')  -> (Some(Asg (e1', e2)), m', ip', out')
    )

  (* Derreferência de variáveis *)
  | Deref(e1) -> (match step (e1, m, ip, out) with
      | (None, m', ip', out') -> (Some (assoc (id_expr(e1)) m), m', ip', out')
      | (Some e1', m', ip', out')  -> (Some (Deref(e1')), m', ip', out')
    )

  (* Let *)
  | Let(x, t, e1, e2) -> (match step (e1, m, ip, out) with
      | (None, m', ip', out') -> (Some (subst(e1, x, e2)), m', ip', out')
      | (Some e1', m', ip', out')  -> (Some (Let(x, t, e1', e2)) , m', ip', out')
    )

  (* New *)
  | New(e1) -> (match step (e1, m, ip, out) with
      | (None, m', ip', out') -> let x = alloc(e1, m) in(Some (Id (fst x)), x :: m', ip', out')
      | (Some e1', m', ip', out')  -> (Some (New(e1')), m', ip', out')
    )
    
  (* Adição: For *)
  | For(e1, e2, e3) -> (Some (If (e1, 
                                  Seq(e3, 
                                      Seq(e2, 
                                          For(e1, e2, e3))), 
                                  Unit)), 
                        m, ip, out)   

(* steps : expr -> expr *)
let rec steps ((e : expr), (m : (string * expr) list), (ip : int list), (out : int list)): expr * (string * expr) list * int list * int list =
  match step (e, m, ip, out) with
  | (None, m', ip', out')   -> (e, m', ip', out')
  | (Some e', m', ip', out') -> steps (e', m', ip', out') 

(* 
                           Utilizando C--

   A chamada das funções step e steps esperam um expressão e três listas. Escolhemos representar 
a memória como uma lista de pares (string * int), utilizando funções de associação para 
busca por chave (string indexa o valor int).

 Uma chamada de uma expressão definida como minha_expr com a memória vazia e com o valor 5 na entrada 
seria feita assim:

                                 steps(minha_expr, [], [5], [])
                                          ^        ^    ^   ^
                                          |        \     \   \
                                         ;         |     |    `--> Fila de saída
       Expressão que será avaliada <----'         |     |
                 Memória (vazia nesse caso) <----´     |
                                 Fila de entrada <----´

As expressões abaixo são alguns programas que podem ser utilizados de exemplo para utilizar a linguagem.

*) 

(* 1 -- Imprimindo valores de 1 até 10 

            let  x: int     =  read() in
            let  z: ref int = new 1 in

            for (!z < !x; !z := !z + 1) (
                print(!z)
            )

*)


let contar = Let("x", TyInt, Read,
                 Let("z", TyRef TyInt, New (Num 1), 
                     For(Binop(Lt, Deref (Id "z"), Binop(Sum, (Id "x"), (Num 1))), 
                         Asg(Id "z", Binop(Sum, Deref (Id "z"), (Num 1))), 
                         Print(Deref(Id "z")))))

let tipoContar = typeInfer [] contar

let numeros1a10 = steps(contar, [], [10], []) 
    
  
(* 2 -- Imprimindo valores pares de 0 até 100 

            let  x: int     =  read() in
            let  z: ref int = new 0 in

            for (!z < !x + 1; !z := !z + 2) (
                print(!z)
            )

*)

let contar_pares = Let("x", TyInt, Read,
                       Let("z", TyRef TyInt, New (Num 0), 
                           For(Binop(Lt, Deref (Id "z"), Binop(Sum, (Id "x"), (Num 1))), 
                               Asg(Id "z", Binop(Sum, Deref (Id "z"), Num 2)),
                               Print(Deref(Id "z")))))

let tipoContarPares = typeInfer [] contar_pares                     

let pares0a100 = steps(contar_pares, [], [100], []) 
    
  
(* 3 -- Soma dos 5 primeiros impares:

            let  x: int     =  read() in
            let  z: ref int = new 0 in

            for (!z < !x; !z := !z + 2) (
                print(!z)
            )

*)  
    

let mul2z   = Binop(Mul, Deref (Id "z"), (Num 2))
let sum1z   = Binop(Sum, (Num 1), mul2z) 
let updtz   = Asg(Id "z", Binop(Sum, Deref (Id "z"), (Num 1)))
let updty   = Asg(Id "y", Binop(Sum, Deref (Id "y"), sum1z))
let forsum  = For(Binop(Lt, Deref (Id "z"), (Id "x")), updtz, updty)
let seq     = Seq(forsum, Print(Deref (Id ("y"))))

let soma_impares = Let("x", TyInt, Read,
                       Let("z", TyRef TyInt, New (Num 0),
                           Let("y", TyRef TyInt, New (Num 0), 
                               seq)))

let tipoSomaImpares = typeInfer [] soma_impares

let soma5_primeiros_impares = steps(soma_impares, [], [5], []) 

(*  4 -- 5 Fatorial:

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

let tipoFat = typeInfer [] fat           
    
let fat5 = steps(fat, [], [5], [])
