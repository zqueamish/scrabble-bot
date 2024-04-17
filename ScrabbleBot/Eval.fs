// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad
    open System

    let add a b = a >>= (fun v1 -> b >>= (fun v2 -> ret (v1+v2)))
    let div a b = a >>= (fun v1 -> b >>= (fun v2 -> if v2 = 0 then (fail DivisionByZero) else ret (v1/v2)))

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let binop f (a : SM<'a>) (b : SM<'a>) = a >>= fun x -> b >>= fun y -> ret (f x y)

    let rec modulo a b = a >>= fun v1 -> (b >>= fun v2 -> if v2 <> 0 then
                                                            match v1 with
                                                            | n when n >= v2 -> modulo (ret (v1 - v2)) (ret v2)
                                                            | n -> ret n
                                                            else fail DivisionByZero
                                                            )

    let rec arithEval a : SM<int> = 
        match a with
        | N i -> ret i
        | V s -> lookup s
        | WL -> wordLength
        | PV a -> arithEval a >>= pointValue 
        | Add (a,b) -> binop (+) (arithEval a) (arithEval b)
        | Sub (a,b) -> binop (-) (arithEval a) (arithEval b)
        | Mul (a,b) -> binop (*) (arithEval a) (arithEval b)
        | Div (a,b) -> div (arithEval a) (arithEval b)
        | Mod (a,b) -> modulo (arithEval a) (arithEval b)
        | CharToInt c -> (charEval c) >>= fun x -> ret (int x)
    and charEval c : SM<char> = 
        match c with
        | C c -> ret c
        | CV a -> arithEval a >>= characterValue
        | ToUpper c -> (charEval c) >>= (fun x -> ret (System.Char.ToUpper x))
        | ToLower c -> (charEval c) >>= (fun x -> ret (System.Char.ToLower x))
        | IntToChar a -> (arithEval a) >>= fun x -> ret (char x)

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a,b) -> binop (=) (arithEval a) (arithEval b)
        | ALt (a,b) -> binop (<) (arithEval a) (arithEval b)
        | Not b -> boolEval b >>= fun x -> ret (not x)
        | Conj (b1, b2) -> binop (&&) (boolEval b1) (boolEval b2)
        | IsDigit c -> (charEval c) >>= fun x -> ret (System.Char.IsDigit x)
        | IsLetter c -> (charEval c) >>= fun x -> ret (System.Char.IsLetter x)
        | IsVowel c -> (charEval c) >>= fun x -> ret (((System.Char.ToLower x) |> List.contains) ['a';'e';'i';'o';'u';'y'])


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"