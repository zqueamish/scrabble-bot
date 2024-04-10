// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad
    open System

    let add a b = 
        a >>= (fun x ->
            b >>= (fun y ->
                ret (x + y)))      
    let div a b =
        a >>= (fun x ->
            b >>= (fun y ->
                ret (x / y)))      

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
       | IsConsonant of cExp  (* check for constant *)

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

    let rec arithEval a : SM<int> =
        match a with
        | N n -> ret n
        | V x -> lookup x
        | WL -> wordLength
        | PV p -> arithEval p >>= pointValue
        | Add (a1, a2) ->
            arithEval a1 >>= fun x ->
            arithEval a2 >>= fun y ->
            ret (x + y)
        | Sub (a1, a2) ->
            arithEval a1 >>= fun x ->
            arithEval a2 >>= fun y ->
            ret (x - y)
        | Mul (a1, a2) ->
            arithEval a1 >>= fun x ->
            arithEval a2 >>= fun y ->
            ret (x * y)
        | Div (a1, a2) ->
            arithEval a1 >>= fun x ->
            arithEval a2 >>= fun y ->
            ret (x / y)
        | Mod (a1, a2) ->
            arithEval a1 >>= fun x ->
            arithEval a2 >>= fun y ->
            if y = 0 then fail DivisionByZero else ret (x / y)
        | CharToInt c ->
            charEval c >>= fun cv ->
            ret (int cv)

    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV cv -> arithEval cv >>= characterValue
        | ToUpper c ->
            charEval c >>= fun c ->
            ret (Char.ToUpper c)
        | ToLower c ->
            charEval c >>= fun c ->
            ret (Char.ToLower c)
        |IntToChar a ->
            arithEval a >>= fun iv ->
            ret (char iv) 

    and boolEval b : SM<bool> =
        let vowels = ['a';'A';'e';'E';'o';'O';'y';'Y']
        let consonants = ['b';'B';'c';'C';'d';'D';'f';'F';'g';'G';'h';'H';'j';'J';'k';'K';'l';'L';'m';'M';'n';'N';'p';'P';'q';'Q';'r';'R';'s';'S';'t';'T';'v';'V';'w';'W';'y';'Y';'z';'Z']
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a1, a2) ->
            arithEval a1 >>= fun x ->
            arithEval a2 >>= fun y ->
            ret (a1 = a2)
        | ALt (a1, a2) ->
            arithEval a1 >>= fun x ->
            arithEval a2 >>= fun y ->
            ret (a1 < a2)
        | Not b ->
            boolEval b >>= fun r ->
            ret (not r)
        | Conj (b1, b2) ->
            boolEval b1 >>= fun x ->
            boolEval b2 >>= fun y ->
            ret (x && y)
        | IsVowel v ->
            charEval v >>= fun vv ->
            ret (List.contains vv vowels)
        | IsConsonant c ->
            charEval c >>= fun cc ->
            ret (List.contains cc consonants)


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