namespace Lars

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =
    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Map<coord, uint32>
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        playerTurn    : uint32
        players       : Map<uint32, bool>
        noPlayers     : uint32
        square_fun    : coord -> bool
    }
    
    let mkState b d pn h pt players np square_fun = {board = b; dict = d;  playerNumber = pn; hand = h; playerTurn = pt; players = players; noPlayers = np; square_fun = square_fun}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand


module LarsBot =
    
    open System.Threading.Tasks

    let rec check_other_words (pieces : Map<uint32, tile>) (state : State.state) (pos : coord) dx dy (word : string) = 
        match (state.board.ContainsKey pos) with
        | false -> word
        | true -> match dx+dy with
                    |  1 -> check_other_words pieces state (fst pos + dx, snd pos + dy) dx dy word+(string (fst (Set.toList (pieces.Item (state.board.Item pos))).[0]))
                    | -1 -> check_other_words pieces state (fst pos + dx, snd pos + dy) dx dy (string (fst (Set.toList (pieces.Item (state.board.Item pos))).[0]))+word

    let is_valid_word (pieces : Map<uint32, tile>) (state : State.state) (pos : coord) (direction : bool) (l : char) (rack : MultiSet.MultiSet<uint32>) =
        let word = match direction with // true is horizontal and false is vertical
                    | true -> check_other_words pieces state (fst pos - 1, snd pos) -1 0 (string l) |> (check_other_words pieces state (fst pos + 1, snd pos) +1 0)
                    | false   -> check_other_words pieces state (fst pos, snd pos - 1) 0 -1 (string l) |> (check_other_words pieces state (fst pos, snd pos + 1) 0 +1)
        if String.length word = 1 then true 
        else Dictionary.lookup word state.dict

    let rec gen (direction : bool) (state : State.state) (anchor : coord) (pos : int32)(rack : MultiSet.MultiSet<uint32>) (arc : Dictionary.Dict) (pieces : Map<uint32, tile>) (word : string) (word_moves : (coord*(uint32*(char*int)))list) =    
        let plays = []
        let pos_coords = 
            match direction with
            | true  -> coord(fst anchor + pos, snd anchor)
            | false -> coord(fst anchor, snd anchor + pos)
        if not (state.square_fun pos_coords) then []
        else 
            if state.board.ContainsKey pos_coords then 
                go_on direction pos pieces (state.board.Item pos_coords) (MultiSet.toList rack) word word_moves (Dictionary.step (fst (Set.toList (pieces.Item (state.board.Item pos_coords))).[0]) arc) anchor state
            else 
                MultiSet.fold (fun plays letter _ -> go_on direction pos pieces letter (MultiSet.toList (MultiSet.remove letter 1u rack)) word word_moves (Dictionary.step (fst (Set.toList (pieces.Item letter)).[0]) arc) anchor state @ plays) plays rack

    and go_on (direction : bool) (pos : int32) (pieces : Map<uint32, Set<char * int>>) (l : uint32) (rack : uint32 list) (word : string) (word_moves : (coord*(uint32*(char*int)))list) (new_arc : (bool*Dictionary.Dict) option) (anchor : coord) (state : State.state)= 
        let plays = []
        let letter = fst (Set.toList (pieces.Item l)).[0]
        let pos_coords = 
            match direction with
            | true  -> coord(fst anchor + pos, snd anchor)
            | false -> coord(fst anchor, snd anchor + pos)
        if not (is_valid_word pieces state pos_coords (not direction) letter (MultiSet.ofList rack)) then plays 
        else if pos <= 0 then
            let next_pos_coords = 
                match direction with
                | true  -> coord(fst anchor + pos - 1, snd anchor)
                | false -> coord(fst anchor, snd anchor + pos - 1)
            let new_word = letter.ToString() + word
            let word_moves = if state.board.ContainsKey pos_coords then word_moves else (pos_coords, (l, ((fst (Set.toList (pieces.Item l)).[0]), (snd (Set.toList (pieces.Item l)).[0])))) :: word_moves
            let plays = 
                if Dictionary.lookup new_word state.dict && (not (state.board.ContainsKey next_pos_coords)) then 
                    if List.length word_moves > 0 then word_moves :: plays else plays
                else
                    plays

            match new_arc with
            | Some arc -> 
                let plays = (gen direction state anchor (pos-1) (MultiSet.ofList rack) (snd arc) pieces new_word word_moves) @ plays
                match Dictionary.step (char 0) (snd arc) with
                | Some arc -> 
                    if  (not (state.board.ContainsKey next_pos_coords)) then 
                        (gen direction state anchor 1 (MultiSet.ofList rack) (snd arc) pieces new_word word_moves) @ plays
                    else plays
                | None -> plays
            | None -> plays

        else
            let next_pos_coords = 
                match direction with
                | true  -> coord(fst anchor + pos + 1, snd anchor)
                | false -> coord(fst anchor, snd anchor + pos + 1)
            let new_word = word + letter.ToString()
            let word_moves = if state.board.ContainsKey pos_coords then word_moves else (pos_coords, (l, ((fst (Set.toList (pieces.Item l)).[0]), (snd (Set.toList (pieces.Item l)).[0])))) :: word_moves
            let plays = 
                if Dictionary.lookup new_word state.dict && (not (state.board.ContainsKey next_pos_coords)) then 
                    if List.length word_moves > 0 then word_moves :: plays else plays
                else
                    plays
            match new_arc with
            | Some arc -> (gen direction state anchor (pos+1) (MultiSet.ofList rack) (snd arc) pieces new_word word_moves) @ plays
            | None -> plays

    let genStart (state : State.state) (pieces : Map<uint32, tile>) (anchor : coord)= 
        let pos = 0
        let rack = state.hand
        let initArc = state.dict
        let word = ""
        let word_moves = []
        let direction = true // True is horizontal and False is vertical

        let horizontal_words =  async { return if (not (state.board.ContainsKey (coord(fst anchor + 1, snd anchor)))) then gen direction state anchor pos rack initArc pieces word word_moves else []}
        let vertical_words = async {return if (not (state.board.ContainsKey (coord(fst anchor, snd anchor + 1)))) then (gen (not direction) state anchor pos rack initArc pieces word word_moves) else []}

        (Async.RunSynchronously horizontal_words) @ (Async.RunSynchronously vertical_words)

    let rec move_value (move : (coord*(uint32*(char*int)))list) = 
        match move with
        | []      -> 0 
        | x :: xs -> x |> snd |> snd |> snd |> (+) (move_value xs)

    let move pieces (state : State.state) : (coord*(uint32*(char*int)))list = 
        let playable_moves = if state.board.ContainsKey (coord(0, 0)) then 
                                let tasks = (Map.fold (fun lst anchor letter  -> async {return genStart state pieces anchor} :: lst) [] state.board)
                                Array.fold (fun moves move -> move @ moves) [] (tasks |> Async.Parallel |> Async.RunSynchronously)
                             else genStart state pieces (0, 0)
        List.fold (fun (best_move : (coord*(uint32*(char*int32))) list) (move : 'b list) -> if move_value move > move_value best_move then move else best_move) [] playable_moves

module Scrabble =
    open System.Threading
    open MultiSet
    open ScrabbleLib


    let rec updateBoard (ms : (coord*(uint32*(char*int)))list) (board : Map<coord, uint32>) = 
        match ms with
        | [] -> board
        | x :: xs -> (updateBoard xs board).Add (fst x, fst(snd x)) 

    let rec removePieces hand ms = 
        match ms with
        | [] -> hand
        | x :: xs -> removePieces (MultiSet.removeSingle (fst (snd x)) hand) xs 

    let rec addPieces (newPieces : (uint32*uint32) list) (hand : MultiSet<uint32>)  = 
        match newPieces with
        | [] -> hand
        | x :: xs -> addPieces xs (MultiSet.add (fst x) (snd x) hand)
        
    let updateHand hand (ms : (coord*(uint32*(char*int)))list) (newPieces : (uint32*uint32) list) = 
        removePieces hand ms |> addPieces newPieces

    let rec printWords (moves : string list) = 
        match moves with
        | [] -> forcePrint "No moves!"
        | [x] -> forcePrint (x + "\n")
        | x::xs -> 
            forcePrint (x + "\n")
            printWords xs

    let playGame cstream pieces (st : State.state) =
    
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            // forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            // let input =  System.Console.ReadLine()
            if (not (st.players.Item st.playerTurn)) then aux (State.mkState st.board st.dict st.playerNumber st.hand (((st.playerTurn)%st.noPlayers)+1u) st.players st.noPlayers st.square_fun)
            else 
                if st.playerTurn = st.playerNumber then
                    let move = LarsBot.move pieces st// RegEx.parseMove input // This should be automated

                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    send cstream (if (List.length move) > 0 then (SMPlay move) else SMPass)
                    debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                let msg = recv cstream
            
                match msg with
                | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                    (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                    let st' = (State.mkState (updateBoard ms st.board)  st.dict st.playerNumber (updateHand st.hand ms newPieces) (((st.playerNumber)%st.noPlayers)+1u) st.players st.noPlayers st.square_fun)//(updateHand st.hand ms newPieces)) // This state needs to be updated
                    aux st'
                | RCM (CMPlayed (pid, ms, points)) ->
                    (* Successful play by other player. Update your state *)
                    let st' = (State.mkState (updateBoard ms st.board)  st.dict st.playerNumber st.hand (((pid)%st.noPlayers)+1u) st.players st.noPlayers st.square_fun) // This state needs to be updated
                    aux st'
                | RCM (CMPlayFailed (pid, ms)) ->
                    (* Failed play. Update your state *)
                    let st' = (State.mkState (updateBoard ms st.board)  st.dict st.playerNumber st.hand (((pid)%st.noPlayers)+1u) st.players st.noPlayers st.square_fun) // This state needs to be updated
                    aux st'
                | RCM (CMPassed (pid)) ->
                    let st' = (State.mkState st.board st.dict st.playerNumber st.hand (((pid)%st.noPlayers)+1u) st.players st.noPlayers st.square_fun)
                    aux st'
                | RCM (CMForfeit (pid)) ->
                    let st' = (State.mkState st.board st.dict st.playerNumber st.hand st.noPlayers (st.players.Add (pid, false)) st.noPlayers st.square_fun)
                    aux st'
                | RCM (CMChange (pid, nt)) ->
                    let st' = (State.mkState st.board st.dict st.playerNumber st.hand (((pid)%st.noPlayers)+1u) st.players st.noPlayers st.square_fun)
                    aux st'
                | RCM (CMTimeout (pid)) -> 
                    let st' = (State.mkState st.board st.dict st.playerNumber st.hand (((pid)%st.noPlayers)+1u) st.players st.noPlayers st.square_fun)
                    aux st'
                | RCM (CMGameOver _) -> ()
                | RCM a -> failwith (sprintf "not implmented: %A" a)
                | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st

        
        //printWords (LarsBot.genStart st pieces (0, 0))

        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        //let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = simpleBoardLangParser.parseSimpleBoardProg boardP
        let rec mkPlayers (n : uint32) =
            match n with
            | 0u -> Map.empty
            | n -> (mkPlayers (n-1u)).Add (n, true)

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        fun () -> playGame cstream tiles (State.mkState Map.empty dict playerNumber handSet playerTurn (mkPlayers numPlayers) numPlayers board)


