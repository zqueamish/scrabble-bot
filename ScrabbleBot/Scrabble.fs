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
    }

    let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand


module LarsBot =
    


    let rec check_other_words (pieces : Map<uint32, (char*uint32)>) (state : State.state) (pos : coord) dx dy (word : string) = 
        match (state.board.ContainsKey pos) with
        | false -> word
        | true -> match dx+dy with
                    |  1 -> check_other_words pieces state (fst pos + dx, snd pos + dy) dx dy word+(string (fst (pieces.Item (state.board.Item pos))))
                    | -1 -> check_other_words pieces state (fst pos + dx, snd pos + dy) dx dy (string (fst (pieces.Item (state.board.Item pos))))+word

    let is_valid_word (pieces : Map<uint32, (char*uint32)>) (state : State.state) (pos : coord) (direction : string) =
        let word = match direction with
                    | "horizontal" -> check_other_words pieces state pos -1 0 "" |> (check_other_words pieces state (fst pos + 1, snd pos) +1 0)
                    | "vertical"   -> check_other_words pieces state pos 0 -1 "" |> (check_other_words pieces state (fst pos, snd pos + 1) 0 +1)
        Dictionary.lookup word state.dict

    let next_letter pieces (state : State.state) word pos direction = failwith ""

    let find_move pieces state anchor pos valid_words = failwith ""

    // let move pieces (state : State.state) : (coord*(uint32*(char*int)))list = 
    //     let valid_words = []
    //     let valid_words = valid_words :: match state.board.IsEmpty with
    //                                         | true  -> find_word pieces state string (0, 0)
    //                                         | false -> 
    let move pieces (state : State.state) : (coord*(uint32*(char*int)))list = failwith ""

    let rec gen (state : State.state) (anchor : coord) (pos : int32)(rack : MultiSet.MultiSet<uint32>) (arc : Dictionary.Dict) (pieces : Map<uint32, (char*uint32)>) (word : string) = 
        // let cur_coords = match direction with
        //                     | "horizontal" -> (fst anchor + pos, snd anchor)
        //                     | "vertical" -> (fst anchor, snd anchor + pos)
        
        let rackList = MultiSet.toList rack
        let plays = []
        match rackList with
        | [] -> []
        | x::xs -> go_on pos pieces x xs word (Dictionary.step (fst (pieces.Item x)) arc) anchor state

    and go_on (pos : int32) (pieces : Map<uint32, (char*uint32)>) (l : uint32) (rack : uint32 list) (word : string) (new_arc : (bool*Dictionary.Dict) option) (anchor : coord) (state : State.state)= 
        let plays = []
        if pos <= 0 then
            let new_word = (fst (pieces.Item l)).ToString() + word
            let plays = 
                if Dictionary.lookup new_word state.dict then 
                    new_word :: plays
                else
                    plays
            match new_arc with
            | Some arc -> 
                let plays = gen state anchor (pos-1) (MultiSet.ofList rack) (snd arc) pieces new_word @ plays
                match Dictionary.step (char 0) (snd arc) with
                | Some arc -> 
                    gen state anchor 1 (MultiSet.ofList rack) (snd arc) pieces new_word @ plays
                | None -> plays
            | None -> plays

        else
            let new_word = word + (fst (pieces.Item l)).ToString()
            let plays = 
                if Dictionary.lookup new_word state.dict then 
                    new_word :: plays
                else
                    plays
            match new_arc with
            | Some arc -> gen state anchor (pos+1) (MultiSet.ofList rack) (snd arc) pieces new_word @ plays
            | None -> plays

    let genStart (state : State.state) (pieces : Map<uint32, (char*uint32)>) (anchor : coord)= 
        let pos = 0
        let rack = state.hand
        let initArc = state.dict
        let word = ""

        gen state anchor pos rack initArc pieces word

module Scrabble =
    open System.Threading
    open MultiSet

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
        | [x] -> forcePrint x
        | x::xs -> 
            forcePrint x
            printWords xs

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            printWords (LarsBot.genStart st pieces (0, 0))
            let move = RegEx.parseMove input//LarsBot.move pieces st// RegEx.parseMove input // This should be automated

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = (State.mkState (updateBoard ms st.board)  st.dict st.playerNumber (updateHand st.hand ms newPieces))//(updateHand st.hand ms newPieces)) // This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = (State.mkState (updateBoard ms st.board)  st.dict st.playerNumber st.hand) // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = (State.mkState (updateBoard ms st.board)  st.dict st.playerNumber st.hand) // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st
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
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState Map.empty dict playerNumber handSet)


