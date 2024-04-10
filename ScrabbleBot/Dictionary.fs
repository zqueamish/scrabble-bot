module Dictionary

type Dict = {map: Map<char, Dict*bool>;}

let empty () = {map = Map.empty}

let rec insertRest str =
    match str with
    | [] -> empty ()
    | [s] -> {map = (empty ()).map.Add (s, (empty (), true))}
    | s :: xs -> {map = (empty ()).map.Add (s, (insertRest xs, false))}

let rec insertNext str dict = 
    match str with
    | [] -> empty ()
    | [s] when dict.map.ContainsKey(s) -> {map = dict.map.Add (s, ((fst (dict.map.Item s)), true))}
    | s :: xs when dict.map.ContainsKey(s) -> {map = dict.map.Add (s,(insertNext xs (fst (dict.map.Item s)), snd (dict.map.Item s)))}
    | s :: xs -> {map = dict.map.Add(s, (insertRest xs, false))}
    
let rec insertGaddag str dict (tmp : list<char>) =
    match str with
    | [] -> dict
    | s :: xs -> insertGaddag xs (insertNext (s::tmp@xs) dict) (s::tmp)

let insert str dict = insertGaddag (Seq.toList str) dict [(char 0)]

let step c dict = 
    match dict.map.ContainsKey c with
    | true -> Some(snd (dict.map.Item c), fst (dict.map.Item c))
    | false -> None

let reverse dict = step (char 0) dict

let rec search str dict =
    match str with
    | [] -> false
    | [s] when dict.map.ContainsKey(s) -> snd (dict.map.Item s)
    | s :: xs when dict.map.ContainsKey(s) -> search xs (fst (dict.map.Item s))
    | _ -> false

let lookup str dict = 
    match (Seq.toList str) with
    | [] -> false
    | [s] -> match step s dict with
                | None -> false
                | Some res -> fst res
    | s :: xs -> match step s dict with
                    | None -> false
                    | Some res -> match reverse (snd res) with
                                    | None -> false
                                    | Some res -> search xs (snd res)