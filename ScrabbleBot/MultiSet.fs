// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32>

    let empty = R (Map.empty)

    let isEmpty (ms : MultiSet<'a>) = 
        match ms with
        | R map -> Map.isEmpty map

    let size (R (map)) = Map.fold (fun acc _ v -> v+acc ) 0u map
    
    let contains (item : 'a) (ms : MultiSet<'a>) = 
        match ms with
        | R map -> Map.containsKey item map

    let numItems (item : 'a) (ms : MultiSet<'a>) = 
        match ms with
        | _ when not (contains item ms) -> 0u
        | R map -> map.Item item

    let add (item : 'a) (n : uint32) (ms : MultiSet<'a>) : MultiSet<'a> = 
        match ms with
        | R map -> R (Map.add item (n+(numItems item ms)) map)

    let addSingle (item : 'a) (ms : MultiSet<'a>) : MultiSet<'a> = add item 1u ms
    
    let remove (item : 'a) (n : uint32) (ms  : MultiSet<'a>) : MultiSet<'a> = 
        match ms with
        | R map when not (contains item ms) -> ms
        | R map when numItems item ms <= n -> R (Map.remove item map)
        | R map -> R (Map.add item ((numItems item ms)-n) map)

    let removeSingle (item : 'a) (ms : MultiSet<'a>) : MultiSet<'a> = remove item 1u ms


    let fold (f : 'b -> 'a -> uint32 -> 'b) (x : 'b) (ms : MultiSet<'a>) = 
        match ms with
        | R map -> Map.fold f x map

    let foldBack (f : 'a -> uint32 -> 'b -> 'b) (ms : MultiSet<'a>) (x : 'b) = 
        match ms with
        | R map -> Map.foldBack f map x
    
    let rec ofList (lst : 'a list) : MultiSet<'a> = 
        match lst with
        | [] -> empty
        | x :: xs -> addSingle x (ofList xs)

    let toList (ms : MultiSet<'a>) : 'a list =
        foldBack (fun item count acc -> List.replicate (int count) item @ acc) ms []
