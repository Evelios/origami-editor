namespace Fold


type StringMap<'a when 'a: comparison>(items: ('a * string) list) =
    let itemIsKey = Map.ofList items

    let stringIsKey =
        Map.ofList (List.map (fun (a, b) -> (b, a)) items)

    member __.ToString key = Map.find key itemIsKey
    member __.TryToString key = Map.tryFind key itemIsKey
    member __.FromString key = Map.find key stringIsKey
    member __.TryFromString key = Map.tryFind key stringIsKey
