open System
open System.IO

let tryFindIndex predicate (array:'a array) =
    let rec tryFindIndexImpl index =
        if predicate array.[index] then
            Some index
        else if index + 1 = Array.length array then
            None
        else
            index + 1 |> tryFindIndexImpl

    tryFindIndexImpl 0

let filterUntil limitIndex predicate  (array:'a array) =
    let mutable results = List.empty<'a>
    let rec filterUntilImpl index =
        let a = array.[index]
        if index = limitIndex then
            results
        else
            if predicate a then
                results <- a::results
            index + 1 |> filterUntilImpl
    results


let sortedArrayFolder (sortedArray,results) n1 =
    match tryFindIndex (fun k -> k > (2020 - n1) && 2020 - n1 >= 0) sortedArray with
    | None -> results
    | Some index1 ->
        for i = 0 to index1 - 1 do
            let n2 = sortedArray.[i]
            match tryFindIndex (fun k -> k > (2020 - n1 - n2) && 2020 - n1 - n2 >= 0) sortedArray with
            | None -> results
            | Some index2 ->
                filterUntil index2 (fun k -> n1 + n2 + k = 2020) sortedArray
                |> List.map (fun n3 -> (n1, n2, n3))
                |> List.append results




File.ReadAllLines("01.input")
|> Array.map Int64.Parse
|> Array.sort
|> Array.fold

