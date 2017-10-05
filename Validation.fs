type ISBN13 = private ISBN13 of string

let try' x = function 
    | true -> Some x 
    | false -> None

let (|NullOrEmpty|_|) = 
    String.IsNullOrEmpty 
    >> try' NullOrEmpty

let (|StringLength|_|) f s = 
    f (String.length s) 
    |> try' StringLength

let (|Matches|_|) pattern s = 
    Regex.IsMatch (pattern, s, RegexOptions.Compiled) 
    |> try' Matches

let flip f x y = f y x

let (|CheckSum|_|) s =
    let multiplyEvenWith3 i x = if i % 2 <> 0 then x * 3 else x
    let substract10IfNotZero x = if x <> 0 then 10 - x else x
    let numbers = [ for c in s -> c |> string |> int ]

    numbers
    |> List.take 12
    |> List.mapi multiplyEvenWith3
    |> List.sum
    |> flip (%) 10
    |> substract10IfNotZero
    |> (<>) <| List.last numbers
    |> try' CheckSum

let isbn13 = function
    | NullOrEmpty 
    | StringLength ((<>) 13)
    | Matches "[0-9]{13}" 
    | CheckSum -> None
    | s -> ISBN13 s |> Some

let isValidIsbn (s : string) =
    let numbers = 
        s.ToCharArray() 
        |> Array.map (string >> int) 
        |> List.ofArray
    numbers
    |> List.take 12
    |> List.zip [ 1; 3; 1; 3; 1; 3; 1; 3; 1; 3; 1; 3; ]
    |> List.fold (fun result (x, y) -> result + (x * y)) 0
    |> fun x -> x % 10
    |> fun x -> if x <> 0 then 10 - x else x
    |> (=) <| List.last numbers 

[<Property>]
let ``ISBN13 gets created if checksum is valid`` () =
    Gen.elements [ 0..9 ]
    |> Gen.listOfLength 13
    |> Gen.map (List.map string >> List.reduce (+))
    |> Gen.filter isValidIsbn
    |> Arb.fromGen
    |> Prop.forAll <| fun x ->
        Some <| ISBN13 x = isbn13 x

type String50 = private String50 of string

let string50 = function
    | NullOrEmpty
    | StringLength ((<>) 50) -> None
    | x -> String50 x |> Some

[<Property>]
let ``String50 don't gets created if length isn't 50 chars`` () =
    Arb.generate<string>
    |> Gen.filter (String.length >> (<>) 50)
    |> Arb.fromGen
    |> Prop.forAll <| fun x ->
        None = string50 x

[<Property>]
let ``String50 gets created if length is 50 chars`` () =
    Arb.generate<char>
    |> Gen.listOfLength 50
    |> Gen.map (List.map string >> List.reduce (+))
    |> Arb.fromGen
    |> Prop.forAll <| fun x ->
        Some <| String50 x = string50 x

type PositiveInt = private PositiveInt of int

let posInt = function
    | x when x < 0 -> None
    | x -> Some <| PositiveInt x

[<Property>]
let ``PositiveInt don't gets created if value is negative`` () =
    Arb.generate<int>
    |> Gen.filter ((>) 0)
    |> Arb.fromGen
    |> Prop.forAll <| fun x -> 
        None = posInt x

[<Property>]
let ``PositiveInt gets created if value isn't negative`` (FsCheck.PositiveInt x) = 
    Some <| PositiveInt x = posInt x

type Book =
    { Author : String50
      ISBN13 : ISBN13
      Pages : PositiveInt }
