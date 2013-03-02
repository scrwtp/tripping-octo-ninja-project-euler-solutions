open System

let toDigits number =
    if number = 0 then [0] else
        let rec inner number acc = 
            match number with
            | num when num = 0 -> acc
            | num ->  let res, rem = num / 10, abs (num % 10)
                      inner res (rem::acc)
        inner number []

let breakup lst = 
    let rec inner lst acc = 
        match lst with
        | a::b::c::rest -> inner rest ([a; b; c]::acc)
        | a::b::[] -> inner [] ([a; b; 0]::acc)
        | a::[] -> inner [] ([a; 0; 0]::acc)
        | [] -> acc
    inner lst []        

let breakIntoSections digits = 
    digits
    |> List.rev 
    |> breakup
    |> List.map (fun lst -> List.rev lst)

let prependIfNonEmpty prefix str = 
    if String.IsNullOrEmpty str then
        str
    else
        prefix + str

let spellOutOnes digit =
    match digit with
    | 0 -> String.Empty
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | _ -> failwith "error"

let spellOutTens group =
    match group with
    | [ 0; d ] -> spellOutOnes d
    | [ 1; 0 ] -> "ten"
    | [ 1; 1 ] -> "eleven"
    | [ 1; 2 ] -> "twelve"
    | [ 1; 3 ] -> "thirteen"
    | [ 1; 4 ] -> "fourteen"
    | [ 1; 5 ] -> "fifteen"
    | [ 1; 6 ] -> "sixteen"
    | [ 1; 7 ] -> "seventeen"
    | [ 1; 8 ] -> "eighteen"
    | [ 1; 9 ] -> "nineteen"
    | [ 2; d ] -> "twenty"      + prependIfNonEmpty "-" (spellOutOnes d)
    | [ 3; d ] -> "thirty"      + prependIfNonEmpty "-" (spellOutOnes d)
    | [ 4; d ] -> "forty"       + prependIfNonEmpty "-" (spellOutOnes d)
    | [ 5; d ] -> "fifty"       + prependIfNonEmpty "-" (spellOutOnes d)
    | [ 6; d ] -> "sixty"       + prependIfNonEmpty "-" (spellOutOnes d)
    | [ 7; d ] -> "seventy"     + prependIfNonEmpty "-" (spellOutOnes d)
    | [ 8; d ] -> "eighty"      + prependIfNonEmpty "-" (spellOutOnes d)
    | [ 9; d ] -> "ninety"      + prependIfNonEmpty "-" (spellOutOnes d)   
    | _ -> failwith "error"

let spellOutHundreds group = 
    match group with
    | 0::rest -> spellOutTens rest
    | h::rest -> sprintf "%s hundred%s" (spellOutOnes h) (prependIfNonEmpty " and " (spellOutTens rest))
    | _ -> failwith "error"

let spell number =
    let sections = 
        number
        |> toDigits
        |> breakIntoSections
        |> List.map (fun group -> spellOutHundreds group)
    match sections with
    | thousands::hundreds::[] -> sprintf "%s thousand%s" thousands (prependIfNonEmpty " " hundreds)
    | hundreds::[] -> hundreds
    | _ -> failwithf "number not supported: %d" number

let sumLetters str = 
    [ for c in str -> c]
    |> List.fold (fun acc c -> if c >= 'a' && c <= 'z' then acc + 1 else acc) 0

let spellRange low high = 
    [ low .. high ] 
    |> List.map spell 

let problem17 () = 
    spellRange 1 1000
    |> List.map (fun x -> sumLetters x)
    |> List.sum