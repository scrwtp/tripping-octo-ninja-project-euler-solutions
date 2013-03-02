let toDigits number =
    if number = 0I then [0I] else
        let rec inner number acc = 
            match number with
            | num when num = 0I -> acc
            | num ->  let res, rem = num / 10I, abs (num % 10I)
                      inner res (rem::acc)
        inner number []

let sumDigits number =
    number 
    |> toDigits
    |> List.reduce (+)

let problem16 () =
    sumDigits (pown 2I 1000)