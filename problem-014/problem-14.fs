//The following iterative sequence is defined for the set of positive integers:
//
//n  n/2 (n is even)
//n  3n + 1 (n is odd)
//
//Using the rule above and starting with 13, we generate the following sequence:
//
//13  40  20  10  5  16  8  4  2  1
//It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
//
//Which starting number, under one million, produces the longest chain?
//
//NOTE: Once the chain starts the terms are allowed to go above one million.

module Problem14

let collatz = function
    | n when n % 2L = 0L -> n/2L
    | n -> 3L*n + 1L

let expandCollatz start =
    let rec inner n acc =
        match n with
            | 1L -> List.rev (1L :: acc)
            | n -> inner (collatz n) (n :: acc)
    in inner start []

let getCollatz start = 
    let lst = expandCollatz start
    let len = List.length lst
    (len, lst)

let getLongestCollatz a b =
    let rec inner acc stop n = 
       match n with 
            | _ when n = a -> acc
            | _ ->
                let collatz = getCollatz n 
                let length = fst collatz
                let maxLength = fst acc
                match length with
                    | _ when length > maxLength -> inner collatz stop (n - 1L)
                    | _ -> inner acc stop (n - 1L)
    in inner (0, []) a b

let problem14 =
    getLongestCollatz 1L 1000000L


    
        