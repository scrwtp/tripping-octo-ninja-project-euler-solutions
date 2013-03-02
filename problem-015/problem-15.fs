#nowarn "40"
open System.Collections.Generic

let memoize f = 
    let cache = new Dictionary<_, _> ()
    (fun x -> 
        let success, v = cache.TryGetValue x
        if success then v else
            let v = f x
            cache.Add(x, v)
            v)

let rec move =
    memoize (fun (right, down) ->
        match right, down with
        | 0, 0 -> 1I
        | r, d when r < 0 || d < 0 -> 0I
        | r, d -> move (r-1, d) + move (r, d-1))
		
let problem15 = 
	move (20, 20)