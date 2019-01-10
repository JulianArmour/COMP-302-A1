let mysqrt (x:float) =
    let rec aux g x =
        let is_close = close ((square g),x) in
        match is_close with
        | true -> g
        | false -> aux ((g +. x /. g) /. 2.0) x
    in
    aux 1.0 x
