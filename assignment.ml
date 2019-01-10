let mysqrt (x:float) =
    let rec aux g x =
        let is_close = close ((square g),x) in
        match is_close with
        | true -> g
        | false -> aux ((g +. x /. g) /. 2.0) x
    in
    aux 1.0 x

let g'_cube g x = (2.0 *. g +. x /. (g ** 2.0)) /. 3.0

let cube_root (x:float) =
    let rec aux g x =
        let is_close = close ((cube g),x) in
        match is_close with
        | true -> g
        | false -> aux (g'_cube g x) x
    in
    aux 1.0 x
