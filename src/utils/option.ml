include Stdlib.Option

let map f = function None -> None | Some x -> Some (f x)

let map_default f default = function None -> default | Some x -> f x
