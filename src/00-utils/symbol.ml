module type S = sig
  type t

  val compare : t -> t -> int
  val fresh : string -> t
  val refresh : t -> t
  val print : t -> Format.formatter -> unit
end

module Make () : S = struct
  type t = int * string

  let compare (n1, _) (n2, _) = Int.compare n1 n2
  let count = ref (-1)

  let fresh ann =
    incr count;
    (!count, ann)

  let refresh (_, ann) = fresh ann
  let print (_n, ann) ppf = Format.fprintf ppf "%s" ann
end

let rec subscript i =
  let last =
    List.nth
      [
        "\226\130\128";
        "\226\130\129";
        "\226\130\130";
        "\226\130\131";
        "\226\130\132";
        "\226\130\133";
        "\226\130\134";
        "\226\130\135";
        "\226\130\136";
        "\226\130\137";
      ]
      (i mod 10)
  in
  if i < 10 then last else subscript (i / 10) ^ last

let greek_letters =
  [|
    "α";
    "β";
    "γ";
    "δ";
    "ε";
    "ζ";
    "η";
    "θ";
    "ι";
    "κ";
    "λ";
    "μ";
    "ν";
    "ξ";
    "ο";
    "π";
    "ρ";
    "σ";
    "τ";
  |]

let type_symbol n =
  if n < Array.length greek_letters then greek_letters.(n)
  else "τ" ^ subscript (n - Array.length greek_letters)
