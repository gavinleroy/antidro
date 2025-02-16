module ResultMonad = struct
  type ('a, 'b) t = 'a -> 'b

  let ( let* ) m f = Result.bind m f

  let ( let+ ) m f = Result.map f m

  let return = Result.ok
end

module OptionMonad = struct
  let ( let* ) m f = Option.bind m f

  let ( let+ ) m f = Option.map f m

  let return = Option.some

  let error = Option.none
end

module Error : sig
  type t

  val of_string : string -> t

  val msg : ('a, Format.formatter, unit, unit, unit, t) format6 -> 'a

  val pp : Format.formatter -> t -> unit
end = struct
  open Sexplib.Std

  type t = [`Msg of string] [@@deriving sexp_of, show]

  let of_string s = `Msg s

  let msg fmt = Format.kasprintf of_string fmt

  let pp fmt = function `Msg s -> Format.fprintf fmt "%s" s
end

module Format = struct
  include Format

  let pp_comma fmt () = fprintf fmt ",@ "

  let pp_list ?(pp_sep = pp_comma) fmt ppf ls = pp_print_list ~pp_sep fmt ppf ls
end

module Result = struct
  include Result

  let bind' f x = bind x f

  let unwrap = function Ok x -> x | Error _ -> assert false

  let value ~default = function Ok x -> x | Error _ -> default

  let error fmt = Format.kasprintf (fun s -> Error (Error.of_string s)) fmt
end

module List = struct
  include List

  let fst2 (ls : ('a * 'b * 'c) list) : ('a * 'b) list =
    List.map (fun (x, y, _) -> (x, y)) ls

  let thrice (ls : ('a * 'b * 'c) list) : 'a list * 'b list * 'c list =
    let rec loop ls acc1 acc2 acc3 =
      match ls with
      | [] ->
          (List.rev acc1, List.rev acc2, List.rev acc3)
      | (x, y, z) :: ls ->
          loop ls (x :: acc1) (y :: acc2) (z :: acc3)
    in
    loop ls [] [] []

  let of_four (ls : ('a * 'b * 'c * 'd) list) :
      'a list * 'b list * 'c list * 'd list =
    let rec loop ls acc1 acc2 acc3 acc4 =
      match ls with
      | [] ->
          (List.rev acc1, List.rev acc2, List.rev acc3, List.rev acc4)
      | (x, y, z, w) :: ls ->
          loop ls (x :: acc1) (y :: acc2) (z :: acc3) (w :: acc4)
    in
    loop ls [] [] [] []

  let zip (l1 : 'a list) (l2 : 'b list) : (('a * 'b) list, 'e) result =
    let rec loop l1 l2 acc =
      match (l1, l2) with
      | [], [] ->
          Result.ok (List.rev acc)
      | [], _ | _, [] ->
          Result.error "Lists have different lengths"
      | x :: xs, y :: ys ->
          loop xs ys ((x, y) :: acc)
    in
    loop l1 l2 []

  let equal_membership ~(equal : 'a -> 'a -> bool) ~(compare : 'a -> 'a -> int)
      (l1 : 'a list) (l2 : 'a list) : bool =
    List.for_all2 equal (List.sort compare l1) (List.sort compare l2)

  let all (f : 'a -> ('b, 'e) result) (ls : 'a list) : ('b list, 'e) result =
    let open ResultMonad in
    List.fold_left
      (fun acc x ->
        let* acc = acc in
        let+ x = f x in
        x :: acc )
      (return []) ls
    |> Result.map List.rev

  let every (f : 'a -> (unit, 'e) result) (ls : 'a list) : (unit, 'e) result =
    let open ResultMonad in
    let+ _ = all f ls in
    ()

  let every2 (f : 'a -> 'b -> (unit, 'e) result) (ls : 'a list) (ks : 'b list) :
      (unit, 'e) result =
    let open ResultMonad in
    let* zipped = zip ls ks in
    let+ _ = all (fun (a, b) -> f a b) zipped in
    ()

  let all2 (f : 'a -> 'b -> ('c, 'e) result) (ls : 'a list) (ks : 'b list) :
      ('c list, 'e) result =
    let open ResultMonad in
    List.fold_left2
      (fun acc x y ->
        let* acc = acc in
        let+ v = f x y in
        v :: acc )
      (return []) ls ks
    |> Result.map List.rev

  let accum (f : 'a -> 'acc -> ('c * 'acc, 'e) result) (acc : 'acc)
      (ls : 'a list) : ('c list * 'acc, 'e) result =
    let open ResultMonad in
    let+ ls, acc =
      List.fold_left
        (fun acc x ->
          let* acc, store = acc in
          let+ x, store = f x store in
          (x :: acc, store) )
        (Result.ok ([], acc))
        ls
    in
    (List.rev ls, acc)

  let interleave (l1 : 'a list) (l2 : 'a list) : ('a list, 'e) result =
    let rec loop acc l1 l2 =
      match (l1, l2) with
      | [], [] ->
          Result.ok (List.rev acc)
      | [], _ | _, [] ->
          Result.error "Lists have different lengths"
      | x :: xs, y :: ys ->
          loop (y :: x :: acc) xs ys
    in
    loop [] l1 l2

  let interleave_exn l1 l2 = interleave l1 l2 |> Result.unwrap

  let uninterleave (ls : 'a list) : ('a list * 'a list, 'e) result =
    let rec loop acc1 acc2 ls =
      match ls with
      | [] ->
          Result.ok (List.rev acc1, List.rev acc2)
      | _x :: [] ->
          Result.error "List has odd length"
      | x :: y :: ls ->
          loop (x :: acc1) (y :: acc2) ls
    in
    loop [] [] ls
end
