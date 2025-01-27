module ResultMonad = struct
  type ('a, 'b) t = 'a -> 'b

  let ( let* ) m f = Result.bind m f

  let ( let+ ) m f = Result.map f m

  let return = Result.ok

  let error = Result.error
end

module Result = struct
  include Result

  let unwrap = function Ok x -> x | Error (`Msg s) -> failwith s
end

module List = struct
  include List

  let zip (l1 : 'a list) (l2 : 'b list) : (('a * 'b) list, 'e) result =
    let rec loop l1 l2 acc =
      match (l1, l2) with
      | [], [] ->
          Result.ok (List.rev acc)
      | [], _ | _, [] ->
          Result.error (`Msg "Lists have different lengths")
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
end
