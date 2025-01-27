module Delta = struct
  module T = Set.Make (String)

  type t = T.t

  let initial = T.empty

  let add = T.add

  let union = T.union

  let mem = T.mem
end
