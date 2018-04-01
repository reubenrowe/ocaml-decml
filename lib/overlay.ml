open Model

module Pervasives = struct
  let ( +  ) = Ex (Lifted ( +  ), Parameters.null)
  let ( -  ) = Ex (Lifted ( -  ), Parameters.null)
  let ( *  ) = Ex (Lifted ( *  ), Parameters.null)
  let ( /  ) = Ex (Lifted ( /  ), Parameters.null)
  let ( +. ) = Ex (Lifted ( +. ), Parameters.null)
  let ( -. ) = Ex (Lifted ( -. ), Parameters.null)
  let ( *. ) = Ex (Lifted ( *. ), Parameters.null)
  let ( /. ) = Ex (Lifted ( /. ), Parameters.null)
  let fst = Ex (Lifted fst, Parameters.null)
  let snd = Ex (Lifted snd, Parameters.null)
end

module List = struct
  open List
  let empty = Ex (Lifted [], Parameters.null)
  let map = Ex (Lifted map, Parameters.null)
end
