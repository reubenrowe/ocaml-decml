open Decml
open   Model

module M : sig end = struct
(* model m x = 1.0[@pc] * x + 0.0[@pc] *)
let m =
  let open Overlay.Pervasives in
  abs 
    (app 
      (app 
        (tx Weak (+.)) 
        (app 
          (app 
            (tx Weak ( *. )) 
            (tx Weak (pc ~init:"1.0" ()))) 
          (var))) 
      (tx Weak (pc ~init:"0.0" ())))

let m =
  let open Overlay.Pervasives in
  abs 
    (app 
      (app 
        (tx Weak (+.)) 
        (app (app (tx Weak ( *. )) (tx Weak [%pc 1])) (var)))
      (tx Weak [%pc 0]))

let m =
  let%pc a = 1 
     and b = 0 in
  let open Overlay.Pervasives in
  abs 
    (app 
      (app 
        (tx Weak (+.)) 
        (app (app (tx Weak ( *. )) (tx Weak a)) (var))) 
      (tx Weak b))
      
let m =
  let%pc a = 1 in
  let%pc b = 0 in
  let open Overlay.Pervasives in
  abs 
    (app 
      (app 
        (tx Weak (+.)) 
        (app (app (tx Weak ( *. )) (tx Weak a)) (var))) 
      (tx Weak b))

let m =
  let%pc a = 1 in
  let%pc b = 0 in
  [%model fun x -> a *. x +. b]
      
(* model m' x = (m x, m x + 1.0) *)
let m' = 
  let open Overlay.Pervasives in
    abs
      (pair
        ((app (tx Weak m) (var)),
        (app 
          (app (tx Weak (+.)) (app (tx Weak m) (var)))
          (tx Weak (Ex (Lifted 1.0, Parameters.null))))))

let m' = 
  let open Overlay.Pervasives in
    abs
      (pair
        ((app (tx Weak m) (var)),
        (app 
          (app (tx Weak (+.)) (app (tx Weak m) (var)))
          (tx Weak [%lift 1.0]))))
          
let m' = 
  let%lift c = 1.0 in
  let open Overlay.Pervasives in
    abs
      (pair
        ((app (tx Weak m) (var)),
        (app (app (tx Weak (+.)) (app (tx Weak m) (var))) (tx Weak c))))
          
(* The following is statically disallowed - we decouple the two models from
   their parameter vectors, and then try to rebind each one with the other's
   parameter vector. *)

(* 
let m, m' =
  let Ex (m, p) = m in
  let Ex (m', p') = m' in
  let m = rebind m p' in
    (* This expression [p'] has type
         $Ex_'c1 Lib.Model.Parameters.t = $Ex_'c1 Lib__Model.Parameters.t
       but an expression was expected of type
         $Ex_'c Lib.Model.Parameters.t = $Ex_'c Lib__Model.Parameters.t
       Type $Ex_'c1 is not compatible with type $Ex_'c *)
  let m' = rebind m' p in
    (* This expression [p] has type
         $Ex_'c Lib.Model.Parameters.t = $Ex_'c Lib__Model.Parameters.t
       but an expression was expected of type
         $Ex_'c1 Lib.Model.Parameters.t = $Ex_'c1 Lib__Model.Parameters.t
       Type $Ex_'c is not compatible with type $Ex_'c1*)
  m, m'
*)

(* In this case, no runtime error would occur in doing this since the two models
   use exactly the same keys to identify the parameters in the underlying 
   implementation.

   However, in general this will not be the case and so we should not allow it.
   For example, the following model is defined as m' above, but makes its
   constant a parameter. Thus, if we were to be able to decouple this model and
   then rebind it with the parameters of the model m defined above, there would
   be a runtime error when the resulting function is applied since this would
   try to look up a value for the new provisional constant within a parameter
   vector that does not contain it. *)

(* model m' x = (m x, m x + 1.0[@pc]) *)
let m' = 
  let open Overlay.Pervasives in
    abs
      (pair
        ((app (tx Weak m) (var)),
        (app 
          (app (tx Weak (+.)) (app (tx Weak m) (var)))
          (tx Weak (pc ~init:"1.0" ())))))

let m' = 
  let open Overlay.Pervasives in
    abs
      (pair
        ((app (tx Weak m) (var)),
        (app 
          (app (tx Weak (+.)) (app (tx Weak m) (var)))
          (tx Weak [%pc 1]))))

let m' =
  let%pc a = 1 in
  [%model fun x -> (m x, m x +. a)]
          
let f, g =
  let Ex (m, p) = m in
  let Ex (m', p') = m' in
  rebind m p, rebind m' p'

let f, g =
  let%decouple (m, p) = m in
  let%decouple (m', p') = m' in
  rebind m p, rebind m' p' 
  
(* Note that although we support the translation of multiple decoupling
    bindings of the form:
    
      let%decouple (m1, p1) = ...
        and ...
        and (m_n, p_n) = ... in
      
    the compiler gives an "Unexpected existential" error for the
    desugaring:
    
      let Decml.Model.Ex (m1, p1) = ...
        and ...
        and Decml.Model.Ex (m_n, p_n) = ... in ...
    
    as documented in the following bug on Mantis:

      https://caml.inria.fr/mantis/view.php?id=6014
    
    We set the locations so merlin can associate the error with the
    sugared patterns. *)

(* 
let f, g =
  let Ex (m, p) = m
  and Ex (m', p') = m' in
  rebind m p, rebind m' p'
    
let f, g =
  let%decouple 
      (m, p) = m
  and (m', p') = m' in
  rebind m p, rebind m' p' 
*)

let m = 
  let%model
      m x = x
  and n y = y in
  abs (pair ((app (tx Weak m) var), (app (tx Weak n) var)))

let m = 
  let%model m x = x in
  let%model n y = y in
  abs (pair ((app (tx Weak m) var), (app (tx Weak n) var)))

let m = 
  let%model m x = x in
  let%model n y = y in
  [%model fun x -> (m x, n x)]

let m = [%model fun x y -> x ]
let m = [%model fun x y -> y x ]

let m = [%model 
  fun x -> 
    let c1 = 1
    and c2 = 2 
    and c3 = 3 in
    ((x c1, x c2), c3)]

let m = [%model let rec m x = m (x + 1) in m ]

;;

let x = f 2.0 in
print_endline
  (Format.sprintf "%f" x)
;;

let x, y = g 2.0 in
print_endline
  (Format.sprintf "%f, %f" x y)
;;

end