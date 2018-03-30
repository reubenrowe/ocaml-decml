open Decml
open   Model

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
          
let m, m' =
  let Ex (m, p) = m in
  let Ex (m', p') = m' in
  let m = rebind m p in
  let m' = rebind m' p' in
  m, m'

;;

let x = m 2.0 in
print_endline
  (Format.sprintf "%f" x)
;;

let x, y = m' 2.0 in
print_endline
  (Format.sprintf "%f, %f" x y)
;;