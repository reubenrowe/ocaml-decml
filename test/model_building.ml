open Decml
open   Model

(* We can define models using a [%model ... ] extension, within which we can
   nest [%pc ... ] extensions to specify model parameters. The parameters take
   the initial value specified in the [%pc ... ] extension. *)

(* The PPX automatically opens the Decml.Overlay.Pervasives module which
   contains lifted versions of the infix operators found in the standard library
   Pervasives module. This means that we can use the operators we expect without
   having to lift them first. *)
let m = [%model fun x -> ([%pc 1.0] *. x) +. [%pc 0.0] ]

(* In fact, we don't have to use floating point constants in the [%pc ... ]
   extensions; if we use an integer constant, this will automatically be
   converted to a floating point value.  *)
let m = [%model fun x -> ([%pc 1] *. x) +. [%pc 0] ]

(* Furthermore, if we don't specify any initial value, a default one will be 
   used, namely 0.0. The default initial value can be set using the function
   [Decml.Model.default_constant]. *)
let m = [%model fun x -> ([%pc 1] *. x) +. [%pc] ]

(* We can use an inline extension syntax for models *)
let%model m x = ([%pc 1] *. x) +. [%pc]

(* We can use let bindings within models, e.g. for provisional constants. *)
let%model m x =
  let a = [%pc 1] in
  let b = [%pc] in
  (a *. x) +. b

(* We can also inline the provisional constant extensions, but then we have to
   specify a initial value. *)
let%model m x =
  let%pc a = 1 in
  let%pc b = 0 in
  (a *. x) +. b

(* We can lift arbitrary expressions using the [%lift ... ] extension. Note that
   the expression is evaluated, and the resulting value lifted to be a model. *)
let avg = 
  let xs = [1;2;3;4] in 
  [%lift (List.fold_left (+) 0 xs) / (List.length xs) ]
(* Note that we should not place a let-binding within a [%lift ... ] extension
   because the PPX will only lift the let-bound expression. *)
(* However we can use let-bindings within inline %lift extensions. *)
let%lift avg =
  let xs = [1;2;3;4] in
  (List.fold_left (+) 0 xs) / (List.length xs)

(* The lift extension can be used inside models. *)
let%model m x =
  let c = [%lift 3.0] in
  ([%pc 1] *. x) +. c

let%model m x =
  let%lift c = 3.0 in
  ([%pc 1] *. x) +. c

(* Any constant expression within a model definition will automatically be
   lifted. *)
let%model m x = ([%pc 1] *. x) +. 3.0

(* Model extensions can be nested within one another. *)
let%model m x =
  let%model inner_m y = ([%pc 1] *. y) +. [%pc] in
  inner_m x, inner_m x +. 1.0
(* However, it isn't necessary to explicitly specify that nested expression 
   are models - they are interpreted as such anyway! *)
let%model m x =
  let inner_m y = ([%pc 1] *. y) +. [%pc] in
  inner_m x, inner_m x +. 1.0
 
(* Models can refer to identifiers in scope at their point of definition, and
   these will automatically be assumed to be bound to models. *)
let%pc a = 1
let%pc b = 0
let%model m x = (a *. x) +. b

let%model m x = ([%pc 1] *. x) +. [%pc]
let%model m' x = 
  m x, m x +. 1.0

(* We can have conditional branching in model definitions. *)
let%model abs x =
  if x < 0.0 then x *. -1.0 else x

(* We can use constructors from the standard library. *)
let%model bools = true, false
let%model int_list = [1;2;3;4]

(* We can acutally use arbitrary constructors that are in scope. *)
type 'a expr =
  | Const of 'a
  | Var of string
  | Add of 'a expr * 'a expr
  | Mult of 'a expr * 'a expr

let%model expr = Mult (Const 3, Add(Var "x", Const 1))

(* We can even define models recursively. *)
let%model rec fact x =
  if x <= 0.0 then [%pc 1] else x *. fact (x -. 1.0)

let%model fact x = 
  let rec fact x acc =
    if x <= 0.0 then acc else fact (x -. 1.0) (x *. acc)
     in
  fact x [%pc 1]

(* We can decouple models using the %decouple inline extension. *)
let optimised_model =
  let%decouple 
    (parameterised_model, params) = 
      [%model fun x -> ([%pc 1] *. x) +. [%pc] ] in
  (* Do some optimisation of the parameters here *)
  (* We can then rebind the parameterised model to the optimised parameters to
     obtain a vanilla OCaml function that implements the optimised model. *)
  rebind parameterised_model params

(* This last example might look neater if we let-bind the model definition. *)
let optimised_model =
  let%model m x = ([%pc 1] *. x) +. [%pc] in
  let%decouple (parameterised_model, params) = m in
  (* Optimise the parameters *)
  rebind parameterised_model params

(* We do not (yet) allow decoupling within model definitions. *)
(* let%model m =
  let m x = x *. [%pc 1] in
  let%decouple (m, p) = m in
  fun x -> x *)
(* Error: Decoupling not supported within models! *)

(* The abstraction of (parameterised) models in the DecML library uses 
   GADTS and existentially quantified type variables to ensure that models can
   only be rebound to parameters from which they have been decoupled (possibly
   after some sequence of operations have been carried out on them). *)
   
(* When a model is decoupled from its parameters, we obtain a parameterised
   model and a parameter vector, both of which have types containing the same
   freshly created existentially quantified polymorphic type variable. *)

(* Thus the following is statically disallowed - we decouple the two models from
   their parameter vectors, and then try to rebind each one with the other's
   parameter vector. *)

(* 
let m, m' =
  let%model m x = ([%pc 1] *. x) +. 2.0 in
  let%model m' x = ([%pc 1] *. x) +. [%pc] in
  let%decouple (m, p) = m in
  let%decouple (m', p') = m' in
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

(* If we were to allow this, a runtime error would occur since the underlying
   implementations of the two models use different keys to identify the 
   parameters. *)

(* However, there is no such thing as a free lunch and the price we pay for 
   these for these static, compile-time checks is that we cannot create fully
   polymorphic models. This has to do with the fact that we create models by
   applying combinator functions, and so OCaml's value restriction prevents the
   polymorphic type variables that are inferred during type checking to be fully
   generalised. *)

(* This is OK inasmuch as we can still use these functions internally within a
   module as long as we do not export them to the top level. *)

module M : sig 
  val m : (unit, Carrier.t -> Carrier.t) Model.t
  val m' : (unit, Carrier.t -> Carrier.t) Model.t
end = struct

  (* This model has type (unit, '_a -> '_b -> '_a) Decml.Model.t *)
  let%model m x y = x

  (* This model has type (unit, '_a -> ('_a -> '_b) -> '_b) Decml.Model.t *)
  let%model m' x y = y x

  (* We can use both of these in building other models that do not contain type
     variables that cannot be generalised, e.g. models with ground types. *)
  let%model m x = 
    m (x *. [%pc]) 2.0
  let%model m' x =
    let m'' y = y *. 2.0 in
    (m' [%pc 2] m'') +. x

end

(* We can also recouple parameterised models with their parameter vectors, and
   then use the resulting model to build new models. This allows subsets of the
   parameters to be optimised in order to provide better initial conditions for
   global optimisation. *)

let global_optimal =
  let sub1 =
    let%model m x = [%pc 1] *. x +. [%pc] in
    let%decouple (m, p) = m in
    (* Optimse one set of parameters *)
    Model.Ex (m, p) in
  let sub2 =
    let%model m x = [%pc 1] *. x +. [%pc] in
    let%decouple (m, p) = m in
    (* Optimse other set of parameters *)
    Model.Ex (m, p) in
  (* Build overall model *)
  let%model m x = (sub1 x, sub2 x +. [%pc]) in
  let%decouple (m, p) = m in
  (* Optimise parameters of global model with initial values obtained by 
     previous optimisations above *)
  rebind m p
  

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
