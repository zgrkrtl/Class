open Printf
open Terms
open Subst

let rec beta(t: term): term =
  match t with
  | App(Lambda(y, t1), t2) -> subst t1 y t2
  | App(t1, t2)            -> App(beta t1, beta t2)
  | Lambda(y, t1)          -> Lambda(y, beta t1)
  | _                      -> t

let rec refl_trans_beta(t: term): term =
  let t' = beta t in
  if t = t' then t else refl_trans_beta t'
