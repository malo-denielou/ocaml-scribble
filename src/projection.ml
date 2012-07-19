(********************************************************************)
(* ocaml-scribble - Projection from global to local types           *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

open Common
open Conversation

let debug_project = debug "Projection"

let project role g =

  debug_project (Printf.sprintf "Projecting for %s" role);
  let nref = ref 0 in
  let create_node_name ()= let i = !nref in let _ = incr nref in i in
  let ndb = ref [] in
  let add_node n =
    if List.mem_assoc n !ndb
    then (List.assoc n !ndb,n)
    else 
      let i = create_node_name () in
      let () = ndb := (n,i)::!ndb in
      (i,n)
  in
  (*
  let next (i,n) =
    let ii = create_node_name () in
    let () = ndb := (n,ii)::!ndb in
    (ii,n) in
  *)
  let rec aux = function
    | GEnd n -> let k = add_node n in TEnd (fst k)
    | GGoto n -> let k = add_node n in TGoto (fst k)
    | GMsg (n,m,s,r,g) -> 
        let k = add_node n in
        if s = role && r = role
        then TSend(fst k,m,r,TRecv(fst k,m,s,aux g))
        else
        if s = role
        then TSend(fst k,m,r,aux g)
        else if r = role
        then TRecv(fst k,m,s,aux g)
        else TNop (fst k,aux g)
    | GPar (n,lg,g) -> 
        let k = add_node n in 
        TPar(fst k,(List.map aux lg),aux g)
    | GChoice (n,r,lg,g) -> 
        let k = add_node n in 
        TChoice (fst k,r,(List.map aux lg),aux g)
    | GJoin (n,g) -> let k = add_node n in TJoin(fst k,aux g)
    | GMerge (n,g) -> let k = add_node n in TMerge(fst k,aux g)
    | GInterrupt (n,g,lm) -> let k = add_node n in TInterrupt(fst k,aux g,lm)
  in aux g


let clean_local_role t = 
  let rec clean_nop subst = function 
    | TEnd n -> TEnd (re_alias subst n) 
    | TGoto n -> TGoto (re_alias subst n)
    | TSend (n,msg,r,t) -> TSend (re_alias subst n,msg,r,clean_nop subst t)
    | TRecv (n,msg,r,t) -> TRecv (re_alias subst n,msg,r,clean_nop subst t) 
    | TPar (n,tl,t) -> TPar (re_alias subst n,
                             List.map (function t -> clean_nop subst t) tl,
                             clean_nop subst t)
    | TChoice (n,r,tl,t) -> TChoice (re_alias subst n,r,
                                     List.map (function t -> clean_nop subst t) tl,
                                     clean_nop subst t)
    | TNop (n,t) -> clean_nop ((re_alias subst n,localnodeof t)::subst) t
    | TJoin (n,t) -> TJoin (re_alias subst n,clean_nop subst t)
    | TMerge (n,t) -> TMerge (re_alias subst n,clean_nop subst t)
    | TInterrupt (n,t,lm) -> TInterrupt (re_alias subst n,clean_nop subst t,lm)
  in
  let tc = clean_nop [] t in
  let rec aux subst = function
    | TEnd n -> TEnd (re_alias subst n)
    | TGoto n -> TGoto (re_alias subst n)
    | TSend (n,msg,r,t) -> TSend (re_alias subst n,msg,r,aux subst t)
    | TRecv (n,msg,r,t) -> TRecv (re_alias subst n,msg,r,aux subst t) 
    | TPar (n,tl,TJoin (o,t)) when (List.for_all (function t -> t=TGoto o)tl)
        -> aux ((re_alias subst n,localnodeof t)::subst) t
    | TPar (n,tl,TJoin (o,t)) -> TPar (re_alias subst n,
                                       List.map (function t -> aux subst t) 
                                         (List.filter (function t ->
                                           t<>TGoto o) tl),
                                       TJoin (o,aux subst t))
    | TPar (n,[t],tt) -> assert false 
    (* do by case analysis to push things forward *)
    | TPar (n,tl,t) -> TPar (re_alias subst n,
                             List.map (function t -> aux subst t) tl,
                             aux subst t)
    | TChoice (n,r,tl,TMerge (o,t)) when (List.for_all (function t -> t=TGoto o)tl)
        -> aux ((re_alias subst n,localnodeof t)::subst) t
    | TChoice (n,r,tl,t) -> TChoice (re_alias subst n,r,
                                     List.map (function t -> aux subst t) tl,
                                     aux subst t)
    | TNop (n,t) -> assert false (* Should have disappeared by now *)
    | TJoin (n,t) -> TJoin (re_alias subst n,aux subst t)
    | TMerge (n,t) -> TMerge (re_alias subst n,aux subst t)
    | TInterrupt (n,t,lm) -> TInterrupt (re_alias subst n,aux subst t,lm)
      
  in aux [] tc
      

let tidy_local_role t =

  ()
