(********************************************************************)
(* ocaml-scribble - Well-formedness checks                          *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)


open Common
open Conversation

let debug_wf = debug "Well-formedness"
let fulldebug_wf = Common.fulldebug "Well-formedness"


(* Builds an index from nodes to global types *)

let rec index (gg:globaltype) = 
  match gg with
    | GEnd n -> [n,gg] 
    | GGoto n -> []
    | GMsg (n,(op,payload),r1,r2,g) -> 
      (n,gg)::(index g)
    | GPar (n,lg,g) -> 
      (n,gg)::(List.concat (List.map index lg))@(index g)
    | GChoice (n,r,lg,g) -> 
      (n,gg)::(List.concat (List.map index lg))@(index g)
    | GJoin (n,g) -> 
      (n,gg)::(index g)
    | GMerge (n,g) -> 
      (n,gg)::(index g)
    | GInterrupt (n,g,lm) -> 
      (n,gg)::(index g)
        


let nonnulprefix l m =
  match l,m with
    | x::q,y::r when x=y -> true
    | _ -> false


(* Checking the linearity property on a global type *)

(* Currently a problem with linearity when there is a join *)
let linearity (g:globaltype) =

  let idqg = index g in

  let rec get_msg ig seen jp = function
    | GEnd n -> []
    | GGoto n -> 
      if List.mem n seen 
      then [] 
      else get_msg ig (n::seen) jp (List.assoc n ig)
    | GMsg (n,(op,payload),r1,r2,g) -> 
      (((r1,r2),op),(jp,payload,g))::(get_msg ig (n::seen) jp g)
    | GPar (n,lg,g) -> 
      List.concat ((List.map (get_msg ig (n::seen) jp) lg))
    | GChoice (n,role,lg,g) -> 
      List.concat ((get_msg ig (n::seen) (jp) g)
                   ::(List.map (get_msg ig (n::seen) jp) lg))
    | GJoin (n,g) -> 
        get_msg ig (n::seen) (n::jp) g
    | GMerge (n,g) -> 
        get_msg ig (n::seen) (jp) g
    | GInterrupt (n,g,lm) -> 
        get_msg ig (n::seen) (jp) g (* Interrupt messages are ignored for
                                       linearity *)
  in
  
  let rec compat lm = function
    | [] -> true
    | (k,(jp,payload,g))::q -> if List.mem_assoc k lm 
      then (
        let jpp,payloadd,gg = List.assoc k lm in (* Pb: it should be all
                                                    occurrences of k? *)
        ((nonnulprefix jp jpp) ||
            let () = debug_wf (Printf.sprintf 
                                 "Linearity WARNING: ambiguous message label %s"
                                 (snd k) (*List.hd jp) (List.hd jpp*)) in
            payload=payloadd && g=gg)) && compat lm q
      (* Missing Linearity check within the same branch ie mu X.(A->B<alpha>||X)*)
      else compat lm q 
  in

  let rec compat_list gend lm = function
    | [] -> true
    | g::r ->     
      let lg = (get_msg idqg [globalnodeof gend] [] g) in
      (compat lm lg) && (compat_list gend lm r)
  in
  let rec check_list gend = function
    | [] -> true
    | [g] -> true
    | g::r -> 
      let lg = (get_msg idqg [globalnodeof gend] [] g) in
      (* Watch out: the trick of adding the join point as 'seen' only works
         when it is known in advance, not in the general case *)
      (compat_list gend lg r) &&
        (check_list gend r)
  in
  let rec check = function
    | GEnd (n) -> true
    | GGoto (n) -> true
    | GMsg (n,(message_op,payload),role_1,role_2,g) ->
      check g
    | GPar (n, global_list, g) ->
      (List.for_all (fun x -> x) (List.map check global_list)) &&
        (check g) &&
        (check_list g global_list)
    | GChoice (n, role, global_list, g) -> 
      (List.for_all (fun x -> x) (List.map check global_list)) &&
        (check g)
    | GJoin (n,g) -> 
        check g
    | GMerge (n,g) -> 
        check g
    | GInterrupt (n,g,lm) -> 
        check g
  in
  let result = check g in
  let () = debug_wf ("Linearity: "^(string_of_bool result)) in 
  result



let local_choice (g:globaltype) =
  let ig = index g in

  let rec get_sender ig seen rc = function
    | GEnd n -> []
    | GGoto n -> 
        if List.mem n seen 
        then [] 
        else get_sender ig (n::seen) rc (List.assoc n ig)
    | GMsg (n,m,s,r,g) -> 
        if List.mem s rc then 
          get_sender ig (n::seen) (r::rc) g
        else
          let () = fulldebug_wf (Printf.sprintf 
                               "Found a sender: at %i, sender %s" n
                               s) in
          s::(get_sender ig (n::seen) (s::r::rc) g)
    | GPar (n,gl,g) -> 
        (List.concat (List.map (get_sender ig (n::seen) rc) gl))
        @(get_sender ig (n::seen) rc g)
    | GChoice (n,role,gl,g) -> 
        (List.concat (List.map (get_sender ig (n::seen) rc) gl))
        @(get_sender ig (n::seen) rc g)
    | GJoin (n,g) -> 
        get_sender ig (n::seen) rc g
    | GMerge (n,g) -> 
        get_sender ig (n::seen) rc g
    | GInterrupt (n,g,lm) -> 
        get_sender ig (n::seen) rc g @ (List.map fst lm)
          (* The interrupters are considered senders *)
  in

  let rec get_receivers ig seen mp rc = function 
    | GEnd n -> []
    | GGoto n -> 
        if List.mem n seen 
        then [] 
        else get_receivers ig (n::seen) mp rc (List.assoc n ig)
    | GMsg (n,(op,p),s,r,g) -> 
        if List.mem r rc then 
          get_receivers ig (n::seen) mp rc g
        else
          let () = fulldebug_wf (Printf.sprintf 
                               "Found a receiver: at %i, receiver %s" n
                               r) in
          (r,(op,mp))::(get_receivers ig (n::seen) mp (s::r::rc) g)
    | GPar (n,gl,g) -> 
        (List.concat (List.map (get_receivers ig (n::seen) mp rc) gl))
        @(get_receivers ig (n::seen) mp rc g)
          (* missing the passing around of the list of merge points *)
    | GChoice (n,role,gl,g) -> 
        (List.concat (List.map (get_receivers ig (n::seen) mp rc) gl))
        @(get_receivers ig (n::seen) ((globalnodeof g)::mp) rc g)
    | GJoin (n,g) -> 
        get_receivers ig (n::seen) mp rc g
    | GMerge (n,g) -> 
        get_receivers ig (n::seen) (n::mp) rc g
    | GInterrupt (n,g,lm) -> (* interrupt is at top-level, so it does not
                                make sense to find the exact receivers *)
        get_receivers ig (n::seen) mp rc g 
  in


  (* Remove duplicates in a list *)
  let rec remove_duplicates l = 
    match  l with
        [] -> []
      | a::q -> if (List.mem a q) 
        then (remove_duplicates q) 
        else a::(remove_duplicates q)
  in

  let rec compat l m =
    match l with [] -> 
          let () = fulldebug_wf 
            (Printf.sprintf 
               "Same receivers detected: %s" (String.concat "," (List.map fst m) )) in
          true
      | (r,(l,mp))::q -> 
          (List.mem_assoc r m) && 
            (let ll,mpp=List.assoc r m in 
             if ll <> l then true
             else 
               let () = fulldebug_wf (Printf.sprintf 
                                    "Examining message %s received by %s ..."
                                    l r) in
               nonnulprefix mp mpp)
          && compat q m
  in

  let rec compat_list = function
    | [] | [_]-> true
    | h::q -> List.for_all (compat h) q
  in

  let rec compat_sender l m =
    match remove_duplicates l, remove_duplicates m with
      | [],_ | _,[] -> 
          let () = fulldebug_wf 
            (Printf.sprintf "Empty branch detected") in
          false
      | [r],[rr] when r=rr -> 
          let () = fulldebug_wf 
            (Printf.sprintf 
               "Unique sender detected: %s" (r) ) in
          true
      | [r],[rr] -> 
          let () = fulldebug_wf 
            (Printf.sprintf 
               "Different senders in different branches %s and %s" (r) (rr) ) in
          false
      | r::s::_,_ | _,r::s::_ -> 
          let () = fulldebug_wf 
            (Printf.sprintf "%s and %s are both active senders in the same branch"
               (r) (s) ) in
          false
  in
  let rec compat_sender_list = function
    | [] | [_]-> true
    | h::q -> List.for_all (compat_sender h) q
  in

  let rec check = function
    | GEnd n -> true
    | GGoto n -> true
    | GMsg (n,(op,p),s,r,g) -> 
         (check g)
    | GPar (n,gl,g) -> 
        (List.for_all check gl) && (check g)
    | GChoice (n,r,gl,g) -> (check g) &&
        (compat_list (List.map (function c -> get_receivers ig [] [] [] c) gl))
        && 
          (compat_sender_list 
             (List.map (function c -> (get_sender ig [] [] c)) gl))
    | GJoin (n,g) -> 
        check g
    | GMerge (n,g) -> 
        check g
    | GInterrupt (n,g,lm) -> 
        check g
  in
  let result = check g in
  let () = debug_wf ("Local choice: "^(string_of_bool result)) in
  result





let check (g:globaltype) =
  linearity g && local_choice g
