(****************************************************)
(* ocaml-scribble - from AST to conversation graphs *)
(****************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

open Syntax

(* Nodes *)

type globalnode = int
type localnode = int


(* Global types *)

type globaltype =
    | GEnd of globalnode
    | GGoto of globalnode
    | GMsg of globalnode * message_sig * role * role * globaltype
    | GPar of globalnode * globaltype list * globaltype
    | GChoice of globalnode * role * globaltype list * globaltype
    | GJoin of globalnode * globaltype
    | GMerge of globalnode * globaltype


(* This function converts a global ast into a globaltype *)
let global_conversion (g:as_global_protocol_body) =

  (* Nodes are taken from a global Hashtbl instead of being syntactic\
   * It means that recursion scopes are not checked currently. But that
   * the *)
  let nref : globalnode ref = ref 0 in

  let create_node_name () : globalnode=
    let i = !nref in 
    let _ = incr nref in
    i
  in 

  let rec muelimination nodes (next:globalnode option) (gend:globaltype option) = 
    function
      | GASEnd -> 
          let n = match next with None -> create_node_name () | Some k -> k
          in
          (match gend with None -> GEnd n | Some g -> g)
      | GASMsg(info,(message_op,payload),role_1,role_2) -> 
          let n = match next with None -> create_node_name () | Some k -> k
          in
          let gend = (match gend with None -> GEnd (create_node_name ()) 
                        | Some g -> g)
          in
          GMsg (n,(message_op,payload),role_1,role_2,gend)          
      | GASSeq(global_protocol_1,global_protocol_2) -> 
          muelimination nodes next (Some (muelimination nodes None gend global_protocol_2)) global_protocol_1
      | GASChoice(info,role_at,global_protocol_list) -> 
          let n = match next with None -> create_node_name () | Some k -> k
          in
          let nm = create_node_name () in
          let gend = (match gend with None -> GEnd (create_node_name ()) 
                        | Some g -> g)
          in
          GChoice(n,role_at,
                  List.map (fun g -> muelimination nodes None (Some (GGoto nm)) g) global_protocol_list,
                  GMerge (nm,gend))
      | GASPar(info,global_protocol_list) -> 
          let n = match next with None -> create_node_name () | Some k -> k
          in
          let nj = create_node_name () in
          let gend = (match gend with None -> GEnd (create_node_name ()) 
                        | Some g -> g)
          in
          GPar(n,
               List.map (fun g -> muelimination nodes None (Some (GGoto nj)) g) global_protocol_list,
               GJoin (nj,gend))
      | GASRec(info,rec_point,global_protocol) -> 
          let n = match next with None -> create_node_name () | Some k -> k
          in
          muelimination ((rec_point,n)::nodes) (Some n) gend global_protocol
      | GASCont(info,rec_point) -> 
          (try 
             GGoto(List.assoc rec_point nodes)
           with 
               Not_found -> assert false (* The recursion variable is out
                                            of scope *)
          )
             
  in
  muelimination [] None None g

let globalnodeof = function
    | GEnd n -> n
    | GGoto n -> n
    | GMsg (n,(op,payload),r1,r2,g) -> n
    | GPar (n,lg,g) -> n
    | GChoice (n,r,lg,g) -> n
    | GJoin (n,g) -> n
    | GMerge (n,g) -> n


(* Local types *)


type localtype =
    | TEnd of localnode
    | TGoto of localnode
    | TSend of localnode * message_sig * role * localtype
    | TRecv of localnode * message_sig * role * localtype
    | TPar of localnode * localtype list * localtype
    | TChoice of localnode * role * localtype list * localtype
    | TNop of localnode * localtype
    | TJoin of localnode * localtype
    | TMerge of localnode * localtype

let localnodeof = function
    | TEnd n -> n
    | TGoto n -> n
    | TSend (n,(op,payload),r2,t) -> n
    | TRecv (n,(op,payload),r1,t) -> n
    | TPar (n,lt,t) -> n
    | TChoice (n,r,lt,t) -> n
    | TNop (n,t) -> n
    | TJoin (n,t) -> n
    | TMerge (n,t) -> n


(* This function converts a local ast into a localtype *)
let local_conversion (g:as_local_protocol_body) =

  (* Nodes are taken from a local Hashtbl instead of being syntactic\
   * It means that recursion scopes are not checked currently. But that
   * the *)
  let nref : localnode ref = ref 0 in

  let create_node_name () : localnode=
    let i = !nref in 
    let _ = incr nref in
    i
  in 

  let rec muelimination nodes (next:localnode option) (gend:localtype option) = 
    function
      | LASEnd -> 
          let n = match next with None -> create_node_name () | Some k -> k
          in
          (match gend with None -> TEnd n | Some g -> g)
      | LASSend(info,(message_op,payload),role) -> 
          let n = match next with None -> create_node_name () | Some k -> k
          in
          let gend = (match gend with None -> TEnd (create_node_name ()) 
                        | Some g -> g)
          in
          TSend (n,(message_op,payload),role,gend)      
      | LASRecv(info,(message_op,payload),role) -> 
          let n = match next with None -> create_node_name () | Some k -> k
          in
          let gend = (match gend with None -> TEnd (create_node_name ()) 
                        | Some g -> g)
          in
          TRecv (n,(message_op,payload),role,gend)          
      | LASSeq(local_protocol_1,local_protocol_2) -> 
          muelimination nodes next (Some (muelimination nodes None gend local_protocol_2)) local_protocol_1
      | LASChoice(info,role_at,local_protocol_list) -> 
          let n = match next with None -> create_node_name () | Some k -> k
          in
          let nm = create_node_name () in
          let gend = (match gend with None -> TEnd (create_node_name ()) 
                        | Some g -> g)
          in
          TChoice(n,role_at,
                  List.map (fun g -> muelimination nodes None (Some (TGoto nm)) g) local_protocol_list,
                  TMerge(nm,gend))
      | LASPar(info,local_protocol_list) -> 
          let n = match next with None -> create_node_name () | Some k -> k
          in
          let nj = create_node_name () in
          let gend = (match gend with None -> TEnd (create_node_name ()) 
                        | Some g -> g)
          in
          TPar(n,
               List.map (fun g -> muelimination nodes None (Some (TGoto nj)) g) local_protocol_list,
               TJoin(nj,gend))
      | LASRec(info,rec_point,local_protocol) -> 
          let n = match next with None -> create_node_name () | Some k -> k
          in
          muelimination ((rec_point,n)::nodes) (Some n) gend local_protocol
      | LASCont(info,rec_point) -> 
          (try 
             TGoto(List.assoc rec_point nodes)
           with 
               Not_found -> assert false (* The recursion variable is out
                                            of scope *)
          )
             
  in
  muelimination [] None None g



let localnodetoAST t = 
  let ii = Common.bogusInfo in
  let x n = "x" ^ (string_of_int n) in
  let rec check_rec nl = function
    | TEnd n -> []
    | TGoto n -> if List.mem n nl then [n] else []
    | TSend (n,(op,payload),r2,TEnd n') -> []
    | TSend (n,(op,payload),r2,t) -> 
      check_rec (n::nl) t
    | TRecv (n,(op,payload),r1,TEnd n') -> []
    | TRecv (n,(op,payload),r1,t) -> 
      check_rec (n::nl) t
    | TPar (n,lt,TEnd n') -> List.flatten (List.map (check_rec (n::nl)) lt)
    | TPar (n,lt,t) -> 
      List.flatten (List.map (check_rec (n::nl)) lt) @ (check_rec (n::nl) t)
    | TChoice (n,r,lt,TEnd n') -> 
      List.flatten (List.map (check_rec (n::nl)) lt)
    | TChoice (n,r,lt,t) -> 
      List.flatten (List.map (check_rec (n::nl)) lt) @ (check_rec (n::nl) t)
    | TNop (n,t) -> check_rec (n::nl) t
    | TJoin (n,t) -> check_rec (n::nl) t
    | TMerge (n,t) -> check_rec (n::nl) t
  in
  let rec aux nl tt = 
    match tt with
      | TEnd n -> LASEnd
      | TGoto n -> if List.mem n nl then LASCont(ii,x n) else LASEnd
      | TSend (n,(op,payload),r2,TEnd n') -> 
        LASSend (ii,(op,payload),r2)
      | TSend (n,(op,payload),r2,t) -> 
        if (List.mem n (check_rec [] tt))
        then LASRec(ii,x n,(LASSeq (LASSend (ii,(op,payload),r2),aux (n::nl) t)))
        else LASSeq (LASSend (ii,(op,payload),r2),aux (n::nl) t)
      | TRecv (n,(op,payload),r1,TEnd n') -> LASRecv (ii,(op,payload),r1)
      | TRecv (n,(op,payload),r1,t) ->
        if (List.mem n (check_rec [] tt))
        then LASRec(ii,x n,(LASSeq (LASRecv (ii,(op,payload),r1),aux (n::nl) t)))
        else LASSeq (LASRecv (ii,(op,payload),r1),aux (n::nl) t)
      | TPar (n,lt,TEnd n') -> 
        if (List.mem n (check_rec [] tt))
        then LASRec(ii,x n,(LASPar (ii,List.map (aux (n::nl)) lt)))
        else LASPar (ii,List.map (aux (n::nl)) lt)
      | TPar (n,lt,t) -> 
        if (List.mem n (check_rec [] tt))
        then LASRec(ii,x n,(LASSeq (LASPar (ii,List.map (aux (n::nl)) lt),aux (n::nl) t)))
        else LASSeq (LASPar (ii,List.map (aux (n::nl)) lt),aux (n::nl) t)
      | TChoice (n,r,lt,TEnd n') -> 
        if (List.mem n (check_rec [] tt))
        then LASRec(ii,x n,(LASChoice (ii,r,List.map (aux (n::nl)) lt)))
        else LASChoice (ii,r,List.map (aux (n::nl)) lt)
      | TChoice (n,r,lt,t) -> 
        if (List.mem n (check_rec [] tt))
        then LASRec(ii,x n,(LASSeq (LASChoice (ii,r,List.map (aux (n::nl)) lt),aux (n::nl) t)))
        else LASSeq (LASChoice (ii,r,List.map (aux (n::nl)) lt),aux (n::nl) t)
      | TNop (n,t) -> aux (n::nl) t
      | TJoin (n,t) -> 
        if (List.mem n (check_rec [] tt))
        then LASRec(ii,x n,aux (n::nl) t)
        else aux (n::nl) t
      | TMerge (n,t) -> 
        if (List.mem n (check_rec [] tt))
        then LASRec(ii,x n,aux (n::nl) t)
        else aux (n::nl) t
  in
  aux [] t
