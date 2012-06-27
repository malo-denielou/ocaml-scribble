(********************************************************************)
(* ocaml-scribble - Pretty-printer                                  *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

open Format

let pp_hbox = pp_open_hbox str_formatter
let pp_vbox = pp_open_vbox str_formatter
let pp_hvbox = pp_open_hvbox str_formatter
let pp_hovbox = pp_open_hovbox str_formatter
let pp_box = pp_open_box str_formatter
let pp_close = pp_close_box str_formatter
let pp_string = pp_print_string str_formatter
let pp_int = pp_print_int str_formatter
let pp_break = pp_print_break str_formatter
let pp_space = pp_print_space str_formatter
let pp_cut = pp_print_cut str_formatter
let pp_flush = pp_print_flush str_formatter

let get_string = flush_str_formatter


(***************************************)
(* Pretty-printing for abstract syntax *)
(***************************************)

open Syntax

let rec print_global_protocol_body (g:as_global_protocol_body) =
  match g with
    | GASEnd -> ()
    | GASMsg(info,(message_op,payload),role_1,role_2) -> 
        pp_hbox ();
        pp_string message_op;
        pp_space ();
        pp_string ("("^payload^")");
        pp_space ();
        pp_string "from";
        pp_space ();
        pp_string role_1;
        pp_space ();
        pp_string "to";
        pp_space ();
        pp_string role_2;
        pp_string ";";
        pp_close ()
    | GASSeq(global_protocol_1,global_protocol_2) -> 
        print_global_protocol_body global_protocol_1;
        pp_break 2 0;
        print_global_protocol_body global_protocol_2;
    | GASChoice(info,role_at,global_protocol_list) -> 
        pp_vbox 0;
        pp_hbox ();
        pp_string "choice";
        pp_space ();
        pp_string "at";
        pp_space ();
        pp_string role_at;
        pp_space ();
        print_global_protocol_list "or" global_protocol_list;
    | GASPar(info,global_protocol_list) -> 
        pp_vbox 0;
        pp_hbox ();
        pp_string "par";
        pp_space ();
        print_global_protocol_list "and" global_protocol_list
    | GASRec(info,rec_point,global_protocol) -> 
        pp_vbox 0;
        pp_hbox ();
        pp_string "rec";
        pp_space ();
        pp_string rec_point;
        pp_space ();
        pp_string "{";
        pp_close ();
        pp_break 2 2;
        print_global_protocol_body global_protocol;
        pp_string "}";
        pp_close ();
    | GASCont(info,rec_point) -> 
        pp_hbox ();
        pp_string "continue";
        pp_space ();
        pp_string rec_point;
        pp_string ";";
        pp_close ()

and print_global_protocol_list sep global_list =
  
  aux_global_protocol_list sep global_list

and aux_global_protocol_list sep = function
  | [] -> assert false (* pp_close () *)
  | [g] -> 
      pp_string "{"; 
      pp_close ();
      pp_break 2 2;
      print_global_protocol_body g; 
      pp_close ();
      pp_string "}"
  | g::r -> 
      pp_string "{"; 
      pp_close ();
      pp_break  2 2;
      print_global_protocol_body g;
      pp_hbox ();
      pp_string "}";
      pp_space ();
      pp_string sep;
      pp_space ();
      aux_global_protocol_list sep r
    


let rec print_local_protocol_body (g:as_local_protocol_body) =
  match g with
    | LASEnd -> ()
    | LASSend(info,(message_op,payload),role) -> 
        pp_hbox ();
        pp_string message_op;
        pp_space ();
        pp_string ("("^payload^")");
        pp_space ();
        pp_string "to";
        pp_space ();
        pp_string role;
        pp_string ";";
        pp_close ()
    | LASRecv(info,(message_op,payload),role) -> 
        pp_hbox ();
        pp_string message_op;
        pp_space ();
        pp_string ("("^payload^")");
        pp_space ();
        pp_string "from";
        pp_space ();
        pp_string role;
        pp_string ";";
        pp_close ()
    | LASSeq(local_protocol_1,local_protocol_2) -> 
        print_local_protocol_body local_protocol_1;
        pp_break 2 2;
        print_local_protocol_body local_protocol_2;
    | LASChoice(info,role_at,local_protocol_list) -> 
        pp_vbox 0;
        pp_hbox ();
        pp_string "choice";
        pp_space ();
        pp_string "at";
        pp_space ();
        pp_string role_at;
        pp_space ();
        print_local_protocol_list "or" local_protocol_list;
    | LASPar(info,local_protocol_list) -> 
        pp_vbox 0;
        pp_hbox ();
        pp_string "par";
        pp_space ();
        print_local_protocol_list "and" local_protocol_list
    | LASRec(info,rec_point,local_protocol) -> 
        pp_vbox 0;
        pp_hbox ();
        pp_string "rec";
        pp_space ();
        pp_string rec_point;
        pp_space ();
        pp_string "{";
        pp_close ();
        pp_break 2 2;
        print_local_protocol_body local_protocol;
        pp_string "}";
        pp_close ();
    | LASCont(info,rec_point) -> 
        pp_hbox ();
        pp_string "continue";
        pp_space ();
        pp_string rec_point;
        pp_string ";";
        pp_close ()

and print_local_protocol_list sep local_list =
  
  aux_local_protocol_list sep local_list

and aux_local_protocol_list sep = function
  | [] -> pp_close ()
  | [g] -> 
      pp_string "{"; 
      pp_close ();
      pp_break 2 2;
      print_local_protocol_body g;
      pp_string "}"; 
      pp_close ()
  | g::r -> 
      pp_string "{"; 
      pp_close ();
      pp_break  2 2;
      print_local_protocol_body g;
      pp_hbox ();
      pp_string "}";
      pp_space ();
      pp_string sep;
      pp_space ();
      aux_local_protocol_list sep r

  

let print_roles roles = 
  pp_box 2;
  let rec aux = function
      [] -> pp_close ()
    | [r] -> pp_string r;pp_close ()
    | r::roles -> pp_string r;pp_string ",";aux roles
  in aux roles


let print_as_protocol : as_protocol -> unit =
  function
    | Localast (name,role_list,protocol_body) -> 
        pp_vbox 0;
        pp_hbox ();
        pp_string "local protocol";
        pp_space ();
        pp_string name;
        pp_space ();
        pp_string "(";
        print_roles role_list;
        pp_string ")";
        pp_space ();
        pp_close ();
        pp_string "{";
        pp_break  2 2;
        print_local_protocol_body protocol_body;
        pp_flush ();
        pp_string "}"
    | Globalast (name,role_list,protocol_body) -> 
        pp_vbox 0;
        pp_hbox ();
        pp_string "global protocol";
        pp_space ();
        pp_string name;
        pp_space ();
        pp_string "(";
        print_roles role_list;
        pp_string ")";
        pp_space ();
        pp_close ();
        pp_string "{";
        pp_break  2 2;
        print_global_protocol_body protocol_body;
        pp_flush ();
        pp_string "}"
        


(* prettys import lists *)
let print_imports imp = 
  pp_vbox 0;
  let print_import (extid,from_opt,as_opt) =
    pp_hbox ();
    pp_string "import";
    pp_space ();
    pp_string extid;
    (match from_opt with
         None -> ()
       | Some id_from ->
           pp_space ();
           pp_string "from";
           pp_space ();
           pp_string id_from);
    (match as_opt with
         None -> ()
       | Some id_as ->
           pp_space ();
           pp_string "as";
           pp_space ();
           pp_string id_as);
    pp_string ";";
    pp_close ()
  in
  
  let rec aux = function
      [] -> pp_close ()
    | [i] -> 
        print_import i ;
        pp_close ()
    | i::r ->
        print_import i;
        pp_break 0 0;
        aux r
  in
  aux imp;
  pp_flush ()
        
        

let print_ast ((imports,as_protocol):ast) =
  pp_vbox 0;
  print_imports imports;
  pp_break 0 0;
  print_as_protocol as_protocol;
  get_string ()
      

(*******************************************)
(* Pretty-printing for conversation graphs *)
(*******************************************)

open Conversation

let print_globaltype (g:globaltype) =

  let rec print_global = function
    | GEnd (n) -> ()
    | GGoto (n) -> 
      pp_string (Printf.sprintf "continue %d;" n);
      pp_break 1 0;
    | GMsg (n,(message_op,payload),role_1,role_2,g) ->
      pp_string (Printf.sprintf "%d::%s (%s) from %s to %s;"
                   n message_op payload role_1 role_2);
      pp_break 1 0;
      print_global g
    | GPar (n, global_list, g) ->
      pp_vbox 2;
      pp_string (Printf.sprintf "%d::par {" n);
      pp_break 1 0;
      print_global_list "and" global_list;
      pp_break 1 0;
      print_global g
    | GChoice (n, role, global_list, g) -> 
      pp_vbox 2;
      pp_string (Printf.sprintf "%d::choice at %s {" n role);
      pp_break 1 0;
      print_global_list "or" global_list;
      pp_break 1 0;
      print_global g
    | GJoin (n,g) -> 
        pp_string (Printf.sprintf "%d::join;" n);
        pp_break 1 0;
        print_global g
    | GMerge (n,g) -> 
        pp_string (Printf.sprintf "%d::merge;" n);
        pp_break 1 0;
        print_global g
  and print_global_list sep = function
    | [] -> assert false
    | [g] -> 
      print_global g;
      pp_close ();
      pp_string "}"
    | g::r -> 
      print_global g;
      pp_close ();
      pp_vbox 0;
      pp_string (Printf.sprintf "} %s {" sep);
      pp_break 1 0;
      print_global_list sep r

  in
  pp_vbox 0;
  print_global g;
  get_string ()
  

let print_localtype (t:localtype) =

  let rec print_local = function
    | TEnd (n) -> ()
    | TGoto (n) -> 
      pp_string (Printf.sprintf "continue %d;" n);
      pp_break 1 0;
    | TSend (n,(message_op,payload),role,g) ->
      pp_string (Printf.sprintf "%d::%s (%s) to %s;"
                   n message_op payload role);
      pp_break 1 0;
      print_local g
    | TRecv (n,(message_op,payload),role,g) ->
      pp_string (Printf.sprintf "%d::%s (%s) from %s;"
                   n message_op payload role);
      pp_break 1 0;
      print_local g
    | TPar (n, local_list, g) ->
      pp_vbox 2;
      pp_string (Printf.sprintf "%d::par {" n);
      pp_break 1 0;
      print_local_list "and" local_list;
      pp_break 1 0;
      print_local g
    | TChoice (n, role, local_list, g) -> 
      pp_vbox 2;
      pp_string (Printf.sprintf "%d::choice at %s {" n role);
      pp_break 1 0;
      print_local_list "or" local_list;
      pp_break 1 0;
      print_local g
    | TNop (n,g) -> 
      pp_string (Printf.sprintf "%d::" n);
      pp_break 1 0;
      print_local g
    | TJoin (n,g) -> 
        pp_string (Printf.sprintf "%d::join;" n);
        pp_break 1 0;
        print_local g
    | TMerge (n,g) -> 
        pp_string (Printf.sprintf "%d::merge;" n);
        pp_break 1 0;
        print_local g
  and print_local_list sep = function
    | [] -> assert false
    | [g] -> 
      print_local g;
      pp_close ();
      pp_string "}"
    | g::r -> 
      print_local g;
      pp_close ();
      pp_vbox 0;
      pp_string (Printf.sprintf "} %s {" sep);
      pp_break 1 0;
      print_local_list sep r

  in
  pp_vbox 0;
  print_local t;
  get_string ()
