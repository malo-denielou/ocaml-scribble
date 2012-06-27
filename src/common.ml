(********************************************************************)
(* ocaml-scribble - Common                                          *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)


let debug_var = ref false
let fulldebug_var = ref false

let debug modul msg =
  if !debug_var then begin
    print_string ("["^modul^"] ");
    print_string (msg);
    print_newline ();
    flush stdout
  end
  else ()

let fulldebug modul msg =
  if !fulldebug_var then begin
    print_string ("["^modul^"] ");
    print_string (msg);
    print_newline ();
    flush stdout
  end
  else ()

let switch_debug_on () = debug_var := true
let switch_fulldebug_on () = debug_var := true;fulldebug_var := true

(* Positionning *)

type pos = int * int
type info = pos * pos

let bogusInfo = ((0,0),(0,0))

let info_to_string ((l1,c1),(l2,c2)) =
  let s = Printf.sprintf "line %d, char %d, to line %d, char %d" l1 c1 l2 c2 in
  s

let merge_info (a,_) (_,d) = (a,d)

let posnewline (l,c) = (l+1,0)
let posaddchar (l,c) n = (l,c+n)


(* Substitution *)
let alias subst =
  function n ->
    if List.mem_assoc n subst 
    then List.assoc n subst
    else n

let rec re_alias subst = function n ->
  let m = alias subst n in
  if n = m then m else re_alias subst m


(* Errors *)
exception Syntax_error of string*info
exception Parse_error of string*info

