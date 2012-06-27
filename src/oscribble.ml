(********************************************************************)
(* ocaml-scribble - Main file                                       *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)


let version=
  let v = Version.version in
  Printf.sprintf 
    "Antelatex version: %s" v

let debug = Common.debug "Main"
let fulldebug = Common.fulldebug "Main"

let msg_usage = 
  "Usage: oscribble [OPTIONS] FILE\n"^
    "Processes the Scribble FILE(s) according to the OPTIONS."


type action = 
  | Parse
  | Check
  | Project of string
 
let command_flag = ref true
let scribble_file = ref None
let action = ref None


let speclist = Arg.align
  [ ("-v", 
     Arg.Unit Common.switch_debug_on,
     " verbose mode");
    ("-vv", 
     Arg.Unit Common.switch_fulldebug_on,
     " verbose mode with additional output");
    ("--version",
     Arg.Unit (function () -> print_string version; exit 0),
     " outputs the version number and exits");
    ("--parse",
     Arg.Unit (fun () -> 
                 if !command_flag 
                 then (command_flag := false;
                       action := Some Parse)
                 else (
                   prerr_string 
                     (Printf.sprintf 
                        "only one action is allowed");
                   exit 1)),
     " checks the syntactic correctness of a Scribble FILE");
    ("--check",
     Arg.Unit (fun () -> 
                 if !command_flag 
                 then (command_flag := false;
                       action := Some Check)
                 else (
                   prerr_string 
                     (Printf.sprintf 
                        "only one action is allowed");
                   exit 1)),
     " checks the well-formedness of a Scribble FILE");
    ("--project",
     Arg.String (fun s -> 
                   if !command_flag 
                   then (command_flag := false;
                         action := Some (Project (s)))
                   else (
                     prerr_string 
                       (Printf.sprintf 
                          "only one action is allowed");
                     exit 1)),
     " followed by ROLE, projects a Scribble FILE to a given ROLE");
  ]
 

let rec parse_until_end chan accum = 
  try 
    let s = input_char chan in
    if s = '\r' then assert false 
    else
      (parse_until_end chan (accum^(String.make 1 s)))
  with End_of_file -> accum


let main () =
  Arg.parse
    speclist
    (fun s -> if !command_flag 
                then (
                  prerr_string 
                    (Printf.sprintf 
                       "One action should be specified");
                  exit 1)
                else (
                  if !scribble_file = None
                  then scribble_file := Some s
                  else (prerr_string 
                          (Printf.sprintf 
                             "Only one Scribble file is supported");
                        exit 1)
                )
(*
(prerr_string 
                 (Printf.sprintf 
                    "additional arguments are not allowed\n");
               exit 1)
*)
    )
    msg_usage;
  let () = debug "Starting oscribble" in
  let file = match !scribble_file with
    | None -> (prerr_string 
                 (Printf.sprintf 
                    "No scribble file is present as argument\n");
               exit 1)
    | Some file -> file in
  let chan = open_in file in
  let raw_scribble = parse_until_end chan "" in
  fulldebug ("Session read: \n"^raw_scribble) ;
  debug "Compilation starting" ;
  let lexbuf = Lexing.from_string raw_scribble in
  debug "Lexer built" ;
  let sessionast =
    (try 
       Parser.scribbleprotocol Lexer.token lexbuf
     with
         Common.Syntax_error (s,i) ->
	   (prerr_string ("Syntax error: "^s^" "^(Common.info_to_string i)^"\n");
            exit 1)
       | Common.Parse_error (s,i)  ->
	   (prerr_string ("Parsing error: "^s^" "^(Common.info_to_string i)^"\n"); 
            exit 2)
    ) in
  let () = close_in chan in
  let () = debug ("Protocol parsed:\n"^(Prettyprint.print_ast sessionast))
  in
  (match sessionast,!action with
     | (imports,Syntax.Globalast (name,role_list,protocol_body)), Some Parse ->
         ()
     | (imports,Syntax.Globalast (name,role_list,protocol_body)), Some Check ->
         let g = Conversation.global_conversion protocol_body in
         let () = fulldebug ("Global type:\n"^(Prettyprint.print_globaltype g))
         in
         let wf = Wellformedness.check g in
         let () = debug ("Wellformed: "^(string_of_bool wf))
         in         
         ()
     | (imports,Syntax.Globalast (name,role_list,protocol_body)), Some (Project role) ->
         let g = Conversation.global_conversion protocol_body in
         let () = fulldebug ("Global type:\n"^(Prettyprint.print_globaltype g))
         in
         let wf = Wellformedness.check g in
         let () = debug ("Wellformed: "^(string_of_bool wf))
         in
         let tr = Projection.project role g in
         let () = fulldebug ("Raw local type:\n"^(Prettyprint.print_localtype tr)) in
         let tc = Projection.clean_local_role tr in
         let () = fulldebug ("Cleaned Local type:") in
         let t  = Conversation.localnodetoAST tc in
         let () = debug ("Local type:") in
         (print_string (Prettyprint.print_ast (imports,Syntax.Localast (name,role_list,t))^"\n"))
     | (imports,Syntax.Localast (name,role_list,protocol_body)), Some Check ->
         let t = Conversation.local_conversion protocol_body in
         let () = debug ("Local type:\n"^(Prettyprint.print_localtype t)) in
         ()
     | _ -> ()
  );     
  let () = debug "Closing oscribble" in
  ()
  
let () = 
  main ()
