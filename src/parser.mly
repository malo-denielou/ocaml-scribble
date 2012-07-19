%{
(********************************************************************)
(* ocaml-scribble - Parser file                                     *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

  open Syntax
  open Lexing

  let debug = Common.fulldebug "Parser"
 
  let error t info =
    debug ("Error: "^t) ;
    let (l,c1),(_,c2) = info in
    let s = Printf.sprintf "%d:%d-%d" l c1 c2 in
    if t = "" then raise (Common.Parse_error (s,info))
    else raise (Common.Parse_error ((*s^ ": " ^ *)t,info))

  let parse_error _ =
    let start_pos = Parsing.symbol_start_pos () in
    let end_pos = Parsing.symbol_end_pos () in
    let (l1,c1),(l2,c2) =
      (start_pos.pos_lnum,start_pos.pos_bol),(end_pos.pos_lnum,end_pos.pos_bol)
    in
(*    let s = Printf.sprintf "%d:%d-%d" l1 c1 c2 in*)
    raise (Common.Parse_error ("",((l1,c1),(l2,c2))))

%}

%token <Common.info> AND AS AT BY CHOICE CONTINUE FROM INTERRUPTIBLE
%token <Common.info> GLOBAL LOCAL OR PAR PROTOCOL REC ROLE SIG TO WITH
%token <Common.info> EOF
%token <Common.info * string> IDENTIFIER DIGOPERATOR
%token <Common.info * string> IMPORT
%token <Common.info> SEMI EQUAL COLON COMMA
%token <Common.info> LPA RPA LCB RCB LAB RAB

%start scribbleprotocol
%type <Syntax.ast> scribbleprotocol

%%

scribbleprotocol:
| typedecl protocol { ($1,$2)}

typedecl:
| IMPORT FROM IDENTIFIER AS IDENTIFIER SEMI typedecl { (snd $1,
                                                        Some (snd $3),
                                                        Some (snd $5))::$7}
| IMPORT FROM IDENTIFIER SEMI typedecl               { (snd $1,Some (snd $3),None)::$5}
| IMPORT AS IDENTIFIER SEMI typedecl                 { (snd $1,None,Some (snd $3))::$5}
| IMPORT SEMI typedecl                               { (snd $1,None,None)::$3 }
|                                                    { [] }

protocol:
| GLOBAL PROTOCOL IDENTIFIER parameters LPA roles RPA globalprotocolbody
    { Globalast (snd $3,$4,$6,$8) }
| LOCAL PROTOCOL IDENTIFIER parameters LPA roles RPA localprotocolbody
    { Localast (snd $3,$4,$6,$8) }

parameters:
| LAB paramlist RAB { $2 }
| LAB RAB { [] }
|         { [] }

paramlist:
| SIG IDENTIFIER COMMA paramlist { (snd $2)::$4 }
| SIG DIGOPERATOR COMMA paramlist { (snd $2)::$4 }
| SIG IDENTIFIER  { [snd $2] }
| SIG DIGOPERATOR  { [snd $2] }

roles:
| IDENTIFIER COMMA roles { (snd $1)::$3 }
| IDENTIFIER             { [snd $1] }
| ROLE IDENTIFIER COMMA roles { (snd $2)::$4 }
| ROLE IDENTIFIER             { [snd $2] }
|                        { [] }

globalprotocolbody:
| globalinteractionblock  { $1 }

globalinteractionblock:
| LCB globalinteractionsequence RCB { $2 }

globalinteractionsequence:
| message globalinteractionsequence     { GASSeq ($1,$2) }
| choice globalinteractionsequence      { GASSeq ($1,$2) }
| parallel globalinteractionsequence    { GASSeq ($1,$2) }
| recursion globalinteractionsequence   { GASSeq ($1,$2) }
| continue globalinteractionsequence    { GASSeq ($1,$2) }
| interrupt globalinteractionsequence   { GASSeq ($1,$2) }
| globalinteractionblock globalinteractionsequence { GASSeq ($1,$2) }
|                                       { GASEnd }

message:
| messagesignature FROM rolename TO rolename SEMI 
    { let (info,(op,payload)) = $1 in
      GASMsg (Common.merge_info info $6, (op,payload), snd $3, snd $5) }
| messagesignature
        { let (info,(op,payload)) = $1 in
          error "Wrong declaration of a message exchange" info }

choice:
| CHOICE AT rolename globalinteractionblock listglobalinteractionblockor
    { GASChoice ($1,snd $3,$4::$5) }
| CHOICE AT rolename globalinteractionblock listglobalinteractionblockor SEMI
    { GASChoice ($1,snd $3,$4::$5) }

listglobalinteractionblockor:
| OR globalinteractionblock listglobalinteractionblockor  { $2::$3 }
| OR globalinteractionblock                               { [$2] }

parallel:
| PAR globalinteractionblock listglobalinteractionblockpar
    { GASPar ($1,$2::$3) }
| PAR globalinteractionblock listglobalinteractionblockpar SEMI
    { GASPar ($1,$2::$3) }

listglobalinteractionblockpar:
| AND globalinteractionblock listglobalinteractionblockpar { $2::$3 }
| AND globalinteractionblock                               { [$2] }

recursion:
| REC IDENTIFIER globalinteractionblock
    { GASRec ($1,snd $2,$3) }

continue:
| CONTINUE IDENTIFIER SEMI
    { GASCont ($1,snd $2) }

interrupt:
| INTERRUPTIBLE globalinteractionblock interruptlist
    { GASInterrupt ($1,$2,$3) }

interruptlist:
| BY rolename WITH messagesignature COMMA interruptlist
    {(snd $2,snd $4)::$6 }
| BY rolename WITH messagesignature SEMI
        { [(snd $2,snd $4)] }

localprotocolbody:
| localinteractionblock  { $1 }

localinteractionblock:
| LCB localinteractionsequence RCB { $2 }

localinteractionsequence:
| send localinteractionsequence        { LASSeq ($1,$2) }
| receive localinteractionsequence     { LASSeq ($1,$2) }
| lchoice localinteractionsequence      { LASSeq ($1,$2) }
| lparallel localinteractionsequence    { LASSeq ($1,$2) }
| lrecursion localinteractionsequence   { LASSeq ($1,$2) }
| lcontinue localinteractionsequence    { LASSeq ($1,$2) }
| linterrupt localinteractionsequence    { LASSeq ($1,$2) }
|                                      { LASEnd }

send:
| messagesignature TO rolename SEMI
    { let (info,(op,payload)) = $1 in
      LASSend (Common.merge_info info $4, (op,payload), snd $3) }

receive:
| messagesignature FROM rolename SEMI
    { let (info,(op,payload)) = $1 in
      LASRecv (Common.merge_info info $4, (op,payload), snd $3) }

lchoice:
| CHOICE AT rolename localinteractionblock listlocalinteractionblockor
    { LASChoice ($1,snd $3,$4::$5) }

listlocalinteractionblockor:
| OR localinteractionblock listlocalinteractionblockor  { $2::$3 }
|                                                         { [] }

lparallel:
| PAR localinteractionblock listlocalinteractionblockpar
    { LASPar ($1,$2::$3) }

listlocalinteractionblockpar:
| AND localinteractionblock listlocalinteractionblockpar { $2::$3 }
|                                                          { [] }

lrecursion:
| REC IDENTIFIER localinteractionblock
    { LASRec ($1,snd $2,$3) }

lcontinue:
| CONTINUE IDENTIFIER SEMI
    { LASCont ($1,snd $2) }

linterrupt:
| INTERRUPTIBLE localinteractionblock linterruptlist
    { LASInterrupt ($1,$2,$3) }

linterruptlist:
| BY rolename WITH messagesignature COMMA linterruptlist
    {(snd $2,snd $4)::$6 }
| BY rolename WITH messagesignature SEMI
        { [(snd $2,snd $4)] }


rolename:
| IDENTIFIER { $1 }

messagesignature:
| IDENTIFIER LPA IDENTIFIER RPA   { (Common.merge_info (fst $1) $4,(snd $1,snd $3)) }
| IDENTIFIER LPA RPA              { (Common.merge_info (fst $1) $3,(snd $1,"")) }
| DIGOPERATOR LPA IDENTIFIER RPA  { (Common.merge_info (fst $1) $4,(snd $1,snd $3)) }
| DIGOPERATOR LPA RPA             { (Common.merge_info (fst $1) $3,(snd $1,"")) }
| LPA IDENTIFIER RPA              { (Common.merge_info $1 $3,("",snd $2)) }
| LPA RPA                         { (Common.merge_info $1 $2,("","")) }
| IDENTIFIER                      { (fst $1,(snd $1,"")) }
