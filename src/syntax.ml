(********************************************************************)
(* ocaml-scribble - Syntax file                                     *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)


open Common

(************************************)
(* Abstract syntax for source files *)
(************************************)

type role_name = string

type message_op = string

type message_sig = message_op * string

type as_role = role_name

type role = role_name
type roles = role_name list

type as_global_protocol_body =
  | GASEnd
  | GASMsg of (info * message_sig * role_name * role_name)
  | GASSeq of (as_global_protocol_body * as_global_protocol_body)
  | GASChoice of (info * role_name * (as_global_protocol_body list))
  | GASPar of (info * (as_global_protocol_body list))
  | GASRec of (info * string * as_global_protocol_body)
  | GASCont of (info * string)
  | GASInterrupt of (info * as_global_protocol_body * ((role_name * message_sig) list))

type as_global =
    string * roles * as_global_protocol_body

type as_local_protocol_body =
  | LASEnd
  | LASSend of (info * message_sig * role_name)
  | LASRecv of (info * message_sig * role_name)
  | LASSeq of (as_local_protocol_body * as_local_protocol_body)
  | LASChoice of (info * role_name * (as_local_protocol_body list))
  | LASPar of (info * (as_local_protocol_body list))
  | LASRec of (info * string * as_local_protocol_body)
  | LASCont of info * string
  | LASInterrupt of (info * as_local_protocol_body * ((role_name * message_sig) list))

type as_local =
    string * roles * as_local_protocol_body

type as_protocol =
    Localast of as_local
  | Globalast of as_global

type as_import =
    (string * (string option) * (string option)) list

type ast =
    as_import * as_protocol
    
